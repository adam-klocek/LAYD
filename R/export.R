# Escaped, standalone HTML reports and portable results bundles.
html_table <- function(x, limit = 100) {
  if (is.null(x) || !nrow(x)) return(htmltools::tags$p("No entries."))
  z <- head(x, limit)
  htmltools::tagList(htmltools::tags$table(class = "report-table",
    htmltools::tags$thead(htmltools::tags$tr(lapply(names(z), htmltools::tags$th))),
    htmltools::tags$tbody(lapply(seq_len(nrow(z)), function(i) htmltools::tags$tr(lapply(z[i, ], function(v) {
      if (is.numeric(v)) v <- ifelse(is.finite(v), round(v, 4), "NA")
      htmltools::tags$td(as.character(v))
    }))))), if (nrow(x) > limit) htmltools::tags$p(paste("First", limit, "entries shown. Full tables are in the results ZIP.")))
}
write_report <- function(r, file, source_label = "Uploaded data") {
  plot_ids <- c("likert", "correlations", "missing", "item_rest", "scree", "loadings")
  embed_plot <- function(p) {
    tf <- tempfile(fileext = ".png"); on.exit(unlink(tf))
    ggplot2::ggsave(tf, p, width = 9, height = 4.5, dpi = 120, bg = "white")
    raw <- readBin(tf, "raw", n = file.info(tf)$size)
    htmltools::tags$img(src = paste0("data:image/png;base64,", jsonlite::base64_enc(raw)), alt = "LAYD diagnostic chart", style = "max-width:100%;height:auto")
  }
  sections <- lapply(unique(r$summaries$scale), function(scale) {
    items <- r$summaries$item[r$summaries$scale == scale]
    m <- r$models[[scale]]
    htmltools::tags$section(htmltools::tags$h2(paste("Scale", scale)),
      html_table(r$summaries[r$summaries$scale == scale, ]),
      embed_plot(plot_likert(r, scale)), embed_plot(plot_item_rest(r, scale)),
      embed_plot(plot_correlation(r$item_cor[items, items, drop = FALSE])),
      embed_plot(plot_missing(r, scale)), embed_plot(plot_scree(r, scale)),
      htmltools::tags$h3("Factor model"), htmltools::tags$p(paste(m$status, m$reason, paste(m$warnings, collapse = "; "))),
      html_table(m$measures), if (!is.null(m$loading)) embed_plot(plot_loadings(r, scale)))
  })
  doc <- htmltools::tags$html(lang = "en", htmltools::tags$head(htmltools::tags$meta(charset = "utf-8"),
    htmltools::tags$title("LAYD data sanity report"), htmltools::tags$style(htmltools::HTML(
      "body{font:14px/1.65 system-ui,sans-serif;max-width:1100px;margin:auto;padding:40px;color:#17243b}h1{font-size:38px;color:#7048f4}h2{margin-top:40px;border-bottom:3px solid #00a6a6;padding-bottom:10px}.intro{background:#f0ebff;padding:24px;border-radius:12px}.report-table{border-collapse:collapse;display:block;overflow:auto;font-size:11px;width:100%;margin:18px 0}.report-table th,.report-table td{padding:8px 11px;text-align:left;border-bottom:1px solid #e4e8f0}.report-table th{background:#f1edff}img{page-break-inside:avoid}section{margin-top:30px}a{color:#7048f4}@media print{body{padding:0}.report-table{font-size:9px}}"))),
    htmltools::tags$body(htmltools::tags$h1("Look at Your Data"), htmltools::tags$p("Psychometric data sanity report"),
      htmltools::tags$div(class = "intro", htmltools::tags$strong(source_label), htmltools::tags$p(paste("Run:", r$created)),
        htmltools::tags$p(paste(r$dimensions[["rows"]], "observations;", ncol(r$prepared), "selected items;", length(unique(r$summaries$scale)), "scales.")),
        htmltools::tags$p("Findings are screening signals. Clear results do not certify error-free data. No rows were excluded or imputed. Invalid item values were logged and withheld; declared reverse keys were applied to a separate analysis copy.")),
      htmltools::tags$h2("Checks requiring review"), html_table(r$findings[r$findings$status == "Review", ]),
      htmltools::tags$h2("Unassessed checks"), html_table(r$findings[r$findings$status == "Not assessed", ]),
      embed_plot(plot_family(r)), sections,
      htmltools::tags$h2("Scale relationships"), embed_plot(plot_correlation(r$scale_cor)), html_table(r$external),
      htmltools::tags$h2("Group summaries"), html_table(r$groups),
      htmltools::tags$h2("Import provenance"), htmltools::tags$pre(jsonlite::toJSON(r$provenance, pretty = TRUE, auto_unbox = TRUE, na = "null")),
      htmltools::tags$h2("Settings used"), htmltools::tags$pre(jsonlite::toJSON(r$settings, pretty = TRUE, auto_unbox = TRUE, na = "null")),
      htmltools::tags$h2("Codebook used"), html_table(r$codebook, 150),
      htmltools::tags$p("Adapted from the supplied LAYD manuscript (Table 1) and commented R pipeline. Configured thresholds are heuristics. Formal measurement invariance, dynamic fit-index simulation, imputation, external-source scraping, cross-file merge checks, and longitudinal time-order checks are not implemented.")))
  writeLines(as.character(htmltools::tagList(htmltools::HTML("<!DOCTYPE html>"), doc)), file, useBytes = TRUE)
}
write_bundle <- function(r, file, source_label = "Uploaded data") {
  dir <- tempfile("layd_results_"); dir.create(dir); on.exit(unlink(dir, recursive = TRUE))
  tables <- list(findings = r$findings, cell_audit = r$cells, row_flags = r$row_flags,
    item_properties = r$summaries, analysis_items = data.frame(row = seq_len(nrow(r$prepared)), r$prepared, check.names = FALSE),
    scale_scores = r$scores, reliability = r$reliability, group_summaries = r$groups,
    response_styles = r$response_style, external_comparisons = r$external, codebook = r$codebook,
    name_audit = r$name_audit, expected_correlations = r$expectations, prior_item_means = r$priors)
  for (name in names(tables)) if (!is.null(tables[[name]])) write.csv(tables[[name]], file.path(dir, paste0(name, ".csv")), row.names = FALSE, na = "")
  write.csv(r$item_cor, file.path(dir, "item_correlations.csv"))
  write.csv(r$scale_cor, file.path(dir, "scale_correlations.csv"))
  model_tables <- lapply(names(r$models), function(scale) {
    m <- r$models[[scale]]
    if (is.null(m$measures)) return(NULL)
    cbind(scale = scale, m$measures)
  })
  model_tables <- Filter(Negate(is.null), model_tables)
  if (length(model_tables)) write.csv(do.call(rbind, model_tables), file.path(dir, "cfa_fit.csv"), row.names = FALSE)
  details <- lapply(r$models, function(m) m[c("status", "reason", "warnings", "omega_note", "loading", "measures", "admissible")])
  saveRDS(list(settings = r$settings, codebook = r$codebook, expectations = r$expectations,
    priors = r$priors, provenance = r$provenance, models = details, parallel = r$parallel, created = r$created), file.path(dir, "run_configuration.rds"))
  jsonlite::write_json(r$settings, file.path(dir, "settings.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
  writeLines(c(paste("LAYD run:", r$created), paste("Source:", source_label),
    "Original metadata are omitted from analysis_items and scale_scores; row is the original row number.",
    "Cell audit includes original response values and may contain sensitive data. Review all files before sharing.",
    "All participants are retained. Invalid values become NA in the analysis copy; declared reverse keys are applied.",
    "The bundle does not contain the full original dataset. To reproduce, use the same original data with the code below.",
    "Restore the app's R dependencies (see install.R); run reproduce.R from this folder."), file.path(dir, "README.txt"))
  file.copy(c("R/io.R", "R/checks.R"), dir)
  writeLines(c("source('io.R')", "source('checks.R')", "config <- readRDS('run_configuration.rds')",
    "p <- config$provenance", "args <- commandArgs(trailingOnly = TRUE)",
    "if (identical(p$kind, 'synthetic')) {",
    "  d <- make_demo(p$seed, p$rows)",
    "} else {",
    "  if (!length(args)) stop('Run: Rscript reproduce.R path/to/original_file')",
    "  if (!identical(unname(tools::md5sum(args[1])), p$md5)) stop('The original file checksum does not match this run.')",
    "  d <- read_upload(args[1], p$filename, object = p$object, sep = p$separator, dec = p$decimal, encoding = p$encoding)",
    "}",
    "result <- analyze_layd(d, config$codebook, config$settings, config$expectations, config$priors)",
    "write.csv(result$findings, 'reproduced_findings.csv', row.names = FALSE)"), file.path(dir, "reproduce.R"))
  writeLines(capture.output(sessionInfo()), file.path(dir, "sessionInfo.txt"))
  write_report(r, file.path(dir, "report.html"), source_label)
  zip::zipr(zipfile = file, files = list.files(dir, full.names = TRUE), root = dir)
}
