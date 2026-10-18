# Requires installed app dependencies; run from project root.
source("R/io.R"); source("R/checks.R"); source("R/plots.R"); source("R/export.R")
assert <- function(ok, msg) { if (!isTRUE(ok)) stop(msg, call. = FALSE); cat("PASS:", msg, "\n") }
d <- make_demo(); cb <- demo_codebook(d)
s <- list(id = "id", group = "language", age = "age", consent = "consent", attention = "attention", duration = "duration", test = "test_account", run_cfa = TRUE)
r <- analyze_layd(d, cb, s, read.csv("examples/expected_correlations.csv"))
r$provenance <- list(kind = "synthetic", seed = 42L, rows = 320L)
assert(all(vapply(r$models, function(m) !is.null(m$measures), logical(1))), "Both demo CFAs converge and return fit indices")
assert(all(vapply(r$models, function(m) is.finite(m$measures$omega), logical(1))), "Both demo scales return model-based omega")
assert(r$models$A$loading$loading[r$models$A$loading$item == "A3"] < 0, "CFA highlights omitted reverse key")
assert(all(vapply(r$models, function(m) is.finite(m$measures$srmr), logical(1))), "CFA reports finite SRMR")
mlr <- analyze_layd(d, cb, modifyList(s, list(estimator = "MLR")))
assert(!is.null(mlr$models$B$measures) && mlr$models$B$measures$n >= r$models$B$measures$n,
  "MLR/FIML retains at least as many cases as complete-case WLSMV")
assert(setequal(import_choices("examples/import_example.xlsx", "test.xlsx"), c("Items", "Metadata")), "Excel import lists both worksheets")
xl <- read_upload("examples/import_example.xlsx", "test.xlsx", "Items")
assert(nrow(xl) == 3 && xl$Q3[3] == 5 && is.na(xl$Q2[2]), "Excel sheet imports values and missing cells")
plots <- list(plot_family(r), plot_missing(r, "A"), plot_missing_bars(r), plot_likert(r, "A"),
  plot_correlation(r$item_cor), plot_item_rest(r, "A"), plot_scree(r, "A"), plot_loadings(r, "A"), plot_path(r, "A"),
  plot_group(r, "A"), plot_group(r, "A", "mean"), plot_group(r, "A", "missing_pct"))
for (p in plots) {
  invisible(ggplot2::ggplot_build(p))
  invisible(plotly::plotly_build(plotly::ggplotly(p)))
}
assert(TRUE, "All diagnostic charts build as interactive Plotly objects")
# Report and ZIP export exercise actual graphics rendering and embedded image generation.
tf <- tempfile(fileext = ".html"); write_report(r, tf, "Demo <escaped>")
html <- paste(readLines(tf), collapse = "\n")
assert(grepl("data:image/png;base64", html, fixed = TRUE), "Standalone report contains embedded chart images")
assert(grepl("Demo &lt;escaped&gt;", html, fixed = TRUE), "Report escapes source labels")
assert(grepl("Declared missing codes", html, fixed = TRUE) || grepl("Range and response coding", html, fixed = TRUE), "Report contains diagnostic findings")
zipfile <- tempfile(fileext = ".zip"); write_bundle(r, zipfile, "Synthetic demo")
contents <- zip::zip_list(zipfile)$filename
assert(all(c("report.html", "findings.csv", "analysis_items.csv", "scale_scores.csv", "run_configuration.rds", "reproduce.R", "sessionInfo.txt") %in% basename(contents)), "Result bundle includes report, audit, scores, config, code, and session")
unlink(c(tf, zipfile))
# Test the native server without a browser; this executes real reactive outputs.
source("app.R")
assert(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"), "Shiny UI constructs successfully")
shiny::testServer(server, {
  session$setInputs(scale = "A", item = "A1", group_metric = "item_rest", distribution_view = "hist",
    id = "id", group = "language", age = "age", consent = "consent", attention = "attention", duration = "duration", test = "test_account")
  session$flushReact()
  assert(!is.null(rv$result) && nrow(rv$result$scores) == 320, "Shiny initializes an actual computed demo")
  session$setInputs(run = 1)
  session$flushReact()
  assert(!is.null(rv$result$models$A$measures), "Run action performs CFA in the server")
  assert(identical(rv$signature, signature()), "Completed run captures current configuration")
  session$setInputs(sentinels = "-999,-998")
  session$flushReact()
  assert(!identical(rv$signature, signature()), "Settings changes make previous results stale")
  session$setInputs(run = 2)
  session$flushReact()
  assert(identical(rv$signature, signature()), "Rerunning refreshes settings snapshot")
  # Raw current codebook differs from the result snapshot after editing a key.
  session$setInputs(codebook_table_cell_edit = list(row = 3, col = 5, value = "TRUE"))
  session$flushReact()
  assert(isTRUE(rv$codebook$reverse[3]) && !isTRUE(rv$result$codebook$reverse[3]), "Codebook editor updates the chosen key and preserves prior run snapshot")
  session$setInputs(run = 3)
  session$flushReact()
  assert(rv$result$summaries$item_rest[3] > .4, "Rerun from edited UI codebook corrects the planted-key signal")
  # Render all major result types to catch dependency/API failures.
  for (nm in c("metrics", "run_status", "priority", "model_metrics", "model_notice", "findings", "descriptives",
               "family_plot", "likert_plot", "item_rest_plot", "missing_map", "missing_bars", "item_heatmap",
               "scale_heatmap", "loadings", "scree", "path", "group_plot", "distribution")) {
    value <- output[[nm]]
    assert(!is.null(value), paste("Reactive output renders:", nm))
  }
})
cat("All integration checks passed.\n")
