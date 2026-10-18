# Run from this directory with shiny::runApp().
required <- c("shiny", "bslib", "DT", "ggplot2", "plotly", "readxl", "jsonlite", "htmltools", "zip")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "), ". Run source('install.R') first.")
library(shiny)
library(bslib)
source("R/io.R", local = TRUE)
source("R/checks.R", local = TRUE)
source("R/plots.R", local = TRUE)
source("R/export.R", local = TRUE)
options(shiny.maxRequestSize = 50 * 1024^2)
chart <- function(id, title, caption = NULL, height = "340px") card(
  card_header(title), plotly::plotlyOutput(id, height = height), if (!is.null(caption)) div(class = "chart-note", caption))
note <- function(...) div(class = "soft-note", ...)
metric <- function(value, label, color = "violet", subtitle = NULL) div(class = paste("metric", color), div(class = "metric-label", label),
  div(class = "metric-value", value), div(class = "metric-sub", subtitle))
theme <- bs_theme(version = 5, bg = "#F4F6FB", fg = "#17243B", primary = "#7048F4", secondary = "#00A6A6",
  success = "#008F88", danger = "#E83259", base_font = "system-ui", border_radius = ".85rem")
ui <- page_sidebar(
  title = div(class = "brand", span(class = "brand-mark", "L"), span("LAYD", span(class = "brand-tag", "Look at Your Data"))),
  theme = theme, fillable = FALSE,
  tags$head(tags$link(rel = "stylesheet", href = "style.css")),
  sidebar = sidebar(width = 295, bg = "#FFFFFF", open = "desktop",
    div(class = "sidebar-intro", span(class = "eyebrow", "YOUR RESEARCH WORKSPACE"), h4("A closer look starts here.")),
    div(class = "step-label", span("01"), "Bring your data"),
    fileInput("upload", NULL, accept = c(".csv", ".xls", ".xlsx", ".RData", ".rda"), buttonLabel = "Choose file", placeholder = "CSV, Excel, or RData"),
    uiOutput("object_picker"),
    accordion(open = FALSE, accordion_panel("CSV import options",
      selectInput("sep", "Delimiter", c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t")),
      selectInput("decimal", "Decimal mark", c("Point" = ".", "Comma" = ",")),
      selectInput("encoding", "Encoding", c("UTF-8", "windows-1250", "windows-1252", "latin1")))),
    actionButton("import", "Import selected data", class = "btn-outline-primary w-100"),
    actionButton("demo", "Explore the planted-error demo", class = "btn-link demo-button"),
    uiOutput("source_label"),
    tags$hr(), div(class = "step-label", span("02"), "Set your rules"),
    helpText("Define items and keys in Data setup. Then select a scale to explore."),
    selectInput("scale", "Scale in focus", choices = c("A", "B")),
    textInput("sentinels", "Declared missing-value codes", "-999,-998,-99"),
    accordion(open = FALSE,
      accordion_panel("Identifiers and groups", uiOutput("metadata_controls")),
      accordion_panel("Eligibility rules", uiOutput("eligibility_controls"),
        numericInput("age_min", "Minimum age", 18, min = 0),
        textInput("consent_pass", "Consent pass value", "TRUE"), textInput("attention_pass", "Attention-check pass value", "TRUE"),
        textInput("test_value", "Value identifying a test account", "TRUE"),
        numericInput("duration_min", "Minimum duration (seconds)", 120, min = 0),
        numericInput("duration_max", "Maximum duration (seconds)", 7200, min = 0)),
      accordion_panel("Diagnostic thresholds",
        sliderInput("missing_threshold", "Missingness review threshold", 0, 1, .20, step = .05),
        sliderInput("floor_threshold", "Floor / ceiling review threshold", .5, 1, .80, step = .05),
        sliderInput("rit_threshold", "Minimum item-rest correlation", 0, .5, .20, step = .05),
        sliderInput("reliability_threshold", "Minimum alpha / omega", 0, 1, .70, step = .05),
        sliderInput("min_answered", "Proportion required for a mean score", .1, 1, .80, step = .05),
        numericInput("longstring_threshold", "Minimum identical response run", 5, min = 2, step = 1),
        selectInput("corr_method", "Correlation method", c("Spearman" = "spearman", "Pearson" = "pearson"))),
      accordion_panel("Factor model settings", checkboxInput("run_cfa", "Fit one-factor CFA per scale", TRUE),
        selectInput("estimator", "Estimator", c("Ordinal · WLSMV (complete cases)" = "WLSMV", "Continuous · MLR (FIML)" = "MLR")),
        numericInput("cfi_min", "Minimum CFI", .95, min = 0, max = 1, step = .01),
        numericInput("rmsea_max", "Maximum RMSEA", .08, min = 0, max = 1, step = .01),
        numericInput("srmr_max", "Maximum SRMR", .08, min = 0, max = 1, step = .01))),
    actionButton("run", "Run data checks", icon = icon("play"), class = "btn-primary w-100 run-button"),
    div(class = "sidebar-foot", "Raw data preserved · All recodes logged", tags$br(), "For cross-sectional, unidimensional scales")
  ),
  div(class = "hero", div(class = "hero-copy", span(class = "eyebrow", "FROM RAW RESPONSES TO RESEARCH CONFIDENCE"),
    h1("Know your data.", tags$br(), span("Catch what hides.")),
    p("A visual sanity check for psychometric scales. Follow six families of checks, investigate the signals, and keep every decision traceable.")),
    div(class = "hero-art", div(class = "orb orb-one"), div(class = "orb orb-two"),
      div(class = "signal-card", div(class = "signal-top", span(class = "pulse-dot"), "SIGNAL DETECTED"),
        div(class = "signal-bars", lapply(c(44, 69, 87, 34, 62, 79, 56), function(v) tags$i(style = paste0("height:", v, "px")))),
        div(class = "signal-caption", "Look closer. Ask better questions.")))),
  uiOutput("run_status"),
  navset_card_tab(id = "workspace",
    nav_panel("Overview", value = "overview", uiOutput("metrics"),
      layout_columns(col_widths = c(7, 5), chart("family_plot", "Six families. One clear view.", "Checks are screening signals. “Clear” means no signal at the configured threshold."),
        card(card_header("Start with these signals"), uiOutput("priority")))),
    nav_panel("Data setup", value = "setup",
      note(strong("Confirm your codebook before interpreting results. "), "Scale membership is suggested from names; ranges default to 1–5. Edit min/max, keys, and inclusion to match the instrument. Uploaded items must contain numeric responses or numeric labels."),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Scale definitions"), p("Double-click cells to edit. Use TRUE/FALSE. Item names are read-only; set include to FALSE for metadata."),
          div(class = "inline-controls", numericInput("range_min", "Default minimum", 1), numericInput("range_max", "Default maximum", 5)),
          actionButton("reset_book", "Rebuild suggested codebook"), fileInput("codebook_file", "Or import a codebook CSV", accept = ".csv"),
          downloadButton("codebook_download", "Download current codebook")),
        card(card_header("Check the import"), uiOutput("import_summary"),
          div(class = "inline-controls", numericInput("expected_rows", "Expected rows (optional)", NA, min = 1), numericInput("expected_cols", "Expected columns (optional)", NA, min = 1)),
          note("One row per participant, one column per item. RData supports multiple tabular objects; Excel supports sheet selection."))),
      card(card_header("Editable item codebook"), DT::DTOutput("codebook_table")),
      card(card_header("Imported data preview"), p("First 100 rows. The original values are preserved."), DT::DTOutput("preview"))),
    nav_panel("Items & missingness", value = "items",
      layout_columns(col_widths = c(6, 6), chart("likert_plot", "Every response tells a story", "Valid keyed values; percentages exclude unavailable responses."),
        chart("item_rest_plot", "Which item points the other way?", "Corrected item-rest correlations exclude the focal item from its comparison score.")),
      layout_columns(col_widths = c(6, 6), chart("missing_map", "See the gaps", "Original missing values, declared missing codes, and invalid responses are distinguished."),
        chart("missing_bars", "Where are responses unavailable?", "Dashed line shows your configured missingness threshold.")),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Explore an individual item"), selectInput("item", "Item", c("A1", "A2")),
          selectInput("distribution_view", "View", c("Histogram" = "hist", "Observed response points" = "points")), plotly::plotlyOutput("distribution", height = "300px")),
        chart("item_heatmap", "Within-scale relationships", "Pairwise complete correlations; grey cells are not estimable.")),
      card(card_header("Item properties"), note("Mean position = (keyed mean − declared minimum) / (maximum − minimum). It is descriptive endorsement, not an IRT difficulty parameter. Skewness and excess kurtosis are moment estimates."), DT::DTOutput("descriptives"))),
    nav_panel("Scale structure", value = "structure",
      uiOutput("model_metrics"), uiOutput("model_notice"),
      layout_columns(col_widths = c(6, 6), chart("loadings", "One-factor loadings", "Mixed signs are a prompt to check keys. The latent factor's sign is arbitrary."),
        chart("scree", "Does one dimension look plausible?", "Pearson PCA parallel analysis · 100 simulations · seed 2026 · complete cases.")),
      chart("path", "The scale at a glance", "One-factor CFA per declared scale; standardized paths."),
      card(card_header("Model details and response patterns"), DT::DTOutput("model_table"),
        h5("Response styles"), p("Long strings use valid responses before reversing, in codebook order. These flags do not establish inattention."), DT::DTOutput("response_table"))),
    nav_panel("Groups & validity", value = "validity",
      note("Compare languages or cohorts to find patterns worth investigating. These descriptive comparisons do not establish measurement invariance or a missing-at-random mechanism."),
      selectInput("group_metric", "Group comparison", c("Item-rest correlation" = "item_rest", "Missingness" = "missing_pct", "Keyed item mean" = "mean")),
      layout_columns(col_widths = c(6, 6), chart("group_plot", "Do groups tell the same story?"), chart("scale_heatmap", "Relationships between scales", "Means follow your declared keys and completion rule.")),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Compare with published relationships"), p("CSV columns: scale1, scale2, expected, tolerance. Scale names must match your codebook. A sign mismatch or difference beyond tolerance triggers review."),
          fileInput("expectations_file", "Expected correlations CSV", accept = ".csv"), downloadButton("expectations_template", "Example correlation CSV"), actionButton("clear_expectations", "Clear expectations", class = "btn-link")),
        card(card_header("Compare item means with prior knowledge"), p("CSV columns: item, expected_mean, tolerance. Means are compared after declared keys, in the original response units."),
          fileInput("priors_file", "Expected item means CSV", accept = ".csv"), downloadButton("priors_template", "Example prior-means CSV"), actionButton("clear_priors", "Clear prior means", class = "btn-link"))),
      card(card_header("Expected versus observed correlations"), DT::DTOutput("external_table")),
      card(card_header("Group-level item summaries"), DT::DTOutput("group_table"))),
    nav_panel("Audit & export", value = "audit",
      note("Review findings in the source context. No participant is automatically excluded. Exports contain the last completed run and its settings; rerun after changes."),
      div(class = "export-row", downloadButton("bundle", "Download reproducible results ZIP", class = "btn-primary"), downloadButton("report", "Download HTML report"), downloadButton("scores", "Download scale scores")),
      div(class = "inline-controls", selectInput("status_filter", "Finding status", c("All", "Review", "Clear", "Info", "Not assessed"), "Review"),
        selectInput("family_filter", "Check family", c("All" = "All", setNames(as.character(1:6), FAMILIES)))),
      card(card_header("Findings register"), DT::DTOutput("findings")),
      card(card_header("Rows to investigate"), p("Original row numbers are retained. Metadata/ID values are omitted from score exports."), DT::DTOutput("row_flags")),
      card(card_header("Cell changes in the analysis copy"), DT::DTOutput("cell_audit")),
      card(card_header("Imported column-name audit"), DT::DTOutput("name_audit"))),
    nav_panel("Tutorial guide", value = "guide",
      card(card_header("A practical way to look at your data"),
        tags$ol(tags$li(strong("Import and inspect. "), "Choose a CSV, Excel sheet, or tabular RData object. Check dimensions, decimal settings, encoding, and column names."),
          tags$li(strong("Declare what you know. "), "Confirm item membership, response endpoints, reverse keys, missing codes, and any eligibility rules. Configure each unidimensional subscale separately."),
          tags$li(strong("Run and investigate. "), "Begin with review signals, compare the item plots, and trace unusual values back to the source or codebook."),
          tags$li(strong("Keep a record. "), "Download the report and results bundle. Make source corrections through documented syntax, then rerun the same checks.")),
        h4("The six families from your manuscript"),
        tags$table(class = "table guide-table", tags$thead(tags$tr(tags$th("Family"), tags$th("Included checks"))), tags$tbody(
          tags$tr(tags$td("1 · Values within variables"), tags$td("Ranges, integer responses, missing codes, numeric types, column names, whitespace/case variants, encoding signals.")),
          tags$tr(tags$td("2 · Across observations"), tags$td("Exact records, response-pattern matches, IDs, age, consent, attention checks, duration, test accounts, missingness and long strings.")),
          tags$tr(tags$td("3 · Against prior knowledge"), tags$td("Histograms and observed points, prior item means, group-specific item-rest signs and missingness.")),
          tags$tr(tags$td("4 · Item properties"), tags$td("Means, medians, SD, range, skewness, kurtosis, mean position, floor/ceiling effects and response profiles.")),
          tags$tr(tags$td("5 · Factorial structure"), tags$td("Keyed item correlations, alpha, CFA-based omega, loadings, fit, admissibility, PCA parallel analysis, mean scores.")),
          tags$tr(tags$td("6 · External validity"), tags$td("Scale correlations compared with user-supplied literature expectations.")))),
        h4("What requires researcher judgment"), p("This app screens for potential problems. A clear result is not a certificate of data quality. Translation meaning, codebook correctness, construct validity, original survey order, and repeated-measures structure need substantive review."),
        p("Automatic imputation, automatic exclusions, formal measurement invariance, dynamic fit-index simulation, time-series checks, cross-file joins, and external-source scraping are outside this version. Public scale norms cannot be inferred from column names."),
        note(strong("Planted-error demo. "), "A3 is stored in reverse direction but its key is deliberately missing. Investigate its negative item-rest correlation, then set reverse = TRUE in Data setup and rerun. B4 has a correctly declared reverse key. The demo also contains an out-of-range response, a fractional response, a missing code, duplicate and missing IDs, and eligibility flags."),
        h4("Sources and implementation"), p("Adapted from the two supplied LAYD manuscript and R Markdown files. The six families follow Table 1; safeguards and transparent analysis-copy handling were added for an interactive workflow."),
        p(tags$a(href = "https://shiny.posit.co/r/components/inputs/file-input/", "Shiny upload documentation", target = "_blank"), " · ",
          tags$a(href = "https://lavaan.ugent.be/tutorial/cat.html", "Ordinal CFA in lavaan", target = "_blank"), " · ",
          tags$a(href = "https://search.r-project.org/CRAN/refmans/semTools/html/compRelSEM.html", "Composite reliability in semTools", target = "_blank"))))
  ), div(class = "app-footer", "LAYD  •  Look at Your Data", span("Explore the signal. Verify the explanation."))
)

server <- function(input, output, session) {
  demo_data <- make_demo()
  rv <- reactiveValues(data = demo_data, codebook = demo_codebook(demo_data), source = "Synthetic planted-error demo",
    result = NULL, signature = NULL, expectations = NULL, priors = NULL, revision = 1L,
    provenance = list(kind = "synthetic", seed = 42L, rows = 320L))
  val <- function(name, fallback) if (is.null(input[[name]])) fallback else input[[name]]
  current_settings <- reactive({
    s <- default_settings()
    for (name in names(s)) s[[name]] <- val(name, s[[name]])
    for (name in c("id", "group", "age", "consent", "attention", "duration", "test")) if (!s[[name]] %in% names(rv$data)) s[[name]] <- ""
    for (name in c("expected_rows", "expected_cols")) if (length(s[[name]]) != 1 || is.na(s[[name]])) s[[name]] <- NA_real_
    s
  })
  signature <- reactive(serialize(list(rv$revision, rv$codebook, current_settings(), rv$expectations, rv$priors), NULL))
  output$source_label <- renderUI(div(class = "source-chip", span(class = "source-dot"), rv$source))
  output$object_picker <- renderUI({
    req(input$upload)
    tryCatch({ choices <- import_choices(input$upload$datapath, input$upload$name)
      if (tolower(tools::file_ext(input$upload$name)) == "csv") return(NULL)
      selectInput("object", "Sheet or RData object", choices = choices)
    }, error = function(e) div(class = "text-danger", conditionMessage(e)))
  })
  update_meta <- function(demo = FALSE) {
    choices <- c("Not selected" = "", setNames(names(rv$data), names(rv$data)))
    for (name in c("id", "group", "age", "consent", "attention", "duration", "test")) {
      default <- if (demo) switch(name, group = "language", test = "test_account", name) else ""
      updateSelectInput(session, name, choices = choices, selected = default)
    }
  }
  metadata_select <- function(id, label, selected = "") selectInput(id, label,
    c("Not selected" = "", setNames(names(rv$data), names(rv$data))), selected = if (rv$source == "Synthetic planted-error demo") selected else "")
  output$metadata_controls <- renderUI(tagList(metadata_select("id", "Participant ID", "id"), metadata_select("group", "Grouping variable", "language")))
  output$eligibility_controls <- renderUI(tagList(metadata_select("age", "Age column", "age"), metadata_select("consent", "Consent column", "consent"),
    metadata_select("attention", "Attention check column", "attention"), metadata_select("duration", "Duration column (seconds)", "duration"), metadata_select("test", "Test account flag", "test_account")))
  observeEvent(rv$codebook, {
    active <- rv$codebook[rv$codebook$include %in% c(TRUE, "TRUE", "true", 1), , drop = FALSE]
    choices <- unique(as.character(active$scale)); choices <- choices[!is.na(choices) & nzchar(choices)]
    selected <- isolate(input$scale)
    updateSelectInput(session, "scale", choices = choices, selected = if (selected %in% choices) selected else head(choices, 1))
  })
  observeEvent(input$scale, {
    items <- as.character(rv$codebook$item[as.character(rv$codebook$scale) == input$scale])
    updateSelectInput(session, "item", choices = items, selected = head(items, 1))
  })
  observeEvent(input$import, {
    req(input$upload)
    tryCatch({
      d <- read_upload(input$upload$datapath, input$upload$name, val("object", NULL), input$sep, input$decimal, input$encoding)
      rv$data <- d; rv$source <- input$upload$name
      rv$provenance <- list(filename = input$upload$name, md5 = unname(tools::md5sum(input$upload$datapath)),
        object = val("object", NULL), separator = input$sep, decimal = input$decimal, encoding = input$encoding)
      rv$codebook <- make_codebook(d, input$range_min, input$range_max)
      rv$expectations <- NULL; rv$priors <- NULL; rv$result <- NULL; rv$signature <- NULL; rv$revision <- rv$revision + 1L
      showNotification("Imported. Confirm scale definitions in Data setup, then run checks.", type = "message")
      nav_select("workspace", "setup", session)
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = 15))
  })
  observeEvent(input$demo, {
    rv$provenance <- list(kind = "synthetic", seed = 42L, rows = 320L)
    rv$data <- make_demo(); rv$source <- "Synthetic planted-error demo"; rv$codebook <- demo_codebook(rv$data)
    rv$expectations <- NULL; rv$priors <- NULL; rv$result <- NULL; rv$signature <- NULL; rv$revision <- rv$revision + 1L
    showNotification("Demo loaded. Click Run data checks to explore its planted signals.")
  })
  observeEvent(input$reset_book, {
    rv$codebook <- make_codebook(rv$data, input$range_min, input$range_max)
    showNotification("Suggested codebook rebuilt. Confirm all keys and ranges before running.")
  })
  observeEvent(input$codebook_file, tryCatch({
    book <- read.csv(input$codebook_file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
    rv$codebook <- validate_codebook(book, rv$data)
  }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = 12)))
  output$codebook_table <- DT::renderDT({
    DT::datatable(rv$codebook, rownames = FALSE, editable = list(target = "cell", disable = list(columns = 1)),
      options = list(pageLength = 15, scrollX = TRUE, dom = "tip"), selection = "none")
  }, server = TRUE)
  observeEvent(input$codebook_table_cell_edit, {
    info <- input$codebook_table_cell_edit; i <- info$row; j <- info$col + 1L
    tryCatch({
      cb <- rv$codebook; name <- names(cb)[j]; value <- info$value
      if (name == "item") return()
      if (name %in% c("include", "reverse", "integer")) {
        if (!tolower(value) %in% c("true", "false", "1", "0")) stop("Use TRUE or FALSE.")
        value <- tolower(value) %in% c("true", "1")
      }
      if (name %in% c("min", "max")) { value <- as.numeric(value); if (!is.finite(value)) stop("Enter a numeric endpoint.") }
      cb[i, j] <- value; rv$codebook <- cb
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  observeEvent(input$expectations_file, tryCatch({
    x <- read.csv(input$expectations_file$datapath, stringsAsFactors = FALSE)
    if (!all(c("scale1", "scale2", "expected", "tolerance") %in% names(x))) stop("Expected columns: scale1, scale2, expected, tolerance.")
    rv$expectations <- x
  }, error = function(e) showNotification(conditionMessage(e), type = "error")))
  observeEvent(input$priors_file, tryCatch({
    x <- read.csv(input$priors_file$datapath, stringsAsFactors = FALSE)
    if (!all(c("item", "expected_mean", "tolerance") %in% names(x))) stop("Expected columns: item, expected_mean, tolerance.")
    rv$priors <- x
  }, error = function(e) showNotification(conditionMessage(e), type = "error")))
  observeEvent(input$clear_expectations, { rv$expectations <- NULL })
  observeEvent(input$clear_priors, { rv$priors <- NULL })
  do_run <- function() {
    tryCatch({
      s <- current_settings()
      if (s$duration_min > s$duration_max) stop("Minimum duration must not exceed maximum duration.")
      if (s$longstring_threshold < 2 || s$longstring_threshold != round(s$longstring_threshold)) stop("Response run threshold must be an integer >=2.")
      withProgress(message = "Looking at your data", value = .15, {
        r <- analyze_layd(rv$data, rv$codebook, s, rv$expectations, rv$priors)
        incProgress(.85)
      })
      r$provenance <- rv$provenance
      rv$result <- r; rv$signature <- signature()
      showNotification("Checks complete. Start with the review signals.", type = "message")
    }, error = function(e) showNotification(paste("Checks could not finish:", conditionMessage(e)), type = "error", duration = 20))
  }
  observeEvent(input$run, do_run())
  # Show a genuine precomputed demo immediately; full CFA is enabled by the Run button.
  observeEvent(TRUE, {
    s <- modifyList(default_settings(), list(id = "id", group = "language", age = "age", consent = "consent", attention = "attention", duration = "duration", test = "test_account", run_cfa = FALSE))
    r <- analyze_layd(rv$data, rv$codebook, s); r$provenance <- rv$provenance; rv$result <- r
  }, once = TRUE)
  result <- reactive({ req(rv$result); rv$result })
  focus <- reactive({ r <- result(); scales <- unique(r$summaries$scale); if (val("scale", "") %in% scales) input$scale else scales[1] })
  output$run_status <- renderUI({
    if (is.null(rv$result)) return(div(class = "status-strip waiting", strong("Ready when you are."), " Confirm your codebook, then click Run data checks."))
    stale <- is.null(rv$signature) || !identical(rv$signature, signature())
    if (stale) div(class = "status-strip waiting", strong(if (is.null(rv$signature) && rv$source == "Synthetic planted-error demo") "Showing a demo snapshot." else "Settings have changed."),
      " Click Run data checks to refresh all results and enable the selected model settings.")
    else div(class = "status-strip complete", strong("Checks complete."), paste("Last run:", rv$result$created, "·", rv$source))
  })
  output$metrics <- renderUI({
    r <- result(); f <- r$findings
    layout_columns(col_widths = c(3, 3, 3, 3),
      metric(format(r$dimensions[["rows"]], big.mark = ","), "OBSERVATIONS", "violet", paste(ncol(r$prepared), "selected items")),
      metric(sum(f$status == "Review"), "CHECKS TO REVIEW", "coral", "Signals to investigate"),
      metric(paste0(round(mean(is.na(r$prepared)) * 100, 1), "%"), "UNAVAILABLE RESPONSES", "teal", "After validity checks"),
      metric(length(unique(r$summaries$scale)), "DECLARED SCALES", "amber", paste(sum(f$status == "Not assessed"), "checks not assessed")))
  })
  output$priority <- renderUI({
    r <- result(); f <- r$findings[r$findings$status == "Review", ]
    if (!nrow(f)) return(note("No review signals at your thresholds. Check the unassessed families and inspect the plots."))
    f <- head(f[order(f$family != 1, f$check != "Corrected item-rest correlation"), ], 5)
    tagList(lapply(seq_len(nrow(f)), function(i) div(class = "finding-card", span(class = "finding-dot"),
      div(strong(paste(f$target[i], "·", f$check[i])), p(f$evidence[i])))),
      div(class = "chart-note", "Open Audit & export for all findings and suggested next steps."))
  })
  output$import_summary <- renderUI(tagList(h3(paste(format(nrow(rv$data), big.mark = ","), "rows ×", ncol(rv$data), "columns")),
    p(rv$source), p(paste("Included items:", sum(rv$codebook$include %in% c(TRUE, "TRUE", "true", 1)))),
    note("Missing-value codes are applied only to selected items. Numeric values outside declared endpoints are logged and withheld from psychometric analyses.")))
  dt <- function(x, digits = NULL) {
    w <- DT::datatable(x, rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE), escape = TRUE)
    if (!is.null(digits) && nrow(x)) {
      nums <- names(x)[vapply(x, is.numeric, logical(1))]; nums <- setdiff(nums, c("row", "n", "n_complete", "answered", "longstring", "invalid_n", "item_rest_n", "family"))
      if (length(nums)) w <- DT::formatRound(w, nums, digits)
    }
    w
  }
  output$preview <- DT::renderDT(dt(head(rv$data, 100)))
  output$descriptives <- DT::renderDT({ r <- result(); dt(r$summaries[r$summaries$scale == focus(), ], 3) })
  output$findings <- DT::renderDT({
    f <- result()$findings
    if (val("status_filter", "All") != "All") f <- f[f$status == input$status_filter, ]
    if (val("family_filter", "All") != "All") f <- f[f$family == as.integer(input$family_filter), ]
    dt(f[, c("family_label", "check", "target", "status", "n", "evidence", "action")])
  })
  output$row_flags <- DT::renderDT(dt(result()$row_flags))
  output$cell_audit <- DT::renderDT(dt(result()$cells))
  output$name_audit <- DT::renderDT(dt(result()$name_audit))
  output$external_table <- DT::renderDT(dt(result()$external, 3))
  output$group_table <- DT::renderDT(dt(result()$groups, 3))
  output$response_table <- DT::renderDT({ r <- result(); dt(r$response_style[r$response_style$scale == focus(), ], 3) })
  output$model_table <- DT::renderDT({ r <- result(); m <- r$models[[focus()]]; dt(if (is.null(m$measures)) data.frame(status = m$status, reason = m$reason) else m$measures, 3) })
  output$model_metrics <- renderUI({
    r <- result(); a <- r$reliability[r$reliability$scale == focus(), ]; m <- r$models[[focus()]]
    fmt <- function(x) if (is.null(x) || !is.finite(x)) "—" else sprintf("%.3f", x)
    layout_columns(col_widths = c(3, 3, 3, 3), metric(fmt(a$alpha[1]), "CRONBACH ALPHA", "violet", paste("Complete n =", a$n_complete[1])),
      metric(fmt(m$measures$omega), "MCDONALD OMEGA", "teal", "Model-based estimate"),
      metric(fmt(m$measures$cfi), "CFA · CFI", "coral", r$settings$estimator),
      metric(fmt(m$measures$rmsea), "CFA · RMSEA", "amber", paste("Scale", focus())))
  })
  output$model_notice <- renderUI({ m <- result()$models[[focus()]]
    note(strong(paste("Model status:", m$status, ". ")), m$reason, if (length(m$warnings)) paste(" Warnings:", paste(m$warnings, collapse = " | ")),
      if (!is.null(m$omega_note)) paste(" ", m$omega_note)) })
  interactive_plot <- function(p) plotly::config(plotly::ggplotly(p, tooltip = c("x", "y", "fill", "colour")), displaylogo = FALSE,
    modeBarButtonsToRemove = c("select2d", "lasso2d"))
  output$family_plot <- plotly::renderPlotly(interactive_plot(plot_family(result())))
  output$likert_plot <- plotly::renderPlotly(interactive_plot(plot_likert(result(), focus())))
  output$item_rest_plot <- plotly::renderPlotly(interactive_plot(plot_item_rest(result(), focus())))
  output$missing_map <- plotly::renderPlotly(interactive_plot(plot_missing(result(), focus())))
  output$missing_bars <- plotly::renderPlotly(interactive_plot(plot_missing_bars(result())))
  output$item_heatmap <- plotly::renderPlotly({ r <- result(); items <- r$summaries$item[r$summaries$scale == focus()]; interactive_plot(plot_correlation(r$item_cor[items, items, drop = FALSE])) })
  output$scale_heatmap <- plotly::renderPlotly(interactive_plot(plot_correlation(result()$scale_cor)))
  output$loadings <- plotly::renderPlotly(interactive_plot(plot_loadings(result(), focus())))
  output$scree <- plotly::renderPlotly(interactive_plot(plot_scree(result(), focus())))
  output$path <- plotly::renderPlotly(interactive_plot(plot_path(result(), focus())))
  output$group_plot <- plotly::renderPlotly(interactive_plot(plot_group(result(), focus(), input$group_metric)))
  output$distribution <- plotly::renderPlotly({
    r <- result(); item <- input$item
    if (is.null(item) || !item %in% names(r$prepared)) item <- r$summaries$item[r$summaries$scale == focus()][1]
    z <- data.frame(response = r$prepared[[item]], row = seq_len(nrow(r$prepared)))
    if (!any(is.finite(z$response))) return(interactive_plot(blank_plot("No valid responses.")))
    p <- if (input$distribution_view == "points") ggplot2::ggplot(z, ggplot2::aes(row, response)) + ggplot2::geom_point(color = "#7048F4", alpha = .6, size = 2, na.rm = TRUE) + ggplot2::labs(x = "Original row", y = "Keyed response")
    else ggplot2::ggplot(z, ggplot2::aes(response)) + ggplot2::geom_histogram(fill = "#7048F4", color = "white", bins = min(25, max(5, length(unique(na.omit(z$response))))), na.rm = TRUE) + ggplot2::labs(x = "Keyed response", y = "Count")
    interactive_plot(p + layd_theme())
  })
  output$codebook_download <- downloadHandler("layd_codebook.csv", function(file) write.csv(rv$codebook, file, row.names = FALSE))
  output$expectations_template <- downloadHandler("expected_correlations.csv", function(file) write.csv(data.frame(scale1 = "A", scale2 = "B", expected = -.45, tolerance = .20), file, row.names = FALSE))
  output$priors_template <- downloadHandler("prior_item_means.csv", function(file) write.csv(data.frame(item = "A1", expected_mean = 3, tolerance = .5), file, row.names = FALSE))
  output$scores <- downloadHandler("layd_scale_scores.csv", function(file) write.csv(result()$scores, file, row.names = FALSE, na = ""))
  output$report <- downloadHandler("layd_report.html", function(file) write_report(result(), file, if (!is.null(result()$provenance$filename)) result()$provenance$filename else "Synthetic planted-error demo"))
  output$bundle <- downloadHandler("layd_results.zip", function(file) write_bundle(result(), file, if (!is.null(result()$provenance$filename)) result()$provenance$filename else "Synthetic planted-error demo"))
}
shinyApp(ui, server)
