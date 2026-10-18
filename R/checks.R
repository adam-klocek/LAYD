# LAYD analysis engine; base R except the optional lavaan/semTools CFA.
FAMILIES <- c("Values within variables", "Across observations", "Against prior knowledge",
              "Item properties", "Factorial structure", "External validity")
default_settings <- function() list(sentinels = "-999,-998,-99", id = "", group = "", age = "",
  age_min = 18, consent = "", consent_pass = "TRUE", attention = "", attention_pass = "TRUE",
  duration = "", duration_min = 120, duration_max = 7200, test = "", test_value = "TRUE",
  missing_threshold = .20, floor_threshold = .80, rit_threshold = .20, reliability_threshold = .70,
  min_answered = .80, longstring_threshold = 5L, estimator = "WLSMV", run_cfa = TRUE,
  cfi_min = .95, rmsea_max = .08, srmr_max = .08, expected_rows = NA_real_, expected_cols = NA_real_,
  corr_method = "spearman")
empty_frame <- function(...) data.frame(..., stringsAsFactors = FALSE)
safe_cor <- function(x, y, method = "spearman") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3 || length(unique(x[ok])) < 2 || length(unique(y[ok])) < 2) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok], method = method))
}
safe_matrix_cor <- function(x, method = "spearman") {
  k <- ncol(x); r <- matrix(NA_real_, k, k, dimnames = list(names(x), names(x)))
  for (i in seq_len(k)) for (j in seq_len(k)) r[i, j] <- safe_cor(x[[i]], x[[j]], method)
  r
}
longest_run <- function(x) {
  # Missing values break a run. Run follows codebook order, before reversing.
  best <- 0L; run <- 0L; prev <- NA_real_
  for (v in x) {
    if (!is.finite(v)) { run <- 0L; prev <- NA_real_; next }
    run <- if (is.finite(prev) && v == prev) run + 1L else 1L
    best <- max(best, run); prev <- v
  }
  best
}
parallel_components <- function(x, seed = 2026L, reps = 100L) {
  x <- x[complete.cases(x), , drop = FALSE]; n <- nrow(x); k <- ncol(x)
  if (k < 3 || n < max(30, k + 2) || any(vapply(x, sd, numeric(1)) == 0)) return(NULL)
  observed <- eigen(cor(x), symmetric = TRUE, only.values = TRUE)$values
  had_rng <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_rng) old_rng <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_rng) assign(".Random.seed", old_rng, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  simulated <- replicate(reps, eigen(cor(matrix(rnorm(n * k), n, k)), symmetric = TRUE, only.values = TRUE)$values)
  bound <- apply(simulated, 1, quantile, probs = .95)
  # Count leading consecutive components above the simulated 95th percentile.
  retained <- if (any(observed <= bound)) which(observed <= bound)[1] - 1L else k
  list(table = data.frame(component = seq_len(k), observed = observed, simulated_95 = bound),
       retained = retained, n = n, reps = reps)
}
fit_scale <- function(x, estimator = "WLSMV") {
  if (!requireNamespace("lavaan", quietly = TRUE)) return(list(status = "Not assessed", reason = "Install lavaan to run CFA."))
  k <- ncol(x)
  if (k < 3) return(list(status = "Not assessed", reason = "CFA requires at least three items; four are preferable for fit assessment."))
  if (k > 40) return(list(status = "Not assessed", reason = "CFA is limited to 40 items per scale in this interactive app."))
  complete_n <- sum(complete.cases(x))
  usable_n <- sum(rowSums(!is.na(x)) > 0)
  n <- if (estimator == "WLSMV") complete_n else usable_n
  if (n < max(30, k * 3)) return(list(status = "Not assessed", reason = paste("Insufficient usable cases for CFA:", n)))
  if (any(vapply(x, function(v) length(unique(na.omit(v))) < 2, logical(1))))
    return(list(status = "Not assessed", reason = "A constant or entirely missing item prevents CFA."))
  safe <- paste0("v", seq_len(k)); original <- names(x); names(x) <- safe
  warnings <- character()
  result <- tryCatch(withCallingHandlers({
    fit <- lavaan::cfa(paste("F =~", paste(safe, collapse = " + ")), data = x,
      estimator = estimator, ordered = if (estimator == "WLSMV") safe else NULL,
      std.lv = TRUE, missing = if (estimator == "WLSMV") "listwise" else "fiml")
    if (!lavaan::lavInspect(fit, "converged")) stop("CFA did not converge.")
    pe <- lavaan::standardizedSolution(fit)
    loads <- pe[pe$op == "=~", , drop = FALSE]
    orientation <- if (median(loads$est.std, na.rm = TRUE) < 0) -1 else 1
    loading <- data.frame(item = original[match(loads$rhs, safe)], loading = loads$est.std * orientation,
                          se = loads$se, p = loads$pvalue)
    variances <- pe[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% safe, "est.std"]
    admissible <- isTRUE(lavaan::lavInspect(fit, "post.check")) &&
      all(is.finite(loading$loading)) && all(abs(loading$loading) <= 1 + 1e-6) && all(variances >= -1e-6)
    fm <- lavaan::fitMeasures(fit)
    pick <- function(robust, scaled, basic) {
      for (n in c(robust, scaled, basic)) if (n %in% names(fm) && is.finite(fm[[n]])) return(unname(fm[[n]]))
      NA_real_
    }
    measures <- data.frame(n = as.integer(lavaan::lavInspect(fit, "nobs")), df = unname(fm["df"]),
      cfi = pick("cfi.robust", "cfi.scaled", "cfi"), tli = pick("tli.robust", "tli.scaled", "tli"),
      rmsea = pick("rmsea.robust", "rmsea.scaled", "rmsea"), srmr = pick("srmr", "srmr", "srmr"),
      omega = NA_real_)
    omega_note <- "Omega unavailable: install semTools."
    if (admissible && requireNamespace("semTools", quietly = TRUE)) {
      om <- tryCatch(semTools::compRelSEM(fit, tau.eq = FALSE, ord.scale = TRUE), error = function(e) e)
      if (inherits(om, "error")) omega_note <- conditionMessage(om)
      else { measures$omega <- as.numeric(om)[1]; omega_note <- "Model-based omega; ordinal response scale for WLSMV." }
    }
    list(status = if (admissible) "Computed" else "Review", reason = if (admissible) "" else
      "Inadmissible solution (e.g., negative residual variance or loading outside [-1, 1]); omega withheld.",
      loading = loading, measures = measures, omega_note = omega_note, admissible = admissible)
  }, warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }),
  error = function(e) list(status = "Not assessed", reason = conditionMessage(e)))
  result$warnings <- unique(warnings); result
}
analyze_layd <- function(d, codebook, settings = list(), expectations = NULL, priors = NULL) {
  s <- modifyList(default_settings(), settings)
  cb <- validate_codebook(codebook, d); cb <- cb[cb$include, , drop = FALSE]
  flags <- empty_frame(family = integer(), check = character(), target = character(), status = character(),
    n = integer(), evidence = character(), action = character())
  cells <- empty_frame(row = integer(), item = character(), raw = character(), issue = character(), action = character())
  row_flags <- empty_frame(row = integer(), check = character(), detail = character())
  add <- function(family, check, target = "Dataset", status = "Clear", n = 0L, evidence = "", action = "") {
    flags[nrow(flags) + 1L, ] <<- list(family, check, target, status, as.integer(n), evidence, action)
  }
  record_cells <- function(idx, item, raw, issue, action = "Set to NA in analysis copy") {
    if (length(idx)) cells <<- rbind(cells, data.frame(row = idx, item = item, raw = as.character(raw[idx]), issue = issue, action = action))
  }
  record_rows <- function(idx, check, detail) {
    if (length(idx)) row_flags <<- rbind(row_flags, data.frame(row = idx, check = check, detail = detail))
  }
  review <- function(n) if (n > 0) "Review" else "Clear"
  codes <- trimws(strsplit(s$sentinels, ",", fixed = TRUE)[[1]]); codes <- codes[nzchar(codes)]
  raw_items <- prepared <- as.data.frame(matrix(NA_real_, nrow(d), nrow(cb)))
  names(raw_items) <- names(prepared) <- cb$item
  missing_kind <- matrix("Observed", nrow(d), nrow(cb), dimnames = list(NULL, cb$item))
  for (i in seq_len(nrow(cb))) {
    item <- cb$item[i]; raw <- d[[item]]; text <- trimws(as.character(raw))
    native <- is.na(raw) | text == ""
    sent <- !native & text %in% codes
    v <- number_values(raw)
    nonnumeric <- !native & !sent & is.na(v)
    invalid <- !native & !sent & !nonnumeric & (!is.finite(v) | v < cb$min[i] | v > cb$max[i])
    fraction <- !native & !sent & !nonnumeric & !invalid & cb$integer[i] & abs(v - round(v)) > 1e-8
    record_cells(which(sent), item, raw, "Declared missing code")
    record_cells(which(nonnumeric), item, raw, "Non-numeric item response")
    record_cells(which(invalid), item, raw, "Outside declared range / non-finite")
    record_cells(which(fraction), item, raw, "Non-integer on integer response scale")
    add(1, "Declared missing codes", item, if (any(sent)) "Info" else "Clear", sum(sent),
        paste("Declared codes:", if (length(codes)) paste(codes, collapse = ", ") else "none"), "Verify these codes against the codebook.")
    add(1, "Numeric item type", item, review(sum(nonnumeric)), sum(nonnumeric),
        paste("Imported class:", paste(class(raw), collapse = "/")), "Inspect text responses or category-to-number mapping.")
    add(1, "Range and response coding", item, review(sum(invalid | fraction)), sum(invalid | fraction),
        paste0("Expected ", cb$min[i], " to ", cb$max[i], if (cb$integer[i]) "; integers only." else "."), "Inspect the source; invalid responses are withheld from analysis.")
    v[native | sent | nonnumeric | invalid | fraction] <- NA_real_
    raw_items[[item]] <- v
    missing_kind[native, i] <- "Missing"
    missing_kind[sent, i] <- "Missing code"
    missing_kind[nonnumeric | invalid | fraction, i] <- "Invalid"
    if (cb$reverse[i]) {
      ok <- which(!is.na(v)); record_cells(ok, item, raw, "Declared reverse key", "Reversed as min + max - response")
      v <- cb$min[i] + cb$max[i] - v
    }
    prepared[[item]] <- v
  }
  audit <- attr(d, "name_audit")
  renamed <- if (is.null(audit)) 0 else sum(audit$original != audit$imported, na.rm = TRUE)
  odd <- sum(!grepl("^[[:alpha:]][[:alnum:]_.]*$", names(d)))
  add(1, "Column names", status = review(renamed + odd), n = renamed + odd,
    evidence = paste(renamed, "empty/duplicate names repaired;", odd, "names contain spaces or special characters."),
    action = "See name audit; internal CFA names are safely mapped without altering source data.")
  for (name in names(d)[vapply(d, function(x) is.character(x) || is.factor(x), logical(1))]) {
    x <- as.character(d[[name]]); norm <- tolower(trimws(x)); ok <- !is.na(x)
    levels_raw <- unique(x[ok]); levels_norm <- tolower(trimws(levels_raw))
    collisions <- unique(levels_norm[duplicated(levels_norm) | duplicated(levels_norm, fromLast = TRUE)])
    ii <- which(ok & (x != trimws(x) | norm %in% collisions))
    add(1, "String and whitespace consistency", name, review(length(ii)), length(ii),
      "Case/whitespace variants are flagged; spelling and semantic equivalence require review.", "Standardize only after verifying labels.")
    record_rows(ii, "String variant", paste("Column:", name))
    encoding_bad <- which(ok & grepl("\uFFFD|\u00C3\u00A2|\u00C3\u00A9|\u00C2\u00A0", x))
    if (length(encoding_bad)) { add(1, "Possible encoding corruption", name, "Review", length(encoding_bad), "Text resembles encoding replacement or mojibake.", "Check CSV encoding and original labels."); record_rows(encoding_bad, "Possible encoding issue", name) }
  }
  if (is.finite(s$expected_rows)) add(1, "Expected row count", status = review(as.integer(nrow(d) != s$expected_rows)),
    n = as.integer(nrow(d) != s$expected_rows), evidence = paste("Imported:", nrow(d), "; expected:", s$expected_rows))
  if (is.finite(s$expected_cols)) add(1, "Expected column count", status = review(as.integer(ncol(d) != s$expected_cols)),
    n = as.integer(ncol(d) != s$expected_cols), evidence = paste("Imported:", ncol(d), "; expected:", s$expected_cols))
  duplicated_full <- which(duplicated(d) | duplicated(d, fromLast = TRUE))
  add(2, "Exact duplicate records", status = review(length(duplicated_full)), n = length(duplicated_full), evidence = "All members of duplicate sets are counted.", action = "Verify whether records represent distinct participants; no rows are deleted.")
  record_rows(duplicated_full, "Duplicate full record", "Entire imported row is duplicated.")
  complete_pattern <- complete.cases(raw_items)
  pattern_duplicate <- (duplicated(raw_items) | duplicated(raw_items, fromLast = TRUE)) & complete_pattern
  add(2, "Repeated complete response patterns", status = review(sum(pattern_duplicate)), n = sum(pattern_duplicate),
    evidence = "Exact item-pattern matches, ignoring metadata. Short scales can match by chance.", action = "Review IDs and metadata before inferring duplication.")
  record_rows(which(pattern_duplicate), "Repeated response pattern", "Complete response vector matches another row; can occur by chance.")
  if (nzchar(s$id) && s$id %in% names(d)) {
    id <- trimws(as.character(d[[s$id]])); absent <- is.na(id) | !nzchar(id)
    repeated <- !absent & (duplicated(id) | duplicated(id, fromLast = TRUE))
    add(2, "Missing identifiers", s$id, review(sum(absent)), sum(absent), "NA and blank identifiers.", "Resolve before joining datasets.")
    add(2, "Non-unique identifiers", s$id, review(sum(repeated)), sum(repeated), "All records sharing a nonmissing ID.", "Check intended unit of observation and join cardinality.")
    record_rows(which(absent), "Missing ID", "Identifier is NA or blank."); record_rows(which(repeated), "Duplicate ID", "Identifier occurs more than once.")
  } else add(2, "Identifier integrity", status = "Not assessed", evidence = "No ID column selected.")
  eligibility <- list(age = s$age, consent = s$consent, attention = s$attention, duration = s$duration, test = s$test)
  for (rule in names(eligibility)) {
    name <- eligibility[[rule]]
    if (!nzchar(name) || !name %in% names(d)) { add(2, paste("Eligibility:", rule), status = "Not assessed", evidence = "No column selected."); next }
    raw <- d[[name]]; text <- trimws(as.character(raw)); unknown <- is.na(raw) | text == ""
    if (rule %in% c("age", "duration")) {
      v <- number_values(raw); unknown <- unknown | !is.finite(v)
      failed <- if (rule == "age") !unknown & v < s$age_min else !unknown & (v < s$duration_min | v > s$duration_max)
    } else {
      expected <- if (rule == "consent") s$consent_pass else if (rule == "attention") s$attention_pass else s$test_value
      failed <- !unknown & if (rule == "test") tolower(text) == tolower(trimws(expected)) else tolower(text) != tolower(trimws(expected))
    }
    add(2, paste("Eligibility:", rule), name, review(sum(failed | unknown)), sum(failed | unknown),
      paste(sum(failed), "fail configured rule;", sum(unknown), "missing/uninterpretable."), "Review eligibility. All rows remain in diagnostics and scores.")
    record_rows(which(failed), paste("Eligibility", rule), "Fails configured rule.")
    record_rows(which(unknown), paste("Unknown", rule), "Missing/uninterpretable eligibility value.")
  }
  row_missing <- rowMeans(is.na(prepared))
  record_rows(which(row_missing > s$missing_threshold), "High item missingness", paste("Above", s$missing_threshold * 100, "% after validity checks."))
  add(2, "Respondent missingness", status = review(sum(row_missing > s$missing_threshold)), n = sum(row_missing > s$missing_threshold),
    evidence = paste("Threshold:", s$missing_threshold * 100, "% of selected items."), action = "Missingness mechanisms cannot be inferred from this check.")
  summaries <- empty_frame(item = character(), scale = character(), n = integer(), missing_pct = numeric(), native_missing_pct = numeric(),
    invalid_n = integer(), mean = numeric(), median = numeric(), sd = numeric(), min = numeric(), max = numeric(),
    skewness = numeric(), excess_kurtosis = numeric(), floor_pct = numeric(), ceiling_pct = numeric(),
    mean_position = numeric(), item_rest = numeric(), item_rest_n = integer())
  scores <- data.frame(row = seq_len(nrow(d)))
  models <- list(); parallel <- list(); reliability <- empty_frame(scale = character(), n_complete = integer(), alpha = numeric())
  response_style <- empty_frame(row = integer(), scale = character(), answered = integer(), longstring = integer(), irv = numeric())
  group_results <- empty_frame(group = character(), scale = character(), item = character(), n = integer(), mean = numeric(), missing_pct = numeric(), item_rest = numeric())
  for (scale in unique(cb$scale)) {
    book <- cb[cb$scale == scale, , drop = FALSE]; items <- book$item; x <- prepared[, items, drop = FALSE]
    answered <- rowSums(!is.na(x)); score <- rowMeans(x, na.rm = TRUE)
    score[answered < ceiling(ncol(x) * s$min_answered) | answered == 0] <- NA_real_
    scores[[scale]] <- score
    rawx <- raw_items[, items, drop = FALSE]
    ls <- apply(rawx, 1, longest_run)
    irv <- apply(rawx, 1, function(z) if (sum(is.finite(z)) > 1) sd(z, na.rm = TRUE) else NA_real_)
    response_style <- rbind(response_style, data.frame(row = seq_len(nrow(d)), scale = scale, answered = answered, longstring = ls, irv = irv))
    suspect <- which(ls >= s$longstring_threshold & answered >= s$longstring_threshold)
    add(2, "Long strings of identical responses", scale, review(length(suspect)), length(suspect),
      paste("Run length >=", s$longstring_threshold, "in uploaded/codebook item order, before reversing; missing values break runs."), "Response-style signal, not proof of careless responding.")
    record_rows(suspect, "Long response string", paste("Scale:", scale))
    complete <- x[complete.cases(x), , drop = FALSE]; k <- ncol(x); alpha <- NA_real_
    if (nrow(complete) > 2 && k > 1 && var(rowSums(complete)) > 0)
      alpha <- k / (k - 1) * (1 - sum(vapply(complete, var, numeric(1))) / var(rowSums(complete)))
    reliability <- rbind(reliability, data.frame(scale = scale, n_complete = nrow(complete), alpha = alpha))
    add(5, "Cronbach alpha", scale, if (!is.finite(alpha)) "Not assessed" else if (alpha < s$reliability_threshold) "Review" else "Clear", 0,
      paste("Complete-case alpha:", round(alpha, 3), "; n =", nrow(complete)), "Low consistency can reflect coding, multidimensionality, or the construct; inspect item correlations.")
    for (j in seq_len(k)) {
      v <- x[[j]]; z <- v[is.finite(v)]; n <- length(z); sdv <- if (n > 1) sd(z) else NA_real_
      rest <- if (k > 1) rowMeans(x[, -j, drop = FALSE], na.rm = TRUE) else rep(NA_real_, nrow(x))
      # Require >=80% of the remaining items for the corrected item-rest diagnostic.
      if (k > 1) rest[rowSums(!is.na(x[, -j, drop = FALSE])) < ceiling((k - 1) * s$min_answered)] <- NA_real_
      rit <- safe_cor(v, rest, s$corr_method); npair <- sum(is.finite(v) & is.finite(rest))
      lo <- book$min[j]; hi <- book$max[j]
      mu <- if (n) mean(z) else NA_real_
      skew <- if (n > 2 && is.finite(sdv) && sdv > 0) mean((z - mu)^3) / mean((z - mu)^2)^1.5 else NA_real_
      kurt <- if (n > 3 && is.finite(sdv) && sdv > 0) mean((z - mu)^4) / mean((z - mu)^2)^2 - 3 else NA_real_
      floor <- if (n) mean(z == lo) else NA_real_; ceilingp <- if (n) mean(z == hi) else NA_real_
      native <- mean(missing_kind[, items[j]] == "Missing")
      invalid_n <- sum(missing_kind[, items[j]] == "Invalid")
      summaries <- rbind(summaries, data.frame(item = items[j], scale = scale, n = n,
        missing_pct = mean(is.na(v)), native_missing_pct = native, invalid_n = invalid_n,
        mean = mu, median = if (n) median(z) else NA_real_, sd = sdv, min = if (n) min(z) else NA_real_, max = if (n) max(z) else NA_real_,
        skewness = skew, excess_kurtosis = kurt, floor_pct = floor, ceiling_pct = ceilingp,
        mean_position = (mu - lo) / (hi - lo), item_rest = rit, item_rest_n = npair))
      add(4, "Missing item responses", items[j], review(sum(is.na(v)) / nrow(x) > s$missing_threshold), sum(is.na(v)),
        paste(round(mean(is.na(v)) * 100, 1), "% unavailable after checking."), "Inspect missingness map and group summaries.")
      add(4, "Constant or empty item", items[j], review(n < 2 || !is.finite(sdv) || sdv == 0), as.integer(n < 2 || !is.finite(sdv) || sdv == 0),
        paste("Usable n =", n, "; SD =", round(sdv, 3)), "Verify response options and export settings.")
      add(4, "Floor or ceiling concentration", items[j], if (!n) "Not assessed" else if (max(floor, ceilingp) >= s$floor_threshold) "Review" else "Clear", 0,
        paste("Floor", round(100 * floor, 1), "%; ceiling", round(100 * ceilingp, 1), "%"), "Compare with the target population and instrument expectations.")
      add(5, "Corrected item-rest correlation", items[j], if (!is.finite(rit) || npair < 10) "Not assessed" else if (rit < s$rit_threshold) "Review" else "Clear", npair,
        paste(s$corr_method, "r =", round(rit, 3), "; paired n =", npair), "Negative/weak correlations suggest checking keys; keys are never guessed from correlations.")
    }
    pa <- tryCatch(parallel_components(x), error = function(e) NULL); parallel[[scale]] <- pa
    add(5, "Exploratory dimensionality", scale, if (is.null(pa)) "Not assessed" else if (pa$retained != 1) "Review" else "Clear", 0,
      if (is.null(pa)) "Insufficient complete cases or non-variable items." else paste(pa$retained, "leading components exceed simulated 95th-percentile bounds; complete n =", pa$n),
      "Pearson PCA parallel analysis is an exploratory screen, not an ordinal factor test.")
    model <- if (isTRUE(s$run_cfa)) fit_scale(x, s$estimator) else list(status = "Not assessed", reason = "CFA disabled for this run.")
    models[[scale]] <- model
    add(5, "CFA estimation", scale, if (model$status == "Computed") "Clear" else model$status, 0,
      paste(s$estimator, model$reason, paste(model$warnings, collapse = "; ")), "WLSMV uses complete cases; MLR uses FIML. Inspect model warnings.")
    if (!is.null(model$measures)) {
      fm <- model$measures
      fit_review <- fm$cfi < s$cfi_min | fm$rmsea > s$rmsea_max | fm$srmr > s$srmr_max
      add(5, "Unidimensional CFA fit", scale, if (!model$admissible || !is.finite(fm$df) || fm$df <= 0 || is.na(fit_review)) "Not assessed" else if (fit_review) "Review" else "Clear", 0,
        paste("CFI", round(fm$cfi, 3), "RMSEA", round(fm$rmsea, 3), "SRMR", round(fm$srmr, 3), "df", fm$df),
        "Configurable heuristic cutoffs; dynamic model-fit simulation is not implemented.")
      om <- fm$omega
      add(5, "McDonald omega", scale, if (!is.finite(om) || om < 0 || om > 1) "Not assessed" else if (om < s$reliability_threshold) "Review" else "Clear", 0,
        paste("Omega", round(om, 3), ";", model$omega_note), "Interpret alongside model admissibility and fit.")
      neg <- sum(model$loading$loading < 0, na.rm = TRUE)
      add(5, "Mixed loading signs", scale, review(neg), neg,
        "Latent sign oriented so median standardized loading is positive.", "Verify reverse keys in original instrument; do not reverse automatically.")
    }
    if (nzchar(s$group) && s$group %in% names(d)) {
      group <- as.character(d[[s$group]]); group[is.na(group) | group == ""] <- "(missing)"
      if (length(unique(group)) <= 30) for (g in unique(group)) {
        gi <- which(group == g)
        for (j in seq_len(k)) {
          rest <- if (k > 1) rowMeans(x[gi, -j, drop = FALSE], na.rm = TRUE) else rep(NA_real_, length(gi))
          if (k > 1) rest[rowSums(!is.na(x[gi, -j, drop = FALSE])) < ceiling((k - 1) * s$min_answered)] <- NA_real_
          v <- x[[j]][gi]; ng <- sum(is.finite(v) & is.finite(rest))
          group_results <- rbind(group_results, data.frame(group = g, scale = scale, item = items[j], n = ng,
            mean = if (any(!is.na(v))) mean(v, na.rm = TRUE) else NA_real_, missing_pct = mean(is.na(v)),
            item_rest = if (ng >= 10) safe_cor(v, rest, s$corr_method) else NA_real_))
        }
      }
    }
  }
  if (nrow(group_results)) for (item in cb$item) {
    z <- group_results[group_results$item == item & group_results$n >= 20, , drop = FALSE]
    v <- z$item_rest[is.finite(z$item_rest)]
    mismatch <- length(v) > 1 && min(v) < -.10 && max(v) > .10
    add(3, "Item-rest sign differs by group", item, if (length(v) < 2) "Not assessed" else if (mismatch) "Review" else "Clear", 0,
      "Groups need >=20 usable item-rest pairs; opposing signs must exceed |.10|.", "Inspect language/coding consistency; this is not measurement-invariance testing.")
  } else add(3, "Group comparisons", status = "Not assessed", evidence = "Choose a grouping column with <=30 levels.")
  if (!is.null(priors) && nrow(priors)) {
    if (!all(c("item", "expected_mean", "tolerance") %in% names(priors))) stop("Prior CSV needs item, expected_mean, tolerance.")
    for (i in seq_len(nrow(priors))) {
      p <- priors[i, ]; match_item <- match(p$item, summaries$item); expmean <- number_values(p$expected_mean); tol <- number_values(p$tolerance)
      if (is.na(match_item) || !is.finite(expmean) || !is.finite(tol) || tol < 0) stop("Prior means require included item names, numeric means, and nonnegative tolerances.")
      observed <- summaries$mean[match_item]
      add(3, "Mean versus prior expectation", p$item, if (!is.finite(observed)) "Not assessed" else if (abs(observed - expmean) > tol) "Review" else "Clear", 0,
        paste("Observed", round(observed, 3), "expected", expmean, "+/-", tol), "Descriptive comparison on keyed responses; verify reference population and coding.")
    }
  } else add(3, "Prior expected means", status = "Not assessed", evidence = "No prior-means CSV supplied. Distributions remain available for visual review.")
  scale_cor <- safe_matrix_cor(scores[, -1, drop = FALSE], s$corr_method)
  external <- empty_frame(scale1 = character(), scale2 = character(), expected = numeric(), tolerance = numeric(), observed = numeric(), n = integer(), status = character())
  if (!is.null(expectations) && nrow(expectations)) {
    if (!all(c("scale1", "scale2", "expected", "tolerance") %in% names(expectations))) stop("Expectation CSV needs scale1, scale2, expected, tolerance.")
    for (i in seq_len(nrow(expectations))) {
      e <- expectations[i, ]; a <- as.character(e$scale1); b <- as.character(e$scale2)
      expected <- number_values(e$expected); tolerance <- number_values(e$tolerance)
      if (!a %in% cb$scale || !b %in% cb$scale || a == b || !is.finite(expected) || abs(expected) > 1 || !is.finite(tolerance) || tolerance < 0)
        stop("Expectations need two distinct included scale names, expected r in [-1,1], and nonnegative tolerance.")
      obs <- safe_cor(scores[[a]], scores[[b]], s$corr_method); np <- sum(is.finite(scores[[a]]) & is.finite(scores[[b]]))
      status <- if (!is.finite(obs) || np < 10) "Not assessed" else if (abs(obs - expected) > tolerance || obs * expected < 0) "Review" else "Clear"
      external <- rbind(external, data.frame(scale1 = a, scale2 = b, expected = expected, tolerance = tolerance, observed = obs, n = np, status = status))
      add(6, "Correlation versus literature", paste(a, "and", b), status, np,
        paste(s$corr_method, "r", round(obs, 3), "; expected", expected, "+/-", tolerance), "Descriptive reference comparison; no formal equivalence or significance test.")
    }
  } else add(6, "Literature comparison", status = "Not assessed", evidence = "Supply expected correlations; observed relationships alone cannot establish external validity.")
  add(5, "Composite score availability", status = if (anyNA(scores[, -1, drop = FALSE])) "Info" else "Clear", n = sum(is.na(scores[, -1, drop = FALSE])),
    evidence = paste("Keyed means require", s$min_answered * 100, "% answered; no imputation and no automatic row exclusions."),
    action = "Consult the instrument's scoring manual; totals/sums may require different rules.")
  flags$family_label <- FAMILIES[flags$family]
  list(findings = flags, cells = cells, row_flags = row_flags, summaries = summaries,
    prepared = prepared, raw_items = raw_items, missing_kind = missing_kind, row_missing = row_missing,
    scores = scores, item_cor = safe_matrix_cor(prepared, s$corr_method), scale_cor = scale_cor,
    reliability = reliability, models = models, parallel = parallel, groups = group_results,
    response_style = response_style, external = external, codebook = codebook, settings = s,
    expectations = expectations, priors = priors, name_audit = audit,
    dimensions = c(rows = nrow(d), columns = ncol(d)), created = format(Sys.time(), tz = "UTC", usetz = TRUE))
}
