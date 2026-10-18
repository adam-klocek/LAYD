# Import helpers. No uploaded R scripts are sourced; RData loads into a private environment.
check_frame <- function(x) {
  if (!is.data.frame(x) && !is.matrix(x)) stop("Choose a data.frame, tibble, or matrix.")
  x <- as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
  if (!nrow(x) || !ncol(x)) stop("The selected dataset has no rows or columns.")
  if (nrow(x) > 50000 || ncol(x) > 1000 || nrow(x) * ncol(x) > 2000000)
    stop("Use at most 50,000 rows, 1,000 columns, and 2 million cells per dataset.")
  if (any(vapply(x, function(z) is.list(z) || !is.null(dim(z)), logical(1))))
    stop("Nested/list or matrix columns are unsupported. Flatten these columns before uploading.")
  original <- names(x)
  fixed <- original
  fixed[is.na(fixed) | !nzchar(trimws(fixed))] <- "unnamed"
  names(x) <- make.unique(fixed, sep = "__")
  attr(x, "name_audit") <- data.frame(original = original, imported = names(x))
  x
}
import_choices <- function(path, filename) {
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% c("xls", "xlsx")) return(readxl::excel_sheets(path))
  if (ext %in% c("rdata", "rda")) {
    e <- new.env(parent = emptyenv())
    nm <- load(path, envir = e)
    good <- nm[vapply(nm, function(n) is.data.frame(e[[n]]) || is.matrix(e[[n]]), logical(1))]
    if (!length(good)) stop("This RData file contains no data.frame or matrix objects.")
    return(good)
  }
  if (ext == "csv") return("CSV data")
  stop("Supported formats: .csv, .xls, .xlsx, .RData, .rda")
}
read_upload <- function(path, filename, object = NULL, sep = ",", dec = ".", encoding = "UTF-8") {
  ext <- tolower(tools::file_ext(filename))
  if (ext == "csv") x <- utils::read.table(path, header = TRUE, sep = sep, dec = dec,
    fileEncoding = encoding, check.names = FALSE, stringsAsFactors = FALSE,
    na.strings = c("", "NA"), comment.char = "", quote = "\"", fill = FALSE)
  else if (ext %in% c("xls", "xlsx")) x <- readxl::read_excel(path, sheet = object, .name_repair = "minimal")
  else if (ext %in% c("rdata", "rda")) {
    e <- new.env(parent = emptyenv()); load(path, envir = e)
    if (is.null(object) || !exists(object, envir = e, inherits = FALSE)) stop("Select an RData object.")
    x <- e[[object]]
  } else stop("Unsupported file format.")
  check_frame(x)
}
# Preserve literal response labels when converting factors (never use factor level indices).
number_values <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (inherits(x, c("Date", "POSIXt"))) return(rep(NA_real_, length(x)))
  suppressWarnings(as.numeric(x))
}
make_codebook <- function(d, minimum = 1, maximum = 5) {
  candidates <- names(d)[vapply(d, function(x) {
    if (inherits(x, c("Date", "POSIXt")) || is.logical(x)) return(FALSE)
    nonmissing <- !is.na(x) & nzchar(trimws(as.character(x)))
    is.numeric(x) || (any(nonmissing) && mean(!is.na(number_values(x)[nonmissing])) > .5)
  }, logical(1))]
  candidates <- candidates[!grepl("^(id|participant.*|subject.*|age|duration.*|time.*|consent.*|attention.*|test.*|group|gender|language|education|wave)$", candidates, ignore.case = TRUE)]
  prefix <- sub("[_.]?[0-9]+.*$", "", candidates)
  prefix[prefix == candidates | !nzchar(prefix)] <- "Scale 1"
  data.frame(include = rep(TRUE, length(candidates)), item = candidates, scale = prefix, min = rep(minimum, length(candidates)),
    max = rep(maximum, length(candidates)), reverse = rep(FALSE, length(candidates)), integer = rep(TRUE, length(candidates)), stringsAsFactors = FALSE)
}
validate_codebook <- function(cb, d) {
  needed <- c("include", "item", "scale", "min", "max", "reverse", "integer")
  if (!all(needed %in% names(cb))) stop("Codebook must contain: ", paste(needed, collapse = ", "))
  cb <- cb[, needed, drop = FALSE]
  for (v in c("include", "reverse", "integer")) {
    s <- tolower(trimws(as.character(cb[[v]])))
    if (any(is.na(s) | !s %in% c("true", "false", "1", "0"))) stop(v, " must be TRUE or FALSE.")
    cb[[v]] <- s %in% c("true", "1")
  }
  if (anyDuplicated(cb$item)) stop("Each item may appear only once in the codebook.")
  if (any(!cb$item %in% names(d))) stop("Codebook items must exactly match imported column names.")
  cb$min <- number_values(cb$min); cb$max <- number_values(cb$max)
  cb$scale <- trimws(as.character(cb$scale))
  active <- cb$include
  if (!any(active)) stop("Include at least one item in the codebook.")
  if (any(!is.finite(cb$min[active]) | !is.finite(cb$max[active]) | cb$min[active] >= cb$max[active]))
    stop("Every included item requires finite min < max from your codebook.")
  if (any(cb$scale[active] == "row", na.rm = TRUE)) stop("Scale name row is reserved for original row indices. Choose another scale name.")
  if (any(is.na(cb$scale[active]) | !nzchar(cb$scale[active]))) stop("Every included item needs a scale name.")
  if (sum(active) > 150) stop("Include at most 150 items per run.")
  cb
}
make_demo <- function(seed = 42L, n = 320L) {
  set.seed(seed)
  a <- rnorm(n); b <- -.45 * a + sqrt(1 - .45^2) * rnorm(n)
  lik <- function(z) as.integer(cut(z, c(-Inf, -.9, -.25, .25, .9, Inf)))
  d <- data.frame(id = sprintf("P%04d", seq_len(n)), language = rep(c("English", "Czech"), length.out = n),
    age = sample(18:70, n, TRUE), consent = TRUE, duration = round(runif(n, 200, 800)),
    attention = TRUE, test_account = FALSE)
  for (j in 1:5) d[[paste0("A", j)]] <- lik(.8 * a + .6 * rnorm(n))
  for (j in 1:5) d[[paste0("B", j)]] <- lik(.8 * b + .6 * rnorm(n))
  d$A3 <- 6L - d$A3 # Deliberately omitted reverse key; expected to be flagged.
  d$B4 <- 6L - d$B4 # Correctly declared reverse key in demo codebook.
  for (j in c(paste0("A", 1:5), paste0("B", 1:5))) d[sample(n, 12), j] <- NA
  d$A2[4] <- 9; d$B1[8] <- -999; d$A1[9] <- 2.5
  d$A5[seq(2, 70, 2)] <- NA
  d[20, paste0("A", 1:5)] <- 3
  d$id[11] <- d$id[10]; d$id[12] <- NA
  d$consent[15] <- FALSE; d$age[16] <- 16; d$duration[17] <- 25
  d$attention[18] <- FALSE; d$test_account[19] <- TRUE
  d$language[21] <- " english "
  d[n, ] <- d[n - 1, ]
  check_frame(d)
}
demo_codebook <- function(d) {
  cb <- make_codebook(d); cb$reverse[cb$item == "B4"] <- TRUE; cb
}
