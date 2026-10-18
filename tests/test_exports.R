# Focused end-to-end reproduction check. Run from project root.
source("R/io.R"); source("R/checks.R"); source("R/plots.R"); source("R/export.R")
d <- make_demo(); cb <- demo_codebook(d)
r <- analyze_layd(d, cb, list(run_cfa = FALSE))
r$provenance <- list(kind = "synthetic", seed = 42L, rows = 320L)
f <- tempfile(fileext = ".zip"); write_bundle(r, f, "Synthetic demo")
dir <- tempfile("reproduce_"); dir.create(dir); zip::unzip(f, exdir = dir)
expected <- r$findings
saved_dir <- getwd(); setwd(dir)
source("reproduce.R")
reproduced <- read.csv("reproduced_findings.csv", stringsAsFactors = FALSE)
stopifnot(isTRUE(all.equal(reproduced, expected, check.attributes = FALSE)))
setwd(saved_dir); unlink(c(f, dir), recursive = TRUE)
cat("PASS: Downloaded results bundle reproduces identical findings from its saved synthetic configuration.\n")
