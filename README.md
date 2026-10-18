# LAYD Shiny

An interactive R Shiny companion to **Look at Your Data (LAYD)**. Upload psychometric item data, declare the codebook, inspect six families of checks, and export a reproducible audit. The app follows Table 1 of the supplied manuscript and the practical checks in the supplied R Markdown pipeline.

The demo deliberately stores **A3 in the opposite direction while reverse = FALSE**. Inspect its negative item-rest correlation, set `reverse = TRUE`, and rerun. Its correlation and scale alpha should improve. B4 is also stored in reverse direction but has a correctly declared key. Other planted signals include a value of 9 on a 1–5 item, a fractional response, -999, duplicate/missing IDs, short duration, age and consent failures, an attention-check failure, and a test account. These simulated scales are teaching examples, not validated questionnaires.

## Input contracts

One row per participant, one column per item. Numeric factors are converted through their **labels**, never their level indices. Non-numeric category labels are flagged; LAYD does not guess category meanings. Dates, nested/list columns, and multidimensional cell values are not supported as items. Blank and duplicate column names are made unique and logged.

Upload limit: 50 MiB. Dataset limits: 50,000 rows, 1,000 columns, and 2 million cells. Analysis is limited to 150 included items; CFA is limited to 40 items per scale. Large correlation matrices and ordinal models may still take time. Group comparisons support up to 30 distinct levels.

### Codebook CSV

See `examples/demo_codebook.csv`. Required columns:

| Column | Meaning |
| --- | --- |
| include | TRUE/FALSE; metadata should be FALSE or absent |
| item | Exact imported column name; each item occurs once |
| scale | User-defined subscale; `row` is reserved for row indices |
| min / max | Theoretical response endpoints, not observed extrema |
| reverse | TRUE only when the uploaded item still needs reversal |
| integer | TRUE when only integer response options are permissible |

The codebook table is editable by double-click. Excluded items are not recoded or analyzed. Each item belongs to one scale in this version. To change a key, edit the table; do not also reverse the uploaded data, which would double-flip the item.

### Expected correlations CSV

`scale1, scale2, expected, tolerance`. See `examples/expected_correlations.csv`. Scale names must match the codebook. The example A/B expectation is synthetic. Replace it with a justified published expectation and choose the same correlation metric used by your source. The app flags a sign mismatch or absolute deviation beyond tolerance when at least 10 paired scores exist. This is a descriptive reference comparison, not a significance test, equivalence test, or proof of external validity.

### Prior item means CSV

`item, expected_mean, tolerance`. See `examples/prior_item_means.csv`. These expectations refer to the **keyed** response scale. They are evaluated as absolute differences, with no automatic population adjustment or statistical test.

## Six families and the implemented checks

| Manuscript family | Automatic checks and views |
| --- | --- |
| 1 Values within variables | Theoretical range, finite numeric values, integer responses, explicit missing codes, mixed text/numeric responses, imported types, column names, case/whitespace variants, possible encoding corruption, optional expected dimensions |
| 2 Across observations | Exact duplicate rows, repeated complete item patterns, missing/non-unique IDs, configured eligibility/test accounts, respondent missingness, long strings and intra-individual response variability |
| 3 Against prior knowledge | Histograms and individual response points, moments, prior expected item means, group item means and missingness, opposing item-rest signs across groups |
| 4 Item properties | Valid n, missingness, mean, median, SD, range, skewness, excess kurtosis, normalized mean position, floor/ceiling proportions, stacked keyed response profiles |
| 5 Factorial structure | Pairwise corrected item-rest/inter-item correlations, declared reversal, complete-case raw-score alpha, PCA parallel analysis, one-factor CFA per scale, standardized loadings/path diagram, model admissibility/fit, model-based omega, keyed scale means |
| 6 External validity | Pairwise scale correlations and user-provided literature expectations, with transparent paired sample sizes |

**Statuses:** Review = configured screening signal; Clear = no signal at the chosen threshold; Info = recorded processing/information; Not assessed = missing prerequisites, insufficient data, failed estimation, or disabled check. A clear check does not establish the absence of an error. Counts in `n` are interpreted with the accompanying evidence: affected records/cells for data checks, paired sample sizes for correlations, and 0 for coefficient/model-level checks.

## Analysis and scoring rules

- Raw imported data remain intact. Selected items are converted in a separate copy. Blank/native missing values, declared sentinels, non-numeric responses, out-of-range/non-finite values, and disallowed fractional responses are unavailable for analysis. Each sentinel/invalid response is logged by original row number. Only selected items are affected.
- Declared keys use `minimum + maximum - response`. Every applied reversal is logged. Reverse keys are never automatically guessed from correlation signs or factor loadings.
- Each score is the mean of its valid keyed items when at least the configured proportion is answered (default 80%, rounded up). Otherwise it is NA. This is a configurable mean-score rule, not a claim about the instrument's official scoring manual. Items retain their original units; scales with different item ranges require a justified scoring strategy.
- No imputation or automatic participant exclusion occurs. Eligibility and response-style signals are review flags, so they can still affect descriptive statistics, models, and scores.
- Spearman correlations are the default. Pairwise correlations require at least 3 usable pairs and variable responses. Corrected item-rest correlations require the completion rule for the remaining items; screening requires at least 10 pairs. Group sign checks require at least 20 pairs in each of two groups, with one r < -.10 and another > .10.
- Alpha is the ordinary covariance-based coefficient on complete rows in each scale. No automatic key correction occurs. The displayed complete-case n is explicit.
- Exploratory dimensionality uses **Pearson PCA parallel analysis**, complete cases, 100 independent normal simulations, 95th-percentile eigenvalue bounds, seed 2026. The retained count is the leading consecutive observed eigenvalues above their respective bounds. This screen is not an ordinal common-factor analysis or a definitive dimensionality test.
- CFA uses one factor per scale. **WLSMV** treats items as ordered and uses complete cases. **MLR** treats items as continuous and uses FIML. Both need at least three items and max(30, 3 × number of items) usable cases. Three-item models are normally just-identified, so global fit screening is withheld when df <= 0. These sample limits are computational safeguards, not proof of statistical adequacy.
- Loadings are oriented so their median is positive; the arbitrary sign of the latent variable is not mistaken for a reverse-key error. Mixed signs remain a review signal. Nonconvergence, negative residual variances, out-of-bounds standardized loadings, and model warnings remain visible. Omega is withheld for inadmissible models.
- CFA fit prefers available robust, then scaled, then ordinary CFI/TLI/RMSEA. Thresholds are configurable heuristics (initial CFI .95, RMSEA/SRMR .08). Dynamic fit-index simulation is not included.
- McDonald's omega uses `semTools::compRelSEM(..., tau.eq = FALSE, ord.scale = TRUE)`. For ordinal CFA, it refers to the observed ordinal response scale. Alpha and omega need not coincide and can use different effective samples under MLR.
- Missingness maps show up to 500 evenly spaced original rows, with native missing, sentinel, and invalid values in different colors. Summary percentages use all rows. Mean-position plots describe item endorsement, not IRT difficulty.
- Repeated response patterns and long strings can occur in legitimate data. Interpret them alongside scale length, response order, and content.

## Exports and reproducibility

The HTML report is standalone and embeds plots. The results contains the report, findings, cell/row audits, item summaries, analysis items, mean scores, correlations, group summaries, response-style indices, codebook, settings, model output/configuration, session information, and reusable analysis functions. Original metadata/ID values are not attached to score exports; `row` links them back to the original dataset. Cell audits include original item response values. Inspect exports before sharing.

The full original dataset is not duplicated inside results exports. `reproduce.R` documents how to run the saved configuration against that original dataset. Import metadata and checksum identify the original upload. Retain the original file separately.

## Validation

```r
source("tests/test_engine.R")
source("tests/test_integration.R")
source("tests/test_exports.R")
```

Engine tests cover declared keys, missing codes, legitimate negative values, invalid/fractional responses, duplicate/missing IDs, eligibility, mixed/factor types, zero-variance/all-missing data, no-item data, invalid codebooks, external expectations, prior means, RNG preservation, and CSV/RData imports. Integration tests cover CFA, Excel import, chart generation, report/bundle exports, and Shiny server reactivity.

## Scope and limitations

This app implements most screening checks rather than every analysis in the supplement. It does not scrape external norms or websites, infer translation meanings, impute data, exclude participants, merge multiple files, check longitudinal time order, fit formal measurement-invariance models, run dynamic fit-index simulations, or compute response autocorrelations. Investigating these requires study-specific choices. Normal-looking distributions, high reliability, and acceptable model fit do not guarantee correct data.

Based on the supplied `LAYD_R_pipeline.Rmd` and `Panzeri, Klocek, et al. (2026) manuscript`. Implementation references: [Shiny file input](https://shiny.posit.co/r/components/inputs/file-input/), [Shiny downloads](https://shiny.posit.co/r/articles/build/download/), [lavaan ordinal data](https://lavaan.ugent.be/tutorial/cat.html), and [semTools composite reliability](https://search.r-project.org/CRAN/refmans/semTools/html/compRelSEM.html).
