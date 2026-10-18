# Run once in this directory, then start app.R.
packages <- c("shiny", "bslib", "DT", "ggplot2", "plotly", "readxl", "jsonlite", "htmltools", "zip", "lavaan", "semTools")
needed <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(needed)) install.packages(needed, repos = "https://cloud.r-project.org")
if (utils::packageVersion("bslib") < "0.7.0") install.packages("bslib", repos = "https://cloud.r-project.org")
message("Ready. Start with shiny::runApp('.')")
