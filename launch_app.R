# launch_app.R
# Simple script to launch the Shiny application

# Check if required packages are installed
required_packages <- c("shiny", "ggplot2", "dplyr", "lme4", "DT", "sjPlot", "sjmisc", "knitr", "rmarkdown", "gridExtra", "car")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  cat("Missing required packages:", paste(missing_packages, collapse = ", "), "\n")
  cat("Please run: source('install_packages.R') first\n")
  stop("Missing packages")
}

# Launch the Shiny app
cat("Launching Lab 1 Shiny Application...\n")
shiny::runApp("Lab1_ShinyApp.R", launch.browser = TRUE)
