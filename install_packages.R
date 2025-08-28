

cat("=== Lab 1 Vegetation Survey - Dependency Installation ===\n\n")

required_packages <- c("shiny", "ggplot2", "dplyr", "lme4", "DT", "sjPlot", "sjmisc", "knitr", "rmarkdown", "gridExtra", "car")

installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed_packages)

if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "https://cran.rstudio.com/")
  cat("Package installation complete!\n\n")
} else {
  cat("All required packages are already installed!\n\n")
}

cat("Verifying R package installations:\n")
for (pkg in required_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("✓", pkg, "- OK\n")
  } else {
    cat("✗", pkg, "- FAILED\n")
  }
}

cat("\nChecking Pandoc installation (required for HTML reports):\n")
tryCatch({
  pandoc_version <- rmarkdown::pandoc_version()
  if (pandoc_version != "0" && !is.null(pandoc_version)) {
    cat("✓ Pandoc version", as.character(pandoc_version), "- OK\n")
    cat("✓ HTML report generation will work!\n")
  } else {
    cat("✗ Pandoc not found or version 0\n")
    cat("⚠️  HTML report generation will NOT work without Pandoc!\n")
    cat("\nTo install Pandoc:\n")
    cat("  macOS: brew install pandoc\n")
    cat("  Windows: choco install pandoc\n")
    cat("  Linux: sudo apt-get install pandoc\n")
    cat("  Or visit: https://pandoc.org/installing.html\n")
  }
}, error = function(e) {
  cat("✗ Error checking Pandoc:", e$message, "\n")
  cat("⚠️  Please install Pandoc for HTML report generation\n")
})

cat("\n=== Installation Summary ===\n")
cat("✓ Ready to run the Shiny app!\n")
cat("✓ Use 'source(\"launch_app.R\")' to start\n")
cat("✓ Use 'Generate HTML Report' for publication-ready analysis reports\n")
cat("✓ Sample data available in 'sample_data.csv'\n\n")
