

cat("Checking PDF generation capabilities...\n")

if (!require("tinytex", quietly = TRUE)) {
  cat("Installing tinytex package...\n")
  install.packages("tinytex")
  library(tinytex)
}

if (!tinytex::is_tinytex()) {
  cat("TinyTeX not found. Installing TinyTeX distribution...\n")
  cat("This may take a few minutes...\n")
  
  tryCatch({
    tinytex::install_tinytex()
    cat("TinyTeX installed successfully!\n")
  }, error = function(e) {
    cat("TinyTeX installation failed:", e$message, "\n")
    cat("PDF generation may not work. HTML reports will be available as alternative.\n")
  })
} else {
  cat("TinyTeX is already installed!\n")
}

cat("Testing PDF generation...\n")
tryCatch({
  test_rmd <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    "title: 'Test Report'",
    "output: pdf_document",
    "---",
    "",
    "# Test",
    "This is a test."
  ), test_rmd)
  
  test_pdf <- tempfile(fileext = ".pdf")
  rmarkdown::render(test_rmd, output_file = test_pdf, quiet = TRUE)
  
  if (file.exists(test_pdf)) {
    cat("PDF generation test: PASSED\n")
    file.remove(test_pdf)
  } else {
    cat("PDF generation test: FAILED\n")
  }
  
  file.remove(test_rmd)
  
}, error = function(e) {
  cat("PDF generation test: FAILED -", e$message, "\n")
  cat("HTML reports will be used as fallback.\n")
})

cat("Setup complete!\n")
