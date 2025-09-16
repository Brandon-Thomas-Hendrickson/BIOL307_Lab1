# Lab 1 - Vegetation Surveys

Welcome to BIOL 307 Field Technique, Lab 1: Vegetation Survey. We will be uploading and analyziing the data you collected from ULLEC. You will generate a report with statistical analyses and figures included. THIS IS A TEMPLATE, not your final report. 
You need to include a written methods, results, and discussion section yourself. Use the generated report as a guide, but you will need to turn in a unique report that you produce. 


## System Requirements

### Required Software
1. **R** (version 4.0 or higher) - Download from [CRAN](https://cran.r-project.org/)
2. **RStudio** (recommended) - Download from [RStudio.com](https://rstudio.com/products/rstudio/download/)
3. **Pandoc** (required for HTML report generation) - See installation instructions below

### Pandoc Installation
**IMPORTANT**: Pandoc is required for HTML report generation. Without it, the "Generate HTML Report" button will not work.

#### macOS:
```bash
# Using Homebrew (recommended)
brew install pandoc

# Or download installer from: https://pandoc.org/installing.html
```

#### Windows:
```bash
# Using Chocolatey (if installed)
choco install pandoc

# Using Scoop (if installed)
scoop install pandoc

# Or download installer from: https://pandoc.org/installing.html
# NOTE: RStudio comes with pandoc built-in, but you may need to update it
```

#### Linux (Ubuntu/Debian):
```bash
sudo apt-get install pandoc
```

### Required R Packages

Install all required packages by running:

```r
# Install all required packages for both Shiny app and R Markdown tutorial
install.packages(c(
  # Core Shiny application packages
  "shiny",        # Web application framework
  "ggplot2",      # Data visualization
  "dplyr",        # Data manipulation
  "lme4",         # Mixed effects models
  "DT",           # Interactive tables
  "sjPlot",       # Model visualization
  "sjmisc",       # Statistical utilities
  "knitr",        # Dynamic report generation
  "rmarkdown",    # R Markdown processing
  "gridExtra",    # Grid-based plots
  "car",          # Statistical testing
  
  # Additional packages for ecological analysis
  "vegan",        # Ecological diversity calculations
  "tidyr",        # Data reshaping and tidying
  "viridis"       # Color palettes for plots
))
```

Or use the provided installation script:
```r
source("install_packages.R")
```

## Quick Start Guide

### Step 1: Install Dependencies
```r
# 1. Install Pandoc (see instructions above)
# 2. Install R packages
source("install_packages.R")
```

### Step 2A: Launch the Shiny Application (Interactive)
**Option A: Using the launcher script (easiest)**
```r
source("launch_app.R")
```

**Option B: Direct launch**
```r
source("Lab1_ShinyApp.R")
```

**Option C: Using Shiny's runApp function**
```r
shiny::runApp("Lab1_ShinyApp.R")
```

### Step 2B: Use the R Markdown Tutorial (Code-only)
**Alternative for students who can't run the Shiny app:**
```r
# Open the R Markdown tutorial in RStudio
file.edit("Lab1_Analysis_Tutorial.Rmd")

# Or knit directly to HTML
rmarkdown::render("Lab1_Analysis_Tutorial.Rmd")
```

## Data Format Requirements

### For Quadrat Sampling:
Your CSV file must contain these exact column names:
- `habitat` : Factor identifying the habitat you laid transects in. (e.g. Grassland, Prairie)
- `transect`: Factor identifying the transect (e.g., "A", "B", "C")
- `quadrat`: Factor identifying the quadrat within each transect (e.g., 1, 2, 3)
- `percent_cover`: Numeric value for percent cover (0-100)
- `richness`: Numeric value for species richness (count of species)

### For Point-Intercept Sampling:
Your CSV file must contain these exact column names:
- `habitat`: Factor identifying habitat type (e.g., "Prairie", "Forest", "Wetland")
- `transect`: Factor identifying the transect (e.g., "T1", "T2", "T3")
- `distance`: Numeric value for distance along transect (e.g., 1, 2, 3, 4, 5)
- `species`: Factor identifying species name (e.g., "Species_A", "Species_B")
- `presence_absence`: Numeric value (1 = present, 0 = absent)

## Sample Data

Two sample CSV files are provided for testing:
- `sample_data.csv`: Quadrat sampling data
- `sample_point_intercept_data.csv`: Point-intercept sampling data


### Common Issues:

**1. "Cannot install packages" error:**
```r
# Try installing from a different mirror
install.packages("package_name", repos = "https://cloud.r-project.org/")

# Or update R and try again
# Check your R version: R.version.string
```

**2. "Pandoc not found" error:**
- Install pandoc using the instructions above
- Restart RStudio after installing pandoc
- Check if pandoc is installed: `Sys.which("pandoc")`