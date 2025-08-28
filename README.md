# Lab 1 - R Shiny Ecological Data Analysis App

Welcome to BIOL 307 Field Technique, Lab 1. We will be uploading and analyziing the data you collected from ULLEC. You will generate a report with statistical analyses and figures included. THIS IS A TEMPLATE, not your final report. 
You need to include a written methods, results, and discussion section yourself. Use the generated report as a guide, but you will need to turn in a unique report that you produce. 

## Features

1. **CSV Upload with Validation**: Upload CSV files with required columns (`transect`, `quadrat`, `percent_cover`, `richness`) THESE COLUMN NAMES ARE NEEDED OR THE ANALYSES WON'T WORK.
2. **Summary Statistics**: Interactive summary table showing average percent cover and richness by transect. These numbers should be included in your report.
3. **Boxplot Visualization**: ggplot2 boxplot showing percent cover distribution by transect. You can save this plot by selecting save at the top right corner of the graphic. 
4. **Linear Mixed Model**: Statistical analysis with richness vs percent cover, accounting for transect random effects. The linear mixed model is a combination of fixed effects (richness) and random effects (transect). We use a 2-way ANOVA to determine significance of the fixed effects. 
5. **Automated Figure Legends**: Professional figure captions for all outputs. Please add more information to the suggested figure legends. 
6. **R Code Display**: Shows the exact R code used for each analysis (educational feature). THIS IS FOR YOUR EDUCATION. PLEASE REVIEW THE CODE BY ASKING GOOGLE, CHATGPT, OR ASK ME.
7. **HTML Report Generation**: Creates comprehensive HTML reports with proper citations and detailed methods. THIS IS A TEMPLATE!

## System Requirements

### Required Software
1. **R** (version 4.0 or higher) - Download from [CRAN](https://cran.r-project.org/)
2. **RStudio** (recommended) - Download from [RStudio.com](https://rstudio.com/products/rstudio/download/)
3. **Pandoc** (required for HTML report generation) - See installation instructions below

### Pandoc Installation
**⚠️ IMPORTANT**: Pandoc is required for HTML report generation. Without it, the "Generate HTML Report" button will not work.

#### macOS:
```bash
# Using Homebrew (recommended)
brew install pandoc

# Or download installer from: https://pandoc.org/installing.html
```

#### Windows:
```bash
# Using Chocolatey
choco install pandoc

# Or download installer from: https://pandoc.org/installing.html
```

#### Linux (Ubuntu/Debian):
```bash
sudo apt-get install pandoc
```

### Required R Packages

Install all required packages by running:

```r
# Install all required packages
install.packages(c(
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
  "car"           # Statistical testing
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

### Step 2: Launch the Application
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

### Step 3: Use the Application
1. **Upload Data**: Click "Choose CSV File" and select your data file (or use `sample_data.csv`)
2. **Run Analyses**: Click buttons in order: "Show Summary Table" → "Generate Boxplot" → "Run Linear Mixed Model"
3. **View Results**: Each analysis opens in a new tab with results and R code
4. **Generate Report**: Click "Generate HTML Report" to create a comprehensive scientific report


## Data Format

Your CSV file must contain these exact column names:
- `transect`: Factor identifying the transect (e.g., "A", "B", "C")
- `quadrat`: Factor identifying the quadrat within each transect (e.g., 1, 2, 3)
- `percent_cover`: Numeric value for percent cover (0-100)
- `richness`: Numeric value for species richness (count of species)

## Sample Data

A sample CSV file (`sample_data.csv`) is provided for testing the application.

## Statistical Model

The application fits a linear mixed effects model:
```
percent_cover ~ richness + (1|transect)
```

This model accounts for the hierarchical structure where quadrats are nested within transects, treating transect as a random effect.

**Important Note:** The application uses `Anova()` from the `car` package with Type II tests to obtain proper p-values for mixed effects models, as the standard `summary()` output doesn't provide p-values for fixed effects in mixed models.

## Output Interpretations

- **Table 1**: Summary statistics by transect and overall
- **Figure 2**: Boxplot showing distribution of percent cover by transect
- **Figure 3**: Scatter plot with linear mixed model fit and statistical significance testing
- **R Code Display**: Educational feature showing the exact R code used for each analysis
- **HTML Report**: Comprehensive scientific report with detailed methods, discussion of limitations, and broader implications
