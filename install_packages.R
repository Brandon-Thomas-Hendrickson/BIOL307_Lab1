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

# Load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(lme4)
library(DT)
library(sjPlot)
library(sjmisc)
library(knitr)
library(rmarkdown)
library(gridExtra)
library(car)
library(vegan)
library(tidyr)
library(viridis)

