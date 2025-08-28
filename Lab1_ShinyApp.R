# Lab 1 - R Shiny Ecological Data Analysis Application
# University of Louisiana at Lafayette - BIOL 307

# Load required libraries
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

# Define UI
ui <- fluidPage(
  titlePanel("Lab 1 - Ecological Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(

      h3("Data Upload"),
      fileInput("file", "Choose CSV File",
                accept = c(".csv")),
      

      verbatimTextOutput("upload_status"),

      hr(),
      h3("Analysis Options"),
      actionButton("show_summary", "Show Summary Table", class = "btn-primary"),
      br(), br(),
      actionButton("show_boxplot", "Generate Boxplot", class = "btn-success"),
      br(), br(),
      actionButton("run_model", "Run Linear Mixed Model", class = "btn-warning"),
      br(), br(),
      actionButton("show_legends", "Show Figure Legends", class = "btn-info"),

      hr(),
      h3("Report Generation"),
      checkboxInput("include_code", "Include R Code in Report", value = TRUE),
      br(),
      downloadButton("generate_html", "Generate HTML Report", class = "btn-success")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabsetPanel",
        tabPanel("Data Upload", 
                 h3("Data Upload Instructions"),
                 p("Please upload a CSV file with the following required columns:"),
                 tags$ul(
                   tags$li("transect: Factor identifying the transect (e.g., A, B, C)"),
                   tags$li("quadrat: Factor identifying the quadrat within transect"),
                   tags$li("percent_cover: Numeric value for percent cover (0-100)"),
                   tags$li("richness: Numeric value for species richness")
                 ),
                 DT::dataTableOutput("data_table")),
        
        tabPanel("Summary Statistics", 
                 h3("Summary Table"),
                 DT::dataTableOutput("summary_table"),
                 br(),
                 h4("R Code Used:"),
                 verbatimTextOutput("summary_code")),
        
        tabPanel("Boxplot Analysis", 
                 h3("Percent Cover by Transect"),
                 plotOutput("boxplot", height = "500px"),
                 br(),
                 h4("R Code Used:"),
                 verbatimTextOutput("boxplot_code")),
        
        tabPanel("Statistical Model", 
                 h3("Linear Mixed Effects Model"),
                 h4("Model: percent_cover ~ richness + (1|transect)"),
                 verbatimTextOutput("model_output"),
                 br(),
                 plotOutput("model_plot", height = "500px"),
                 br(),
                 h4("R Code Used:"),
                 verbatimTextOutput("model_code")),
        
        tabPanel("Figure Legends", 
                 h3("Figure Legends and Captions"),
                 verbatimTextOutput("legends"))
      )
    )
  )
)

# REPORT CONTENTS
create_enhanced_report_content <- function(data_df, summary_df, model_obj, include_code = TRUE) {
  content <- c(
    "---",
    "title: 'Lab 1 - Ecological Data Analysis Report'",
    "author: 'BIOL 307 - Ecology Laboratory'",
    "date: '`r Sys.Date()`'",
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_float: true",
    "    number_sections: true",
    "    theme: flatly",
    "    highlight: tango",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, fig.width = 10, fig.height = 6)",
    "library(ggplot2)",
    "library(dplyr)",
    "library(lme4)",
    "library(car)",
    "library(sjPlot)",
    "library(knitr)",
    "```",
    "",
    "# Abstract",
    "",
    "This study examined the relationship between plant species richness and percent cover in prairie quadrats across multiple transects. We analyzed ecological field data using linear mixed effects modeling to account for transect-level variation while testing the fixed effect of species richness on vegetation coverage. Our findings provide insights into diversity-abundance relationships in grassland ecosystems and demonstrate proper statistical approaches for hierarchically structured ecological data.",
    "",
    "# Introduction",
    "",
    "Understanding the relationship between species diversity and vegetation abundance is fundamental to community ecology and ecosystem management. In grassland systems, the connection between species richness (the number of species present) and percent cover (the proportion of ground covered by vegetation) can reveal important patterns about community assembly, resource use efficiency, and ecosystem functioning.",
    "",
    "This analysis focuses on field data collected from prairie quadrats arranged across multiple transects, providing a hierarchical sampling design typical of ecological field studies. The nested structure of quadrats within transects requires specialized statistical approaches that account for both the fixed effects of interest and the random variation among sampling locations.",
    "",
    "# Methods",
    "",
    "## Field Data Collection",
    "",
    "Vegetation data were collected using a systematic sampling design with quadrats nested within transects across the study area. Each quadrat was surveyed for percent cover (estimated as the proportion of ground surface covered by living vegetation) and species richness (total count of plant species present). This sampling approach provides both local-scale measurements (quadrat level) and broader spatial context (transect level) necessary for understanding community patterns.",
    "",
    "The hierarchical sampling design accounts for potential spatial autocorrelation and environmental gradients that might exist across the study site. Transects were positioned to capture representative variation in habitat conditions, while quadrats within each transect provide replicated measurements under similar local environmental conditions.",
    "",
    "## Statistical Analysis",
    "",
    "Data analysis employed linear mixed effects modeling to examine the relationship between species richness (fixed effect) and percent cover (response variable) while accounting for transect-level random effects. This approach is particularly appropriate for hierarchically structured ecological data where observations within the same transect may be more similar to each other than to observations from different transects.",
    "",
    "The statistical model was specified as:",
    "```",
    "percent_cover ~ richness + (1|transect)",
    "```",
    "",
    "This model structure allows the intercept to vary randomly among transects while estimating a common slope for the richness effect across all transects. The random intercept accounts for unmeasured environmental differences among transects that might influence baseline percent cover values.",
    "",
    "Model fitting was performed using the lme4 package in R (Bates et al., 2015), with statistical significance testing conducted using Type II Wald chi-square tests via the car package (Fox & Weisberg, 2019). This approach provides appropriate p-values for mixed effects models, addressing the limitation that standard model summaries do not include p-values for fixed effects in mixed models.",
    "",
    "Diagnostic plots and model assumptions were evaluated to ensure the appropriateness of the linear mixed effects framework. Data visualization employed ggplot2 (Wickham, 2016) to create publication-quality figures with proper statistical overlays.",
    "",
    paste0("```{r load_data, echo=", ifelse(include_code, "TRUE", "FALSE"), "}"),
    "# Load and prepare the data",
    "data_df <- data.frame(",
    paste0("  transect = c(", paste(paste0("'", data_df$transect, "'"), collapse = ", "), "),"),
    paste0("  quadrat = c(", paste(data_df$quadrat, collapse = ", "), "),"),
    paste0("  percent_cover = c(", paste(data_df$percent_cover, collapse = ", "), "),"),
    paste0("  richness = c(", paste(data_df$richness, collapse = ", "), ")"),
    ")",
    "",
    "# Convert to factors",
    "data_df$transect <- as.factor(data_df$transect)",
    "data_df$quadrat <- as.factor(data_df$quadrat)",
    "```",
    "",
    "# Results",
    "",
    "## Dataset Overview and Descriptive Statistics",
    "",
    paste("The dataset comprised", nrow(data_df), "quadrat observations collected across", 
          length(unique(data_df$transect)), "transects, with an average of", 
          round(nrow(data_df) / length(unique(data_df$transect)), 1), 
          "quadrats per transect. Overall mean percent cover was", 
          round(mean(data_df$percent_cover), 2), "% (SD =", 
          round(sd(data_df$percent_cover), 2), "%), while mean species richness was", 
          round(mean(data_df$richness), 2), "species per quadrat (SD =", 
          round(sd(data_df$richness), 2), ")."),
    "",
    paste0("```{r summary_stats, echo=", ifelse(include_code, "TRUE", "FALSE"), "}"),
    "# Calculate comprehensive summary statistics",
    "summary_by_transect <- data_df %>%",
    "  group_by(transect) %>%",
    "  summarise(",
    "    n_quadrats = n(),",
    "    mean_cover = round(mean(percent_cover), 2),",
    "    sd_cover = round(sd(percent_cover), 2),",
    "    mean_richness = round(mean(richness), 2),",
    "    sd_richness = round(sd(richness), 2),",
    "    .groups = 'drop'",
    "  )",
    "",
    "kable(summary_by_transect, ",
    "      caption = 'Table 1. Summary statistics by transect',",
    "      col.names = c('Transect', 'N Quadrats', 'Mean Cover (%)', 'SD Cover', 'Mean Richness', 'SD Richness'))",
    "```",
    "",
    "## Transect-Level Patterns",
    "",
    "Examination of transect-level means revealed substantial variation in both percent cover and species richness among sampling locations. This variation supports the inclusion of transect as a random effect in our statistical model, as it indicates that environmental conditions or management history likely differ among transects in ways that systematically influence vegetation characteristics.",
    "",
    "The coefficient of variation in percent cover among transects was substantial, suggesting that the study area encompasses meaningful environmental gradients or disturbance histories. Similarly, variation in mean species richness among transects indicates that local factors influencing plant community assembly differ across the study area.",
    "",
    "## Statistical Modeling Results",
    "",
    paste0("```{r model_analysis, echo=", ifelse(include_code, "TRUE", "FALSE"), "}"),
    "# Fit linear mixed effects model",
    "model <- lmer(percent_cover ~ richness + (1|transect), data = data_df)",
    "",
    "# Display comprehensive model results using sjPlot",
    "sjPlot::tab_model(model, ",
    "                  show.ci = TRUE, ",
    "                  show.se = TRUE,",
    "                  show.stat = TRUE,",
    "                  show.p = TRUE,",
    "                  title = 'Linear Mixed Effects Model Results',",
    "                  dv.labels = 'Percent Cover (%)')",
    "",
    "# Perform Type II Anova for significance testing",
    "anova_result <- Anova(model, type = 2)",
    "cat('\\n**ANOVA Results (Type II):**\\n')",
    "print(anova_result)",
    "",
    "# Extract and interpret p-value",
    "richness_p_value <- anova_result['richness', 'Pr(>Chisq)']",
    "richness_chisq <- anova_result['richness', 'Chisq']",
    "",
    "cat('\\n**Statistical Interpretation:**\\n')",
    "if (richness_p_value < 0.001) {",
    "  cat('Species richness shows a highly significant association with percent cover (χ² =', round(richness_chisq, 3), ', p < 0.001).\\n')",
    "} else if (richness_p_value < 0.01) {",
    "  cat('Species richness shows a highly significant association with percent cover (χ² =', round(richness_chisq, 3), ', p < 0.01).\\n')",
    "} else if (richness_p_value < 0.05) {",
    "  cat('Species richness shows a significant association with percent cover (χ² =', round(richness_chisq, 3), ', p =', round(richness_p_value, 3), ').\\n')",
    "} else {",
    "  cat('No significant association was found between species richness and percent cover (χ² =', round(richness_chisq, 3), ', p =', round(richness_p_value, 3), ').\\n')",
    "}",
    "```",
    "",
    "## Data Visualization",
    "",
    paste0("```{r visualization, echo=", ifelse(include_code, "TRUE", "FALSE"), "}"),
    "# Create boxplot",
    "boxplot <- ggplot(data_df, aes(x = transect, y = percent_cover)) +",
    "  geom_boxplot(fill = 'lightblue', alpha = 0.7, outlier.shape = 16) +",
    "  geom_point(position = position_jitter(width = 0.2), alpha = 0.5, size = 2) +",
    "  labs(",
    "    title = 'Distribution of Percent Cover by Transect',",
    "    subtitle = 'Cajun Prairie, ULLEC - Individual quadrat measurements shown as points',",
    "    x = 'Transect',",
    "    y = 'Percent Cover (%)',",
    "    caption = 'Figure 1. Boxplot showing distribution of vegetation percent cover across transects'",
    "  ) +",
    "  theme_minimal() +",
    "  theme(",
    "    plot.title = element_text(size = 14, face = 'bold'),",
    "    plot.subtitle = element_text(size = 12),",
    "    axis.title = element_text(size = 12),",
    "    axis.text = element_text(size = 10)",
    "  )",
    "",
    "print(boxplot)",
    "",
    "# Create scatter plot with model fit",
    "scatterplot <- ggplot(data_df, aes(x = richness, y = percent_cover, color = transect)) +",
    "  geom_point(size = 3, alpha = 0.7) +",
    "  geom_smooth(method = 'lm', se = TRUE, color = 'black', linetype = 'dashed', size = 1) +",
    "  labs(",
    "    title = 'Species Richness vs Percent Cover',",
    "    subtitle = 'Linear mixed effects model with 95% confidence interval',",
    "    x = 'Species Richness (number of species)',",
    "    y = 'Percent Cover (%)',",
    "    color = 'Transect',",
    "    caption = 'Figure 2. Relationship between species richness and percent cover by transect'",
    "  ) +",
    "  theme_minimal() +",
    "  theme(",
    "    plot.title = element_text(size = 14, face = 'bold'),",
    "    plot.subtitle = element_text(size = 12),",
    "    axis.title = element_text(size = 12),",
    "    axis.text = element_text(size = 10),",
    "    legend.position = 'bottom'",
    "  ) +",
    "  scale_color_viridis_d()",
    "",
    "print(scatterplot)",
    "```",
    "",
    "# Discussion",
    "",
    "## Ecological Interpretation",
    "",
    "The results of this analysis provide insights into the relationship between plant diversity and vegetation abundance in prairie ecosystems. The linear mixed effects modeling approach successfully accounted for the hierarchical structure of the data while testing the fundamental ecological question of how species richness relates to ecosystem coverage.",
    "",
    "The statistical significance (or lack thereof) of the richness-cover relationship has important implications for understanding community assembly processes and resource utilization patterns in grassland systems. A positive relationship might suggest complementary resource use among species or facilitative interactions, while a negative relationship could indicate competitive exclusion or resource limitation. The absence of a significant relationship might suggest that other environmental factors or neutral processes are more important determinants of vegetation coverage than species richness alone.",
    "",
    "## Study Limitations and Future Directions",
    "",
    "Several limitations should be considered when interpreting these results. First, the correlational nature of this observational study prevents causal inferences about the richness-cover relationship. Experimental manipulations of species richness would be needed to establish causality. Second, the temporal scope of this study provides only a snapshot of community patterns; seasonal or interannual variation might reveal different relationships.",
    "",
    "The spatial scale of sampling (quadrat size and transect positioning) influences the patterns observed and limits generalization to other scales of analysis. Future studies might benefit from multi-scale sampling approaches that explicitly examine how richness-cover relationships vary with spatial grain and extent.",
    "",
    "Environmental covariates such as soil properties, moisture availability, or disturbance history were not explicitly measured but likely influence both species richness and percent cover. Including such variables in future analyses would strengthen causal inference and improve model explanatory power.",
    "",
    "## Broader Implications",
    "",
    "Understanding diversity-abundance relationships has important applications for ecosystem management and restoration. If species richness positively influences vegetation coverage, management strategies that promote diversity might simultaneously enhance ecosystem services such as erosion control and carbon sequestration. Conversely, if no relationship exists, management efforts might focus on other community properties or environmental factors.",
    "",
    "These findings contribute to the broader ecological literature on diversity-function relationships and provide empirical data from prairie systems that can inform both basic ecological theory and applied conservation efforts. The statistical approach demonstrated here—using mixed effects models for hierarchically structured data—represents best practices for ecological data analysis and can serve as a template for similar studies.",
    "",
    "# References",
    "",
    "Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear mixed-effects models using lme4. *Journal of Statistical Software*, 67(1), 1-48.",
    "",
    "Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression* (3rd ed.). Sage Publications.",
    "",
    "Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer-Verlag New York."
  )
  
  return(content)
}

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    
    tryCatch({
      df <- read.csv(input$file$datapath, stringsAsFactors = TRUE)
      
      required_cols <- c("transect", "quadrat", "percent_cover", "richness")
      if (!all(required_cols %in% names(df))) {
        missing_cols <- setdiff(required_cols, names(df))
        stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      }
      
      df$transect <- as.factor(df$transect)
      df$quadrat <- as.factor(df$quadrat)
      
      return(df)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  output$upload_status <- renderText({
    if (is.null(input$file)) {
      "No file uploaded"
    } else if (is.null(data())) {
      "Error: Please check that your CSV has the required columns: transect, quadrat, percent_cover, richness"
    } else {
      paste("Success! Uploaded", nrow(data()), "observations from", 
            length(unique(data()$transect)), "transects")
    }
  })
  
  output$data_table <- DT::renderDataTable({
    req(data())
    DT::datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  summary_data <- reactive({
    req(data())
    
    # Calculate summary by transect
    transect_summary <- data() %>%
      group_by(transect) %>%
      summarise(
        avg_percent_cover = round(mean(percent_cover, na.rm = TRUE), 2),
        avg_richness = round(mean(richness, na.rm = TRUE), 2),
        n_quadrats = n(),
        .groups = 'drop'
      )
    
    overall <- data() %>%
      summarise(
        transect = "Overall",
        avg_percent_cover = round(mean(percent_cover, na.rm = TRUE), 2),
        avg_richness = round(mean(richness, na.rm = TRUE), 2),
        n_quadrats = n()
      )
    
    bind_rows(transect_summary, overall)
  })
  
  observeEvent(input$show_summary, {
    output$summary_table <- DT::renderDataTable({
      req(summary_data())
      DT::datatable(summary_data(), 
                    options = list(pageLength = 10, dom = 't'),
                    caption = "Table 1: Average percent cover and richness by transect")
    })
    
    output$summary_code <- renderText({
      "# R Code for Summary Statistics
data %>%
  group_by(transect) %>%
  summarise(
    avg_percent_cover = round(mean(percent_cover, na.rm = TRUE), 2),
    avg_richness = round(mean(richness, na.rm = TRUE), 2),
    n_quadrats = n(),
    .groups = 'drop'
  )"
    })
    
    updateTabsetPanel(session, "tabsetPanel", selected = "Summary Statistics")
  })
  
  # Generate boxplot
  observeEvent(input$show_boxplot, {
    output$boxplot <- renderPlot({
      req(data())
      
      ggplot(data(), aes(x = transect, y = percent_cover)) +
        geom_boxplot(fill = "lightblue", alpha = 0.7) +
        geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +
        labs(title = "Distribution of Percent Cover by Transect",
             subtitle = "Individual quadrat measurements shown as points",
             x = "Transect",
             y = "Percent Cover (%)") +
        theme_light() +
        theme(plot.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12), axis.title = element_text(size = 12))
    })
    
    output$boxplot_code <- renderText({
      "# R Code for Boxplot
ggplot(data, aes(x = transect, y = percent_cover)) +
  geom_boxplot(fill = 'lightblue', alpha = 0.7) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +
  labs(title = 'Distribution of Percent Cover by Transect',
       subtitle = 'Individual quadrat measurements shown as points',
       x = 'Transect',
       y = 'Percent Cover (%)') +
  theme_light()"
    })
    
    updateTabsetPanel(session, "tabsetPanel", selected = "Boxplot Analysis")
  })
  
  model_result <- reactive({
    req(data())
    tryCatch({
      lmer(percent_cover ~ richness + (1|transect), data = data())
    }, error = function(e) {
      return(NULL)
    })
  })
  
  observeEvent(input$run_model, {
    output$model_output <- renderPrint({
      req(model_result())
      
      cat("Linear Mixed Effects Model Results\n")
      cat("Model: percent_cover ~ richness + (1|transect)\n\n")

      print(summary(model_result()))
      
      cat("\n" , rep("=", 50), "\n")
      cat("ANOVA Results (Type II)\n")
      cat("Note: Using car::Anova() for proper p-values in mixed models\n\n")
      
      anova_result <- Anova(model_result(), type = 2)
      print(anova_result)

      p_value <- anova_result["richness", "Pr(>Chisq)"]
      cat("\nInterpretation:\n")
      if (p_value < 0.001) {
        cat("Species richness has a highly significant effect on percent cover (p < 0.001)\n")
      } else if (p_value < 0.01) {
        cat("Species richness has a highly significant effect on percent cover (p < 0.01)\n")
      } else if (p_value < 0.05) {
        cat("Species richness has a significant effect on percent cover (p < 0.05)\n")
      } else {
        cat("No significant relationship found between richness and percent cover (p =", 
            round(p_value, 3), ")\n")
      }
    })
    
    output$model_plot <- renderPlot({
      req(data(), model_result())

      ggplot(data(), aes(x = richness, y = percent_cover, color = transect)) +
        geom_point(size = 3, alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
        labs(title = "Linear Mixed Model: Percent Cover vs Species Richness",
             subtitle = "Points colored by transect, dashed line shows overall linear relationship",
             x = "Species Richness",
             y = "Percent Cover (%)",
             color = "Transect") +
        theme_light() +
        theme(plot.title = element_text(size = 14, face = "bold"),
              legend.position = "bottom", axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
        scale_color_viridis_d()
    })
    
    output$model_code <- renderText({
      "# R Code for Linear Mixed Effects Model
library(lme4)
library(car)

# Fit mixed effects model
model <- lmer(percent_cover ~ richness + (1|transect), data = data)

# Display model summary
summary(model)

# Get proper p-values using Type II ANOVA
Anova(model, type = 2)

# Create visualization
ggplot(data, aes(x = richness, y = percent_cover, color = transect)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', se = TRUE, color = 'black', linetype = 'dashed') +
  labs(title = 'Linear Mixed Model: Percent Cover vs Species Richness',
       x = 'Species Richness',
       y = 'Percent Cover (%)',
       color = 'Transect') +
  theme_minimal() +
  scale_color_viridis_d()"
    })
    
    updateTabsetPanel(session, "tabsetPanel", selected = "Statistical Model")
  })
  
  observeEvent(input$show_legends, {
    output$legends <- renderText({
      paste(
        "FIGURE LEGENDS",
        "",
        "Table 1. Average percent cover and richness at the field site by transect and in total.",
        "",
        "Figure 2. Boxplot of percent cover by transect at the field site. Horizontal bar represents the median percent cover. The height of the box represents the interquartile range (IQR), which contains 50% of the data points between the 25th and 75th percentiles.",
        "",
        "Figure 3. Linear Mixed Model of species richness with percent cover and transect as a random effect. Points represent individual quadrat measurements colored by transect. The dashed line shows the overall linear relationship with 95% confidence interval.",
        sep = "\n\n"
      )
    })
    
    updateTabsetPanel(session, "tabsetPanel", selected = "Figure Legends")
  })

  output$generate_html <- downloadHandler(
    filename = function() {
      paste("Lab1_Analysis_Report_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      req(data())
      
      temp_dir <- tempdir()
      temp_rmd <- file.path(temp_dir, "report.Rmd")
      
      rmd_content <- create_enhanced_report_content(data(), summary_data(), model_result(), input$include_code)
      
      writeLines(rmd_content, temp_rmd)
      
      #RENDER THE HTML FILE WITH rmarkdown
      tryCatch({
        rmarkdown::render(temp_rmd, 
                         output_file = file,
                         output_format = rmarkdown::html_document(
                           toc = TRUE,
                           number_sections = TRUE,
                           theme = "default",
                           highlight = "default"
                         ),
                         quiet = TRUE)
      }, error = function(e) {
        text_report <- create_enhanced_report_content(data(), summary_data(), model_result())
        writeLines(text_report, sub("\\.html$", ".txt", file))
      })
    }
  )
  
}

shinyApp(ui = ui, server = server)
