
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


ui <- fluidPage(
  titlePanel("Lab 1 - Ecological Data Analysis"),
  
  tabsetPanel(
    id = "main_tabs",
    
    tabPanel("Quadrat Sampling",
      sidebarLayout(
        sidebarPanel(
          h3("Quadrat Data Upload"),
          fileInput("quadrat_file", "Choose CSV File for Quadrat Data",
                    accept = c(".csv")),
          p("Required columns: habitat, transect, quadrat, percent_cover, richness"),
          verbatimTextOutput("quadrat_upload_status"),
          
          hr(),
          h3("Analysis Options"),
          actionButton("quadrat_summary", "Show Summary Table", class = "btn-primary"),
          br(), br(),
          actionButton("quadrat_boxplot", "Generate Boxplot", class = "btn-success"),
          br(), br(),
          actionButton("quadrat_model", "Run Linear Mixed Model", class = "btn-warning")
        ),
        
        mainPanel(
          tabsetPanel(
            id = "quadrat_tabs",
            tabPanel("Data Upload", 
                     h3("Quadrat Data Upload Instructions"),
                     p("Please upload a CSV file with the following required columns:"),
                     tags$ul(
                       tags$li("habitat: Factor identifying the habitat type (e.g., grassland, forest, wetland)"),
                       tags$li("transect: Factor identifying the transect (e.g., A, B, C)"),
                       tags$li("quadrat: Factor identifying the quadrat within transect"),
                       tags$li("percent_cover: Numeric value for percent cover (0-100)"),
                       tags$li("richness: Numeric value for species richness")
                     ),
                     DT::dataTableOutput("quadrat_data_table")),
            
            tabPanel("Summary Statistics", 
                     h3("Summary Table"),
                     DT::dataTableOutput("quadrat_summary_table"),
                     br(),
                     h4("R Code Used:"),
                     verbatimTextOutput("quadrat_summary_code")),
            
            tabPanel("Boxplot Analysis", 
                     h3("Percent Cover by Transect"),
                     plotOutput("quadrat_boxplot", height = "500px"),
                     br(),
                     h4("R Code Used:"),
                     verbatimTextOutput("quadrat_boxplot_code")),
            
            tabPanel("Statistical Model", 
                     h3("Linear Mixed Effects Model"),
                     h4("Model: percent_cover ~ richness + (1|transect)"),
                     verbatimTextOutput("quadrat_model_output"),
                     br(),
                     plotOutput("quadrat_model_plot", height = "500px"),
                     br(),
                     h4("R Code Used:"),
                     verbatimTextOutput("quadrat_model_code")),
            
            tabPanel("Habitat Analysis",
                     h3("Habitat Effects on Percent Cover"),
                     h4("ANOVA: percent_cover ~ habitat"),
                     actionButton("quadrat_habitat", "Run Habitat Analysis", class = "btn-success"),
                     br(), br(),
                     verbatimTextOutput("quadrat_habitat_output"),
                     br(),
                     plotOutput("quadrat_habitat_plot", height = "500px"),
                     br(),
                     h4("R Code Used:"),
                     verbatimTextOutput("quadrat_habitat_code"))
          )
        )
      )
    ),
    
    tabPanel("Point-Intercept Sampling",
      sidebarLayout(
        sidebarPanel(
          h3("Point-Intercept Data Upload"),
          fileInput("point_file", "Choose CSV File for Point-Intercept Data",
                    accept = c(".csv")),
          p("Required columns: habitat, transect, distance, species, presence_absence"),
          verbatimTextOutput("point_upload_status"),
          
          hr(),
          h3("Analysis Options"),
          actionButton("point_process", "Process Data & Fill Missing", class = "btn-primary"),
          br(), br(),
          actionButton("point_diversity", "Calculate Diversity Metrics", class = "btn-success"),
          br(), br(),
          actionButton("point_plots", "Generate Diversity Plots", class = "btn-warning"),
          br(), br(),
          actionButton("point_anova", "Run ANOVA", class = "btn-danger")
        ),
        
        mainPanel(
          tabsetPanel(
            id = "point_tabs",
            tabPanel("Data Upload", 
                     h3("Point-Intercept Data Upload Instructions"),
                     p("Please upload a CSV file with the following required columns:"),
                     tags$ul(
                       tags$li("habitat: Factor identifying the habitat type"),
                       tags$li("transect: Factor identifying the transect"),
                       tags$li("distance: Numeric value for distance along transect"),
                       tags$li("species: Factor identifying the species"),
                       tags$li("presence_absence: Numeric value (1 for present, 0 for absent)")
                     ),
                     DT::dataTableOutput("point_data_table")),
            
            tabPanel("Processed Data", 
                     h3("Data with Missing Values Filled"),
                     p("This shows the data after filling in 0s for missing species at each distance."),
                     DT::dataTableOutput("point_processed_table"),
                     br(),
                     h4("R Code Used:"),
                     verbatimTextOutput("point_process_code")),
            
            tabPanel("Diversity Metrics", 
                     h3("Alpha and Gamma Diversity"),
                     h4("Alpha Diversity (per transect)"),
                     DT::dataTableOutput("alpha_diversity_table"),
                     br(),
                     h4("Gamma Diversity (per habitat)"),
                     DT::dataTableOutput("gamma_diversity_table"),
                     br(),
                     h4("Beta Diversity"),
                     DT::dataTableOutput("beta_diversity_table"),
                     br(),
                     h4("R Code Used:"),
                     verbatimTextOutput("diversity_code")),
            
            tabPanel("Diversity Plots", 
                     h3("Alpha Diversity Comparison"),
                     plotOutput("alpha_plot", height = "500px"),
                     br(),
                     h4("R Code Used:"),
                     verbatimTextOutput("plot_code")),
            
            tabPanel("ANOVA Results", 
                     h3("ANOVA: Habitat Effect on Species Richness"),
                     verbatimTextOutput("anova_output"),
                     br(),
                     plotOutput("anova_plot", height = "500px"),
                     br(),
                     h4("R Code Used:"),
                     verbatimTextOutput("anova_code"))
          )
        )
      )
    ),
    
    tabPanel("Combined Report",
      fluidRow(
        column(12,
          h2("Generate Combined Analysis Report"),
          p("This tab allows you to generate a comprehensive HTML report that includes both Quadrat and Point-Intercept analyses in a single document."),
          br(),
          
          div(class = "well",
            h4("Report Options"),
            checkboxInput("combined_include_code", "Include R Code in Report", value = TRUE),
            br(),
            downloadButton("generate_combined_html", "Generate Combined HTML Report", 
                          class = "btn-success btn-lg", 
                          style = "font-size: 18px; padding: 12px 24px;")
          ),
          
          br(),
          
          div(class = "alert alert-info",
            h4("Report Contents"),
            p("The combined report will include:"),
            tags$ul(
              tags$li("Comprehensive introduction and methodology for both sampling methods"),
              tags$li("Quadrat analysis results (if quadrat data is uploaded and analyzed)"),
              tags$li("Point-intercept analysis results (if point-intercept data is uploaded and analyzed)"),
              tags$li("Integrated discussion comparing both approaches"),
              tags$li("Combined conclusions and management implications")
            )
          ),
          
          div(class = "alert alert-warning",
            h4("Requirements"),
            p("To generate a complete report:"),
            tags$ul(
              tags$li("Upload and analyze data in at least one of the analysis tabs above"),
              tags$li("Run the desired analyses (summary statistics, models, ANOVA) before generating the report"),
              tags$li("The report will include sections for any analyses that have been completed")
            )
          )
        )
      )
    )
  )
)

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

create_combined_report_content <- function(quadrat_data_df = NULL, quadrat_summary_df = NULL, quadrat_model_obj = NULL, 
                                         processed_point_data = NULL, alpha_div = NULL, gamma_div = NULL, anova_obj = NULL, 
                                         include_code = TRUE) {
  content <- c(
    "---",
    "title: 'Lab 1 - Vegetation Survey Report'",
    "author: 'BIOL 307 - Field Techniques'",
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
    "library(vegan)",
    "library(tidyr)",
    "```",
    "",
    "# Question(s)",
    "",
    "How does plant species richness influence percent cover in prairie quadrats across multiple transects? How do different habitat types compare in their percent cover? Additionally, how do habitats differ in species diversity based on point-intercept sampling data?",
    ""
  )
  
  if (!is.null(quadrat_data_df) && !is.null(quadrat_model_obj)) {
    quadrat_section <- c(
      "# Part I: Quadrat Sampling Analysis",
      "",
      "## Methods - Quadrat Sampling",
      "",
      "On [Date] at [Time], we conducted vegetation surveys at [Place]. We established [number of transects per habitat] [length of transect] transects across each habitat (ie. [habitat 1, habitat 2]). Quadrats were placed ever [interval used] in a systematic design. Within each quadrat, we estimated percent cover of [species or species recorded]. We also recorded species richness as the total number of different plant species observed within each quadrat (this could have been done, or not done)",
      "",
      "Statistical analysis employed two complementary approaches: (1) linear mixed effects modeling to examine species richness effects on percent cover while accounting for transect-level random effects, and (2) one-way ANOVA to compare habitat effects on percent cover, with transects nested within habitats.",
      "",
      "## Results - Quadrat Sampling",
      "",
      "### Dataset Overview",
      "",
      paste("The quadrat dataset comprised", nrow(quadrat_data_df), "quadrat observations collected across", 
            length(unique(quadrat_data_df$transect)), "transects, with an average of", 
            round(nrow(quadrat_data_df) / length(unique(quadrat_data_df$transect)), 1), 
            "quadrats per transect. Overall mean percent cover was", 
            round(mean(quadrat_data_df$percent_cover), 2), "% (SD =", 
            round(sd(quadrat_data_df$percent_cover), 2), "%). The dataset included", 
            length(unique(quadrat_data_df$habitat)), "habitat types with mean species richness of", 
            round(mean(quadrat_data_df$richness), 2), "species per quadrat (SD =", 
            round(sd(quadrat_data_df$richness), 2), ")."),
      "",
      paste0("```{r quadrat_summary, echo=", ifelse(include_code, "TRUE", "FALSE"), "}"),
      "# Load the quadrat data",
      paste("quadrat_data <- data.frame("),
      paste("habitat = c(", paste0("'", quadrat_data_df$habitat, "'", collapse = ", "), "),"),
      paste("transect = c(", paste0("'", quadrat_data_df$transect, "'", collapse = ", "), "),"),
      paste("quadrat = c(", paste0("'", quadrat_data_df$quadrat, "'", collapse = ", "), "),"),
      paste("percent_cover = c(", paste(quadrat_data_df$percent_cover, collapse = ", "), "),"),
      paste("richness = c(", paste(quadrat_data_df$richness, collapse = ", "), ")"),
      ")",
      "",
      "# Convert to factors",
      "quadrat_data$habitat <- as.factor(quadrat_data$habitat)",
      "quadrat_data$transect <- as.factor(quadrat_data$transect)",
      "quadrat_data$quadrat <- as.factor(quadrat_data$quadrat)",
      "",
      "# Summary statistics by transect",
      "summary_by_transect <- quadrat_data %>%",
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
      "      caption = 'Summary statistics for quadrat data by transect',",
      "      col.names = c('Transect', 'N Quadrats', 'Mean Cover (%)', 'SD Cover', 'Mean Richness', 'SD Richness'))",
      "```",
      "",
      "### Species Richness Analysis",
      "",
      paste0("```{r quadrat_model, echo=", ifelse(include_code, "TRUE", "FALSE"), "}"),
      "# Fit linear mixed effects model for richness effects",
      "model <- lmer(percent_cover ~ richness + (1|transect), data = quadrat_data)",
      "",
      "# Display model results",
      "sjPlot::tab_model(model, ",
      "                  show.ci = TRUE, ",
      "                  show.se = TRUE,",
      "                  show.stat = TRUE,",
      "                  show.p = TRUE,",
      "                  title = 'Linear Mixed Effects Model Results - Quadrat Data',",
      "                  dv.labels = 'Percent Cover (%)')",
      "",
      "# ANOVA for significance testing",
      "anova_result <- Anova(model, type = 2)",
      "print(anova_result)",
      "```",
      "",
      "### Data Visualization",
      "",
      paste0("```{r quadrat_plots, echo=", ifelse(include_code, "TRUE", "FALSE"), "}"),
      "# Create visualizations for richness analysis",
      "p1 <- ggplot(quadrat_data, aes(x = transect, y = percent_cover)) +",
      "  geom_boxplot(fill = 'lightblue', alpha = 0.7) +",
      "  geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +",
      "  labs(title = 'Percent Cover Distribution by Transect',",
      "       x = 'Transect', y = 'Percent Cover (%)') +",
      "  theme_minimal()",
      "",
      "p2 <- ggplot(quadrat_data, aes(x = richness, y = percent_cover, color = transect)) +",
      "  geom_point(size = 3, alpha = 0.7) +",
      "  geom_smooth(method = 'lm', se = TRUE, color = 'black', linetype = 'dashed') +",
      "  labs(title = 'Species Richness vs Percent Cover',",
      "       x = 'Species Richness', y = 'Percent Cover (%)', color = 'Transect') +",
      "  theme_minimal() +",
      "  scale_color_viridis_d()",
      "",
      "print(p1)",
      "print(p2)",
      "```",
      ""
    )
    content <- c(content, quadrat_section)
  }
  
  if (!is.null(quadrat_data_df)) {
    habitat_section <- c(
      "### Habitat Analysis",
      "",
      "In addition to examining richness-cover relationships, we analyzed habitat effects on percent cover using ANOVA. Since transects are nested within habitats, we used a simple one-way ANOVA rather than a mixed effects model.",
      "",
      paste0("```{r habitat_analysis, echo=", ifelse(include_code, "TRUE", "FALSE"), "}"),
      "# Habitat effects ANOVA (transects nested within habitats)",
      "habitat_model <- aov(percent_cover ~ habitat, data = quadrat_data)",
      "",
      "# Display ANOVA results",
      "summary(habitat_model)",
      "",
      "# Post-hoc comparisons if significant",
      "if(summary(habitat_model)[[1]]$'Pr(>F)'[1] < 0.05) {",
      "  cat('\\nPost-hoc Tukey HSD test:\\n')",
      "  print(TukeyHSD(habitat_model))",
      "}",
      "```",
      "",
      paste0("```{r habitat_plot, echo=", ifelse(include_code, "TRUE", "FALSE"), "}"),
      "# Habitat comparison plot",
      "habitat_plot <- ggplot(quadrat_data, aes(x = habitat, y = percent_cover, fill = habitat)) +",
      "  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +",
      "  geom_jitter(width = 0.2, alpha = 0.5, size = 2) +",
      "  labs(title = 'Habitat Effects on Percent Cover',",
      "       x = 'Habitat Type', y = 'Percent Cover (%)', fill = 'Habitat') +",
      "  theme_minimal() +",
      "  scale_fill_viridis_d() +",
      "  theme(axis.text.x = element_text(angle = 45, hjust = 1))",
      "",
      "print(habitat_plot)",
      "```",
      ""
    )
    content <- c(content, habitat_section)
  }
  
  if (!is.null(processed_point_data) && !is.null(anova_obj)) {
    point_section <- c(
      "# Part II: Point-Intercept Sampling Analysis",
      "",
      "## Methods - Point-Intercept Sampling",
      "",
      "Point-intercept sampling was conducted along transects positioned across different habitat types. At regular intervals along each transect, species presence/absence was recorded for all plant species encountered. Species richness was calculated at the quadrat level (distance points along transects) for statistical analysis.",
      "",
      "Analysis of variance (ANOVA) was used to test for significant differences in species richness between habitat types using quadrat-level data where each distance point represents an independent sampling unit.",
      "",
      "## Results - Point-Intercept Sampling",
      "",
      "### Data Summary",
      "",
      paste("Total number of observations processed:", nrow(processed_point_data)),
      paste("Number of habitat types:", length(unique(processed_point_data$habitat))),
      paste("Number of transects:", length(unique(processed_point_data$transect))),
      paste("Number of species recorded:", length(unique(processed_point_data$species))),
      "",
      "### Diversity Patterns",
      "",
      if (!is.null(alpha_div)) {
        paste("Alpha diversity analysis revealed variation in species richness among transects and habitats. Mean alpha diversity was", 
              round(mean(alpha_div$alpha_diversity), 2), "species per transect (SD =", 
              round(sd(alpha_div$alpha_diversity), 2), ").")
      } else {
        "Alpha diversity patterns varied among transects and habitats, providing insights into local-scale community structure."
      },
      "",
      if (!is.null(gamma_div)) {
        paste("Gamma diversity analysis showed habitat-level species pools ranging from", 
              min(gamma_div$gamma_diversity), "to", max(gamma_div$gamma_diversity), "species.")
      } else {
        "Gamma diversity analysis revealed different total species pools among habitat types."
      },
      "",
      "### Statistical Analysis - ANOVA",
      "",
      paste0("```{r point_anova, echo=", ifelse(include_code, "TRUE", "FALSE"), "}"),
      "# Create point-intercept data for analysis",
      "point_data <- data.frame(",
      paste0("  habitat = c(", paste(paste0("'", processed_point_data$habitat, "'"), collapse = ", "), "),"),
      paste0("  transect = c(", paste(paste0("'", processed_point_data$transect, "'"), collapse = ", "), "),"),
      paste0("  distance = c(", paste(processed_point_data$distance, collapse = ", "), "),"),
      paste0("  species = c(", paste(paste0("'", processed_point_data$species, "'"), collapse = ", "), "),"),
      paste0("  presence_absence = c(", paste(processed_point_data$presence_absence, collapse = ", "), ")"),
      ")",
      "",
      "# Create quadrat-level richness data",
      "quadrat_level_data <- point_data %>%",
      "  group_by(habitat, transect, distance) %>%",
      "  summarise(richness = sum(presence_absence), .groups = 'drop')",
      "",
      "# Run ANOVA on quadrat-level richness",
      "anova_model <- aov(richness ~ habitat, data = quadrat_level_data)",
      "anova_summary <- summary(anova_model)",
      "print(anova_summary)",
      "",
      "# Post-hoc comparisons",
      "tukey_results <- TukeyHSD(anova_model)",
      "print(tukey_results)",
      "```",
      "",
      paste0("```{r point_visualization, echo=", ifelse(include_code, "TRUE", "FALSE"), "}"),
      "# Visualization of richness patterns",
      "ggplot(quadrat_level_data, aes(x = habitat, y = richness, fill = habitat)) +",
      "  geom_boxplot(alpha = 0.7) +",
      "  geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +",
      "  labs(title = 'Species Richness by Habitat Type',",
      "       subtitle = 'Quadrat-level analysis (each distance point = 1 quadrat)',",
      "       x = 'Habitat', y = 'Species Richness', fill = 'Habitat') +",
      "  theme_minimal() +",
      "  theme(legend.position = 'none')",
      "```",
      ""
    )
    content <- c(content, point_section)
  }
  
  final_section <- c(
    "# Integrated Discussion",
    "",
    "The combination of quadrat sampling and point-intercept sampling methods provides complementary perspectives on plant community ecology. While quadrat sampling reveals relationships between diversity and vegetation coverage at fine spatial scales, point-intercept sampling captures broader patterns of species composition and habitat-level diversity.",
    "",
    "Each sampling method has distinct advantages and limitations. Quadrat sampling provides detailed quantitative data on vegetation coverage and local species richness relationships, making it ideal for understanding resource-diversity dynamics. Point-intercept sampling efficiently captures species composition across larger spatial extents and multiple habitat types, making it valuable for landscape-level diversity assessments.",
    "",
    "The results from both analyses provide complementary information for ecosystem management and conservation planning. Understanding both local-scale diversity-function relationships and broader habitat-level diversity patterns is essential for developing effective conservation strategies that operate at multiple spatial scales.",
    "",
    "Future studies could benefit from integrating these sampling approaches within the same study system to directly compare patterns and processes across spatial scales. Long-term monitoring using both methods would provide insights into temporal dynamics and stability of ecological patterns.",
    "",
    "This comprehensive analysis demonstrates the value of using multiple sampling approaches to understand plant community ecology. The quadrat and point-intercept methods each contribute unique insights into diversity patterns, species-environment relationships, and community structure. Together, they provide a robust foundation for ecological understanding and evidence-based management decisions."
  )
  
  content <- c(content, final_section)
  return(content)
}

create_point_intercept_report_content <- function(processed_data, alpha_div, gamma_div, anova_obj, include_code = TRUE) {
  content <- c(
    "---",
    "title: 'Lab 1 - Point-Intercept Sampling Analysis Report'",
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
    "library(vegan)",
    "library(tidyr)",
    "```",
    "",
    "# Methods",
    "",
    "## Field Data Collection",
    "",
    "Point-intercept sampling was conducted along transects positioned across different habitat types. At regular intervals along each transect, species presence/absence was recorded for all plant species encountered. This systematic approach ensures standardized sampling effort and allows for robust statistical comparisons between habitats.",
    "",
    "## Data Processing",
    "",
    "Raw point-intercept data were processed to create a complete species-by-site matrix. Missing values (species not recorded at specific points) were filled with zeros, representing confirmed absence at those locations. This data cleaning step is essential for accurate diversity calculations and ensures that all transects have complete species inventories.",
    "",
    "## Diversity Metrics",
    "",
    "Three complementary diversity metrics were calculated:",
    "",
    "**Alpha Diversity**: Species richness was calculated for each individual transect by counting the total number of species present across all sampling points within that transect.",
    "",
    "**Gamma Diversity**: Total species richness was calculated for each habitat type by counting all unique species found across all transects within that habitat.",
    "",
    "**Beta Diversity**: Community dissimilarity was quantified using the Jaccard distance metric, which measures the proportion of species that differ between communities. Beta diversity was calculated both between individual transects and between habitat types.",
    "",
    "## Statistical Analysis",
    "",
    "Analysis of variance (ANOVA) was used to test for significant differences in alpha diversity (species richness) between habitat types. The model was specified as:",
    "",
    "```",
    "alpha_diversity ~ habitat",
    "```",
    "",
    "Post-hoc pairwise comparisons were conducted using Tukey's HSD test to identify which specific habitat pairs differed significantly. Model assumptions were evaluated using residual plots and normality tests.",
    "",
    "# Results",
    "",
    "## Data Summary",
    "",
    paste("Total number of observations processed:", nrow(processed_data)),
    paste("Number of habitat types:", length(unique(processed_data$habitat))),
    paste("Number of transects:", length(unique(processed_data$transect))),
    paste("Number of species recorded:", length(unique(processed_data$species))),
    "",
    "## Diversity Patterns",
    "",
    if (include_code) {
      c("```{r alpha-diversity}",
        "# Display alpha diversity results",
        "knitr::kable(alpha_diversity_data, caption = 'Alpha Diversity (Species Richness) by Transect')",
        "```",
        "",
        "```{r gamma-diversity}",
        "# Display gamma diversity results", 
        "knitr::kable(gamma_diversity_data, caption = 'Gamma Diversity (Total Species Richness) by Habitat')",
        "```")
    } else {
      ""
    },
    "",
    "### Alpha Diversity Results",
    "",
    "Alpha diversity (species richness per transect) varied both within and between habitat types. These local-scale diversity measurements provide insight into the fine-scale heterogeneity of plant communities.",
    "",
    "### Gamma Diversity Results", 
    "",
    "Gamma diversity represents the total species pool available within each habitat type. Higher gamma diversity indicates greater regional species richness and potentially more diverse ecological niches within the habitat.",
    "",
    "### Beta Diversity Patterns",
    "",
    "Beta diversity analysis revealed patterns of community similarity and dissimilarity between transects and habitats. High beta diversity indicates greater community turnover and more heterogeneous species composition across space.",
    "",
    "## Statistical Comparisons",
    "",
    if (include_code) {
      c("```{r anova-analysis}",
        "# ANOVA results for habitat differences",
        "summary(anova_model)",
        "",
        "# Post-hoc comparisons",
        "TukeyHSD(anova_model)",
        "```",
        "",
        "```{r diversity-plot}",
        "# Boxplot of alpha diversity by habitat",
        "ggplot(alpha_diversity_data, aes(x = habitat, y = alpha_diversity, fill = habitat)) +",
        "  geom_boxplot(alpha = 0.7) +",
        "  geom_point(position = position_jitter(width = 0.2), size = 3, alpha = 0.8) +",
        "  labs(title = 'Alpha Diversity by Habitat Type',",
        "       x = 'Habitat', y = 'Species Richness (Alpha Diversity)',",
        "       fill = 'Habitat') +",
        "  theme_minimal() +",
        "  theme(legend.position = 'none')",
        "```")
    } else {
      ""
    },
    "",
    "# Discussion",
    "",
    "The point-intercept sampling analysis revealed important patterns in plant community diversity across different habitat types. The relationship between alpha and gamma diversity provides insights into how local and regional processes influence community assembly and species coexistence.",
    "",
    "Differences in alpha diversity between habitats may reflect varying environmental conditions, resource availability, or disturbance regimes. Habitats with consistently higher alpha diversity across transects suggest more favorable conditions for species coexistence or greater habitat heterogeneity at fine spatial scales.",
    "",
    "The magnitude of gamma diversity relative to alpha diversity indicates the degree of species turnover within habitats. High gamma diversity coupled with moderate alpha diversity suggests significant beta diversity, indicating that different transects within the same habitat type support distinct species assemblages.",
    "",
    "Understanding diversity patterns at multiple spatial scales has important implications for conservation and habitat management. Habitats with high gamma diversity represent important regional species pools that should be prioritized for conservation efforts. High beta diversity within habitats suggests that preserving multiple sites within each habitat type is necessary to maintain full species complements.",
    "",
    "Several limitations should be considered when interpreting these results. The point-intercept method may underestimate rare species that occur at low densities. Temporal variation in species composition was not assessed, as sampling represents a single time point. Environmental variables that might explain diversity patterns were not measured but could provide additional insights into the mechanisms driving observed patterns.",
    "",
    "This point-intercept sampling analysis successfully quantified plant community diversity patterns across multiple spatial scales. The results demonstrate the value of multi-scale diversity assessments for understanding community ecology and provide a foundation for evidence-based habitat management decisions."
  )
  
  return(content)
}

server <- function(input, output, session) {
  
  
  quadrat_data <- reactive({
    req(input$quadrat_file)
    
    tryCatch({
      df <- read.csv(input$quadrat_file$datapath, stringsAsFactors = TRUE)
      
      required_cols <- c("habitat", "transect", "quadrat", "percent_cover", "richness")
      if (!all(required_cols %in% names(df))) {
        missing_cols <- setdiff(required_cols, names(df))
        stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      }
      
      df$habitat <- as.factor(df$habitat)
      df$transect <- as.factor(df$transect)
      df$quadrat <- as.factor(df$quadrat)
      
      return(df)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  output$quadrat_upload_status <- renderText({
    if (is.null(input$quadrat_file)) {
      "No file uploaded"
    } else if (is.null(quadrat_data())) {
      "Error: Please check that your CSV has the required columns: habitat, transect, quadrat, percent_cover, richness"
    } else {
      paste("Success! Uploaded", nrow(quadrat_data()), "observations from", 
            length(unique(quadrat_data()$transect)), "transects")
    }
  })
  
  output$quadrat_data_table <- DT::renderDataTable({
    req(quadrat_data())
    DT::datatable(quadrat_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  quadrat_summary_data <- reactive({
    req(quadrat_data())
    
    transect_summary <- quadrat_data() %>%
      group_by(transect) %>%
      summarise(
        avg_percent_cover = round(mean(percent_cover, na.rm = TRUE), 2),
        avg_richness = round(mean(richness, na.rm = TRUE), 2),
        n_quadrats = n(),
        .groups = 'drop'
      )
    
    overall <- quadrat_data() %>%
      summarise(
        transect = "Overall",
        avg_percent_cover = round(mean(percent_cover, na.rm = TRUE), 2),
        avg_richness = round(mean(richness, na.rm = TRUE), 2),
        n_quadrats = n()
      )
    
    bind_rows(transect_summary, overall)
  })
  
  observeEvent(input$quadrat_summary, {
    output$quadrat_summary_table <- DT::renderDataTable({
      req(quadrat_summary_data())
      DT::datatable(quadrat_summary_data(), 
                    options = list(pageLength = 10, dom = 't'),
                    caption = "Table 1: Average percent cover and richness by transect")
    })
    
    output$quadrat_summary_code <- renderText({
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
    
    updateTabsetPanel(session, "quadrat_tabs", selected = "Summary Statistics")
  })
  
  observeEvent(input$quadrat_boxplot, {
    output$quadrat_boxplot <- renderPlot({
      req(quadrat_data())
      
      ggplot(quadrat_data(), aes(x = transect, y = percent_cover)) +
        geom_boxplot(fill = "lightblue", alpha = 0.7) +
        geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +
        labs(title = "Distribution of Percent Cover by Transect",
             subtitle = "Individual quadrat measurements shown as points",
             x = "Transect",
             y = "Percent Cover (%)") +
        theme_light() +
        theme(plot.title = element_text(size = 14, face = "bold"))
    })
    
    output$quadrat_boxplot_code <- renderText({
      "# R Code for Boxplot
ggplot(data, aes(x = transect, y = percent_cover)) +
  geom_boxplot(fill = 'lightblue', alpha = 0.7) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +
  labs(title = 'Distribution of Percent Cover by Transect',
       x = 'Transect', y = 'Percent Cover (%)') +
  theme_light()"
    })
    
    updateTabsetPanel(session, "quadrat_tabs", selected = "Boxplot Analysis")
  })
  
  quadrat_model_result <- reactive({
    req(quadrat_data())
    tryCatch({
      lmer(percent_cover ~ richness + (1|transect), data = quadrat_data())
    }, error = function(e) {
      return(NULL)
    })
  })
  
  observeEvent(input$quadrat_model, {
    output$quadrat_model_output <- renderPrint({
      req(quadrat_model_result())
      
      cat("Linear Mixed Effects Model Results\n")
      cat("Model: percent_cover ~ richness + (1|transect)\n\n")
      print(summary(quadrat_model_result()))
      
      cat("\n", rep("=", 50), "\n")
      cat("ANOVA Results (Type II)\n\n")
      
      anova_result <- Anova(quadrat_model_result(), type = 2)
      print(anova_result)
    })
    
    output$quadrat_model_plot <- renderPlot({
      req(quadrat_data(), quadrat_model_result())
      
      ggplot(quadrat_data(), aes(x = richness, y = percent_cover, color = transect)) +
        geom_point(size = 3, alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
        labs(title = "Linear Mixed Model: Percent Cover vs Species Richness",
             x = "Species Richness", y = "Percent Cover (%)", color = "Transect") +
        theme_light() +
        scale_color_viridis_d()
    })
    
    output$quadrat_model_code <- renderText({
      "# R Code for Linear Mixed Effects Model
library(lme4); library(car)
model <- lmer(percent_cover ~ richness + (1|transect), data = data)
summary(model)
Anova(model, type = 2)"
    })
    
    updateTabsetPanel(session, "quadrat_tabs", selected = "Statistical Model")
  })
  
  quadrat_habitat_result <- reactive({
    req(quadrat_data())
    tryCatch({
      aov(percent_cover ~ habitat, data = quadrat_data())
    }, error = function(e) {
      return(NULL)
    })
  })
  
  observeEvent(input$quadrat_habitat, {
    output$quadrat_habitat_output <- renderPrint({
      req(quadrat_habitat_result())
      
      cat("Habitat Effects Analysis (ANOVA)\n")
      cat("Model: percent_cover ~ habitat\n\n")
      print(summary(quadrat_habitat_result()))
      
      cat("\n", rep("=", 50), "\n")
      cat("ANOVA Table\n\n")
      print(anova(quadrat_habitat_result()))
    })
    
    output$quadrat_habitat_plot <- renderPlot({
      req(quadrat_data(), quadrat_habitat_result())
      
      ggplot(quadrat_data(), aes(x = habitat, y = percent_cover, fill = habitat)) +
        geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
        geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
        labs(title = "Habitat Effects on Percent Cover",
             x = "Habitat Type", y = "Percent Cover (%)", fill = "Habitat") +
        theme_light() +
        scale_fill_viridis_d() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$quadrat_habitat_code <- renderText({
      "# R Code for Habitat Analysis (ANOVA)
habitat_model <- aov(percent_cover ~ habitat, data = data)
summary(habitat_model)
anova(habitat_model)"
    })
    
    updateTabsetPanel(session, "quadrat_tabs", selected = "Habitat Analysis")
  })
  
  
  point_data <- reactive({
    req(input$point_file)
    
    tryCatch({
      df <- read.csv(input$point_file$datapath, stringsAsFactors = TRUE)
      
      required_cols <- c("habitat", "transect", "distance", "species", "presence_absence")
      if (!all(required_cols %in% names(df))) {
        missing_cols <- setdiff(required_cols, names(df))
        stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      }
      
      df$habitat <- as.factor(df$habitat)
      df$transect <- as.factor(df$transect)
      df$species <- as.factor(df$species)
      
      return(df)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  output$point_upload_status <- renderText({
    if (is.null(input$point_file)) {
      "No file uploaded"
    } else if (is.null(point_data())) {
      "Error: Please check that your CSV has the required columns: habitat, transect, distance, species, presence_absence"
    } else {
      paste("Success! Uploaded", nrow(point_data()), "observations from", 
            length(unique(point_data()$transect)), "transects,",
            length(unique(point_data()$species)), "species")
    }
  })
  
  output$point_data_table <- DT::renderDataTable({
    req(point_data())
    DT::datatable(point_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  processed_point_data <- reactive({
    req(point_data())

    all_combinations <- point_data() %>%
      tidyr::expand(transect, distance, species) %>%
      left_join(point_data() %>% select(transect, habitat) %>% distinct(), by = "transect")
    
    # Fill in missing data with 0s
    filled_data <- all_combinations %>%
      left_join(point_data(), by = c("habitat", "transect", "distance", "species")) %>%
      mutate(presence_absence = ifelse(is.na(presence_absence), 0, presence_absence)) %>%
      arrange(habitat, transect, distance, species)
    
    return(filled_data)
  })
  
  observeEvent(input$point_process, {
    output$point_processed_table <- DT::renderDataTable({
      req(processed_point_data())
      DT::datatable(processed_point_data(), options = list(pageLength = 15, scrollX = TRUE))
    })
    
    output$point_process_code <- renderText({
      "# R Code for Data Processing
# Create all combinations of transect, distance, and species
all_combinations <- data %>%
  tidyr::expand(transect, distance, species) %>%
  left_join(data %>% select(transect, habitat) %>% distinct(), by = 'transect')

# Fill missing values with 0s
filled_data <- all_combinations %>%
  left_join(data, by = c('habitat', 'transect', 'distance', 'species')) %>%
  mutate(presence_absence = ifelse(is.na(presence_absence), 0, presence_absence))"
    })
    
    updateTabsetPanel(session, "point_tabs", selected = "Processed Data")
  })

  diversity_metrics <- reactive({
    req(processed_point_data())

    alpha_div <- processed_point_data() %>%
      group_by(habitat, transect, species) %>%
      summarise(present = max(presence_absence), .groups = 'drop') %>%
      group_by(habitat, transect) %>%
      summarise(alpha_diversity = sum(present > 0), .groups = 'drop')

    gamma_div <- processed_point_data() %>%
      group_by(habitat, species) %>%
      summarise(present = max(presence_absence), .groups = 'drop') %>%
      group_by(habitat) %>%
      summarise(gamma_diversity = sum(present > 0), .groups = 'drop')

    data_wide <- processed_point_data() %>%
      tidyr::pivot_wider(names_from = species, values_from = presence_absence, values_fill = 0)

    beta_transects <- data_wide %>%
      select(-habitat, -transect, -distance) %>%
      group_by(data_wide$transect) %>%
      summarise(across(everything(), ~ max(.x, na.rm = TRUE)), .groups = 'drop')
    
    beta_transects_matrix <- as.matrix(beta_transects[,-1])
    rownames(beta_transects_matrix) <- unique(data_wide$transect)
    
    beta_div_transects <- vegan::vegdist(beta_transects_matrix, method = "jaccard")

    beta_habitats <- processed_point_data() %>%
      group_by(habitat, species) %>%
      summarise(present = max(presence_absence), .groups = 'drop') %>%
      tidyr::pivot_wider(names_from = species, values_from = present, values_fill = 0)
    
    beta_habitats_matrix <- as.matrix(beta_habitats[,-1])
    rownames(beta_habitats_matrix) <- beta_habitats$habitat
    
    beta_div_habitats <- vegan::vegdist(beta_habitats_matrix, method = "jaccard")
    
    return(list(
      alpha = alpha_div,
      gamma = gamma_div,
      beta_transects = beta_div_transects,
      beta_habitats = beta_div_habitats
    ))
  })

  quadrat_level_data <- reactive({
    req(processed_point_data())

    quadrat_richness <- processed_point_data() %>%
      group_by(habitat, transect, distance, species) %>%
      summarise(present = max(presence_absence), .groups = 'drop') %>%
      group_by(habitat, transect, distance) %>%
      summarise(richness = sum(present > 0), .groups = 'drop')
    
    return(quadrat_richness)
  })
  
  observeEvent(input$point_diversity, {
    output$alpha_diversity_table <- DT::renderDataTable({
      req(diversity_metrics())
      DT::datatable(diversity_metrics()$alpha, 
                    caption = "Alpha Diversity (Species Richness per Transect)")
    })
    
    output$gamma_diversity_table <- DT::renderDataTable({
      req(diversity_metrics())
      DT::datatable(diversity_metrics()$gamma, 
                    caption = "Gamma Diversity (Species Richness per Habitat)")
    })
    
    output$beta_diversity_table <- DT::renderDataTable({
      req(diversity_metrics())

      beta_transects_df <- as.data.frame(as.matrix(diversity_metrics()$beta_transects))
      beta_transects_df$Transect <- rownames(beta_transects_df)
      beta_transects_df <- beta_transects_df[, c("Transect", setdiff(names(beta_transects_df), "Transect"))]
      
      beta_habitats_df <- as.data.frame(as.matrix(diversity_metrics()$beta_habitats))
      beta_habitats_df$Habitat <- rownames(beta_habitats_df)
      beta_habitats_df <- beta_habitats_df[, c("Habitat", setdiff(names(beta_habitats_df), "Habitat"))]
      
      combined_df <- list(
        "Between Transects" = beta_transects_df,
        "Between Habitats" = beta_habitats_df
      )
      
      DT::datatable(beta_transects_df, caption = "Beta Diversity (Jaccard Distance) Between Transects")
    })
    
    output$diversity_code <- renderText({
      "# R Code for Diversity Calculations
library(vegan)

# Alpha diversity (per transect) - species richness for each transect
alpha_div <- data %>%
  group_by(habitat, transect, species) %>%
  summarise(present = max(presence_absence), .groups = 'drop') %>%
  group_by(habitat, transect) %>%
  summarise(alpha_diversity = sum(present > 0), .groups = 'drop')

# Gamma diversity (per habitat) - total unique species across all transects  
gamma_div <- data %>%
  group_by(habitat, species) %>%
  summarise(present = max(presence_absence), .groups = 'drop') %>%
  group_by(habitat) %>%
  summarise(gamma_diversity = sum(present > 0), .groups = 'drop')

# Beta diversity using Jaccard distance
beta_div <- vegan::vegdist(community_matrix, method = 'jaccard')"
    })
    
    updateTabsetPanel(session, "point_tabs", selected = "Diversity Metrics")
  })
  
  observeEvent(input$point_plots, {
    output$alpha_plot <- renderPlot({
      req(diversity_metrics())
      
      ggplot(diversity_metrics()$alpha, aes(x = habitat, y = alpha_diversity, fill = habitat)) +
        geom_boxplot(alpha = 0.7) +
        geom_point(position = position_jitter(width = 0.2), size = 3, alpha = 0.8) +
        labs(title = "Alpha Diversity (Species Richness) by Habitat",
             subtitle = "Each point represents one transect",
             x = "Habitat", y = "Species Richness (Alpha Diversity)",
             fill = "Habitat") +
        theme_light() +
        theme(plot.title = element_text(size = 14, face = "bold"),
              legend.position = "none") +
        scale_fill_viridis_d()
    })
    
    output$plot_code <- renderText({
      "# R Code for Alpha Diversity Plot
ggplot(alpha_diversity, aes(x = habitat, y = alpha_diversity, fill = habitat)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(position = position_jitter(width = 0.2), size = 3) +
  labs(title = 'Alpha Diversity by Habitat',
       x = 'Habitat', y = 'Species Richness') +
  theme_light() +
  scale_fill_viridis_d()"
    })
    
    updateTabsetPanel(session, "point_tabs", selected = "Diversity Plots")
  })

  anova_result <- reactive({
    req(quadrat_level_data())
    aov(richness ~ habitat, data = quadrat_level_data())
  })
  
  observeEvent(input$point_anova, {
    output$anova_output <- renderPrint({
      req(anova_result())
      
      cat("ANOVA: Testing Habitat Effect on Species Richness (Quadrat-Level Analysis)\n")
      cat("Model: richness ~ habitat\n")
      cat("Each distance point along transects treated as an independent quadrat\n\n")
      
      print(summary(anova_result()))
      
      cat("\nTukey HSD Post-hoc Test:\n")
      print(TukeyHSD(anova_result()))
    })
    
    output$anova_plot <- renderPlot({
      req(quadrat_level_data())
      
      # Residuals vs Fitted plot
      p1 <- ggplot(data = data.frame(fitted = fitted(anova_result()), 
                                   residuals = residuals(anova_result())),
                  aes(x = fitted, y = residuals)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
        theme_light()
      
      # Q-Q plot
      p2 <- ggplot(data = data.frame(sample = residuals(anova_result())),
                  aes(sample = sample)) +
        stat_qq() +
        stat_qq_line() +
        labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
        theme_light()
      
      # Boxplot of richness by habitat
      p3 <- ggplot(quadrat_level_data(), aes(x = habitat, y = richness, fill = habitat)) +
        geom_boxplot() +
        labs(title = "Species Richness by Habitat", x = "Habitat", y = "Species Richness") +
        theme_light() +
        theme(legend.position = "none")
      
      gridExtra::grid.arrange(p1, p2, p3, ncol = 2)
    })
    
    updateTabsetPanel(session, "point_tabs", selected = "ANOVA Results")
  })
  
  output$generate_combined_html <- downloadHandler(
    filename = function() {
      paste("Ecological_Analysis_Report_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # Check what data is available
      has_quadrat <- !is.null(quadrat_data()) && !is.null(quadrat_model_result())
      has_point <- !is.null(processed_point_data()) && !is.null(anova_result())
      
      if (!has_quadrat && !has_point) {
        stop("Please upload and analyze data in at least one of the analysis tabs before generating a report.")
      }
      
      temp_dir <- tempdir()
      temp_rmd <- file.path(temp_dir, "combined_report.Rmd")

      quadrat_data_df <- if (has_quadrat) quadrat_data() else NULL
      quadrat_summary_df <- if (has_quadrat) quadrat_summary_data() else NULL
      quadrat_model_obj <- if (has_quadrat) quadrat_model_result() else NULL
      
      processed_point_data_df <- if (has_point) processed_point_data() else NULL
      alpha_div <- if (has_point && !is.null(diversity_metrics())) diversity_metrics()$alpha else NULL
      gamma_div <- if (has_point && !is.null(diversity_metrics())) diversity_metrics()$gamma else NULL
      anova_obj <- if (has_point) anova_result() else NULL
      
      rmd_content <- create_combined_report_content(
        quadrat_data_df = quadrat_data_df,
        quadrat_summary_df = quadrat_summary_df,
        quadrat_model_obj = quadrat_model_obj,
        processed_point_data = processed_point_data_df,
        alpha_div = alpha_div,
        gamma_div = gamma_div,
        anova_obj = anova_obj,
        include_code = input$combined_include_code
      )
      
      writeLines(rmd_content, temp_rmd)
      
      tryCatch({
        rmarkdown::render(temp_rmd,
                         output_file = file,
                         output_format = rmarkdown::html_document(
                           toc = TRUE,
                           toc_float = TRUE,
                           number_sections = TRUE,
                           theme = "flatly",
                           highlight = "tango"
                         ),
                         quiet = TRUE)
      }, error = function(e) {
        writeLines(rmd_content, sub("\\.html$", ".txt", file))
      })
    }
  )
  
}

shinyApp(ui = ui, server = server)
