# Auto-install missing packages
required_packages <- c("shiny", "tidyverse", "plotly", "corrplot", "reshape2", "cluster", "bslib", "DT", "GGally", "stats")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, repos = "http://cran.us.r-project.org")

library(shiny)
library(tidyverse)
library(plotly)
library(corrplot)
library(reshape2)
library(cluster)
library(bslib)
library(DT)
library(GGally)

# Load data
diabetes_data <- read.csv("diabetes.csv")
diabetes_data$Outcome <- as.factor(diabetes_data$Outcome)

# Feature Engineering for more visuals
diabetes_data$Age_Group <- cut(diabetes_data$Age, 
                               breaks=c(20, 30, 40, 50, 60, 100), 
                               labels=c("20s", "30s", "40s", "50s", "60+"))
diabetes_data$BMI_Cat <- cut(diabetes_data$BMI,
                             breaks=c(0, 18.5, 25, 30, 100),
                             labels=c("Underweight", "Normal", "Overweight", "Obese"))

# Define UI
ui <- page_navbar(
  title = "Diabetes Analytics Pro v2.0",
  theme = bs_theme(
    version = 5,
    bootswatch = "lux",
    primary = "#2c3e50",
    base_font = font_google("Inter")
  ),
  
  sidebar = sidebar(
    title = "Analysis Controls",
    selectInput("target_var", "Primary Variable:", choices = names(diabetes_data)[1:8], selected = "Glucose"),
    hr(),
    conditionalPanel(
      condition = "input.navbar == 'Comparative Analysis'",
      selectInput("comp_var", "Comparison Variable:", choices = names(diabetes_data)[1:8], selected = "BMI")
    ),
    conditionalPanel(
      condition = "input.navbar == 'Clustering'",
      numericInput("k_clusters", "Clusters (K):", 3, min = 2, max = 6)
    ),
    helpText("Use the tabs above to switch analysis modules.")
  ),
  
  id = "navbar",
  
  nav_panel("Executive Summary",
    layout_column_wrap(
      width = 1/4,
      value_box(title = "Total Records", value = nrow(diabetes_data), showcase = icon("database"), theme = "primary"),
      value_box(title = "Avg Glucose", value = round(mean(diabetes_data$Glucose),1), showcase = icon("droplet"), theme = "info"),
      value_box(title = "Diabetes Prevalance", value = scales::percent(mean(as.numeric(as.character(diabetes_data$Outcome)))), showcase = icon("virus"), theme = "danger"),
      value_box(title = "Median Age", value = median(diabetes_data$Age), showcase = icon("calendar"), theme = "success")
    ),
    layout_column_wrap(
      width = 1/2,
      card(card_header("Population by Age Group"), plotlyOutput("age_group_plot")),
      card(card_header("BMI Distribution"), plotlyOutput("bmi_pie_plot"))
    ),
    card(card_header("Data Explorer"), DTOutput("raw_data"))
  ),
  
  nav_panel("Distributions",
    layout_column_wrap(
      width = 1/2,
      card(card_header("Histogram & Density"), plotlyOutput("dist_hist")),
      card(card_header("Violin & Box Plot"), plotlyOutput("dist_violin"))
    ),
    card(card_header("Density Overlay by Outcome"), plotlyOutput("dist_density"))
  ),
  
  nav_panel("Comparative Analysis",
    layout_column_wrap(
      width = 1/2,
      card(card_header("Interactive Scatter Matrix"), plotlyOutput("scatter_2d")),
      card(card_header("Feature Interaction Trend"), plotlyOutput("trend_plot"))
    ),
    card(card_header("Parallel Coordinates Profile"), plotlyOutput("parallel_plot"))
  ),
  
  nav_panel("3D Health Models",
    layout_column_wrap(
      width = 1/2,
      card(card_header("Glucose-BMI-Age Model"), plotlyOutput("model_3d_1")),
      card(card_header("Insulin-Skin-Blood Pressure"), plotlyOutput("model_3d_2"))
    )
  ),
  
  nav_panel("Advanced Analytics",
    navset_card_tab(
      nav_panel("Correlation Matrix", plotOutput("cor_heatmap")),
      nav_panel("K-Means Clusters", plotlyOutput("cluster_3d")),
      nav_panel("PCA Variance", plotlyOutput("pca_plot")),
      nav_panel("Feature Importance (Proxy)", plotlyOutput("importance_plot"))
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # 1. Data Table
  output$raw_data <- renderDT({ datatable(diabetes_data, options = list(pageLength = 5, scrollX = TRUE)) })
  
  # 2. Age Group Bar
  output$age_group_plot <- renderPlotly({
    p <- ggplot(diabetes_data, aes(x=Age_Group, fill=Outcome)) + geom_bar(position="dodge") + theme_minimal() + scale_fill_manual(values=c("#3498db", "#e74c3c"))
    ggplotly(p)
  })
  
  # 3. BMI Pie
  output$bmi_pie_plot <- renderPlotly({
    df <- diabetes_data %>% group_by(BMI_Cat) %>% summarise(count = n())
    plot_ly(df, labels = ~BMI_Cat, values = ~count, type = 'pie', hole = 0.6) %>% layout(showlegend = T)
  })
  
  # 4. Distribution Histogram
  output$dist_hist <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x=input$target_var, fill="Outcome")) + geom_histogram(bins=30, alpha=0.7) + theme_minimal() + scale_fill_manual(values=c("#3498db", "#e74c3c"))
    ggplotly(p)
  })
  
  # 5. Violin Plot
  output$dist_violin <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x="Outcome", y=input$target_var, fill="Outcome")) + geom_violin() + geom_boxplot(width=0.1, fill="white") + theme_minimal()
    ggplotly(p)
  })
  
  # 6. Density Overlay
  output$dist_density <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x=input$target_var, fill="Outcome")) + geom_density(alpha=0.5) + theme_minimal() + scale_fill_manual(values=c("#3498db", "#e74c3c"))
    ggplotly(p)
  })
  
  # 7. 2D Scatter
  output$scatter_2d <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x=input$target_var, y=input$comp_var, color="Outcome")) + geom_point(alpha=0.6) + geom_smooth(method="lm") + theme_minimal()
    ggplotly(p)
  })
  
  # 8. Trend Plot (Binned Trend)
  output$trend_plot <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x="Age", y=input$target_var, color="Outcome")) + geom_line(stat="summary", fun=mean) + theme_minimal()
    ggplotly(p)
  })
  
  # 9. Parallel Coordinates
  output$parallel_plot <- renderPlotly({
    plot_ly(diabetes_data, type = 'parcoords',
            line = list(color = ~as.numeric(Outcome), colorscale = list(c(0, '#3498db'), c(1, '#e74c3c'))),
            dimensions = list(
              list(range = range(diabetes_data$Glucose), label = 'Glucose', values = ~Glucose),
              list(range = range(diabetes_data$BMI), label = 'BMI', values = ~BMI),
              list(range = range(diabetes_data$Age), label = 'Age', values = ~Age)
            ))
  })
  
  # 10. 3D Model 1
  output$model_3d_1 <- renderPlotly({
    plot_ly(diabetes_data, x = ~Glucose, y = ~BMI, z = ~Age, color = ~Outcome, type = "scatter3d", mode = "markers", marker=list(size=3))
  })
  
  # 11. 3D Model 2
  output$model_3d_2 <- renderPlotly({
    plot_ly(diabetes_data, x = ~Insulin, y = ~SkinThickness, z = ~BloodPressure, color = ~Outcome, type = "scatter3d", mode = "markers", marker=list(size=3))
  })
  
  # 12. Correlation Heatmap
  output$cor_heatmap <- renderPlot({
    corrplot(cor(diabetes_data %>% select_if(is.numeric)), method="color", type="upper", tl.col="black", tl.srt=45)
  })
  
  # 13. Cluster 3D
  output$cluster_3d <- renderPlotly({
    km <- kmeans(scale(diabetes_data[,c("Glucose", "BMI", "Age")]), centers = input$k_clusters)
    plot_ly(diabetes_data, x = ~Glucose, y = ~BMI, z = ~Age, color = as.factor(km$cluster), type = "scatter3d", mode = "markers")
  })
  
  # 14. PCA Plot
  output$pca_plot <- renderPlotly({
    pca <- prcomp(diabetes_data %>% select_if(is.numeric), scale. = TRUE)
    vars <- pca$sdev^2 / sum(pca$sdev^2)
    plot_ly(x = 1:length(vars), y = vars, type = "bar", name = "Variance Explained") %>% layout(title="PCA Component Variance")
  })
  
  # 15. Feature Importance
  output$importance_plot <- renderPlotly({
    fit <- lm(as.numeric(Outcome) ~ ., data = diabetes_data %>% select(-Age_Group, -BMI_Cat))
    imp <- abs(coef(fit)[-1])
    df <- data.frame(Feature = names(imp), Importance = imp)
    p <- ggplot(df, aes(x=reorder(Feature, Importance), y=Importance, fill=Importance)) + geom_bar(stat="identity") + coord_flip() + theme_minimal()
    ggplotly(p)
  })
}

shinyApp(ui, server)
