# Auto-install missing packages
required_packages <- c("shiny", "tidyverse", "plotly", "corrplot", "reshape2", "cluster", 
                       "bslib", "DT", "GGally", "shinyjs", "shinyWidgets", "thematic", "bsicons", "waiter")
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
library(shinyjs)
library(shinyWidgets)
library(thematic)
library(bsicons)
library(waiter)

thematic_shiny()

# Load data
diabetes_data <- read.csv("diabetes.csv")
diabetes_data$Outcome <- as.factor(diabetes_data$Outcome)

# Feature Engineering
diabetes_data$Age_Group <- cut(diabetes_data$Age, 
                               breaks=c(20, 30, 40, 50, 60, 100), 
                               labels=c("20s", "30s", "40s", "50s", "60+"))
diabetes_data$BMI_Cat <- cut(diabetes_data$BMI,
                             breaks=c(0, 18.5, 25, 30, 100),
                             labels=c("Underweight", "Normal", "Overweight", "Obese"))

# Custom CSS
css <- "
  @import url('https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700&display=swap');
  
  * { transition: all 0.25s ease; }
  
  body {
    background: linear-gradient(135deg, #0f0c29, #302b63, #24243e) !important;
    font-family: 'Outfit', sans-serif !important;
    color: #e0e0e0;
  }
  
  .navbar {
    background: linear-gradient(90deg, #667eea 0%, #764ba2 100%) !important;
    box-shadow: 0 4px 30px rgba(102, 126, 234, 0.4);
    border: none !important;
  }
  .navbar .navbar-brand { font-weight: 700; font-size: 1.1rem; letter-spacing: 1px; }
  .nav-link { font-weight: 500 !important; font-size: 0.85rem !important; letter-spacing: 0.5px; }
  .nav-link.active { background: rgba(255,255,255,0.2) !important; border-radius: 8px; }
  
  .card {
    background: rgba(255, 255, 255, 0.06) !important;
    backdrop-filter: blur(12px);
    border: 1px solid rgba(255, 255, 255, 0.1) !important;
    border-radius: 16px !important;
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.2) !important;
    color: #e0e0e0 !important;
  }
  .card:hover {
    transform: translateY(-4px);
    box-shadow: 0 16px 48px rgba(102, 126, 234, 0.15) !important;
    border-color: rgba(102, 126, 234, 0.3) !important;
  }
  .card-header {
    background: transparent !important;
    border-bottom: 1px solid rgba(255,255,255,0.08) !important;
    font-weight: 600;
    font-size: 0.85rem;
    letter-spacing: 0.5px;
    text-transform: uppercase;
    color: #b8c5e0 !important;
  }
  
  .value-box {
    border-radius: 16px !important;
    border: 1px solid rgba(255,255,255,0.1) !important;
    box-shadow: 0 8px 24px rgba(0,0,0,0.2) !important;
  }
  .value-box:hover {
    transform: translateY(-3px) scale(1.02);
    filter: brightness(1.15);
  }
  .value-box .value-box-title { font-size: 0.7rem !important; text-transform: uppercase; letter-spacing: 1px; opacity: 0.85; }
  .value-box .value-box-value { font-size: 1.6rem !important; font-weight: 700 !important; }
  .value-box p { font-size: 0.65rem !important; opacity: 0.7; margin: 0 !important; }
  .value-box .value-box-showcase .bi { font-size: 1.8rem !important; }
  
  .sidebar {
    background: rgba(15, 12, 41, 0.95) !important;
    border-right: 1px solid rgba(255,255,255,0.08) !important;
    color: #c0c0c0 !important;
  }
  .sidebar .sidebar-title { font-weight: 600; font-size: 0.9rem; color: #fff !important; }
  
  .form-control, .form-select {
    background: rgba(255,255,255,0.08) !important;
    border: 1px solid rgba(255,255,255,0.15) !important;
    color: #e0e0e0 !important;
    border-radius: 10px !important;
  }
  
  .dataTables_wrapper { color: #c0c0c0 !important; font-size: 0.8rem; }
  table.dataTable { color: #d0d0d0 !important; }
  table.dataTable thead th { color: #b8c5e0 !important; font-size: 0.75rem; text-transform: uppercase; }
  .dataTables_info, .dataTables_length, .dataTables_filter { color: #999 !important; }
  
  .nav-pills .nav-link { color: #b0b0b0 !important; border-radius: 10px; font-size: 0.8rem; }
  .nav-pills .nav-link.active { background: linear-gradient(135deg, #667eea, #764ba2) !important; color: #fff !important; }
  
  .nav-underline .nav-link { color: #b0b0b0 !important; }
  .nav-underline .nav-link.active { color: #667eea !important; border-bottom-color: #667eea !important; }
  
  ::-webkit-scrollbar { width: 6px; }
  ::-webkit-scrollbar-track { background: rgba(0,0,0,0.1); }
  ::-webkit-scrollbar-thumb { background: rgba(102, 126, 234, 0.4); border-radius: 3px; }
  
  .plotly .modebar { opacity: 0.5; }
  .plotly .modebar:hover { opacity: 1; }
"

# Plotly dark theme helper
plotly_dark <- function(p, title = "") {
  p %>% layout(
    paper_bgcolor = 'rgba(0,0,0,0)',
    plot_bgcolor = 'rgba(0,0,0,0)',
    font = list(color = '#b0b0b0', family = 'Outfit'),
    title = list(text = title, font = list(size = 13, color = '#d0d0d0')),
    xaxis = list(gridcolor = 'rgba(255,255,255,0.05)', zerolinecolor = 'rgba(255,255,255,0.08)'),
    yaxis = list(gridcolor = 'rgba(255,255,255,0.05)', zerolinecolor = 'rgba(255,255,255,0.08)'),
    legend = list(font = list(color = '#b0b0b0'))
  ) %>% config(displayModeBar = FALSE)
}

# Color palette
pal <- c("#667eea", "#f093fb", "#4facfe", "#43e97b", "#fa709a", "#fee140", "#a18cd1", "#fbc2eb")
outcome_pal <- c("0" = "#4facfe", "1" = "#f093fb")

# UI
ui <- page_navbar(
  title = span(bs_icon("heart-pulse-fill", style="color: #f093fb; margin-right: 8px;"), "DiabetesIQ"),
  id = "nav",
  theme = bs_theme(version = 5, bootswatch = "lux", primary = "#667eea", base_font = font_google("Outfit")),
  header = tagList(useShinyjs(), use_waiter(), tags$style(css)),
  
  sidebar = sidebar(
    title = div(bs_icon("gear-wide-connected"), " Command Center"),
    width = 280,
    
    virtualSelectInput("target_var", "Focus Metric:", choices = names(diabetes_data)[1:8], selected = "Glucose", search = TRUE),
    sliderInput("opacity", "Visual Intensity:", 0.3, 1, 0.7, step = 0.1),
    materialSwitch("show_trend", "AI Trendlines", value = TRUE, status = "primary"),
    hr(),
    actionButton("refresh", "Sync Models", icon = icon("bolt"),
                 style = "background: linear-gradient(135deg, #667eea, #764ba2); color: white; border: none; padding: 10px; border-radius: 12px; width: 100%; font-weight: 600;"),
    br(), br(),
    div(style="text-align:center; opacity:0.5; font-size: 0.65rem;", p("DiabetesIQ v5.0 Elite"))
  ),
  
  # Tab 1: Executive Hub
  nav_panel("Executive Hub",
    layout_column_wrap(
      width = 1/4,
      value_box(title = "Cohort Size", value = textOutput("val_count"), showcase = bs_icon("people-fill"),
                theme = value_box_theme(bg = "linear-gradient(135deg, #667eea, #764ba2)"), p("Total records")),
      value_box(title = "Peak Glucose", value = textOutput("val_glucose"), showcase = bs_icon("graph-up-arrow"),
                theme = value_box_theme(bg = "linear-gradient(135deg, #4facfe, #00f2fe)"), p("Maximum level")),
      value_box(title = "Diabetes Rate", value = textOutput("val_ratio"), showcase = bs_icon("activity"),
                theme = value_box_theme(bg = "linear-gradient(135deg, #f093fb, #f5576c)"), p("Positive outcome")),
      value_box(title = "Median Age", value = textOutput("val_age"), showcase = bs_icon("calendar-heart"),
                theme = value_box_theme(bg = "linear-gradient(135deg, #43e97b, #38f9d7)"), p("Group center"))
    ),
    layout_column_wrap(
      width = 1/2,
      card(card_header(div(bs_icon("bar-chart-steps"), " Age Demographics")), plotlyOutput("age_group_plot", height = "320px")),
      card(card_header(div(bs_icon("pie-chart-fill"), " BMI Classification")), plotlyOutput("bmi_pie_plot", height = "320px"))
    ),
    card(card_header(div(bs_icon("table"), " Live Data Explorer")), DTOutput("raw_data"), full_screen = TRUE)
  ),
  
  # Tab 2: Deep Insights
  nav_panel("Deep Insights",
    navset_card_pill(
      nav_panel("Distributions",
        layout_column_wrap(
          width = 1/2,
          card(card_header("Dynamic Histogram"), plotlyOutput("dist_hist", height = "300px")),
          card(card_header("Violin-Box Hybrid"), plotlyOutput("dist_violin", height = "300px"))
        ),
        card(card_header("Density Landscape"), plotlyOutput("dist_density", height = "280px"))
      ),
      nav_panel("Relationships",
        layout_column_wrap(
          width = 1/2,
          card(card_header("Scatter Correlation"), plotlyOutput("scatter_2d", height = "300px")),
          card(card_header("Age-Metric Trend"), plotlyOutput("trend_plot", height = "300px"))
        ),
        card(card_header("Parallel Coordinate Profiles"), plotlyOutput("parallel_plot", height = "300px"))
      )
    )
  ),
  
  # Tab 3: 3D Discovery
  nav_panel("3D Discovery",
    layout_column_wrap(
      width = 1/2,
      card(card_header("Metabolic 3D Space"), plotlyOutput("model_3d_1", height = "400px"), full_screen = TRUE),
      card(card_header("Biometric 3D Space"), plotlyOutput("model_3d_2", height = "400px"), full_screen = TRUE)
    ),
    card(card_header("Interactive Correlation Heatmap"), plotlyOutput("interactive_heatmap", height = "380px"), full_screen = TRUE)
  ),
  
  # Tab 4: AI Analytics
  nav_panel("AI Analytics",
    layout_sidebar(
      sidebar = sidebar(
        title = "ML Config",
        numericInput("k_clusters", "Cluster K:", 3, 2, 6),
        checkboxGroupInput("cluster_vars", "Cluster Features:",
                           choices = names(diabetes_data)[1:8],
                           selected = c("Glucose", "BMI", "Age"))
      ),
      navset_card_underline(
        nav_panel("K-Means 3D", plotlyOutput("cluster_3d", height = "400px")),
        nav_panel("PCA Variance", plotlyOutput("pca_plot", height = "400px")),
        nav_panel("Feature Impact", plotlyOutput("importance_plot", height = "400px"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Loading animation (fixed: use spin_3 instead of spin_fold)
  w <- Waiter$new(html = tagList(spin_3(), br(), h4("Syncing Models...", style="color:#b0b0b0; font-family:Outfit;")), color = "rgba(15,12,41,0.95)")
  
  observeEvent(input$refresh, {
    w$show()
    Sys.sleep(1.2)
    w$hide()
    sendSweetAlert(session, title = "Synced!", text = "All analytics models refreshed.", type = "success")
  })

  # Value Boxes
  output$val_count <- renderText({ nrow(diabetes_data) })
  output$val_glucose <- renderText({ max(diabetes_data$Glucose) })
  output$val_ratio <- renderText({ scales::percent(mean(as.numeric(as.character(diabetes_data$Outcome)))) })
  output$val_age <- renderText({ paste(median(diabetes_data$Age), "yrs") })

  # Data Table
  output$raw_data <- renderDT({
    datatable(diabetes_data, options = list(pageLength = 8, scrollX = TRUE, dom = 'frtip'),
              class = 'compact hover', rownames = FALSE)
  })

  # Age Group
  output$age_group_plot <- renderPlotly({
    p <- ggplot(diabetes_data, aes(x = Age_Group, fill = Outcome)) +
      geom_bar(position = "dodge", alpha = input$opacity, width = 0.7) +
      scale_fill_manual(values = c("#4facfe", "#f093fb"), labels = c("Healthy", "Diabetic")) +
      theme_void() + theme(legend.position = "top", legend.text = element_text(color = "#b0b0b0", size = 9),
                           axis.text.x = element_text(color = "#b0b0b0", size = 9)) +
      labs(fill = NULL)
    ggplotly(p) %>% plotly_dark()
  })
  
  # BMI Pie
  output$bmi_pie_plot <- renderPlotly({
    df <- diabetes_data %>% group_by(BMI_Cat) %>% summarise(count = n(), .groups = "drop")
    plot_ly(df, labels = ~BMI_Cat, values = ~count, type = 'pie', hole = 0.55,
            marker = list(colors = c("#667eea", "#4facfe", "#f093fb", "#fa709a"),
                         line = list(color = 'rgba(255,255,255,0.15)', width = 2)),
            textfont = list(color = '#fff', size = 11)) %>% plotly_dark()
  })

  # Histogram
  output$dist_hist <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x = input$target_var, fill = "Outcome")) +
      geom_histogram(bins = 30, alpha = input$opacity, color = "transparent") +
      scale_fill_manual(values = c("#4facfe", "#f093fb"), labels = c("Healthy", "Diabetic")) +
      theme_void() + theme(legend.position = "top", legend.text = element_text(color = "#b0b0b0", size = 9))
    ggplotly(p) %>% plotly_dark()
  })

  # Violin
  output$dist_violin <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x = "Outcome", y = input$target_var, fill = "Outcome")) +
      geom_violin(alpha = input$opacity, color = "transparent") +
      geom_boxplot(width = 0.12, fill = "rgba(255,255,255,0.15)", color = "#fff", alpha = 0.6, outlier.shape = NA) +
      scale_fill_manual(values = c("#4facfe", "#f093fb")) +
      theme_void() + theme(legend.position = "none", axis.text = element_text(color = "#b0b0b0", size = 9))
    ggplotly(p) %>% plotly_dark()
  })

  # Density
  output$dist_density <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x = input$target_var, fill = "Outcome")) +
      geom_density(alpha = input$opacity * 0.7, color = "transparent") +
      scale_fill_manual(values = c("#4facfe", "#f093fb"), labels = c("Healthy", "Diabetic")) +
      theme_void() + theme(legend.position = "top", legend.text = element_text(color = "#b0b0b0", size = 9))
    ggplotly(p) %>% plotly_dark()
  })

  # Scatter
  output$scatter_2d <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x = "Glucose", y = input$target_var, color = "Outcome")) +
      geom_point(alpha = input$opacity, size = 1.5) +
      scale_color_manual(values = c("#4facfe", "#f093fb"))
    if(input$show_trend) p <- p + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.8)
    p <- p + theme_void() + theme(legend.position = "none")
    ggplotly(p) %>% plotly_dark()
  })

  # Trend
  output$trend_plot <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x = "Age", y = input$target_var, color = "Outcome")) +
      geom_line(stat = "summary", fun = mean, linewidth = 1.2) +
      scale_color_manual(values = c("#4facfe", "#f093fb")) +
      theme_void() + theme(legend.position = "none")
    ggplotly(p) %>% plotly_dark()
  })

  # Parallel Coordinates
  output$parallel_plot <- renderPlotly({
    plot_ly(diabetes_data, type = 'parcoords',
            line = list(color = ~as.numeric(Outcome), colorscale = list(c(0, '#4facfe'), c(1, '#f093fb'))),
            dimensions = list(
              list(range = range(diabetes_data$Glucose), label = 'Glucose', values = ~Glucose),
              list(range = range(diabetes_data$BMI), label = 'BMI', values = ~BMI),
              list(range = range(diabetes_data$Age), label = 'Age', values = ~Age),
              list(range = range(diabetes_data$BloodPressure), label = 'BP', values = ~BloodPressure)
            )) %>% plotly_dark()
  })

  # 3D Model 1
  output$model_3d_1 <- renderPlotly({
    plot_ly(diabetes_data, x = ~Glucose, y = ~BMI, z = ~Age,
            color = ~Outcome, colors = c("#4facfe", "#f093fb"),
            type = "scatter3d", mode = "markers",
            marker = list(size = 3, opacity = 0.75, line = list(width = 0.5, color = 'rgba(255,255,255,0.2)'))) %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#b0b0b0', family = 'Outfit'),
             scene = list(
               xaxis = list(title = 'Glucose', gridcolor = 'rgba(255,255,255,0.05)', backgroundcolor = 'rgba(0,0,0,0)'),
               yaxis = list(title = 'BMI', gridcolor = 'rgba(255,255,255,0.05)', backgroundcolor = 'rgba(0,0,0,0)'),
               zaxis = list(title = 'Age', gridcolor = 'rgba(255,255,255,0.05)', backgroundcolor = 'rgba(0,0,0,0)')
             ))
  })

  # 3D Model 2
  output$model_3d_2 <- renderPlotly({
    plot_ly(diabetes_data, x = ~Insulin, y = ~SkinThickness, z = ~BloodPressure,
            color = ~Outcome, colors = c("#43e97b", "#fa709a"),
            type = "scatter3d", mode = "markers",
            marker = list(size = 3, opacity = 0.75)) %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#b0b0b0', family = 'Outfit'),
             scene = list(
               xaxis = list(title = 'Insulin', gridcolor = 'rgba(255,255,255,0.05)', backgroundcolor = 'rgba(0,0,0,0)'),
               yaxis = list(title = 'Skin Thickness', gridcolor = 'rgba(255,255,255,0.05)', backgroundcolor = 'rgba(0,0,0,0)'),
               zaxis = list(title = 'Blood Pressure', gridcolor = 'rgba(255,255,255,0.05)', backgroundcolor = 'rgba(0,0,0,0)')
             ))
  })

  # Interactive Heatmap
  output$interactive_heatmap <- renderPlotly({
    cor_mat <- cor(diabetes_data %>% select_if(is.numeric))
    plot_ly(x = colnames(cor_mat), y = rownames(cor_mat), z = cor_mat,
            type = "heatmap", colorscale = list(c(0, '#4facfe'), c(0.5, '#1a1a2e'), c(1, '#f093fb')),
            zmin = -1, zmax = 1) %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#b0b0b0', family = 'Outfit', size = 10),
             xaxis = list(title = ""), yaxis = list(title = ""))
  })

  # Cluster 3D
  output$cluster_3d <- renderPlotly({
    req(input$cluster_vars)
    if(length(input$cluster_vars) < 3) return(NULL)
    cluster_data <- scale(diabetes_data[, input$cluster_vars])
    km <- kmeans(cluster_data, centers = input$k_clusters)
    plot_ly(diabetes_data,
            x = as.formula(paste0("~", input$cluster_vars[1])),
            y = as.formula(paste0("~", input$cluster_vars[2])),
            z = as.formula(paste0("~", input$cluster_vars[3])),
            color = as.factor(km$cluster),
            colors = pal[1:input$k_clusters],
            type = "scatter3d", mode = "markers", marker = list(size = 4, opacity = 0.8)) %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#b0b0b0', family = 'Outfit'),
             title = list(text = paste("K =", input$k_clusters, "Clusters"), font = list(size = 13, color = '#d0d0d0')))
  })

  # PCA
  output$pca_plot <- renderPlotly({
    pca <- prcomp(diabetes_data %>% select_if(is.numeric), scale. = TRUE)
    vars <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)
    plot_ly(x = paste0("PC", 1:length(vars)), y = vars, type = "bar",
            marker = list(color = pal[1:length(vars)],
                         line = list(color = 'rgba(255,255,255,0.15)', width = 1))) %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#b0b0b0', family = 'Outfit'),
             xaxis = list(title = "Component", gridcolor = 'rgba(255,255,255,0.05)'),
             yaxis = list(title = "Variance %", gridcolor = 'rgba(255,255,255,0.05)'))
  })

  # Feature Importance
  output$importance_plot <- renderPlotly({
    fit <- lm(as.numeric(Outcome) ~ ., data = diabetes_data %>% select(-Age_Group, -BMI_Cat))
    imp <- abs(coef(fit)[-1])
    df <- data.frame(Feature = names(imp), Importance = imp) %>% arrange(Importance)
    df$Feature <- factor(df$Feature, levels = df$Feature)
    
    plot_ly(df, x = ~Importance, y = ~Feature, type = "bar", orientation = 'h',
            marker = list(color = ~Importance,
                         colorscale = list(c(0, '#4facfe'), c(1, '#f093fb')),
                         line = list(color = 'rgba(255,255,255,0.1)', width = 1))) %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#b0b0b0', family = 'Outfit'),
             xaxis = list(title = "Impact Score", gridcolor = 'rgba(255,255,255,0.05)'),
             yaxis = list(title = ""))
  })
}

shinyApp(ui, server)
