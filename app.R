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

# Light Theme CSS
css <- "
  @import url('https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700&display=swap');
  
  * { transition: all 0.2s ease; }
  
  body {
    background: #f0f2f5 !important;
    font-family: 'Outfit', sans-serif !important;
    color: #333;
  }
  
  .navbar {
    background: #1e293b !important;
    box-shadow: 0 2px 15px rgba(0,0,0,0.15);
    border: none !important;
  }
  .navbar .navbar-brand { font-weight: 700; font-size: 1rem; letter-spacing: 0.5px; color: #fff !important; }
  .nav-link { font-weight: 500 !important; font-size: 0.82rem !important; color: #cbd5e1 !important; }
  .nav-link:hover { color: #fff !important; }
  .nav-link.active { background: rgba(99, 102, 241, 0.8) !important; border-radius: 8px; color: #fff !important; }
  
  .card {
    background: #ffffff !important;
    border: 1px solid #e2e8f0 !important;
    border-radius: 14px !important;
    box-shadow: 0 1px 3px rgba(0,0,0,0.06) !important;
    color: #334155 !important;
  }
  .card:hover {
    transform: translateY(-3px);
    box-shadow: 0 8px 25px rgba(0,0,0,0.08) !important;
  }
  .card-header {
    background: #fafbfc !important;
    border-bottom: 1px solid #e2e8f0 !important;
    font-weight: 600;
    font-size: 0.8rem;
    letter-spacing: 0.3px;
    text-transform: uppercase;
    color: #475569 !important;
    padding: 12px 16px !important;
  }
  
  /* VALUE BOX FIX */
  .value-box {
    border-radius: 14px !important;
    border: none !important;
    box-shadow: 0 4px 15px rgba(0,0,0,0.1) !important;
    min-height: 120px !important;
    overflow: visible !important;
  }
  .value-box:hover {
    transform: translateY(-3px);
    box-shadow: 0 8px 25px rgba(0,0,0,0.15) !important;
  }
  .value-box .value-box-title {
    font-size: 0.65rem !important;
    font-weight: 600 !important;
    text-transform: uppercase;
    letter-spacing: 1.2px;
    opacity: 0.9;
    color: rgba(255,255,255,0.9) !important;
    line-height: 1.2 !important;
    margin-bottom: 4px !important;
  }
  .value-box .value-box-value {
    font-size: 1.5rem !important;
    font-weight: 700 !important;
    color: #fff !important;
    line-height: 1.3 !important;
    margin-bottom: 2px !important;
  }
  .value-box .value-box-showcase {
    opacity: 0.85;
  }
  .value-box .value-box-showcase .bi {
    font-size: 2rem !important;
    color: rgba(255,255,255,0.9) !important;
  }
  .value-box .value-box-area p {
    font-size: 0.6rem !important;
    opacity: 0.75;
    color: rgba(255,255,255,0.85) !important;
    margin: 0 !important;
    padding: 0 !important;
    line-height: 1.2 !important;
  }
  
  .sidebar {
    background: #fff !important;
    border-right: 1px solid #e2e8f0 !important;
    color: #475569 !important;
  }
  .sidebar .sidebar-title { font-weight: 600; font-size: 0.85rem; color: #1e293b !important; }
  
  .form-control, .form-select {
    background: #f8fafc !important;
    border: 1px solid #e2e8f0 !important;
    color: #334155 !important;
    border-radius: 8px !important;
    font-size: 0.85rem !important;
  }
  .form-label, label { color: #475569 !important; font-size: 0.8rem !important; }
  
  .dataTables_wrapper { color: #475569 !important; font-size: 0.8rem; }
  table.dataTable { color: #334155 !important; }
  table.dataTable thead th { color: #1e293b !important; font-size: 0.75rem; text-transform: uppercase; font-weight: 600; }
  .dataTables_info, .dataTables_length, .dataTables_filter { color: #64748b !important; }
  
  .nav-pills .nav-link { color: #64748b !important; border-radius: 8px; font-size: 0.8rem; }
  .nav-pills .nav-link.active { background: #6366f1 !important; color: #fff !important; }
  
  .nav-underline .nav-link { color: #64748b !important; }
  .nav-underline .nav-link.active { color: #6366f1 !important; border-bottom-color: #6366f1 !important; }
  
  ::-webkit-scrollbar { width: 6px; }
  ::-webkit-scrollbar-track { background: #f1f5f9; }
  ::-webkit-scrollbar-thumb { background: #cbd5e1; border-radius: 3px; }
  
  .plotly .modebar { opacity: 0.4; }
  .plotly .modebar:hover { opacity: 1; }
  
  .btn-sync {
    background: #6366f1 !important; color: #fff !important; border: none !important;
    padding: 10px !important; border-radius: 10px !important; width: 100%;
    font-weight: 600 !important; font-size: 0.85rem !important;
  }
  .btn-sync:hover { background: #4f46e5 !important; }
"

# Plotly light theme helper
plotly_light <- function(p, title = "") {
  p %>% layout(
    paper_bgcolor = 'rgba(0,0,0,0)',
    plot_bgcolor = 'rgba(0,0,0,0)',
    font = list(color = '#475569', family = 'Outfit', size = 11),
    title = list(text = title, font = list(size = 13, color = '#1e293b')),
    xaxis = list(gridcolor = '#f1f5f9', zerolinecolor = '#e2e8f0'),
    yaxis = list(gridcolor = '#f1f5f9', zerolinecolor = '#e2e8f0'),
    legend = list(font = list(color = '#475569', size = 10))
  ) %>% config(displayModeBar = FALSE)
}

# Color palette
pal <- c("#6366f1", "#f43f5e", "#0ea5e9", "#10b981", "#f59e0b", "#8b5cf6", "#ec4899", "#14b8a6")
chart_healthy <- "#0ea5e9"
chart_diabetic <- "#f43f5e"

# UI
ui <- page_navbar(
  title = span(bs_icon("heart-pulse-fill", style="color: #f43f5e; margin-right: 8px;"), "DiabetesIQ"),
  id = "nav",
  theme = bs_theme(version = 5, bootswatch = "lux", primary = "#6366f1", base_font = font_google("Outfit")),
  header = tagList(useShinyjs(), use_waiter(), tags$style(css)),
  
  sidebar = sidebar(
    title = div(bs_icon("gear-wide-connected"), " Control Panel"),
    width = 280,
    
    virtualSelectInput("target_var", "Focus Metric:", choices = names(diabetes_data)[1:8], selected = "Glucose", search = TRUE),
    sliderInput("opacity", "Plot Transparency:", 0.3, 1, 0.7, step = 0.1),
    materialSwitch("show_trend", "Show Trendlines", value = TRUE, status = "primary"),
    hr(),
    actionButton("refresh", "Sync Models", icon = icon("rotate"), class = "btn-sync"),
    br(), br(),
    div(style="text-align:center; opacity:0.4; font-size: 0.6rem;", p("DiabetesIQ v6.0"))
  ),
  
  # Tab 1
  nav_panel("Executive Hub",
    layout_column_wrap(
      width = 1/4,
      value_box(
        title = "Total Patients", 
        value = nrow(diabetes_data),
        showcase = bs_icon("people-fill"),
        theme = value_box_theme(bg = "#6366f1", fg = "#fff")
      ),
      value_box(
        title = "Avg Glucose", 
        value = paste0(round(mean(diabetes_data$Glucose), 1), " mg/dL"),
        showcase = bs_icon("droplet-half"),
        theme = value_box_theme(bg = "#0ea5e9", fg = "#fff")
      ),
      value_box(
        title = "Diabetes Rate", 
        value = paste0(round(mean(as.numeric(as.character(diabetes_data$Outcome))) * 100, 1), "%"),
        showcase = bs_icon("activity"),
        theme = value_box_theme(bg = "#f43f5e", fg = "#fff")
      ),
      value_box(
        title = "Median Age", 
        value = paste0(median(diabetes_data$Age), " Years"),
        showcase = bs_icon("calendar-heart"),
        theme = value_box_theme(bg = "#10b981", fg = "#fff")
      )
    ),
    layout_column_wrap(
      width = 1/2,
      card(card_header(div(bs_icon("bar-chart-steps"), " Age Demographics")), plotlyOutput("age_group_plot", height = "320px")),
      card(card_header(div(bs_icon("pie-chart-fill"), " BMI Classification")), plotlyOutput("bmi_pie_plot", height = "320px"))
    ),
    card(card_header(div(bs_icon("table"), " Data Explorer")), DTOutput("raw_data"), full_screen = TRUE)
  ),
  
  # Tab 2
  nav_panel("Deep Insights",
    navset_card_pill(
      nav_panel("Distributions",
        layout_column_wrap(
          width = 1/2,
          card(card_header("Histogram Analysis"), plotlyOutput("dist_hist", height = "300px")),
          card(card_header("Violin + Box Plot"), plotlyOutput("dist_violin", height = "300px"))
        ),
        card(card_header("Density Overlay"), plotlyOutput("dist_density", height = "280px"))
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
  
  # Tab 3
  nav_panel("3D Discovery",
    layout_column_wrap(
      width = 1/2,
      card(card_header("Metabolic 3D Space"), plotlyOutput("model_3d_1", height = "400px"), full_screen = TRUE),
      card(card_header("Biometric 3D Space"), plotlyOutput("model_3d_2", height = "400px"), full_screen = TRUE)
    ),
    card(card_header("Interactive Correlation Heatmap"), plotlyOutput("interactive_heatmap", height = "380px"), full_screen = TRUE)
  ),
  
  # Tab 4
  nav_panel("AI Analytics",
    layout_sidebar(
      sidebar = sidebar(
        title = "ML Settings",
        numericInput("k_clusters", "Cluster Count:", 3, 2, 6),
        checkboxGroupInput("cluster_vars", "Features:",
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

  w <- Waiter$new(html = tagList(spin_3(), br(), h4("Refreshing...", style="color:#475569; font-family:Outfit;")), color = "rgba(240,242,245,0.95)")
  
  observeEvent(input$refresh, {
    w$show()
    Sys.sleep(1)
    w$hide()
    sendSweetAlert(session, title = "Done!", text = "Models refreshed successfully.", type = "success")
  })

  # Data Table
  output$raw_data <- renderDT({
    datatable(diabetes_data, options = list(pageLength = 8, scrollX = TRUE, dom = 'frtip'),
              class = 'compact hover stripe', rownames = FALSE)
  })

  # Age Group Bar
  output$age_group_plot <- renderPlotly({
    p <- ggplot(diabetes_data, aes(x = Age_Group, fill = Outcome)) +
      geom_bar(position = "dodge", alpha = input$opacity, width = 0.7) +
      scale_fill_manual(values = c(chart_healthy, chart_diabetic), labels = c("Healthy", "Diabetic")) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top", panel.grid.minor = element_blank()) +
      labs(fill = NULL, x = NULL, y = "Count")
    ggplotly(p) %>% plotly_light()
  })
  
  # BMI Pie
  output$bmi_pie_plot <- renderPlotly({
    df <- diabetes_data %>% group_by(BMI_Cat) %>% summarise(count = n(), .groups = "drop")
    plot_ly(df, labels = ~BMI_Cat, values = ~count, type = 'pie', hole = 0.5,
            marker = list(colors = c("#6366f1", "#0ea5e9", "#f59e0b", "#f43f5e"),
                         line = list(color = '#fff', width = 2)),
            textfont = list(color = '#334155', size = 11),
            textinfo = "label+percent") %>% plotly_light()
  })

  # Histogram
  output$dist_hist <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x = input$target_var, fill = "Outcome")) +
      geom_histogram(bins = 30, alpha = input$opacity, color = "white") +
      scale_fill_manual(values = c(chart_healthy, chart_diabetic), labels = c("Healthy", "Diabetic")) +
      theme_minimal(base_size = 11) + theme(legend.position = "top", panel.grid.minor = element_blank()) +
      labs(fill = NULL, x = input$target_var, y = "Frequency")
    ggplotly(p) %>% plotly_light()
  })

  # Violin
  output$dist_violin <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x = "Outcome", y = input$target_var, fill = "Outcome")) +
      geom_violin(alpha = input$opacity) +
      geom_boxplot(width = 0.12, fill = "white", alpha = 0.8, outlier.shape = NA) +
      scale_fill_manual(values = c(chart_healthy, chart_diabetic), labels = c("Healthy", "Diabetic")) +
      theme_minimal(base_size = 11) + theme(legend.position = "none", panel.grid.minor = element_blank()) +
      labs(x = "Outcome", y = input$target_var)
    ggplotly(p) %>% plotly_light()
  })

  # Density
  output$dist_density <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x = input$target_var, fill = "Outcome")) +
      geom_density(alpha = input$opacity * 0.6) +
      scale_fill_manual(values = c(chart_healthy, chart_diabetic), labels = c("Healthy", "Diabetic")) +
      theme_minimal(base_size = 11) + theme(legend.position = "top", panel.grid.minor = element_blank()) +
      labs(fill = NULL, x = input$target_var, y = "Density")
    ggplotly(p) %>% plotly_light()
  })

  # Scatter
  output$scatter_2d <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x = "Glucose", y = input$target_var, color = "Outcome")) +
      geom_point(alpha = input$opacity, size = 1.8) +
      scale_color_manual(values = c(chart_healthy, chart_diabetic), labels = c("Healthy", "Diabetic"))
    if(input$show_trend) p <- p + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.8)
    p <- p + theme_minimal(base_size = 11) + theme(legend.position = "top", panel.grid.minor = element_blank()) +
      labs(color = NULL, x = "Glucose", y = input$target_var)
    ggplotly(p) %>% plotly_light()
  })

  # Trend
  output$trend_plot <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x = "Age", y = input$target_var, color = "Outcome")) +
      geom_line(stat = "summary", fun = mean, linewidth = 1.2) +
      scale_color_manual(values = c(chart_healthy, chart_diabetic), labels = c("Healthy", "Diabetic")) +
      theme_minimal(base_size = 11) + theme(legend.position = "top", panel.grid.minor = element_blank()) +
      labs(color = NULL, x = "Age", y = paste("Mean", input$target_var))
    ggplotly(p) %>% plotly_light()
  })

  # Parallel Coordinates
  output$parallel_plot <- renderPlotly({
    plot_ly(diabetes_data, type = 'parcoords',
            line = list(color = ~as.numeric(Outcome), colorscale = list(c(0, chart_healthy), c(1, chart_diabetic))),
            dimensions = list(
              list(range = range(diabetes_data$Glucose), label = 'Glucose', values = ~Glucose),
              list(range = range(diabetes_data$BMI), label = 'BMI', values = ~BMI),
              list(range = range(diabetes_data$Age), label = 'Age', values = ~Age),
              list(range = range(diabetes_data$BloodPressure), label = 'BP', values = ~BloodPressure)
            )) %>% plotly_light()
  })

  # 3D Model 1
  output$model_3d_1 <- renderPlotly({
    plot_ly(diabetes_data, x = ~Glucose, y = ~BMI, z = ~Age,
            color = ~Outcome, colors = c(chart_healthy, chart_diabetic),
            type = "scatter3d", mode = "markers",
            marker = list(size = 3, opacity = 0.7)) %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#475569', family = 'Outfit'),
             scene = list(
               xaxis = list(title = 'Glucose', backgroundcolor = '#fafbfc', gridcolor = '#e2e8f0'),
               yaxis = list(title = 'BMI', backgroundcolor = '#fafbfc', gridcolor = '#e2e8f0'),
               zaxis = list(title = 'Age', backgroundcolor = '#fafbfc', gridcolor = '#e2e8f0')
             ))
  })

  # 3D Model 2
  output$model_3d_2 <- renderPlotly({
    plot_ly(diabetes_data, x = ~Insulin, y = ~SkinThickness, z = ~BloodPressure,
            color = ~Outcome, colors = c("#10b981", "#f59e0b"),
            type = "scatter3d", mode = "markers",
            marker = list(size = 3, opacity = 0.7)) %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#475569', family = 'Outfit'),
             scene = list(
               xaxis = list(title = 'Insulin', backgroundcolor = '#fafbfc', gridcolor = '#e2e8f0'),
               yaxis = list(title = 'Skin Thickness', backgroundcolor = '#fafbfc', gridcolor = '#e2e8f0'),
               zaxis = list(title = 'Blood Pressure', backgroundcolor = '#fafbfc', gridcolor = '#e2e8f0')
             ))
  })

  # Interactive Heatmap
  output$interactive_heatmap <- renderPlotly({
    cor_mat <- cor(diabetes_data %>% select_if(is.numeric))
    plot_ly(x = colnames(cor_mat), y = rownames(cor_mat), z = cor_mat,
            type = "heatmap", colorscale = "RdBu", reversescale = TRUE,
            zmin = -1, zmax = 1) %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#475569', family = 'Outfit', size = 10),
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
            color = as.factor(km$cluster), colors = pal[1:input$k_clusters],
            type = "scatter3d", mode = "markers", marker = list(size = 4, opacity = 0.8)) %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#475569', family = 'Outfit'),
             title = list(text = paste("K =", input$k_clusters, "Clusters"), font = list(size = 13, color = '#1e293b')))
  })

  # PCA
  output$pca_plot <- renderPlotly({
    pca <- prcomp(diabetes_data %>% select_if(is.numeric), scale. = TRUE)
    vars <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)
    plot_ly(x = paste0("PC", 1:length(vars)), y = vars, type = "bar",
            marker = list(color = pal[1:length(vars)])) %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#475569', family = 'Outfit'),
             xaxis = list(title = "Component", gridcolor = '#f1f5f9'),
             yaxis = list(title = "Variance %", gridcolor = '#f1f5f9'))
  })

  # Feature Importance
  output$importance_plot <- renderPlotly({
    fit <- lm(as.numeric(Outcome) ~ ., data = diabetes_data %>% select(-Age_Group, -BMI_Cat))
    imp <- abs(coef(fit)[-1])
    df <- data.frame(Feature = names(imp), Importance = imp) %>% arrange(Importance)
    df$Feature <- factor(df$Feature, levels = df$Feature)
    
    plot_ly(df, x = ~Importance, y = ~Feature, type = "bar", orientation = 'h',
            marker = list(color = ~Importance,
                         colorscale = list(c(0, '#0ea5e9'), c(1, '#6366f1')))) %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#475569', family = 'Outfit'),
             xaxis = list(title = "Impact Score", gridcolor = '#f1f5f9'),
             yaxis = list(title = ""))
  })
}

shinyApp(ui, server)
