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

# Enable automatic plot theming
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

# Custom CSS for effects
css <- "
  .card {
    transition: transform 0.3s ease, box-shadow 0.3s ease;
    border-radius: 15px;
    overflow: hidden;
    border: none;
    background: rgba(255, 255, 255, 0.9);
    backdrop-filter: blur(10px);
  }
  .card:hover {
    transform: translateY(-5px);
    box-shadow: 0 10px 20px rgba(0,0,0,0.15);
  }
  .value-box {
    border-radius: 15px;
  }
  .navbar {
    background: #2c3e50 !important;
    box-shadow: 0 2px 10px rgba(0,0,0,0.2);
  }
  body {
    background-color: #f8f9fa;
  }
"

# Define UI
ui <- page_navbar(
  title = "Diabetes Analytics Ultra",
  id = "nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "lux",
    primary = "#1a1a1a",
    secondary = "#6c757d",
    success = "#28a745",
    info = "#17a2b8",
    warning = "#ffc107",
    danger = "#dc3545",
    base_font = font_google("Outfit")
  ),
  header = tagList(
    useShinyjs(),
    use_waiter(),
    tags$style(css)
  ),
  
  sidebar = sidebar(
    title = "Control Panel",
    width = 300,
    pickerInput("target_var", "Primary Metric:", 
                choices = names(diabetes_data)[1:8], 
                selected = "Glucose",
                options = list(`style` = "btn-primary")),
    hr(),
    h6("App Settings"),
    switchInput("show_points", "Show Data Points", value = TRUE),
    colorPickr("color_accent", "Theme Accent", "#3498db"),
    hr(),
    actionButton("refresh", "Recalculate Models", icon = icon("sync"), class = "btn-dark w-100"),
    div(style="margin-top: 20px; text-align: center;",
        helpText("Version 3.0 Ultra Interactivity"))
  ),
  
  nav_panel("Executive Hub",
    layout_column_wrap(
      width = 1/4,
      value_box(
        title = "Patient Count",
        value = nrow(diabetes_data),
        showcase = bs_icon("people-fill"),
        theme = "primary",
        p("Total dataset size")
      ),
      value_box(
        title = "Avg Glucose",
        value = round(mean(diabetes_data$Glucose), 1),
        showcase = bs_icon("droplet-half"),
        theme = "info",
        p("System average")
      ),
      value_box(
        title = "Positive Rate",
        value = scales::percent(mean(as.numeric(as.character(diabetes_data$Outcome)))),
        showcase = bs_icon("activity"),
        theme = "danger",
        p("Diabetic prevalence")
      ),
      value_box(
        title = "Median Age",
        value = median(diabetes_data$Age),
        showcase = bs_icon("calendar-date"),
        theme = "success",
        p("Middle age value")
      )
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Demographic Overview"),
        plotlyOutput("age_group_plot")
      ),
      card(
        card_header("BMI Classification"),
        plotlyOutput("bmi_pie_plot")
      )
    ),
    card(
      card_header("Deep Data Explorer"),
      DTOutput("raw_data"),
      full_screen = TRUE
    )
  ),
  
  nav_panel("Visual Lab",
    navset_card_pill(
      nav_panel("Distributions", 
        layout_column_wrap(
          width = 1/2,
          card(card_header("Histogram Matrix"), plotlyOutput("dist_hist")),
          card(card_header("Violin Analysis"), plotlyOutput("dist_violin"))
        ),
        card(card_header("Density Landscape"), plotlyOutput("dist_density"))
      ),
      nav_panel("Correlations", 
        layout_column_wrap(
          width = 1/2,
          card(card_header("Interactive Scatter"), plotlyOutput("scatter_2d")),
          card(card_header("Cross-Metric Trend"), plotlyOutput("trend_plot"))
        ),
        card(card_header("Parallel Coordinate Profiles"), plotlyOutput("parallel_plot"))
      )
    )
  ),
  
  nav_panel("3D Modeling",
    layout_column_wrap(
      width = 1/2,
      card(card_header("Metabolic 3D Space"), plotlyOutput("model_3d_1"), full_screen = TRUE),
      card(card_header("Biometric 3D Space"), plotlyOutput("model_3d_2"), full_screen = TRUE)
    )
  ),
  
  nav_panel("AI Analytics",
    layout_sidebar(
      sidebar = sidebar(
        title = "ML Parameters",
        sliderInput("k_clusters", "Target Clusters:", 2, 6, 3),
        selectInput("cluster_vars", "Cluster Basis:", 
                    choices = names(diabetes_data)[1:8], 
                    multiple = TRUE, 
                    selected = c("Glucose", "BMI", "Age"))
      ),
      navset_card_underline(
        nav_panel("Clustering Output", plotlyOutput("cluster_3d")),
        nav_panel("Variance Analysis", plotlyOutput("pca_plot")),
        nav_panel("Metric Importance", plotlyOutput("importance_plot")),
        nav_panel("Correlation Map", plotOutput("cor_heatmap"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Loading Screen
  w <- Waiter$new(html = spin_dots(), color = "#1a1a1a")
  
  observeEvent(input$refresh, {
    w$show()
    Sys.sleep(1) # Simulate calc
    w$hide()
  })

  # Reactive Data Filtering (Optional extension)
  
  # Outputs
  output$raw_data <- renderDT({ 
    datatable(diabetes_data, 
              options = list(pageLength = 5, scrollX = TRUE),
              class = 'display nowrap compact') 
  })
  
  output$age_group_plot <- renderPlotly({
    p <- ggplot(diabetes_data, aes(x=Age_Group, fill=Outcome)) + 
      geom_bar(position="dodge", alpha=0.8) + 
      scale_fill_manual(values=c("#3498db", "#e74c3c")) +
      labs(x=NULL, y="Count")
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$bmi_pie_plot <- renderPlotly({
    df <- diabetes_data %>% group_by(BMI_Cat) %>% summarise(count = n())
    plot_ly(df, labels = ~BMI_Cat, values = ~count, type = 'pie', hole = 0.5,
            marker = list(colors = c("#2c3e50", "#34495e", "#7f8c8d", "#bdc3c7")))
  })
  
  output$dist_hist <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x=input$target_var, fill="Outcome")) + 
      geom_histogram(bins=30, alpha=0.7, color="white") + 
      scale_fill_manual(values=c("#3498db", "#e74c3c"))
    ggplotly(p)
  })
  
  output$dist_violin <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x="Outcome", y=input$target_var, fill="Outcome")) + 
      geom_violin(alpha=0.6) + 
      geom_boxplot(width=0.1, fill="white", alpha=0.9) +
      scale_fill_manual(values=c("#3498db", "#e74c3c"))
    ggplotly(p)
  })
  
  output$dist_density <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x=input$target_var, fill="Outcome")) + 
      geom_density(alpha=0.5) + 
      scale_fill_manual(values=c("#3498db", "#e74c3c"))
    ggplotly(p)
  })
  
  output$scatter_2d <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x="Glucose", y=input$target_var, color="Outcome")) + 
      geom_point(alpha=0.5) + 
      geom_smooth(method="lm", se=FALSE) +
      scale_color_manual(values=c("#3498db", "#e74c3c"))
    ggplotly(p)
  })
  
  output$trend_plot <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x="Age", y=input$target_var, color="Outcome")) + 
      geom_line(stat="summary", fun=mean, size=1.2) +
      scale_color_manual(values=c("#3498db", "#e74c3c"))
    ggplotly(p)
  })
  
  output$parallel_plot <- renderPlotly({
    plot_ly(diabetes_data, type = 'parcoords',
            line = list(color = ~as.numeric(Outcome), 
                        colorscale = list(c(0, '#3498db'), c(1, '#e74c3c'))),
            dimensions = list(
              list(range = range(diabetes_data$Glucose), label = 'Glucose', values = ~Glucose),
              list(range = range(diabetes_data$BMI), label = 'BMI', values = ~BMI),
              list(range = range(diabetes_data$Age), label = 'Age', values = ~Age),
              list(range = range(diabetes_data$Insulin), label = 'Insulin', values = ~Insulin)
            ))
  })
  
  output$model_3d_1 <- renderPlotly({
    plot_ly(diabetes_data, x = ~Glucose, y = ~BMI, z = ~Age, 
            color = ~Outcome, colors = c("#3498db", "#e74c3c"),
            type = "scatter3d", mode = "markers", marker=list(size=3, opacity=0.7)) %>%
      layout(scene = list(aspectmode='cube'))
  })
  
  output$model_3d_2 <- renderPlotly({
    plot_ly(diabetes_data, x = ~Insulin, y = ~SkinThickness, z = ~BloodPressure, 
            color = ~Outcome, colors = c("#3498db", "#e74c3c"),
            type = "scatter3d", mode = "markers", marker=list(size=3, opacity=0.7))
  })
  
  output$cor_heatmap <- renderPlot({
    corrplot(cor(diabetes_data %>% select_if(is.numeric)), 
             method="shade", type="upper", tl.col="black", tl.srt=45,
             addCoef.col = "black", number.cex = 0.7)
  })
  
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
            type = "scatter3d", mode = "markers")
  })
  
  output$pca_plot <- renderPlotly({
    pca <- prcomp(diabetes_data %>% select_if(is.numeric), scale. = TRUE)
    vars <- pca$sdev^2 / sum(pca$sdev^2)
    plot_ly(x = paste0("PC", 1:length(vars)), y = vars, type = "bar", 
            marker = list(color = '#2c3e50')) %>% 
      layout(title="Principal Component Variance Contribution")
  })
  
  output$importance_plot <- renderPlotly({
    fit <- lm(as.numeric(Outcome) ~ ., data = diabetes_data %>% select(-Age_Group, -BMI_Cat))
    imp <- abs(coef(fit)[-1])
    df <- data.frame(Feature = names(imp), Importance = imp)
    p <- ggplot(df, aes(x=reorder(Feature, Importance), y=Importance, fill=Importance)) + 
      geom_bar(stat="identity") + coord_flip() + 
      scale_fill_gradient(low="#3498db", high="#1a1a1a") +
      labs(x=NULL)
    ggplotly(p)
  })
}

shinyApp(ui, server)
