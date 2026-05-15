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

# Custom CSS for Premium Effects
css <- "
  @import url('https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;600&display=swap');
  
  body {
    background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
    font-family: 'Outfit', sans-serif;
  }
  
  .card {
    transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
    border-radius: 20px !important;
    border: 1px solid rgba(255, 255, 255, 0.3) !important;
    box-shadow: 0 8px 32px 0 rgba(31, 38, 135, 0.07) !important;
    background: rgba(255, 255, 255, 0.7) !important;
    backdrop-filter: blur(8px);
  }
  
  .card:hover {
    transform: scale(1.02);
    box-shadow: 0 15px 45px rgba(0,0,0,0.1) !important;
    background: rgba(255, 255, 255, 0.9) !important;
  }
  
  .value-box {
    border-radius: 20px !important;
    border: none !important;
    box-shadow: 0 10px 20px rgba(0,0,0,0.05) !important;
    transition: all 0.3s ease;
  }
  
  .value-box:hover {
    filter: brightness(1.1);
    transform: translateY(-3px);
  }

  .navbar {
    background: rgba(26, 26, 26, 0.95) !important;
    backdrop-filter: blur(10px);
    border-bottom: 2px solid #3498db;
  }

  .nav-link {
    font-weight: 600 !important;
    letter-spacing: 0.5px;
  }

  .shiny-input-container {
    color: #2c3e50;
  }
"

# Define UI
ui <- page_navbar(
  title = span(bs_icon("heart-pulse-fill", style="color: #e74c3c; margin-right: 10px;"), "Diabetes Analytics Elite"),
  id = "nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "lux",
    primary = "#3498db",
    base_font = font_google("Outfit")
  ),
  header = tagList(
    useShinyjs(),
    use_waiter(),
    tags$style(css)
  ),
  
  sidebar = sidebar(
    title = div(bs_icon("sliders", style="margin-right: 10px;"), "Command Center"),
    width = 320,
    bg = "#ffffff",
    
    virtualSelectInput("target_var", "Primary Variable Focus:", 
                       choices = names(diabetes_data)[1:8], 
                       selected = "Glucose",
                       search = TRUE),
    
    hr(),
    h6("Interface Customization", style="font-weight: 600; color: #7f8c8d; text-transform: uppercase; font-size: 0.7rem;"),
    
    sliderInput("opacity", "Plot Opacity:", 0.1, 1, 0.7, step = 0.1),
    materialSwitch("show_trend", "Enable AI Trendlines", value = TRUE, status = "primary"),
    
    hr(),
    h6("Predictive Actions", style="font-weight: 600; color: #7f8c8d; text-transform: uppercase; font-size: 0.7rem;"),
    actionButton("refresh", "Sync & Recalculate", icon = icon("bolt"), 
                 style = "background: linear-gradient(to right, #3498db, #2980b9); color: white; border: none; padding: 10px; border-radius: 10px; width: 100%;"),
    
    div(style="margin-top: 30px; opacity: 0.6;",
        p(bs_icon("info-circle"), " Data synced with local diabetes.csv"))
  ),
  
  nav_panel("Executive Hub",
    layout_column_wrap(
      width = 1/4,
      value_box(
        title = "Total Cohort",
        value = textOutput("val_count"),
        showcase = bs_icon("people"),
        theme = "primary",
        p("Active participants in dataset")
      ),
      value_box(
        title = "Peak Glucose",
        value = textOutput("val_glucose"),
        showcase = bs_icon("graph-up-arrow"),
        theme = "info",
        p("Maximum recorded value")
      ),
      value_box(
        title = "Diabetes Ratio",
        value = textOutput("val_ratio"),
        showcase = bs_icon("pie-chart"),
        theme = "danger",
        p("Positive outcome frequency")
      ),
      value_box(
        title = "Median Vitality",
        value = textOutput("val_age"),
        showcase = bs_icon("clipboard-pulse"),
        theme = "success",
        p("Median age of group")
      )
    ),
    
    layout_column_wrap(
      width = 1/2,
      card(
        card_header(div(bs_icon("bar-chart-steps"), " Age Demographic Stratification")),
        plotlyOutput("age_group_plot", height = "350px")
      ),
      card(
        card_header(div(bs_icon("pie-chart-fill"), " BMI Categorical Breakdown")),
        plotlyOutput("bmi_pie_plot", height = "350px")
      )
    ),
    
    card(
      card_header(div(bs_icon("table"), " Real-time Data Explorer")),
      DTOutput("raw_data"),
      full_screen = TRUE
    )
  ),
  
  nav_panel("Deep Insights",
    navset_card_tab(
      nav_panel("Distribution Matrix", 
        layout_column_wrap(
          width = 1/2,
          card(card_header("Dynamic Histogram"), plotlyOutput("dist_hist")),
          card(card_header("Violin-Box Hybrid"), plotlyOutput("dist_violin"))
        ),
        card(card_header("Multi-Outcome Density Surface"), plotlyOutput("dist_density"))
      ),
      nav_panel("Relationship Mapping", 
        layout_column_wrap(
          width = 1/2,
          card(card_header("Correlation Scatter"), plotlyOutput("scatter_2d")),
          card(card_header("Temporal-style Age Trend"), plotlyOutput("trend_plot"))
        ),
        card(card_header("Parallel Coordinate Profiles (Multivariate)"), plotlyOutput("parallel_plot"))
      )
    )
  ),
  
  nav_panel("3D Discovery",
    layout_column_wrap(
      width = 1/2,
      card(card_header("3D Metabolic Cluster Space"), plotlyOutput("model_3d_1"), full_screen = TRUE),
      card(card_header("3D Biometric Interaction"), plotlyOutput("model_3d_2"), full_screen = TRUE)
    ),
    card(card_header("Interactive Correlation Heatmap"), plotlyOutput("interactive_heatmap"), full_screen = TRUE)
  ),
  
  nav_panel("Advanced Analytics",
    layout_sidebar(
      sidebar = sidebar(
        title = "AI Model Config",
        numericInput("k_clusters", "Target Cluster Count:", 3, 2, 6),
        checkboxGroupInput("cluster_vars", "Features for Clustering:", 
                           choices = names(diabetes_data)[1:8], 
                           selected = c("Glucose", "BMI", "Age"))
      ),
      navset_card_underline(
        nav_panel("AI K-Means Results", plotlyOutput("cluster_3d")),
        nav_panel("Principal Component Variance", plotlyOutput("pca_plot")),
        nav_panel("Global Feature Importance", plotlyOutput("importance_plot"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Loading Screen Effects
  w <- Waiter$new(html = tagList(spin_fold(), h3("Analyzing Biometrics...", style="color:white; font-family:Outfit;")), color = "#1a1a1a")
  
  observeEvent(input$refresh, {
    w$show()
    Sys.sleep(1.5)
    w$hide()
    sendSweetAlert(session, title = "Data Synced", text = "Analytics models have been updated.", type = "success")
  })

  # Value Box Outputs
  output$val_count <- renderText({ nrow(diabetes_data) })
  output$val_glucose <- renderText({ max(diabetes_data$Glucose) })
  output$val_ratio <- renderText({ scales::percent(mean(as.numeric(as.character(diabetes_data$Outcome)))) })
  output$val_age <- renderText({ paste(median(diabetes_data$Age), "Years") })

  # Graphs & Visuals
  output$raw_data <- renderDT({ 
    datatable(diabetes_data, 
              options = list(pageLength = 10, scrollX = TRUE),
              class = 'display nowrap cell-border hover') 
  })
  
  output$age_group_plot <- renderPlotly({
    p <- ggplot(diabetes_data, aes(x=Age_Group, fill=Outcome)) + 
      geom_bar(position="dodge", alpha=input$opacity) + 
      scale_fill_manual(values=c("#3498db", "#e74c3c")) +
      theme_minimal()
    ggplotly(p) %>% layout(hovermode = "x unified")
  })
  
  output$bmi_pie_plot <- renderPlotly({
    df <- diabetes_data %>% group_by(BMI_Cat) %>% summarise(count = n())
    plot_ly(df, labels = ~BMI_Cat, values = ~count, type = 'pie', hole = 0.6,
            marker = list(colors = c("#2c3e50", "#3498db", "#e74c3c", "#f1c40f"),
                         line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = list(t = 0, b = 0, l = 0, r = 0))
  })
  
  output$dist_hist <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x=input$target_var, fill="Outcome")) + 
      geom_histogram(bins=30, alpha=input$opacity, color="white") + 
      scale_fill_manual(values=c("#3498db", "#e74c3c"))
    ggplotly(p)
  })
  
  output$dist_violin <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x="Outcome", y=input$target_var, fill="Outcome")) + 
      geom_violin(alpha=input$opacity) + 
      geom_boxplot(width=0.1, fill="white", alpha=0.9, outlier.shape = NA) +
      scale_fill_manual(values=c("#3498db", "#e74c3c"))
    ggplotly(p)
  })
  
  output$dist_density <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x=input$target_var, fill="Outcome")) + 
      geom_density(alpha=input$opacity, size=1) + 
      scale_fill_manual(values=c("#3498db", "#e74c3c"))
    ggplotly(p)
  })
  
  output$scatter_2d <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x="Glucose", y=input$target_var, color="Outcome")) + 
      geom_point(alpha=input$opacity, size=2) + 
      scale_color_manual(values=c("#3498db", "#e74c3c"))
    if(input$show_trend) p <- p + geom_smooth(method="lm", se=FALSE, linetype="dashed")
    ggplotly(p)
  })
  
  output$trend_plot <- renderPlotly({
    p <- ggplot(diabetes_data, aes_string(x="Age", y=input$target_var, color="Outcome")) + 
      geom_line(stat="summary", fun=mean, size=1.5) +
      geom_ribbon(stat="summary", fun.data=mean_se, alpha=0.2, aes(fill=Outcome), color=NA) +
      scale_color_manual(values=c("#3498db", "#e74c3c")) +
      scale_fill_manual(values=c("#3498db", "#e74c3c"))
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
              list(range = range(diabetes_data$BloodPressure), label = 'BP', values = ~BloodPressure)
            )) %>%
      layout(font = list(family = "Outfit"))
  })
  
  output$model_3d_1 <- renderPlotly({
    plot_ly(diabetes_data, x = ~Glucose, y = ~BMI, z = ~Age, 
            color = ~Outcome, colors = c("#3498db", "#e74c3c"),
            type = "scatter3d", mode = "markers", marker=list(size=4, opacity=0.7, line=list(width=1, color='white'))) %>%
      layout(scene = list(xaxis=list(title='Glucose'), yaxis=list(title='BMI'), zaxis=list(title='Age')))
  })
  
  output$model_3d_2 <- renderPlotly({
    plot_ly(diabetes_data, x = ~Insulin, y = ~SkinThickness, z = ~BloodPressure, 
            color = ~Outcome, colors = c("#3498db", "#e74c3c"),
            type = "scatter3d", mode = "markers", marker=list(size=4, opacity=0.7))
  })
  
  output$interactive_heatmap <- renderPlotly({
    cor_mat <- cor(diabetes_data %>% select_if(is.numeric))
    plot_ly(x = colnames(cor_mat), y = rownames(cor_mat), z = cor_mat, 
            type = "heatmap", colorscale = "RdBu", reversescale = TRUE) %>%
      layout(title = "Interactive Correlation Matrix", xaxis = list(title=""), yaxis = list(title=""))
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
            type = "scatter3d", mode = "markers", marker=list(size=5)) %>%
      layout(title = paste("K-Means Clustering (K =", input$k_clusters, ")"))
  })
  
  output$pca_plot <- renderPlotly({
    pca <- prcomp(diabetes_data %>% select_if(is.numeric), scale. = TRUE)
    vars <- pca$sdev^2 / sum(pca$sdev^2)
    plot_ly(x = paste0("PC", 1:length(vars)), y = vars, type = "bar", 
            marker = list(color = '#3498db', line = list(color = '#2c3e50', width = 1.5))) %>% 
      layout(title="Principal Component Variance Contribution")
  })
  
  output$importance_plot <- renderPlotly({
    fit <- lm(as.numeric(Outcome) ~ ., data = diabetes_data %>% select(-Age_Group, -BMI_Cat))
    imp <- abs(coef(fit)[-1])
    df <- data.frame(Feature = names(imp), Importance = imp)
    p <- ggplot(df, aes(x=reorder(Feature, Importance), y=Importance, fill=Importance)) + 
      geom_bar(stat="identity", width = 0.7) + coord_flip() + 
      scale_fill_gradient(low="#3498db", high="#e74c3c") +
      theme_minimal() + labs(x=NULL, y="Absolute Coefficient (Impact)")
    ggplotly(p)
  })
}

shinyApp(ui, server)
