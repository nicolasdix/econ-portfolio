library(shiny)
library(bslib)
library(quantmod)
library(tidyverse)
library(here)
library(ggplot2)

filepath <- here("monte_carlo_stock/function.R")
source(filepath)

if(file.exists(filepath)) {
  source(filepath)
} else {
  stop(paste("Could not find function.R at:", filepath))
}

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "flatly"), 
  title = "GBM Stock Predictor",
  
  sidebar = sidebar(
    title = "Configuration",
    # Input: Stock Symbol
    textInput("symbol", "Stock Symbol", value = "AMZN"),
    
    # Input: Date
    dateInput("start_date", "Historical Data Start", value = "2019-01-01"),
    
    # Input: Days to simulate
    numericInput("days", "Days Ahead", value = 252, min = 10, max = 1000),
    
    # Input: Paths (Slider is easier for UI than typing numbers)
    sliderInput("paths", "Number of Paths", min = 100, max = 20000, value = 1000, step = 100),
    
    # Action Button: The trigger
    actionButton("run", "Run Simulation", class = "btn-primary w-100 mt-3")
  ),
  
  # Dashboard Layout
  layout_columns(
    fill = FALSE,
    # Metric Cards
    value_box(
      title = "Current Price (S0)",
      value = textOutput("stat_s0"),
      showcase = bsicons::bs_icon("currency-dollar")
    ),
    value_box(
      title = "Expected Return",
      value = textOutput("stat_ret"),
      showcase = bsicons::bs_icon("graph-up-arrow")
    ),
    value_box(
      title = "Prob. of Loss",
      value = textOutput("stat_loss"),
      showcase = bsicons::bs_icon("shield-exclamation")
    )
  ),
  
  # Tabs for Plots
  navset_card_underline(
    title = "Visualizations",
    nav_panel("Monte Carlo Paths", plotOutput("plot_paths", height = "500px")),
    nav_panel("Final Price Distribution", plotOutput("plot_hist", height = "500px"))
  ),
  
  # Detailed Stats Table
  card(
    card_header("Detailed Statistics"),
    tableOutput("stats_table")
  )
)

# --- 3. Server Logic ---
server <- function(input, output) {
  
  # This eventReactive makes the app wait for the "Run" button
  sim_results <- eventReactive(input$run, {
    req(input$symbol) # Stop if symbol is empty
    
    # Notification "Toast" so the user knows it's working
    id <- showNotification("Fetching data and simulating...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    # Run the function loaded from your source file
    tryCatch({
      simulate_stock(
        symbol = input$symbol,
        from = input$start_date,
        days_ahead = input$days,
        n_paths = input$paths
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # --- Outputs ---
  
  # Plots
  output$plot_paths <- renderPlot({
    req(sim_results())
    sim_results()$plots$paths
  })
  
  output$plot_hist <- renderPlot({
    req(sim_results())
    sim_results()$plots$hist
  })
  
  # Value Box Metrics
  output$stat_s0 <- renderText({
    req(sim_results())
    paste0("$", round(sim_results()$stats$S0, 2))
  })
  
  output$stat_ret <- renderText({
    req(sim_results())
    val <- sim_results()$stats$expected_return
    paste0(round(val * 100, 2), "%")
  })
  
  output$stat_loss <- renderText({
    req(sim_results())
    val <- sim_results()$stats$prob_loss
    paste0(round(val * 100, 1), "%")
  })
  
  # Stats Table
  output$stats_table <- renderTable({
    req(sim_results())
    s <- sim_results()$stats
    
    data.frame(
      Metric = c("Mu (Annualized)", "Sigma (Annualized)", "Prob. Gain > 20%", "Prob. Drop > 20%", "Expected Final Price"),
      Value = c(
        sprintf("%.4f", s$mu_hat),
        sprintf("%.4f", s$sigma_hat),
        sprintf("%.1f%%", s$prob_gain_20 * 100),
        sprintf("%.1f%%", s$prob_drop_20 * 100),
        sprintf("$%.2f", s$expected_final)
      )
    )
  })
}

# Run App
shinyApp(ui, server)