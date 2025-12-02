## GBM STOCK PRICE PREDICTOR

simulate_stock <- function(symbol,
                           from = "2019-01-01",
                           to = Sys.Date(),
                           days_ahead = 252,
                           n_paths = 10000,
                           return_plots = TRUE) {

    if (!requireNamespace("quantmod", quietly = TRUE)) {
    stop("Please install.packages('quantmod') first.")
  }
  if (!requireNamespace("tidyverse", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install.packages(c('tidyverse', 'ggplot2')) first.")
  }
  
  library(tidyverse)
  library(ggplot2)
  
  suppressWarnings(
    getSymbols(symbol, src = "yahoo", from = from, to = to, auto.assign = TRUE)
  )
  
  price_xts <- Ad(get(symbol))
  
  price_df <- data.frame(
    date  = index(price_xts),
    price = as.numeric(price_xts)
  ) %>%
    arrange(date)
  
  price_df <- price_df %>%
    mutate(
      log_return = log(price / dplyr::lag(price))
    )
  
  returns_df <- price_df %>%
    filter(!is.na(log_return))
  
  if (nrow(returns_df) < 10) {
    stop("Not enough historical data to estimate parameters.")
  }
  
  dt <- 1 / 252
  
  mean_daily <- mean(returns_df$log_return)
  sd_daily   <- sd(returns_df$log_return)
  
  sigma_hat <- sd_daily / sqrt(dt)
  mu_hat    <- mean_daily / dt + 0.5 * sigma_hat^2
  
  S0 <- tail(price_df$price, 1)
  
  set.seed(123)  # you can expose this as an argument if you like
  
  eps_mat <- matrix(
    rnorm(days_ahead * n_paths, mean = 0, sd = 1),
    nrow = days_ahead,
    ncol = n_paths
  )
  
  growth_mat <- exp(
    (mu_hat - 0.5 * sigma_hat^2) * dt +
      sigma_hat * sqrt(dt) * eps_mat
  )
  
  prices_future <- apply(
    growth_mat,
    MARGIN = 2,
    FUN = function(x) S0 * cumprod(x)
  )
  
  prices_future <- rbind(S0, prices_future)
  
  day_vec_future <- 0:days_ahead
  
  future_df <- as.data.frame(prices_future) %>%
    mutate(day = day_vec_future) %>%
    tidyr::pivot_longer(
      cols = -day,
      names_to = "path",
      values_to = "price"
    )
  
  final_prices <- prices_future[nrow(prices_future), ]
  
  prob_loss      <- mean(final_prices < S0)
  prob_gain_20   <- mean(final_prices >= 1.2 * S0)
  prob_drop_20   <- mean(final_prices <= 0.8 * S0)
  expected_final <- mean(final_prices)
  expected_ret   <- expected_final / S0 - 1
  
  stats <- list(
    S0              = S0,
    mu_hat          = mu_hat,
    sigma_hat       = sigma_hat,
    prob_loss       = prob_loss,
    prob_gain_20    = prob_gain_20,
    prob_drop_20    = prob_drop_20,
    expected_final  = expected_final,
    expected_return = expected_ret
  )
  
  plots <- list()
  
  if (return_plots) {
    set.seed(123)
    paths_to_plot <- sample(unique(future_df$path), min(30, n_paths))
    
    paths_plot_df <- future_df %>%
      filter(path %in% paths_to_plot)
    
    path_plot <- ggplot(paths_plot_df,
                        aes(x = day, y = price, group = path, color = path)) +
      geom_line(alpha = 0.7) +
      labs(
        title = paste0(symbol, " – GBM Simulated Future Paths"),
        x = "Days Ahead",
        y = "Price"
      ) +
      theme(legend.position = "none")
    
    hist_plot <- ggplot(data.frame(final_price = final_prices),
                        aes(x = final_price)) +
      geom_histogram(bins = 60) +
      labs(
        title = paste0(symbol, " – Distribution of Simulated Price in ",
                       days_ahead, " Days"),
        x = "Final Price",
        y = "Number of Simulations"
      )
    
    plots$paths <- path_plot
    plots$hist  <- hist_plot
  }
  
  list(
    symbol      = symbol,
    from        = from,
    to          = to,
    days_ahead  = days_ahead,
    n_paths     = n_paths,
    price_df    = price_df,
    returns_df  = returns_df,
    future_df   = future_df,
    final_prices = final_prices,
    stats       = stats,
    plots       = plots
  )
}

## Configure here
result <- simulate_stock(
  symbol     = "AMZN",
  from       = "2019-01-01",
  days_ahead = 252,
  n_paths    = 10000
)

result$stats
result$plots$paths
result$plots$hist
