# MULTIPLE MODEL FITTING AUTOMATION FUNCTION ----
multi_model_fitting <- function(time_series, hw_seasonality = "multiplicative", test_size = 0.2, horizon = 12) {
  
  # Load necessary libraries 
  library(ggplot2)
  library(tidyverse)
  library(forecast)
  
  # SUBSETTING IN TRAIN AND TEST SET ---- 
  n <- length(time_series)
  test_size <- round(n * test_size)
  train_size <- n - test_size
  
  train <- ts(time_series[1:train_size], start = start(time_series), frequency = frequency(time_series))
  test <- ts(time_series[(train_size+1):n], start = time(time_series)[train_size+1], frequency = frequency(time_series))
  
  time_series_by_subset <- data.frame(
    Date = time(time_series),
    Price = c(c(train), c(test)),
    Set = c(rep("Train", length(train)), rep("Test", length(test)))
  )
  
  # Create objects 
  number_of_models <- 4
  names <- c("ETS", "Holt Winters", "ARIMA", "TBATS")
  models <- list()
  forecasted_values <- c()
  plots <- list()
  plots_2 <- list()
  arima_lambda <- BoxCox.lambda(train)
  results_df <- data.frame(model = character(),
                           RESIDUAL_ASSUMPTIONS = character(),
                           RMSE_train = numeric(),
                           RMSE_test = numeric(), 
                           RMSE_cv = numeric(), 
                           MAPE_train = numeric(), 
                           MAPE_test = numeric(), 
                           MAPE_cv = numeric(), 
                           MAPE_index = numeric(),
                           stringsAsFactors = FALSE)
  
  # Loop over the different models
  for(i in 1:number_of_models){
    models[[i]] <- switch(i,
                          ets(train),
                          hw(train, seasonal = hw_seasonality, h = horizon*3),
                          auto.arima(train, lambda = arima_lambda, seasonal = TRUE, stepwise = FALSE),
                          tbats(train))
    
    model <- models[[i]]
    forecasted_values <- forecast(model, h = length(test), PI = TRUE)
    
    # PLOT 1 -------
    plot_data <- data.frame(
      Date = time(train),
      Real = as.numeric(train),
      Predicted = as.numeric(fitted(model))
    )
    
    plots[[i]] <- ggplot(plot_data, aes(x = Date)) + 
      geom_line(aes(y = Real, color = "Real"), size = 0.7) + 
      geom_line(aes(y = Predicted, color = "Predicted"), size = 0.7) + 
      ggtitle(paste("Predicted values by", names[i], "model")) +
      scale_color_manual(name = "Series",
                         values = c("Real" = "black", "Predicted" = "blue")) + 
      theme(
        plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.key = element_rect(fill = "white")
      )
    
    
    # PLOT 2 --------------------- 
    max_train <- max(time(train))
    max_train_value <- as.numeric(train[which.max(time(train))])
    min_test <- min(time(forecasted_values$mean))
    min_test_value <- forecasted_values$mean[1] # Adjust as needed for test set
    
    plots_2[[i]] <- autoplot(forecasted_values) + 
      geom_line(data = filter(time_series_by_subset, Set == "Test"), 
                aes(x = Date, y = Price, color = "Test"), size = 0.9) +
      geom_segment(aes(x = max_train, 
                       y = max_train_value, 
                       xend = min_test, 
                       yend = min_test_value), 
                   color = "#F8766D", size = 0.7) + 
      scale_color_manual(name = "Series",
                         values = c("Forecast" = "blue", "Test" = "#F8766D",
                                    "95% CI" = "grey")) +
      scale_shape_manual(name = "Series", 
                         values = c("Forecast" = "line", "Test" = "line", 
                                    "95% CI" = 0)) + 
      ggtitle(paste("Forecast from", names[i], "model")) + 
      ylab("Price") + 
      theme(
        plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.key = element_rect(fill = "white")
      )
    
    # CHECKING RESIDUALS -------- 
    ljung_box_output <- capture.output({
      ljung_box <- ifelse(checkresiduals(model, plot = FALSE)$p.value > 0.05, "WHITE NOISE", "NOT WHITE NOISE")
    })
    
    # MEASURING TRAIN AND TEST SET ERRORS ---------
    errors <- accuracy(forecast(model), time_series)
    
    # MEASURING ONE STEP FORECAST ERROR THROUGH CROSS-VALIDATION --------- 
    model_forecast_function <- function(x, h) {
      model <- switch(i,
                      ets(x),
                      hw(x, seasonal = hw_seasonality, h = horizon*3),
                      auto.arima(x, lambda = arima_lambda, seasonal = TRUE, stepwise = FALSE),
                      tbats(x))
      forecast(model, h = h)
    }
    
    cv_result <- tsCV(time_series, model_forecast_function, h = 1)
    RMSE_cv <- sqrt(mean(cv_result^2, na.rm = TRUE))
    MAPE_cv <- mean(abs((cv_result[-length(cv_result)]/time_series[-length(time_series)])), na.rm = TRUE) * 100
    
    # Averaging test set error with one-step error ----
    MAPE_index <- (MAPE_cv + errors[2, 5]) / 2
    
    # ADD TO RESULTS TABLE ---------
    results_df <- results_df %>% 
      add_row(model = names[i],
              RESIDUAL_ASSUMPTIONS = ljung_box,
              RMSE_train = errors[1, 2], 
              RMSE_test = errors[2, 2], 
              RMSE_cv = RMSE_cv, 
              MAPE_train = errors[1, 5], 
              MAPE_test = errors[2, 5], 
              MAPE_cv = MAPE_cv,
              MAPE_index = MAPE_index)
  } 
  
  # Determine the best model
  best_model_index <- which.min(results_df$MAPE_index)
  best_model_name <- results_df$model[best_model_index]
  best_model_MAPE_cv <- results_df$MAPE_cv[best_model_index]
  best_model_MAPE_test <- results_df$MAPE_test[best_model_index]
  
  # Refit the best model to the full time series (train + test)
  full_time_series <- ts(time_series, start = start(time_series), frequency = frequency(time_series))
  
  best_model <- switch(best_model_index,
                       ets(full_time_series),
                       hw(full_time_series, seasonal = hw_seasonality, h = horizon*3),
                       auto.arima(full_time_series, lambda = arima_lambda, seasonal = TRUE, stepwise = FALSE),
                       tbats(full_time_series))
  
  final_forecast <- forecast(best_model, h = horizon)
  
  
  # Plot the final forecast for the next 12 months
  final_plot <- autoplot(final_forecast) + 
    ggtitle(paste("12-Month Forecast from the Best Model (", best_model_name, ")")) + 
    xlab("Time") + 
    ylab("Price") + 
    theme(
      plot.title = element_text(hjust = 0.5), 
      panel.background = element_blank(), 
      panel.grid = element_blank(), 
      axis.line = element_line(colour = "black"), 
      legend.key = element_rect(fill = "white")
    )
  
  return(list(results_df = results_df, best_model_name = best_model_name, best_model_MAPE_cv = best_model_MAPE_cv, best_model_MAPE_test = best_model_MAPE_test, final_plot = final_plot, plots = plots, plots_2 = plots_2, final_forecast = final_forecast))
}



# APPLYING FUNCTION TO MULTIPLE PRODUCTS ------------------------------------------- 
library(lubridate)
library(zoo)

products <- unique(month_year_price_by_product$Commodity)
selected_products <- products[products %in% c("Cauli Local", "Okara", "Banana")]
full_data <- month_year_price_by_product


forecast_df <- data.frame(
  Product = character(),
  Date = as.Date(character()),  # Use as.Date() to handle date vectors
  Forecasted_Values = numeric(),
  stringsAsFactors = FALSE
)
results <- list()

for(p in selected_products) {
  # Data for selected product is stored
  data <- full_data[full_data$Commodity == p, ]
  
  # Minimum date is calculated to find the start of the time series 
  min_date <- min(data$month_year)
  start_values <- c(year(min_date), month(min_date))
  
  # Time series object is created
  time_series <- ts(data[, 3], start = start_values, frequency = 12)
  
  # Multiple Model Automation loop is applied 
  results[[p]] <- multi_model_fitting(time_series, hw_seasonality = "multiplicative", test_size = 0.2, horizon = 12)
  
  # Extract the final forecast for the best model
  best_model_name <- results[[p]]$best_model_name
  final_forecast <- results[[p]]$final_forecast
  MAPE_cv <- results[[p]]$best_model_MAPE_cv
  MAPE_test <- results[[p]]$best_model_MAPE_test
  
  # Create a data frame with the forecasted values for this product
  product_forecast_df <- data.frame(
    Product = rep(p, nrow(as.data.frame(final_forecast))),
    Date = format(as.Date(date_decimal(index(final_forecast$mean))), "%d-%m-%Y"),
    Forecasted_Values = as.vector(final_forecast$mean),
    Model = best_model_name,
    MAPE_cv = rep(MAPE_cv, nrow(as.data.frame(final_forecast))), 
    MAPE_test = rep(MAPE_test, nrow(as.data.frame(final_forecast))),
    stringsAsFactors = FALSE
  )
  
  # Append the product_forecast_df to the main forecast_df
  forecast_df <- rbind(forecast_df, product_forecast_df)
  
  # Optionally, print results for all products
  print(results[[p]]$results_df)
  print(results[[p]]$final_plot)
}

# Display the final forecast data frame
forecast_df































# ----- POWER BI VERSION -------------------------------------------------------- 
# Datasets 
# 'dataset' holds the input data for this script
# Datasets 
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))

prices <- as.data.frame(read.csv("C:/Users/User/Documents/Data/Proyecto Forecast/Prices.csv", header = TRUE, sep = ","))

# DATA CLEANING AND WRANGLING ----- 
# Prices 
prices$Date <- as.Date(prices$Date) # Formatting date column

dataset <- prices %>% mutate(month_year = lubridate::floor_date(Date, "month")) %>% # Obtaining average price at a monthly level for each product
  group_by(month_year, Commodity) %>%
  summarise(Avg_price = mean(Average)) %>% 
  arrange(Commodity, month_year) %>% 
  as.data.frame()


selected_products <- c("Cauli Local", "Okara", "Banana")
full_data <- dataset


# CREATING FUNCTION -------
multi_model_fitting <- function(time_series, hw_seasonality = "multiplicative", test_size = 0.2, horizon = 12) {
  
  # Load necessary libraries 
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(forecast))
  
  # SUBSETTING IN TRAIN AND TEST SET ---- 
  n <- length(time_series)
  test_size <- round(n * test_size)
  train_size <- n - test_size
  
  train <- ts(time_series[1:train_size], start = start(time_series), frequency = frequency(time_series))
  test <- ts(time_series[(train_size+1):n], start = time(time_series)[train_size+1], frequency = frequency(time_series))
  
  time_series_by_subset <- data.frame(
    Date = time(time_series),
    Price = c(c(train), c(test)),
    Set = c(rep("Train", length(train)), rep("Test", length(test)))
  )
  
  # Create objects 
  number_of_models <- 4
  names <- c("ETS", "Holt Winters", "ARIMA", "TBATS")
  models <- list()
  forecasted_values <- c()
  plots <- list()
  plots_2 <- list()
  arima_lambda <- BoxCox.lambda(train)
  results_df <- data.frame(model = character(),
                           RESIDUAL_ASSUMPTIONS = character(),
                           RMSE_train = numeric(),
                           RMSE_test = numeric(), 
                           RMSE_cv = numeric(), 
                           MAPE_train = numeric(), 
                           MAPE_test = numeric(), 
                           MAPE_cv = numeric(), 
                           MAPE_index = numeric(),
                           stringsAsFactors = FALSE)
  
  # Loop over the different models
  for(i in 1:number_of_models){
    models[[i]] <- switch(i,
                          ets(train),
                          hw(train, seasonal = hw_seasonality, h = horizon*3),
                          auto.arima(train, lambda = arima_lambda, seasonal = TRUE, stepwise = FALSE),
                          tbats(train))
    
    model <- models[[i]]
    
    
    # CHECKING RESIDUALS -------- 
    ljung_box_output <- capture.output({
      ljung_box <- ifelse(checkresiduals(model, plot = FALSE)$p.value > 0.05, "WHITE NOISE", "NOT WHITE NOISE")
    })
    
    # MEASURING TRAIN AND TEST SET ERRORS ---------
    errors <- accuracy(forecast(model), time_series)
    
    # MEASURING ONE STEP FORECAST ERROR THROUGH CROSS-VALIDATION --------- 
    model_forecast_function <- function(x, h) {
      model <- switch(i,
                      ets(x),
                      hw(x, seasonal = hw_seasonality, h = horizon*3),
                      auto.arima(x, lambda = arima_lambda, seasonal = TRUE, stepwise = FALSE),
                      tbats(x))
      forecast(model, h = h)
    }
    
    cv_result <- tsCV(time_series, model_forecast_function, h = 1)
    RMSE_cv <- sqrt(mean(cv_result^2, na.rm = TRUE))
    MAPE_cv <- mean(abs((cv_result[-length(cv_result)]/time_series[-length(time_series)])), na.rm = TRUE) * 100
    
    # Averaging test set error with one-step error ----
    MAPE_index <- (MAPE_cv + errors[2, 5]) / 2
    
    # ADD TO RESULTS TABLE ---------
    results_df <- results_df %>% 
      add_row(model = names[i],
              RESIDUAL_ASSUMPTIONS = ljung_box,
              RMSE_train = errors[1, 2], 
              RMSE_test = errors[2, 2], 
              RMSE_cv = RMSE_cv, 
              MAPE_train = errors[1, 5], 
              MAPE_test = errors[2, 5], 
              MAPE_cv = MAPE_cv,
              MAPE_index = MAPE_index)
  } 
  
  # Determine the best model
  best_model_index <- which.min(results_df$MAPE_index)
  best_model_name <- results_df$model[best_model_index]
  best_model_MAPE_cv <- results_df$MAPE_cv[best_model_index]
  best_model_MAPE_test <- results_df$MAPE_test[best_model_index]
  
  # Refit the best model to the full time series (train + test)
  
  best_model <- switch(best_model_index,
                       ets(time_series),
                       hw(time_series, seasonal = hw_seasonality, h = horizon*3),
                       auto.arima(time_series, lambda = arima_lambda, seasonal = TRUE, stepwise = FALSE),
                       tbats(time_series))
  
  final_forecast <- forecast(best_model, h = horizon)
  
  
  
  
  return(list(results_df = results_df, best_model_name = best_model_name, final_forecast = final_forecast, best_model_MAPE_cv = best_model_MAPE_cv, best_model_MAPE_test = best_model_MAPE_test))
}



# APPLYING FUNCTION ------- 
suppressPackageStartupMessages(library(zoo))

forecast_df <- data.frame(
  Product = character(),
  Date = as.Date(character()),  # Use as.Date() to handle date vectors
  Forecasted_Values = numeric(),
  stringsAsFactors = FALSE
)
results <- list()

for(p in selected_products) {
  # Data for selected product is stored
  data <- full_data[full_data$Commodity == p, ]
  
  # Minimum date is calculated to find the start of the time series 
  min_date <- min(data$month_year)
  start_values <- c(year(min_date), month(min_date))
  
  # Time series object is created
  time_series <- ts(data[, 3], start = start_values, frequency = 12)
  
  # Multiple Model Automation loop is applied 
  results[[p]] <- multi_model_fitting(time_series, hw_seasonality = "multiplicative", test_size = 0.2, horizon = 12)
  
  # Extract the final forecast for the best model
  best_model_name <- results[[p]]$best_model_name
  final_forecast <- results[[p]]$final_forecast
  MAPE_cv <- results[[p]]$best_model_MAPE_cv
  MAPE_test <- results[[p]]$best_model_MAPE_test
  
  
  # Create a data frame with the forecasted values for this product
  forecast_dates <- as.Date(lubridate::date_decimal(index(final_forecast$mean)))
  
  product_forecast_df <- data.frame(
    Product = rep(p, nrow(as.data.frame(final_forecast))),
    Date = as.character(forecast_dates),
    Forecasted_Values = as.vector(final_forecast$mean),
    Model = best_model_name,
    MAPE_cv = rep(MAPE_cv, nrow(as.data.frame(final_forecast))), 
    MAPE_test = rep(MAPE_test, nrow(as.data.frame(final_forecast))),
    stringsAsFactors = FALSE
  )
  
  # Append the product_forecast_df to the main forecast_df
  forecast_df <- rbind(forecast_df, product_forecast_df)
  
}

# Display the final forecast data frame
forecast_df

# Adjusting the dates in the forecast_df
forecast_df$Adjusted_Date <- ifelse(
  format(as.Date(forecast_df$Date), "%d") == "31" & format(as.Date(forecast_df$Date), "%m") == "01", 
  format(as.Date(forecast_df$Date) + 1, "%Y-%02-%01"),
  ifelse(format(as.Date(forecast_df$Date), "%d") == "02", 
         format(as.Date(forecast_df$Date) - 1, "%Y-%m-%d"), 
         forecast_df$Date)
)

# Converting Adjusted_Date back to Date format
forecast_df$Adjusted_Date <- as.Date(forecast_df$Adjusted_Date)
forecast_df


