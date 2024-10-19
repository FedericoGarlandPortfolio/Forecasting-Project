# FORECASTING PROJECT ------------------- 
# Load packages
library(tidyverse)
library(ggplot2)
library(psych)
library(lubridate)
library(fpp1)
library(forecast)
library(tseries)
library(readxl)
library(urca)
library(fUnitRoots)
library(ggcorrplot)
library(broom)
library(car)
library(lmtest)
library(gvlma)
library(lm.beta)
library(zoo)

# Datasets 
prices <- as.data.frame(read.csv("C:/Users/User/Documents/Data/Proyecto Forecast/Prices.csv", header = TRUE, sep = ","))
weather <- as.data.frame(read.csv("C:/Users/User/Documents/Data/Proyecto Forecast/Data/Weather.csv", header = TRUE, sep = ","))
head(prices)
head(weather)

# DATA CLEANING AND WRANGLING ----- 
# Prices 
str(prices)
prices$Date <- as.Date(prices$Date) # Formatting date column
month_year_price <- prices %>% mutate(month_year = lubridate::floor_date(Date, "month")) %>% # Obtaining all-product average price by month year
  group_by(month_year) %>%
  summarise(Avg_price = mean(Average)) 
month_year_price


month_year_price_by_product <- prices %>% mutate(month_year = lubridate::floor_date(Date, "month")) %>% # Obtaining average price at a monthly level for each product
  group_by(month_year, Commodity) %>%
  summarise(Avg_price = mean(Average)) %>% 
  arrange(Commodity, month_year) %>% 
  as.data.frame()
month_year_price_by_product


# Find number of products with no missing months
min(prices$Date)
max(prices$Date)
min(weather$DATE)
max(weather$DATE) # To have the weather and price datasets converge, dates between 2013-06-1 and 2019-12-01 must be considered
interval("2013-06-01", "2019-12-31") %/% months(1) # 78 months of difference, so 79 months should be considered including the first one


# Finding products that have 79 rows
no_miss_products <- month_year_price_by_product %>% 
  filter(month_year >= "2013-06-01" & month_year <= "2019-12-01") %>% 
  group_by(Commodity) %>% 
  summarise(Months = n_distinct(month_year)) %>% 
  filter(Months == 79)
print(no_miss_products, n = nrow(no_miss_products)) # 40 products suitable for correlation with weather data



# Weather 
weather_district <- weather %>% rename(Precipitation = PRECTOT, # Renaming columns and selecting variables of interest
                              Temperature = T2M, 
                              Humidity = RH2M, 
                              Max_temp = T2M_MAX, 
                              Min_temp = T2M_MIN,
                              Temp_range = T2M_RANGE,
                              Wind = WS10M,
                              Max_wind = WS10M_MAX, 
                              Min_wind = WS10M_MIN, 
                              Wind_range = WS10M_RANGE,
                              Date = DATE) %>% 
  dplyr::select(Date, DISTRICT, Precipitation, Temperature, Humidity, Max_temp, Min_temp, Temp_range, Wind, Max_wind, Min_wind, Wind_range)

weather_district$Date <- sub("T.*$", "", weather_district$Date) %>% as.Date() # Formatting date 
head(weather_district)

weather_total <- weather_district %>% mutate(month_year = lubridate::floor_date(Date, "month"))  %>% # Obtaining weather variables averaged for all districts
  group_by(month_year) %>% 
  summarise(Precipitation = mean(Precipitation), 
            Temperature = mean(Temperature),
            Humidity = mean(Humidity), 
            Max_temp = mean(Max_temp), 
            Min_temp = mean(Min_temp), 
            Temp_range = mean(Temp_range),
            Wind = mean(Wind), 
            Max_wind = mean(Max_wind), 
            Min_wind = mean(Min_wind), 
            Wind_range = mean(Wind_range))
weather_month_year <- weather_total %>% as.data.frame()
weather_month_year

# PRINCIPAL COMPONENT ANALYSIS ---- To determine most important products and best weather external variables for price prediction
# Preparing data for PCA 
weather_pca <- weather_month_year %>% filter(month_year >= "2013-06-01" & month_year <= "2019-12-01") %>% as.data.frame()
product_prices <- month_year_price_by_product %>% filter(Commodity %in% no_miss_products$Commodity, month_year >= "2013-06-01" & month_year <= "2019-12-01") %>% as.data.frame() # Selecting data only for 40 products with no missing months

prices_pca <- data.frame(matrix(nrow = 79, ncol = length(unique(product_prices$Commodity))))
colnames(prices_pca) <- unique(product_prices$Commodity)
for(i in unique(product_prices$Commodity)){
  prices_pca[,i] <- product_prices[product_prices$Commodity == i, "Avg_price"]
  
} # Converting the time series of each product into a vector 
prices_pca <- cbind(data.frame(month_year = weather_pca$month_year), prices_pca) # Adding month_year
prices_pca

pca_data <- left_join(weather_pca, prices_pca, by = c("month_year" = "month_year")) # Binding weather and price data 
pca_data

# Running PCA 
library(factoextra)
PCA <- prcomp(pca_data[,-1], scale = TRUE)
summary(PCA)
plot(PCA,type="l")

fviz_eig(PCA, addlabels = TRUE, ylim = c(0, 100))

eig.val <- get_eigenvalue(PCA)
eig.val

var_pca <- get_pca_var(PCA)

# Correlación 
library(corrplot)
var_pca$cor 
corrplot(var_pca$cor[,1:3], is.corr = FALSE) # Gráfico de correlación 


# Contribución 
var_pca$contrib #Contribución porcentual de cada variable al PC
# Contributions of variables to PC1
fviz_contrib(PCA, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(PCA, choice = "var", axes = 2, top = 10) # PC2 contains most of the weather variability, mainly temperature related
# Contributions of variables to PC3
fviz_contrib(PCA, choice = "var", axes = 3, top = 10) # PC3 contains most of the wind variability

# Calidad de Representación 
var_pca$cos2 # Calidad de representación de la variable por cada PC según Cos2
corrplot(var_pca$cos2, is.corr=FALSE) # Gráfico de Cos2

fviz_cos2(PCA, choice = "var", axes = 1:3) # Calidad de representación de cada variable entre los primeros 3 PC's

# Variable Correlation Plot 
# PRINCIPAL COMPONENTS 1 AND 2
fviz_pca_var(PCA, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             ggtheme = theme_minimal(), 
             axes = c(1, 2)
)

# PRINCIPAL COMPONENTS 3 AND 4
fviz_pca_var(PCA, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             ggtheme = theme_minimal(), 
             axes = c(3, 4)
)





# Selecting products (those with the highest correlation with first 3 PC's)
products_pc1 <- var_pca$cor %>% 
  as.data.frame() %>% 
  filter(Dim.1 > 0.8 | Dim.1 < -0.8) %>% 
  rownames_to_column(var = "Product") %>% 
  dplyr::select(Product, Dim.1) %>% 
  arrange(-abs(Dim.1))
products_pc1 # Cauli Local will be forecasted without external climatic variables, criterion of correlation established as R > 0.8 


products_pc2 <- var_pca$cor %>% 
  as.data.frame() %>% 
  filter(Dim.2 > 0.5 | Dim.2 < -0.5) %>% 
  rownames_to_column(var = "Product") %>% 
  dplyr::select(Product, Dim.2) %>% 
  arrange(-abs(Dim.2))
products_pc2 # Okara will be forecasted using dynamic regression with temperature variables 

products_pc3 <- var_pca$cor %>% 
  as.data.frame() %>% 
  filter(Dim.3 > 0.5 | Dim.3 < -0.5) %>% 
  rownames_to_column(var = "Product") %>% 
  dplyr::select(Product, Dim.3) %>% 
  arrange(-abs(Dim.3))
products_pc3 # Banana will be forecasted using dynamic regression with wind variables 



# CREATING FUNCTIONS TO AUTOMATE FITTING OF TIME SERIES MODELS ------------------- 
multi_model_fitting <- function(time_series, hw_seasonality = "multiplicative", lambda = FALSE, test_size = 0.2, horizon = 12) {
  
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
  
  
  if (!is.null(lambda) && lambda == TRUE) { 
    arima_lambda <- BoxCox.lambda(train)
    arima_lambda_full <- BoxCox.lambda(time_series)
  } else if (!is.null(lambda) && lambda != TRUE) {
    arima_lambda <- lambda
    arima_lambda_full <- lambda
  } else {
    arima_lambda <- NULL 
    arima_lambda_full <- NULL
  }
  

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
                      auto.arima(x, lambda = arima_lambda_full, seasonal = TRUE, stepwise = FALSE),
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
                       auto.arima(full_time_series, lambda = arima_lambda_full, seasonal = TRUE, stepwise = FALSE),
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
  
  return(list(models = models, results_df = results_df, best_model_name = best_model_name, best_model_MAPE_cv = best_model_MAPE_cv, best_model_MAPE_test = best_model_MAPE_test, final_plot = final_plot, plots = plots, plots_2 = plots_2, final_forecast = final_forecast))
} # This function fits ETS, Holt-Winters, ARIMA and TBATS models and selects the one with the smallest MAPE



arima_iteration_function <- function(time_series, p_values, d_values, q_values, 
                                     P_values, D_values, Q_values, s = 12, test_size = 0.2, lambda = FALSE){
  
  
  library(forecast)
  library(dplyr)
  
  # Define the parameter grids
  s_values <- s # Seasonality (e.g., 12 for monthly data)
  
  # Initialize variables to track the best models and MAPE
  best_MAPE_Index <- Inf
  best_MAPE_Test <- Inf
  best_MAPE_CV <- Inf
  best_params_Index <- c()
  best_params_Test <- c()
  best_params_CV <- c()
  
  
  n <- length(time_series)
  test_size <- round(n * test_size)
  train_size <- n - test_size
  
  train <- ts(time_series[1:train_size], start = start(time_series), frequency = frequency(time_series))
  test <- ts(time_series[(train_size+1):n], start = time(time_series)[train_size+1], frequency = frequency(time_series))
  
  # Initialize a list to store models and a data frame for RMSE values
  model_list <- list()
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
  
  
  # Selecting lambda 
  if (!is.null(lambda) && lambda == TRUE) { 
    lambda_value_train <- BoxCox.lambda(train)
    lambda_value_full <- BoxCox.lambda(time_series)
  } else if (!is.null(lambda) && lambda != TRUE) {
    lambda_value_train <- lambda
    lambda_value_full <- lambda
  } else {
    lambda_value_train <- NULL 
    lambda_value_full <- NULL
  }
  
  # Counter for model list indexing
  model_index <- 1
  
  # Iterate through all combinations of parameters
  for (p in p_values) {
    for (d in d_values) {
      for (q in q_values) {
        for (P in P_values) {
          for (D in D_values) {
            for (Q in Q_values) {
              model <- tryCatch({
                Arima(train, order = c(p, d, q), seasonal = c(P, D, Q), lambda = lambda_value_train)
              }, error = function(e) {
                NULL  # Return NULL or skip to the next iteration if there's an error
              })
              
              if (!is.null(model)) {
                rmse <- sqrt(mean(residuals(model)^2))
                
                model_list[[model_index]] <- list(model = model, 
                                                  params = c(p, d, q, P, D, Q),
                                                  RMSE = rmse)
                
                # ANALYZING RESIDUALS ---------- 
                ljung_box <- ifelse(checkresiduals(model, plot = FALSE)$p.value > 0.05, "WHITE NOISE", "NOT WHITE NOISE")
                
                
                # MEASURING ERRORS ------------
                # Test set validation ----
                errors <- accuracy(forecast(model), time_series)
                
                # Creating function for cross validation
                arima_manual_forecast <- function(x, h) {
                  forecast(Arima(x, order = c(p, d, q), seasonal = c(P, D, Q), lambda = lambda_value_full), h = h)
                }
                
                # One step cross-validation (rolling origin) ---
                cv_result <- tsCV(time_series, arima_manual_forecast, h = 1)
                RMSE_cv <- sqrt(mean(cv_result^2, na.rm = TRUE))
                MAPE_cv <- mean(abs((cv_result/time_series)), na.rm = TRUE) * 100
                
                # Averaging test set error with one-step error ----
                MAPE_index <- (MAPE_cv + errors[2, 5]) / 2
                
                
                # ADD TO RESULTS TABLE ---------
                results_df <- results_df %>% 
                  add_row(model = paste("(", p, ",", d, ",", q, ")x(", P, ",", D, ",", Q, ")"),
                          RESIDUAL_ASSUMPTIONS = ljung_box,
                          RMSE_train = errors[1, 2], 
                          RMSE_test = errors[2, 2], 
                          RMSE_cv = RMSE_cv, 
                          MAPE_train = errors[1, 5], 
                          MAPE_test = errors[2, 5], 
                          MAPE_cv = MAPE_cv,
                          MAPE_index = MAPE_index)
                
                
                # SELECTING BEST MODELS -------- 
                # Check and store the best models based on different criteria
                if (MAPE_index < best_MAPE_Index) {
                  best_MAPE_Index <- MAPE_index
                  best_params_Index <- c(p, d, q, P, D, Q)
                }
                
                if (errors[2, 5] < best_MAPE_Test) {
                  best_MAPE_Test <- errors[2, 5]
                  best_params_Test <- c(p, d, q, P, D, Q)
                }
                
                if (MAPE_cv < best_MAPE_CV) {
                  best_MAPE_CV <- MAPE_cv
                  best_params_CV <- c(p, d, q, P, D, Q)
                }
                
                model_index <- model_index + 1
              }
            }
          }
        }
      }
    }
  }
  
  # Output the best parameters and MAPE for each criterion
  cat("Best ARIMA model based on MAPE Index:",
      "p =", best_params_Index[1], "d =", best_params_Index[2], "q =", best_params_Index[3],
      "P =", best_params_Index[4], "D =", best_params_Index[5], "Q =", best_params_Index[6], "\n")
  cat("Best MAPE Index:", best_MAPE_Index, "\n\n")
  
  cat("Best ARIMA model based on MAPE Test:",
      "p =", best_params_Test[1], "d =", best_params_Test[2], "q =", best_params_Test[3],
      "P =", best_params_Test[4], "D =", best_params_Test[5], "Q =", best_params_Test[6], "\n")
  cat("Best MAPE Test:", best_MAPE_Test, "\n\n")
  
  cat("Best ARIMA model based on MAPE CV:",
      "p =", best_params_CV[1], "d =", best_params_CV[2], "q =", best_params_CV[3],
      "P =", best_params_CV[4], "D =", best_params_CV[5], "Q =", best_params_CV[6], "\n")
  cat("Best MAPE CV:", best_MAPE_CV, "\n")
  
  
  top_models_df <- results_df %>% 
    filter(
      model == paste("(", best_params_Index[1], ",", best_params_Index[2], ",", best_params_Index[3], ")x(", best_params_Index[4], ",", best_params_Index[5], ",", best_params_Index[6], ")") |
        model == paste("(", best_params_Test[1], ",", best_params_Test[2], ",", best_params_Test[3], ")x(", best_params_Test[4], ",", best_params_Test[5], ",", best_params_Test[6], ")") |
        model == paste("(", best_params_CV[1], ",", best_params_CV[2], ",", best_params_CV[3], ")x(", best_params_CV[4], ",", best_params_CV[5], ",", best_params_CV[6], ")")
    )
  
  return(list(models = model_list, results_df = results_df, top_models_df = top_models_df, best_models = list(MAPE_Index = best_params_Index, 
                                                                                         MAPE_Test = best_params_Test, 
                                                                                         MAPE_CV = best_params_CV)))
} # This function fits an ARIMA model for each combination of the values provided in the arguments and selects the one with the smallest MAPE







# TIME SERIES ANALYSIS ----- 
# 1. FIRST PRODUCT (CAULI LOCAL) ---- 
# 1.1. Creating and visualizing time series
data_p1 <- month_year_price_by_product[month_year_price_by_product$Commodity == "Cauli Local", ] # storing data for product 1 (Cauli Local)
ts_p1 <- ts(data_p1[, 3], start = c(2013, 6), frequency = 12) # Creating time series object
autoplot(ts_p1) # Time series seems heteroscedastic and seasonal

# Time series plot with trend line
ggplot(data_p1, aes(x = month_year, y = Avg_price)) + 
  geom_line(aes(colour = "Real")) + 
  geom_smooth(aes(color = "Trend"), method = "auto", linetype = 20, se = FALSE, size = 0.55) +
  scale_color_manual(name = "Series", 
                     values = c("Trend" = "red", "Real" = "black")) + 
  scale_linetype_manual(values = c("Trend" = 20, "Real" = 1)) + 
  guides(color = guide_legend(override.aes = list(linetype = c("Trend" = 20, "Real" = 1)))) +
  labs(title = "Time Series with Trend Line for product Cauli Local") + 
  xlab(label = "Time") + 
  ylab(label = "Price") + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(colour = "black"), 
    legend.key = element_rect(fill = "white")) # Time series seems to have a slight trend 


# Season plots
season_data_m <- data_p1 %>% mutate(Month = format(month_year, "%m"), Year = format(month_year, "%Y")) %>% 
  group_by(Year, Month) %>% 
  summarise(Price = mean(Avg_price)) 
season_data_m$Month <- as.numeric(season_data_m$Month)

# Adding average line to season plot
monthly_avg <- data_p1 %>% mutate(Month = format(month_year, "%m")) %>% # Obtaining average price by month to plot average curve in season plot
  group_by(Month) %>% 
  summarise(Price = mean(Avg_price)) 
monthly_avg$Month <- as.numeric(monthly_avg$Month)

# Generating season plots
season_data_m
ggplot(season_data_m, aes(x = Month, y = Price, color = Year)) +
  geom_line() +
  geom_line(data = monthly_avg, aes(x = Month, y = Price), color = "black", size = 1) + 
  ggtitle(label = "Season plot for price of product 'Cauli Local'") + 
  scale_x_continuous(breaks = 1:12) +
  theme(
    panel.grid = element_blank(), 
    panel.background = element_blank(), 
    legend.background = element_rect(colour = "white"), 
    legend.key = element_rect(fill = "white"), 
    panel.grid.major.x = element_line(color = "lightgrey", size = 0.01),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust =  0.5)
  )
  



# SUBSETTING IN TRAIN AND TEST SET ---- 
time_series <- ts_p1
n <- length(time_series)
test_size <- round(n * 0.2)
train_size <- n - test_size

train <- ts(time_series[1:train_size], start = start(time_series), frequency = frequency(time_series))
test <- ts(time_series[(train_size+1):n], start = time(time_series)[train_size+1], frequency = frequency(time_series))
train_test <- data.frame(Date = time(ts_p1),
                            Price = union_all(c(train), c(test)),
                            Set = c(rep("Train", length(train)), rep("Test", length(test))))


# PLOTTING TRAIN AND TEST SET ---- 
# obtain maximum time of train set and minimum time of test set, with their corresponding values. This is to connect the 2 lines for visual display. 
max_train <- max(train_test[train_test$Set == "Train","Date"])
min_test <- min(train_test[train_test$Set == "Test", "Date"])
max_train_value <- train_test[train_test$Date == max_train, "Price"]
min_test_value <- train_test[train_test$Date == min_test, "Price"]

ggplot(train_test, aes(x = Date, y = Price, col = Set)) + 
  geom_line(size = 0.7) + 
  geom_segment(aes(x = max_train, 
                   y = max_train_value, 
                   xend = min_test, 
                   yend = min_test_value), 
               color = "#F8766D", size = 0.7) + 
  scale_color_manual(values = c("Train" = "black", "Test" = "#F8766D")) +
  ggtitle("Training and test set for product 'Cauli Local'") + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(colour = "black"), 
    legend.key = element_rect(fill = "white")
    
  )

# CHECKING STATISTICAL ASSUMPTIONS -------------- 
# a. STATIONARITY (select parameter "d")
# ADF test (p < 0.05 means STATIONARY)
adf_result <- adf.test(train)
adf_decision <- ifelse(adf_result$p.value < 0.05, c("STATIONARY"), c("NON STATIONARY"))

# # KPSS Test (Kwiatkowski-Phillips-Schmidt-Shin)
kpss_result <- ur.kpss(train)
summary(kpss_result) # Si critical value es mayor al critical value para 5pct significance level, proceso es NO estacionario
kpss_decision <- ifelse(abs(kpss_result@teststat) > abs(kpss_result@cval[2]), "NON-STATIONARY", "STATIONARY")

# PP Test (Phillips-Perron)
pp_result <- ur.pp(train)
summary(pp_result) # Si Z-alpha es menor en magnitud absoluta (o sea sin signo) a Z-tau, entonces proceso es NO estacionario
pp_decision <- ifelse(abs(pp_result@teststat) < abs(pp_result@auxstat[1]), "NON-STATIONARY", "STATIONARY")

# ERS Test (Elliott-Rothenberg-Stock) - Sirve particularmente para data SEASONAL 
ers_result <- ur.ers(train)
summary(ers_result) # Si test-statistic es mayor en magnitud absoluta (o sea sin signo) al critical value, entonces proceso es NO estacionario 
ers_decision <- ifelse(abs(ers_result@teststat) > abs(ers_result@cval[2]), "NON-STATIONARY", "STATIONARY")


# STATIONARITY RESULTS 
# Create a vector of decisions
decisions <- c(ADF = adf_decision, KPSS = kpss_decision, PP = pp_decision, ERS = ers_decision)

# Count the number of STATIONARY decisions
num_stationary <- length(decisions[decisions == "STATIONARY"])

# Determine the final DECISION based on the majority
final_decision <- ifelse(num_stationary >= 2, "STATIONARY", "NON STATIONARY")

# Stationarity results 
stationarity_results <- data.frame(
  ADF = adf_decision,
  KPSS = kpss_decision,
  PP = pp_decision,
  ERS = ers_decision,
  DECISION = final_decision
)
stationarity_results # The time series IS STATIONARY 



#b. AUTOCORRELATION (helps in selecting parameters "p" and "q" for ARIMA)
ggAcf(train) # parameter "q" appears to be equal to 0, and Q equal to 1
ggPacf(train, lag.max = 48) # parameter "p" appears to be equal 1, "P" could be 2
Box.test(train, lag = 10, type = "Ljung") # p < 0.05, time series is not white noise.



# FITTING AND VALIDATING MODELS ------------- 
#1. ETS, Holt Winters, ARIMA and TBATS ---- 
multi_model_results <- multi_model_fitting(time_series, lambda = TRUE)



#2. ARIMA (manual) ---- 
#a. STATIONARITY (helps in selecting parameter "d" and transformations)
library(astsa)
autoplot(train) # Time series is heteroscedastic, so BoxCox transformation will be used. There seems to be no trend so d = 0
stationarity_results # The time series IS STATIONARY 
shapiro.test(train)
lambda <- BoxCox.lambda(train)
autoplot(BoxCox(train, lambda))


#b. AUTOCORRELATION (helps in selecting parameters "p" and "q" for ARIMA)
ggAcf(train) # parameter "q" appears to be equal to 0, and Q equal to 0
ggPacf(train, lag.max = 48) # parameter "p" appears to be equal 1, "P" could be 0 or 2. Consider that both ACF and PACF tail off at seasonal lags, Q and P could also be equal to 1.
acf2(train) # d = 1 because time series has slight trend, ACF tails off so q = 0, PACF cuts off at lag 1 so p = 1. ACF and PACF both tail off at seasonal lags so P = 1 and Q = 1
Box.test(train, lag = 10, type = "Ljung") # p < 0.05, time series is not white noise.


# Selecting possible values for each parameter
p <- c(1)
d <- c(0)
q <- c(0)
P <- c(1, 2)
D <- c(0, 1)
Q <- c(0, 1)

# Applying ARIMA iteration function over all possible combinations of the parameters
arima_results <- arima_iteration_function(time_series, p_values = p, d_values = d, q_values = q, 
                         P_values = P, D_values = D, Q_values = Q, lambda = TRUE)

arima_results$results_df
arima_results$best_models



#3. CONSOLIDATING FINAL RESULTS TABLE WITH RMSE, RMSEcv and RMSEtest TO CHOOSE MODEL --------  
final_results <- union_all(multi_model_results$results_df, arima_results$results_df)
final_results_arranged <- union_all(multi_model_results$results_df, arima_results$results_df) %>% 
  filter(RESIDUAL_ASSUMPTIONS == "WHITE NOISE") %>% 
  arrange(MAPE_index)

best_model <- final_results_arranged[1, ]
best_model 

lambda <- BoxCox.lambda(time_series)
final_model <- Arima(time_series, order = c(1, 0, 0), seasonal = c(1, 0, 1), lambda = lambda)

# FORECASTING (MODEL DEPLOYMENT) ------- 
final_forecast <- forecast(final_model, h = 12)
model_name <- best_model$model
forecast_dates <- as.Date(lubridate::date_decimal(index(final_forecast$mean)))

# Final results data frame ---- 
product_forecast_df <- data.frame(
  Product = rep("Cauli", nrow(as.data.frame(final_forecast))),
  Date = as.character(forecast_dates),
  Forecasted_Values = as.vector(final_forecast$mean),
  Model = model_name,
  MAPE_cv = rep(best_model$MAPE_cv, nrow(as.data.frame(final_forecast))), 
  MAPE_test = rep(best_model$MAPE_test, nrow(as.data.frame(final_forecast))),
  Lower_CI = final_forecast$lower[,2],
  Upper_CI = final_forecast$upper[,2],
  stringsAsFactors = FALSE
)

product_forecast_df
autoplot(final_forecast)


final_forecast

#2. SECOND PRODUCT (OKARA) ---------------------------------------------------------------------------- 
# 1.1. Creating and visualizing time series
data_p2 <- month_year_price_by_product[month_year_price_by_product$Commodity == "Okara", ] # storing data for product 1 (Cauli Local)
ts_p2 <- ts(data_p2[, 3], start = c(2013, 06), frequency = 12) # Creating time series object
autoplot(ts_p2) # Time series seems heteroscedastic and seasonal

# Time series plot with trend line
ggplot(data_p2, aes(x = month_year, y = Avg_price)) + 
  geom_line(aes(colour = "Real")) + 
  geom_smooth(aes(color = "Trend"), method = "auto", linetype = 20, se = FALSE, size = 0.55) +
  scale_color_manual(name = "Series", 
                     values = c("Trend" = "red", "Real" = "black")) + 
  scale_linetype_manual(values = c("Trend" = 20, "Real" = 1)) + 
  guides(color = guide_legend(override.aes = list(linetype = c("Trend" = 20, "Real" = 1)))) +
  labs(title = "Time Series with Trend Line for product Okara") + 
  xlab(label = "Time") + 
  ylab(label = "Price") + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(colour = "black"), 
    legend.key = element_rect(fill = "white")) # Time series seems to have a slight trend 


# Season plots
season_data_m <- data_p2 %>% mutate(Month = format(month_year, "%m"), Year = format(month_year, "%Y")) %>% 
  group_by(Year, Month) %>% 
  summarise(Price = mean(Avg_price)) 
season_data_m$Month <- as.numeric(season_data_m$Month)

# Adding average line to season plot
monthly_avg <- data_p2 %>% mutate(Month = format(month_year, "%m")) %>% # Obtaining average price by month to plot average curve in season plot
  group_by(Month) %>% 
  summarise(Price = mean(Avg_price)) 
monthly_avg$Month <- as.numeric(monthly_avg$Month)

# Generating season plots
season_data_m
ggplot(season_data_m, aes(x = Month, y = Price, color = Year)) +
  geom_line() +
  geom_line(data = monthly_avg, aes(x = Month, y = Price), color = "black", size = 1) + 
  ggtitle(label = "Season plot for price of product 'Cauli Local'") + 
  scale_x_continuous(breaks = 1:12) +
  theme(
    panel.grid = element_blank(), 
    panel.background = element_blank(), 
    legend.background = element_rect(colour = "white"), 
    legend.key = element_rect(fill = "white"), 
    panel.grid.major.x = element_line(color = "lightgrey", size = 0.01),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust =  0.5)
  )




# SUBSETTING IN TRAIN AND TEST SET ---- 
time_series <- ts_p2
n <- length(time_series)
test_size <- round(n * 0.2)
train_size <- n - test_size

train <- ts(time_series[1:train_size], start = start(time_series), frequency = frequency(time_series))
test <- ts(time_series[(train_size+1):n], start = time(time_series)[train_size+1], frequency = frequency(time_series))
train_test <- data.frame(Date = time(ts_p2),
                            Price = union_all(c(train), c(test)),
                            Set = c(rep("Train", length(train)), rep("Test", length(test))))


# PLOTTING TRAIN AND TEST SET ---- 
# obtain maximum time of train set and minimum time of test set, with their corresponding values. This is to connect the 2 lines for visual display. 
max_train <- max(train_test[train_test$Set == "Train","Date"])
min_test <- min(train_test[train_test$Set == "Test", "Date"])
max_train_value <- train_test[train_test$Date == max_train, "Price"]
min_test_value <- train_test[train_test$Date == min_test, "Price"]

ggplot(train_test, aes(x = Date, y = Price, col = Set)) + 
  geom_line(size = 0.7) + 
  geom_segment(aes(x = max_train, 
                   y = max_train_value, 
                   xend = min_test, 
                   yend = min_test_value), 
               color = "#F8766D", size = 0.7) + 
  scale_color_manual(values = c("Train" = "black", "Test" = "#F8766D")) +
  ggtitle("Training and test set for product 'Cauli Local'") + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(colour = "black"), 
    legend.key = element_rect(fill = "white")
    
  )

# CHECKING STATISTICAL ASSUMPTIONS -------------- 
# a. STATIONARITY (select parameter "d")
# ADF test (p < 0.05 means STATIONARY)
adf_result <- adf.test(train)
adf_decision <- ifelse(adf_result$p.value < 0.05, c("STATIONARY"), c("NON STATIONARY"))

# # KPSS Test (Kwiatkowski-Phillips-Schmidt-Shin)
kpss_result <- ur.kpss(train)
summary(kpss_result) # Si critical value es mayor al critical value para 5pct significance level, proceso es NO estacionario
kpss_decision <- ifelse(abs(kpss_result@teststat) > abs(kpss_result@cval[2]), "NON-STATIONARY", "STATIONARY")

# PP Test (Phillips-Perron)
pp_result <- ur.pp(train)
summary(pp_result) # Si Z-alpha es menor en magnitud absoluta (o sea sin signo) a Z-tau, entonces proceso es NO estacionario
pp_decision <- ifelse(abs(pp_result@teststat) < abs(pp_result@auxstat[1]), "NON-STATIONARY", "STATIONARY")

# ERS Test (Elliott-Rothenberg-Stock) - Sirve particularmente para data SEASONAL 
ers_result <- ur.ers(train)
summary(ers_result) # Si test-statistic es mayor en magnitud absoluta (o sea sin signo) al critical value, entonces proceso es NO estacionario 
ers_decision <- ifelse(abs(ers_result@teststat) > abs(ers_result@cval[2]), "NON-STATIONARY", "STATIONARY")


# STATIONARITY RESULTS 
# Create a vector of decisions
decisions <- c(ADF = adf_decision, KPSS = kpss_decision, PP = pp_decision, ERS = ers_decision)

# Count the number of STATIONARY decisions
num_stationary <- length(decisions[decisions == "STATIONARY"])

# Determine the final DECISION based on the majority
final_decision <- ifelse(num_stationary >= 2, "STATIONARY", "NON STATIONARY")

# Stationarity results 
stationarity_results <- data.frame(
  ADF = adf_decision,
  KPSS = kpss_decision,
  PP = pp_decision,
  ERS = ers_decision,
  DECISION = final_decision
)
stationarity_results # The time series IS STATIONARY 



#b. AUTOCORRELATION (helps in selecting parameters "p" and "q" for ARIMA)
ggAcf(train) # parameter "q" appears to be equal to 0, and Q equal to 1
ggPacf(train, lag.max = 48) # parameter "p" appears to be equal 1, "P" could be 2
Box.test(train, lag = 10, type = "Ljung") # p < 0.05, time series is not white noise.



# FITTING AND VALIDATING MODELS ------------- 
#1. ETS, Holt Winters, ARIMA and TBATS ---- 
multi_model_results <- multi_model_fitting(time_series, lambda = TRUE)



#4. ARIMA (manual) ---- 
#a. STATIONARITY (helps in selecting parameter "d" and transformations)
library(astsa)
autoplot(train) # Time series is heteroscedastic, so BoxCox transformation will be used. There seems to be no trend so d = 0
stationarity_results # The time series IS STATIONARY 
shapiro.test(train)
lambda <- BoxCox.lambda(train)
lambda
autoplot(BoxCox(train, lambda))


#b. AUTOCORRELATION (helps in selecting parameters "p" and "q" for ARIMA)
ggAcf(train, lag.max = 48) # parameter "q" appears to be equal to 0, and Q equal to 0
ggPacf(train, lag.max = 48) # parameter "p" appears to be equal 1, "P" could be 0 or 2. Consider that both ACF and PACF tail off at seasonal lags, Q and P could also be equal to 1.
acf2(train) # d = 0 because time series has no trend, ACF tails off so q = 0, PACF cuts off at lag 1 so p = 1. ACF and PACF both tail off at seasonal lags so P = 1 and Q = 1
Box.test(train, lag = 10, type = "Ljung") # p < 0.05, time series is not white noise.

# Selecting possible values for each parameter
p <- c(1)
d <- c(0)
q <- c(0)
P <- c(0, 1, 2)
D <- c(0, 1)
Q <- c(0, 1)

# Applying ARIMA iteration function over all possible combinations of the parameters
arima_results <- arima_iteration_function(time_series, p_values = p, d_values = d, q_values = q, 
                                          P_values = P, D_values = D, Q_values = Q, lambda = TRUE)

arima_results$results_df



#3. Forecasting with WEATHER external variables ---------------------------------------------------------------------------------- 
#3.1. REGRESSION WITH EXTERNAL VARIABLES ----------------------------- 
# Data wrangling
min_ts_date <- as.Date(lubridate::date_decimal(min(index(time(time_series)))))-1 # -1 because date was finishing in day 2
max_ts_date <- as.Date(lubridate::date_decimal(max(index(time(time_series)))))
min_train_date <- as.Date(lubridate::date_decimal(min(index(time(train)))))-1 # -1 because date was finishing in day 2
max_train_date <- as.Date(lubridate::date_decimal(max(index(time(train)))))
min_test_date <- as.Date(lubridate::date_decimal(min(index(time(test))))) # -1 because date was finishing in day 2
max_test_date <- as.Date(lubridate::date_decimal(max(index(time(test)))))
min_weather_date <- min(weather_month_year$month_year)
max_weather_date <- max(weather_month_year$month_year)


weather_data <- weather_month_year[weather_month_year$month_year >= min_ts_date &
                                     weather_month_year$month_year <= max_ts_date,] # Filtering only the periods that both series share 

price_data <- as.data.frame(data_p2[data_p2$month_year >= min_weather_date &
                        data_p2$month_year <= max_weather_date,])

regression_data <- cbind(dplyr::select(price_data, -Commodity, -month_year), dplyr::select(weather_data, -month_year)) # Bind data and remove month_year from price_data to avoid having a duplicated column 
regression_data <- regression_data %>% apply(2, as.numeric) %>% as.data.frame()
head(regression_data)


# Exploring relationships
corr_matrix <- round(cor(regression_data), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 2.3)




# Dividing regression data into train and test set 
regression_data <- cbind(regression_data, data.frame(month_year = price_data$month_year))
n <- nrow(regression_data)
test_size <- round(n * 0.2)
train_size <- n - test_size

train_w1_full <- regression_data[1:train_size,]
test_w1_full <- regression_data[(train_size+1):n,]

train_w1 <- train_w1_full %>% dplyr::select(-month_year)
test_w1 <- test_w1_full %>% dplyr::select(-month_year)


# Fitting regression model 1 with all weather variables
lm <- lm(Avg_price ~ ., data = train_w1)
summary(lm)
autoplot(lm)
vif(lm) # Checking for multicollinearity for Model 1 through VIF (Variance Inflation Factor)
augment(lm)
gvlma(lm)

predictions_lm <- predict(lm, test_w1)
RMSE_lm <- mean((predictions_lm - test_w1$Avg_price)^2)
MAPE_lm <- mean(abs((predictions_lm - test_w1$Avg_price)/test_w1$Avg_price))*100


# Fitting model 2
lm2 <- lm(Avg_price ~ Precipitation + Max_temp, train_w1)
summary(lm2)
vif(lm2)
augment(lm2)
ggfortify::autoplot(lm2)
gvlma(lm2) # All assumptions are acceptable

predictions_lm2 <- predict(lm2, test_w1)
RMSE_lm2 <- mean((predictions_lm2 - test_w1$Avg_price)^2)
MAPE_lm2 <- mean(abs((predictions_lm2 - test_w1$Avg_price)/test_w1$Avg_price))*100


# Fitting model 3 
lm3 <- lm(Avg_price ~ Min_temp, train_w1) 
summary(lm3)
vif(lm3)
augment(lm3)
autoplot(lm3)
gvlma(lm3)

predictions_lm3 <- predict(lm3, test_w1)
RMSE_lm3 <- mean((predictions_lm3 - test_w1$Avg_price)^2)
MAPE_lm3 <- mean(abs((predictions_lm3 - test_w1$Avg_price)/test_w1$Avg_price))*100


# Fitting model USING STEPWISE REGRESSION FUNCTION 
# Stepwise regression model
library(MASS)
step.model <- stepAIC(lm, direction = "both", 
                      trace = FALSE)
summary(step.model)
gvlma(step.model)

# Fit model with only the significant variables
step.model2 <- lm(Avg_price ~ Min_temp + Temp_range, train_w1)
summary(step.model2)





# Fit model intuitively 
lm4 <- lm(Avg_price ~ Wind + Precipitation + Max_temp, train_w1)
summary(lm4)
vif(lm4)
gvlma(lm4)
autoplot(lm4)

predictions_lm4 <- predict(lm4, test_w1)
RMSE_lm4 <- mean((predictions_lm4 - test_w1$Avg_price)^2)
MAPE_lm4 <- mean(abs((predictions_lm4 - test_w1$Avg_price)/test_w1$Avg_price))*100
MAPE_lm4



# Chossing best model 
summary(lm3) # R2 = 0.80, AIC = 524, MAPE = 16%, RMSE = 285
AIC(lm3)
MAPE_lm3
RMSE_lm3

summary(lm4) # R2 = 0.82, AIC = 521, MAPE = 17%, RMSE = 257
AIC(lm4)
MAPE_lm4
RMSE_lm4

model <- lm4


# Final goodness of fit graph
final.model.diag.metrics <- augment(model)
measured <- train_w1$Avg_price
predicted <- final.model.diag.metrics$.fitted
datos_gráfica <- as.data.frame(cbind(Measured = measured, Predicted = predicted))

ggplot(datos_gráfica, aes(Measured, Predicted)) + 
  geom_point(aes(col = "Predicted"), size = 2) + 
  geom_smooth(method = "lm", se = FALSE, aes(col = "Model line")) + 
  geom_line(data = datos_gráfica, aes(Measured, Measured, col = "Real line")) + 
  labs(x = "Measured values", y = "Predicted values") +
  coord_cartesian(xlim = c(min(measured), 1.1*max(predicted)), ylim = c(min(predicted), 1.1*max(predicted))) +
  scale_color_manual(name= "Legend",
                     breaks=c("Real line", "Model line", "Predicted"),
                     values=c("Real line" = "black", "Model line" = "blue" , "Predicted"= "red")) +
  guides(color = guide_legend(override.aes = list(linetype = c(1, 1, 0),
                                                  shape = c(NULL, NULL, 20),
                                                  size = c(1, 1, 2)))) +
  ggtitle(label = "Goodness of fit of Okara price prediction weather-based model") +
  theme(legend.title= element_text(size= 15),
        legend.text= element_text(size= 13),
        legend.position = c(0.85, 0.2),
        legend.background = element_rect(fill="lightblue", 
                                         size=0.5, linetype="solid",
                                         colour ="black"), 
        legend.key = element_rect(fill = "lightblue"),
        axis.line.y = element_line(),
        axis.line.x = element_line(), 
        panel.border = element_rect(colour = "black", fill = NA), 
        plot.title = element_text(hjust = 0.5, size = 13), 
        axis.text.y = element_text(size = 13, angle = 90, hjust = 0.5),
        axis.text.x = element_text(size = 13),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))





# AUTOMATIC ARIMA MODEL INCORPORATING THE EXTERNAL VARIABLES FROM THE REGRESSION ---------------- 
# Wrangling the time series train and test set so that it matches the weather data
max(train_w1_full$month_year) # Maximum date in the regression train set
max(test_w1_full$month_year) # Maximum date in the regression test set
min(train_w1_full$month_year) # Minimum date in the regression train set
min(test_w1_full$month_year) # Minimum date in the regression test set

train_xreg <- window(train, end = c(2018, 8))
test_xreg <- window(time_series, start = c(2018, 9), end = c(2019, 12))
ts_xreg <- window(time_series, start = c(2013, 6), end = c(2019, 12))


# Fitting model 
lambda <- BoxCox.lambda(train)
lambda
xreg_matrix_train <- as.matrix(train_w1[,c("Wind", "Precipitation", "Max_temp")])
xreg_matrix_test <- as.matrix(test_w1[,c("Wind", "Precipitation", "Max_temp")])
arima_p2_xreg <- auto.arima(train_xreg, seasonal = TRUE, lambda = lambda, 
                       xreg = xreg_matrix_train, stepwise = FALSE)
summary(arima_p2_xreg)

# Graph 1 -- Training set and fitted prediction 
ggplot(filter(train_test_p2, Set == "Train"), aes(x = Date, y = Price)) + 
  geom_line(aes(color = "Real"), size = 0.7) + 
  geom_line(aes(x = time(fitted(arima_p2_xreg)), y = fitted(arima_p2_xreg), color = "Predicted"), size = 0.7) + 
  ggtitle("Predicted values by ETS model for training set of 'Cauli Local'") +
  scale_color_manual(name = "Series",
                     values = c("Real" = "black", "Predicted" = "blue")) + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(colour = "black"), 
    legend.key = element_rect(fill = "white")
  )



# Graph 2 -- Forecast vs test set 
arima_p2_xreg %>% 
  forecast(h = 15, PI = TRUE, xreg = xreg_matrix_test) %>% 
  forecast::autoplot() + 
   geom_line(data = train_test_p2[train_test_p2$Set == "Test", ], 
            aes(x = Date, y = Price, color = "Test"), size = 0.9) +
  geom_segment(aes(x = max_train, 
                   y = max_train_value, 
                   xend = min_test, 
                   yend = min_test_value), 
               color = "#F8766D", size = 0.7) + # Plotting forecasts and residuals
  scale_color_manual(name = "Series",
                     values = c("Forecast" = "blue", "Test" = "#F8766D", "Train" = "black")) +
  scale_shape_manual(name = "Series", 
                     values = c("Forecast" = "line", "Test" = "line", "Train" = "line")) + 
  ggtitle("Forecast from ARIMA model with added climatic variables") + 
  ylab(label = "Price") + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(colour = "black"), 
    legend.key = element_rect(fill = "white")
  )




checkresiduals(arima_p2_xreg) # Analyzing residuals (should be independent, mean 0, homoscedastic and normally distributed)
ljung_box_arima_p2_xreg <- ifelse(checkresiduals(arima_p2_xreg)$p.value > 0.05, "WHITE NOISE", "NOT WHITE NOISE")
mean(arima_p2_xreg$residuals) # Mean should be close to 0, the model has a slight positive bias 


errors_arima_p2_xreg <- accuracy(forecast(arima_p2_xreg, xreg = xreg_matrix_test), ts_xreg) # Measuring error through train and test set validation 
errors_arima_p2_xreg

lambda_ts <- BoxCox.lambda(ts_xreg)
arima_forecast_xreg <- function(x, xreg, newxreg, lambda, h) {
  forecast(auto.arima(x, seasonal = TRUE, lambda = lambda_ts, xreg = xreg, stepwise = FALSE), xreg = newxreg, h = h)
}

xreg_matrix <- rbind(xreg_matrix_train, xreg_matrix_test)
cv_result <- tsCV(ts_xreg, arima_forecast_xreg, xreg = xreg_matrix, h = 1, lambda = lambda)  # Measuring error through cross-validation
RMSE_cv <- sqrt(mean(cv_result^2, na.rm = TRUE))
RMSE_cv
MAPE_cv <- mean(abs((cv_result[-length(cv_result)]/ts_xreg[-length(ts_xreg)])), na.rm = TRUE)*100
MAPE_cv

errorsummary_arima_xreg <- data.frame(model = "ARIMA_XREG",
                                    RESIDUAL_ASSUMPTIONS = ljung_box_arima_p2_xreg,
                                    RMSE_train = errors_arima_p2_xreg[1, 2], 
                                    RMSE_test = errors_arima_p2_xreg[2, 2], 
                                    RMSE_cv = RMSE_cv, 
                                    MAPE_train = errors_arima_p2_xreg[1, 5], 
                                    MAPE_test = errors_arima_p2_xreg[2, 5], 
                                    MAPE_cv = MAPE_cv, 
                                    MAPE_index = (errors_arima_p2_xreg[2, 5]+MAPE_cv)/2
)
rownames(errorsummary_arima_xreg) <- "ARIMA_XREG"
errorsummary_arima_xreg # Model doesn't have white noise residuals



#3. CONSOLIDATING FINAL RESULTS TABLE WITH RMSE, RMSEcv and RMSEtest TO CHOOSE MODEL --------  
preliminar_results <- union_all(arima_results$results_df, errorsummary_arima_xreg)
final_results <- union_all(multi_model_results$results_df, preliminar_results)
final_results_arranged <- final_results %>% 
  filter(RESIDUAL_ASSUMPTIONS == "WHITE NOISE") %>% 
  arrange(MAPE_index)

final_results_arranged
best_model <- final_results_arranged[1, ]
best_model

lambda <- BoxCox.lambda(time_series)
final_model <- Arima(time_series, order = c(1, 0, 0), seasonal = c(1, 0, 1), lambda = lambda)

# FORECASTING (MODEL DEPLOYMENT) ------- 
final_forecast <- forecast(final_model, h = 12)
model_name <- best_model$model
forecast_dates <- as.Date(lubridate::date_decimal(index(final_forecast$mean)))

# Final results data frame ---- 
product_forecast_df <- product_forecast_df %>% union_all(
  data.frame(
  Product = rep("Okara", nrow(as.data.frame(final_forecast))),
  Date = as.character(forecast_dates),
  Forecasted_Values = as.vector(final_forecast$mean),
  Model = model_name,
  MAPE_cv = rep(best_model$MAPE_cv, nrow(as.data.frame(final_forecast))), 
  MAPE_test = rep(best_model$MAPE_test, nrow(as.data.frame(final_forecast))),
  Lower_CI = final_forecast$lower[,2],
  Upper_CI = final_forecast$upper[,2],
  stringsAsFactors = FALSE
))

product_forecast_df

autoplot(final_forecast)




#3. THIRD PRODUCT (BANANA) ---------------------------------------------------------------------------- 
# 1.1. Creating and visualizing time series
weather_month_year$month_year %>% max() # To match the interval with the weather data, dates will be considered between 2013-06-01 and 2019-12-01
data_p3 <- month_year_price_by_product[month_year_price_by_product$Commodity == "Banana" &
                                         month_year_price_by_product$month_year <= "2019-12-01", ]
ts_p3 <- ts(data_p3[, 3], start = c(2013, 06), frequency = 12) # Creating time series object
autoplot(ts_p3) # Time series seems heteroscedastic and seasonal

# Time series plot with trend line
ggplot(data_p3, aes(x = month_year, y = Avg_price)) + 
  geom_line(aes(colour = "Real")) + 
  geom_smooth(aes(color = "Trend"), method = "auto", linetype = 20, se = FALSE, size = 0.55) +
  scale_color_manual(name = "Series", 
                     values = c("Trend" = "red", "Real" = "black")) + 
  scale_linetype_manual(values = c("Trend" = 20, "Real" = 1)) + 
  guides(color = guide_legend(override.aes = list(linetype = c("Trend" = 20, "Real" = 1)))) +
  labs(title = "Time Series with Trend Line for product Okara") + 
  xlab(label = "Time") + 
  ylab(label = "Price") + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(colour = "black"), 
    legend.key = element_rect(fill = "white")) # Time series seems to have a slight trend 


# Season plots
season_data_m <- data_p3 %>% mutate(Month = format(month_year, "%m"), Year = format(month_year, "%Y")) %>% 
  group_by(Year, Month) %>% 
  summarise(Price = mean(Avg_price)) 
season_data_m$Month <- as.numeric(season_data_m$Month)

# Adding average line to season plot
monthly_avg <- data_p3 %>% mutate(Month = format(month_year, "%m")) %>% # Obtaining average price by month to plot average curve in season plot
  group_by(Month) %>% 
  summarise(Price = mean(Avg_price)) 
monthly_avg$Month <- as.numeric(monthly_avg$Month)

# Generating season plots
season_data_m
ggplot(season_data_m, aes(x = Month, y = Price, color = Year)) +
  geom_line() +
  geom_line(data = monthly_avg, aes(x = Month, y = Price), color = "black", size = 1) + 
  ggtitle(label = "Season plot for price of product 'Cauli Local'") + 
  scale_x_continuous(breaks = 1:12) +
  theme(
    panel.grid = element_blank(), 
    panel.background = element_blank(), 
    legend.background = element_rect(colour = "white"), 
    legend.key = element_rect(fill = "white"), 
    panel.grid.major.x = element_line(color = "lightgrey", size = 0.01),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust =  0.5)
  )




# SUBSETTING IN TRAIN AND TEST SET ---- 
time_series <- ts_p3
n <- length(time_series)
test_size <- round(n * 0.2)
train_size <- n - test_size

train <- ts(time_series[1:train_size], start = start(time_series), frequency = frequency(time_series))
test <- ts(time_series[(train_size+1):n], start = time(time_series)[train_size+1], frequency = frequency(time_series))
train_test <- data.frame(Date = time(ts_p3),
                            Price = union_all(c(train), c(test)),
                            Set = c(rep("Train", length(train)), rep("Test", length(test))))


# PLOTTING TRAIN AND TEST SET ---- 
# obtain maximum time of train set and minimum time of test set, with their corresponding values. This is to connect the 2 lines for visual display. 
max_train <- max(train_test[train_test$Set == "Train","Date"])
min_test <- min(train_test[train_test$Set == "Test", "Date"])
max_train_value <- train_test[train_test$Date == max_train, "Price"]
min_test_value <- train_test[train_test$Date == min_test, "Price"]

ggplot(train_test, aes(x = Date, y = Price, col = Set)) + 
  geom_line(size = 0.7) + 
  geom_segment(aes(x = max_train, 
                   y = max_train_value, 
                   xend = min_test, 
                   yend = min_test_value), 
               color = "#F8766D", size = 0.7) + 
  scale_color_manual(values = c("Train" = "black", "Test" = "#F8766D")) +
  ggtitle("Training and test set for product 'Cauli Local'") + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(colour = "black"), 
    legend.key = element_rect(fill = "white")
    
  )

# CHECKING STATISTICAL ASSUMPTIONS -------------- 
# a. STATIONARITY (select parameter "d")
# ADF test (p < 0.05 means STATIONARY)
adf_result <- adf.test(train)
adf_decision <- ifelse(adf_result$p.value < 0.05, c("STATIONARY"), c("NON STATIONARY"))

# # KPSS Test (Kwiatkowski-Phillips-Schmidt-Shin)
kpss_result <- ur.kpss(train)
summary(kpss_result) # Si critical value es mayor al critical value para 5pct significance level, proceso es NO estacionario
kpss_decision <- ifelse(abs(kpss_result@teststat) > abs(kpss_result@cval[2]), "NON-STATIONARY", "STATIONARY")

# PP Test (Phillips-Perron)
pp_result <- ur.pp(train)
summary(pp_result) # Si Z-alpha es menor en magnitud absoluta (o sea sin signo) a Z-tau, entonces proceso es NO estacionario
pp_decision <- ifelse(abs(pp_result@teststat) < abs(pp_result@auxstat[1]), "NON-STATIONARY", "STATIONARY")

# ERS Test (Elliott-Rothenberg-Stock) - Sirve particularmente para data SEASONAL 
ers_result <- ur.ers(train)
summary(ers_result) # Si test-statistic es mayor en magnitud absoluta (o sea sin signo) al critical value, entonces proceso es NO estacionario 
ers_decision <- ifelse(abs(ers_result@teststat) > abs(ers_result@cval[2]), "NON-STATIONARY", "STATIONARY")


# STATIONARITY RESULTS 
# Create a vector of decisions
decisions <- c(ADF = adf_decision, KPSS = kpss_decision, PP = pp_decision, ERS = ers_decision)

# Count the number of STATIONARY decisions
num_stationary <- length(decisions[decisions == "STATIONARY"])

# Determine the final DECISION based on the majority
final_decision <- ifelse(num_stationary >= 2, "STATIONARY", "NON STATIONARY")

# Stationarity results 
stationarity_results <- data.frame(
  ADF = adf_decision,
  KPSS = kpss_decision,
  PP = pp_decision,
  ERS = ers_decision,
  DECISION = final_decision
)
stationarity_results # The time series IS STATIONARY 



#b. AUTOCORRELATION (helps in selecting parameters "p" and "q" for ARIMA)
ggAcf(train) # parameter "q" appears to be equal to 0, and Q equal to 1
ggPacf(train, lag.max = 48) # parameter "p" appears to be equal 1, "P" could be 2
Box.test(train, lag = 10, type = "Ljung") # p < 0.05, time series is not white noise.



# FITTING AND VALIDATING MODELS ------------- 
#1. ETS, Holt Winters, ARIMA and TBATS ---- 
multi_model_results <- multi_model_fitting(time_series, lambda = TRUE)



#4. ARIMA (manual) ---- 
#a. STATIONARITY (helps in selecting parameter "d" and transformations)
library(astsa)
autoplot(train) # Time series is heteroscedastic, so BoxCox transformation will be used. There seems to be no trend so d = 0
stationarity_results # The time series IS STATIONARY 
shapiro.test(train)
lambda <- BoxCox.lambda(train)
lambda
autoplot(BoxCox(train_p1, lambda))


#b. AUTOCORRELATION (helps in selecting parameters "p" and "q" for ARIMA)
ggAcf(train, lag.max = 48) # parameter "q" appears to be equal to 0, 1 or 2, and Q equal to 1 or 0 
ggPacf(train, lag.max = 48) # parameter "p" appears to be equal 1, "P" could be 0 or 2. Consider that both ACF and PACF tail off at seasonal lags, Q and P could also be equal to 1.
acf2(train) # d = 0 because time series has no trend, ACF tails off so q = 0, PACF cuts off at lag 1 so p = 1. ACF and PACF both tail off at seasonal lags so P = 1 and Q = 1
Box.test(train, lag = 10, type = "Ljung") # p < 0.05, time series is not white noise.


# Selecting possible values for each parameter
p <- c(0, 1)
d <- c(0, 1)
q <- c(0, 1, 2)
P <- c(0, 1, 2)
D <- c(0, 1)
Q <- c(0, 1)

# Applying ARIMA iteration function over all possible combinations of the parameters
arima_results <- arima_iteration_function(time_series, p_values = p, d_values = d, q_values = q, 
                                          P_values = P, D_values = D, Q_values = Q, lambda = TRUE)

arima_results$results_df



#3. Forecasting with WEATHER external variables ---------------------------------------------------------------------------------- 
#3.1. REGRESSION WITH EXTERNAL VARIABLES ----------------------------- 
# Data wrangling
min_ts_date <- as.Date(lubridate::date_decimal(min(index(time(time_series)))))-1 # -1 because date was finishing in day 2
max_ts_date <- as.Date(lubridate::date_decimal(max(index(time(time_series)))))
min_train_date <- as.Date(lubridate::date_decimal(min(index(time(train)))))-1 # -1 because date was finishing in day 2
max_train_date <- as.Date(lubridate::date_decimal(max(index(time(train)))))
min_test_date <- as.Date(lubridate::date_decimal(min(index(time(test))))) # -1 because date was finishing in day 2
max_test_date <- as.Date(lubridate::date_decimal(max(index(time(test)))))

weather_data <- weather_month_year[weather_month_year$month_year >= min_ts_date &
                                     weather_month_year$month_year <= max_ts_date,] # Filtering only the periods that both series share 


price_data <- as.data.frame(data_p3[data_p3$month_year >= min_ts_date &
                                      data_p3$month_year <= max_ts_date,])

regression_data <- cbind(dplyr::select(price_data, -Commodity, -month_year), dplyr::select(weather_data, -month_year)) # Bind data and remove month_year from price_data to avoid having a duplicated column 
regression_data <- regression_data %>% apply(2, as.numeric) %>% as.data.frame()
head(regression_data)


# Exploring relationships
corr_matrix <- round(cor(regression_data), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 2.3)




# Dividing regression data into train and test set 
regression_data <- cbind(regression_data, data.frame(month_year = price_data$month_year))
train_w1 <- regression_data %>% filter(month_year <= max_train_date) %>% dplyr::select(-month_year)
test_w1 <- regression_data %>% filter(month_year > max_train_date) %>% dplyr::select(-month_year)




# Fitting regression model 1 with all weather variables
lm <- lm(Avg_price ~ ., data = train_w1)
summary(lm)
autoplot(lm)
vif(lm) # Checking for multicollinearity for Model 1 through VIF (Variance Inflation Factor)
augment(lm)
gvlma(lm)

predictions_lm <- predict(lm, test_w1)
RMSE_lm <- mean((predictions_lm - test_w1$Avg_price)^2)
MAPE_lm <- mean(abs((predictions_lm - test_w1$Avg_price)/test_w1$Avg_price))*100


# Fitting model 2
lm2 <- lm(Avg_price ~ Wind, train_w1)
summary(lm2)
vif(lm2)
augment(lm2)
autoplot(lm2)
gvlma(lm2) # All assumptions are acceptable

predictions_lm2 <- predict(lm2, test_w1)
RMSE_lm2 <- mean((predictions_lm2 - test_w1$Avg_price)^2)
MAPE_lm2 <- mean(abs((predictions_lm2 - test_w1$Avg_price)/test_w1$Avg_price))*100


# Fitting model 3 
lm3 <- lm(Avg_price ~ Wind_range, train_w1) 
summary(lm3)
vif(lm3)
augment(lm3)
autoplot(lm3)
gvlma(lm3)

predictions_lm3 <- predict(lm3, test_w1)
RMSE_lm3 <- mean((predictions_lm3 - test_w1$Avg_price)^2)
MAPE_lm3 <- mean(abs((predictions_lm3 - test_w1$Avg_price)/test_w1$Avg_price))*100


# Fitting model USING STEPWISE REGRESSION FUNCTION 
# Stepwise regression model
library(MASS)
step.model <- stepAIC(lm, direction = "both", 
                      trace = FALSE)
summary(step.model)
gvlma(step.model)

# Fit model with only the significant variables
step.model2 <- lm(log(Avg_price) ~ Precipitation + Humidity + Max_temp + Min_temp + Wind + Min_wind + Wind_range, train_w1)
summary(step.model2)

gvlma(step.model2)
autoplot(step.model2)

predictions_stepmodel2 <- predict(step.model2, test_w1)
RMSE_stepmodel2 <- mean((predictions_stepmodel2 - test_w1$Avg_price)^2)
MAPE_stepmodel2 <- mean(abs((predictions_stepmodel2 - test_w1$Avg_price)/test_w1$Avg_price))*100




# Chossing best model 
summary(lm3) # R2 = 0.80, AIC = 524, MAPE = 16%, RMSE = 285
AIC(lm3)
MAPE_lm3
RMSE_lm3

summary(step.model2) # R2 = 0.82, AIC = 521, MAPE = 17%, RMSE = 257
AIC(step.model2)
MAPE_stepmodel2
RMSE_stepmodel2

model <- lm4


# Final goodness of fit graph
final.model.diag.metrics <- augment(model)
measured <- train_w1$Avg_price
predicted <- final.model.diag.metrics$.fitted
datos_gráfica <- as.data.frame(cbind(Measured = measured, Predicted = predicted))

ggplot(datos_gráfica, aes(Measured, Predicted)) + 
  geom_point(aes(col = "Predicted"), size = 2) + 
  geom_smooth(method = "lm", se = FALSE, aes(col = "Model line")) + 
  geom_line(data = datos_gráfica, aes(Measured, Measured, col = "Real line")) + 
  labs(x = "Measured values", y = "Predicted values") +
  coord_cartesian(xlim = c(min(measured), 1.1*max(predicted)), ylim = c(min(predicted), 1.1*max(predicted))) +
  scale_color_manual(name= "Legend",
                     breaks=c("Real line", "Model line", "Predicted"),
                     values=c("Real line" = "black", "Model line" = "blue" , "Predicted"= "red")) +
  guides(color = guide_legend(override.aes = list(linetype = c(1, 1, 0),
                                                  shape = c(NULL, NULL, 20),
                                                  size = c(1, 1, 2)))) +
  ggtitle(label = "Goodness of fit of Okara price prediction weather-based model") +
  theme(legend.title= element_text(size= 15),
        legend.text= element_text(size= 13),
        legend.position = c(0.85, 0.2),
        legend.background = element_rect(fill="lightblue", 
                                         size=0.5, linetype="solid",
                                         colour ="black"), 
        legend.key = element_rect(fill = "lightblue"),
        axis.line.y = element_line(),
        axis.line.x = element_line(), 
        panel.border = element_rect(colour = "black", fill = NA), 
        plot.title = element_text(hjust = 0.5, size = 13), 
        axis.text.y = element_text(size = 13, angle = 90, hjust = 0.5),
        axis.text.x = element_text(size = 13),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))





# AUTOMATIC ARIMA MODEL INCORPORATING THE EXTERNAL VARIABLES FROM THE REGRESSION ---------------- 
autoplot(train) # Time series is heteroscedastic, so BoxCox transformation will be used 
shapiro.test(train)
lambda_train <- BoxCox.lambda(train)
lambda_train
autoplot(BoxCox(train, lambda_train))
xreg_matrix_train <- as.matrix(train_w1[,c("Min_wind")])
xreg_matrix_test <- as.matrix(test_w1[,c("Min_wind")])
model <- auto.arima(train, seasonal = TRUE, lambda = lambda_train, 
                            xreg = xreg_matrix_train, stepwise = FALSE)
summary(model)

# Graph 1 -- Training set and fitted prediction 
ggplot(filter(train_test, Set == "Train"), aes(x = Date, y = Price)) + 
  geom_line(aes(color = "Real"), size = 0.7) + 
  geom_line(aes(x = time(fitted(model)), y = fitted(model), color = "Predicted"), size = 0.7) + 
  ggtitle("Predicted values by ETS model for training set of 'Cauli Local'") +
  scale_color_manual(name = "Series",
                     values = c("Real" = "black", "Predicted" = "blue")) + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(colour = "black"), 
    legend.key = element_rect(fill = "white")
  )



# Graph 2 -- Forecast vs test set 
model %>% 
  forecast(h = 15, PI = TRUE, xreg = xreg_matrix_test) %>% 
  forecast::autoplot() + 
  geom_line(data = train_test[train_test$Set == "Test", ], 
            aes(x = Date, y = Price, color = "Test"), size = 0.9) +
  geom_segment(aes(x = max_train, 
                   y = max_train_value, 
                   xend = min_test, 
                   yend = min_test_value), 
               color = "#F8766D", size = 0.7) + # Plotting forecasts and residuals
  scale_color_manual(name = "Series",
                     values = c("Forecast" = "blue", "Test" = "#F8766D", "Train" = "black")) +
  scale_shape_manual(name = "Series", 
                     values = c("Forecast" = "line", "Test" = "line", "Train" = "line")) + 
  ggtitle("Forecast from ARIMA model with added climatic variables") + 
  ylab(label = "Price") + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(colour = "black"), 
    legend.key = element_rect(fill = "white")
  )




checkresiduals(model) # Analyzing residuals (should be independent, mean 0, homoscedastic and normally distributed)
ljung_box <- ifelse(checkresiduals(model)$p.value > 0.05, "WHITE NOISE", "NOT WHITE NOISE")
mean(model$residuals) # Mean should be close to 0, the model has a slight positive bias 


errors_model <- accuracy(forecast(model, xreg = xreg_matrix_test), time_series) # Measuring error through train and test set validation 
errors_model

lambda_ts <- BoxCox.lambda(time_series)
arima_forecast_xreg <- function(x, xreg, newxreg, h) {
  forecast(auto.arima(x, seasonal = TRUE, lambda = lambda_ts, xreg = xreg, stepwise = FALSE), xreg = newxreg, h = h)
}

xreg_matrix <- rbind(xreg_matrix_train, xreg_matrix_test)
cv_result <- tsCV(ts_p3, arima_forecast_xreg, xreg = xreg_matrix, h = 1)  # Measuring error through cross-validation
cv_result
RMSE_cv <- sqrt(mean(cv_result^2, na.rm = TRUE))
RMSE_cv
MAPE_cv <- mean(abs((cv_result[-length(cv_result)]/ts_p3[-length(ts_p3)])), na.rm = TRUE)*100
MAPE_cv

errorsummary_model <- data.frame(model = "ARIMA_XREG",
                                         RESIDUAL_ASSUMPTIONS = ljung_box,
                                         RMSE_train = errors_model[1, 2], 
                                         RMSE_test = errors_model[2, 2], 
                                         RMSE_cv = RMSE_cv, 
                                         MAPE_train = errors_model[1, 5], 
                                         MAPE_test = errors_model[2, 5], 
                                         MAPE_cv = MAPE_cv, 
                                         MAPE_index = (errors_model[2, 5] + MAPE_cv)/2)
rownames(errorsummary_model) <- "ARIMA_xreg_p3"
errorsummary_model # Model doesn't have white noise residuals





# MANUAL ARIMA MODEL INCORPORATING THE EXTERNAL VARIABLES FROM THE REGRESSION ---------------- 
autoplot(train) # Time series is heteroscedastic, so BoxCox transformation will be used 
shapiro.test(train)
lambda <- BoxCox.lambda(train)
lambda
autoplot(BoxCox(train, lambda))
xreg_matrix_train <- as.matrix(train_w1[,c("Min_wind")])
xreg_matrix_test <- as.matrix(test_w1[,c("Min_wind")])
arima_manual_p3_xreg <- Arima(train, order = c(1, 1, 1), seasonal = c(1, 0, 1), xreg = xreg_matrix_train)
summary(arima_manual_p3_xreg)

# Graph 1 -- Training set and fitted prediction 
ggplot(filter(train_test_p3, Set == "Train"), aes(x = Date, y = Price)) + 
  geom_line(aes(color = "Real"), size = 0.7) + 
  geom_line(aes(x = time(fitted(arima_manual_p3_xreg)), y = fitted(arima_manual_p3_xreg), color = "Predicted"), size = 0.7) + 
  ggtitle("Predicted values by ETS model for training set of 'Cauli Local'") +
  scale_color_manual(name = "Series",
                     values = c("Real" = "black", "Predicted" = "blue")) + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(colour = "black"), 
    legend.key = element_rect(fill = "white")
  )



# Graph 2 -- Forecast vs test set 
arima_manual_p3_xreg %>% 
  forecast(h = 15, PI = TRUE, xreg = xreg_matrix_test) %>% 
  forecast::autoplot() + 
  geom_line(data = train_test_p3[train_test_p3$Set == "Test", ], 
            aes(x = Date, y = Price, color = "Test"), size = 0.9) +
  geom_segment(aes(x = max_train, 
                   y = max_train_value, 
                   xend = min_test, 
                   yend = min_test_value), 
               color = "#F8766D", size = 0.7) + # Plotting forecasts and residuals
  scale_color_manual(name = "Series",
                     values = c("Forecast" = "blue", "Test" = "#F8766D", "Train" = "black")) +
  scale_shape_manual(name = "Series", 
                     values = c("Forecast" = "line", "Test" = "line", "Train" = "line")) + 
  ggtitle("Forecast from ARIMA model with added climatic variables") + 
  ylab(label = "Price") + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(colour = "black"), 
    legend.key = element_rect(fill = "white")
  )




checkresiduals(arima_manual_p3_xreg) # Analyzing residuals (should be independent, mean 0, homoscedastic and normally distributed)
ljung_box_arima_manual_p3_xreg <- ifelse(checkresiduals(arima_manual_p3_xreg)$p.value > 0.05, "WHITE NOISE", "NOT WHITE NOISE")
mean(arima_manual_p3_xreg$residuals) # Mean should be close to 0, the model has a slight positive bias 


errors_arima_manual_p3_xreg <- accuracy(forecast(arima_manual_p3_xreg, xreg = xreg_matrix_test), ts_p3) # Measuring error through train and test set validation 
errors_arima_manual_p3_xreg

arima_forecast_xreg <- function(x, xreg, newxreg, h) {
  forecast(Arima(x, order = c(1, 1, 1), seasonal = c(1, 0, 1), xreg = xreg), xreg = newxreg, h = h)
}

xreg_matrix <- rbind(xreg_matrix_train, xreg_matrix_test)
cv_result <- tsCV(ts_p3, arima_forecast_xreg, xreg = xreg_matrix, h = 1)  # Measuring error through cross-validation
cv_result
RMSE_cv <- sqrt(mean(cv_result^2, na.rm = TRUE))
RMSE_cv
MAPE_cv <- mean(abs((cv_result[-length(cv_result)]/ts_p3[-length(ts_p3)])), na.rm = TRUE)*100
MAPE_cv

errorsummary_arima_manual_p3_xreg <- data.frame(RESIDUAL_ASSUMPTIONS = ljung_box_arima_manual_p3_xreg,
                                         RMSE_train = errors_arima_manual_p3_xreg[1, 2], 
                                         RMSE_test = errors_arima_manual_p3_xreg[2, 2], 
                                         RMSE_cv = RMSE_cv, 
                                         MAPE_train = errors_arima_manual_p3_xreg[1, 5], 
                                         MAPE_test = errors_arima_manual_p3_xreg[2, 5], 
                                         MAPE_cv = MAPE_cv)
rownames(errorsummary_arima_manual_p3_xreg) <- "ARIMA_xreg_p3"
errorsummary_arima_manual_p3_xreg # Model doesn't have white noise residuals


# Consolidating error summaries of both ETS, HW and ARIMA models 
error_summary_p3 <- union(error_summary_p3, 
                          errorsummary_arima_manual_p3_xreg)
error_summary_p3 # ETS remains the best for 1 step forecasts followed by auto ARIMA, whereas the Holt-Winters multiplicative model is the best for multi-step forecasts. ARIMA has the least residuality. 




# CONSOLIDATING FINAL RESULTS TABLE WITH RMSE, RMSEcv and RMSEtest TO CHOOSE MODEL --------  
preliminar_results <- union_all(arima_results$results_df, errorsummary_model)
final_results <- union_all(multi_model_results$results_df, preliminar_results)
final_results_arranged <- final_results %>% 
  filter(RESIDUAL_ASSUMPTIONS == "WHITE NOISE") %>% 
  arrange(MAPE_index)

final_results_arranged
best_model <- final_results_arranged[1, ]
best_model

lambda <- BoxCox.lambda(time_series)
final_model <- Arima(time_series, order = c(0, 1, 2), seasonal = c(1, 0, 1), lambda = lambda)

# FORECASTING (MODEL DEPLOYMENT) ------- 
final_forecast <- forecast(final_model, h = 12)
model_name <- best_model$model
forecast_dates <- as.Date(lubridate::date_decimal(index(final_forecast$mean)))

# Final results data frame ---- 
product_forecast_df <- product_forecast_df %>% union_all(
  data.frame(
  Product = rep("Banana", nrow(as.data.frame(final_forecast))),
  Date = as.character(forecast_dates),
  Forecasted_Values = as.vector(final_forecast$mean),
  Model = model_name,
  MAPE_cv = rep(best_model$MAPE_cv, nrow(as.data.frame(final_forecast))), 
  MAPE_test = rep(best_model$MAPE_test, nrow(as.data.frame(final_forecast))),
  Lower_CI = final_forecast$lower[,2],
  Upper_CI = final_forecast$upper[,2],
  stringsAsFactors = FALSE
))

product_forecast_df

write.csv(product_forecast_df, "C:/Users/User/Documents/Data/Proyecto Forecast/Data/Final Forecasts - Full Loop.csv")
product_forecast_df



