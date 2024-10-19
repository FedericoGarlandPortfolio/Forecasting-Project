# AUTOMATION OF ARIMA HYPERPARAMETER ITERATION ------------------
# --- Fits different combinations of p, d, q, P, D and Q and returns the 3 best models according to one step MAPE cross-validation, MAPE on the test set and the average of the 2 metrics. 

arima_iteration_function <- function(time_series, p_values, d_values, q_values, 
                                     P_values, D_values, Q_values, s = 12){
  

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
              Arima(train, order = c(p, d, q), seasonal = c(P, D, Q))
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
              
              arima_manual_forecast <- function(x, lambda, h) {
                forecast(Arima(x, order = c(p, d, q), seasonal = c(P, D, Q)), h = h)
              }
              
              # One step cross-validation (rolling origin) ---
              cv_result <- tsCV(time_series, arima_manual_forecast, h = 1)
              RMSE_cv <- sqrt(mean(cv_result^2, na.rm = TRUE))
              MAPE_cv <- mean(abs((cv_result[-length(cv_result)]/time_series[-length(time_series)])), na.rm = TRUE) * 100
              
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

print(results_df)
print(top_models_df)
}

