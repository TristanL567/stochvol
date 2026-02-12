#==============================================================================#
#==== 00 - Description ========================================================#
#==============================================================================#





#==============================================================================#
#==== 00 - Libraries & Functions ==============================================#
#==============================================================================#

#==== Libraries ===============================================================#

packages <- c("stochvol", "dplyr", "tibble", "data.table",
              "DeepValuePackage", "PerformanceAnalytics",
              "ggplot2", "mistr"
)

for(i in 1:length(packages)){
  package_name <- packages[i]
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, character.only = TRUE)
    cat(paste("Package '", package_name, "' was not installed. It has now been installed and loaded.\n", sep = ""))
  } else {
    cat(paste("Package '", package_name, "' is already installed and has been loaded.\n", sep = ""))
  }
  library(package_name, character.only = TRUE)
}

#==== Functions ===============================================================#

##==============================##
##==== Test function.
##==============================##

forecast_stochvol <- function(data, h) {
  
  # ---------------------------------------------------------
  # 1. PREPARE DATA
  # ---------------------------------------------------------
  # Assumption: Data is time-ordered (past -> present)
  # Assumption: Returns are already log-returns (not raw prices)
  y <- data %>% pull(ret) 
  
  # ---------------------------------------------------------
  # 2. ESTIMATION (Capturing the Clustering)
  # ---------------------------------------------------------
  # We run the MCMC sampler to estimate parameters.
  # CLUSTERING IMPLEMENTATION: The sampler estimates 'phi' (persistence).
  # If phi is close to 1, high volatility today implies high volatility tomorrow.
  # Assumption: Volatility follows a stochastic AR(1) process[cite: 153].
  # Assumption: Errors are Gaussian (standard SV)[cite: 172].
  fit <- svsample(y, quiet = FALSE) 
  
  # ---------------------------------------------------------
  # 3. FORECASTING (Propagating the Clustering)
  # ---------------------------------------------------------
  # We simulate h steps forward for EVERY posterior draw.
  # CLUSTERING IMPLEMENTATION: The projection uses the equation:
  # h[t+1] = mu + phi * (h[t] - mu) + sigma * eta[cite: 153].
  # This enforces time dependency: the forecast starts from the 
  # last estimated volatility state (h_T) and decays slowly based on phi.
  pred <- predict(fit, steps = h)
  
  # ---------------------------------------------------------
  # 4. AGGREGATE RESULTS (The Fix)
  # ---------------------------------------------------------
  # Extract the 'vol' component (Predicted Volatility)
  # This is an mcmc.list. We convert it to a matrix (Rows = Draws, Cols = Days Ahead).
  vol_draws <- as.matrix(pred$vol)
  
  final_day_values <- vol_draws[, h]
  
  # 2. Determine the target values for the 5%, 50%, and 95% quantiles on that final day
  targets <- quantile(final_day_values, probs = c(0.05, 0.5, 0.95))
  
  # 3. Find the index (row number) of the simulation that ends closest to those targets
  # This picks a SINGLE coherent path for each scenario
  idx_lower  <- which.min(abs(final_day_values - targets[1])) # Scenario ending low
  idx_median <- which.min(abs(final_day_values - targets[2])) # Scenario ending in middle
  idx_upper  <- which.min(abs(final_day_values - targets[3])) # Scenario ending high
  
  # 4. Extract those full paths (rows) to preserve the "wiggles"
  out <- tibble(
    day_ahead = 1:h,
    vol_lower = vol_draws[idx_lower, ],   # A specific low-volatility path
    vol_forecast = vol_draws[idx_median, ], # A specific median-volatility path
    vol_upper = vol_draws[idx_upper, ]    # A specific high-volatility path
  )
  
  return(out)
  
  # Calculate the Mean volatility for each day ahead across all MCMC draws.
  # This aggregates the uncertainty into a point forecast.
  # vol_forecast <- colMeans(vol_draws)
  # 
  # return(tibble(
  #   day_ahead = 1:h,
  #   volatility_forecast = vol_forecast
  # ))
}

##==============================##
##==== Test function (custom distribution).
##==============================##

forecast_stochvol <- function(data, h) {
  
  # ---------------------------------------------------------
  # 1. PREPARE DATA
  # ---------------------------------------------------------
  # Assumption: Data is time-ordered (past -> present)
  # Assumption: Returns are already log-returns (not raw prices)
  y <- data %>% pull(ret) 
  
  # ---------------------------------------------------------
  # 2. ESTIMATION (Capturing the Clustering)
  # ---------------------------------------------------------
  # We run the MCMC sampler to estimate parameters.
  # CLUSTERING IMPLEMENTATION: The sampler estimates 'phi' (persistence).
  # If phi is close to 1, high volatility today implies high volatility tomorrow.
  # Assumption: Volatility follows a stochastic AR(1) process[cite: 153].
  # Assumption: Errors are Gaussian (standard SV)[cite: 172].
  fit <- svsample(y, quiet = FALSE) 
  
  # ---------------------------------------------------------
  # 3. FORECASTING (Propagating the Clustering)
  # ---------------------------------------------------------
  # We simulate h steps forward for EVERY posterior draw.
  # CLUSTERING IMPLEMENTATION: The projection uses the equation:
  # h[t+1] = mu + phi * (h[t] - mu) + sigma * eta[cite: 153].
  # This enforces time dependency: the forecast starts from the 
  # last estimated volatility state (h_T) and decays slowly based on phi.
  pred <- predict(fit, steps = h)
  
  # ---------------------------------------------------------
  # 4. AGGREGATE RESULTS (The Fix)
  # ---------------------------------------------------------
  # Extract the 'vol' component (Predicted Volatility)
  # This is an mcmc.list. We convert it to a matrix (Rows = Draws, Cols = Days Ahead).
  vol_draws <- as.matrix(pred$vol)
  
  final_day_values <- vol_draws[, h]
  
  # 2. Determine the target values for the 5%, 50%, and 95% quantiles on that final day
  targets <- quantile(final_day_values, probs = c(0.05, 0.5, 0.95))
  
  # 3. Find the index (row number) of the simulation that ends closest to those targets
  # This picks a SINGLE coherent path for each scenario
  idx_lower  <- which.min(abs(final_day_values - targets[1])) # Scenario ending low
  idx_median <- which.min(abs(final_day_values - targets[2])) # Scenario ending in middle
  idx_upper  <- which.min(abs(final_day_values - targets[3])) # Scenario ending high
  
  # 4. Extract those full paths (rows) to preserve the "wiggles"
  out <- tibble(
    day_ahead = 1:h,
    vol_lower = vol_draws[idx_lower, ],   # A specific low-volatility path
    vol_forecast = vol_draws[idx_median, ], # A specific median-volatility path
    vol_upper = vol_draws[idx_upper, ]    # A specific high-volatility path
  )
  
  return(out)
  
  # Calculate the Mean volatility for each day ahead across all MCMC draws.
  # This aggregates the uncertainty into a point forecast.
  # vol_forecast <- colMeans(vol_draws)
  # 
  # return(tibble(
  #   day_ahead = 1:h,
  #   volatility_forecast = vol_forecast
  # ))
}


##==============================##
##==== Test function for the stock price.
##==============================##

forecast_stochvol_price <- function(data, h) {
  
  # ---------------------------------------------------------
  # 1. PREPARE DATA
  # ---------------------------------------------------------
  y <- data %>% pull(ret)
  last_price <- tail(data$Price, 1)
  
  # ---------------------------------------------------------
  # 2. ESTIMATION (With Drift)
  # ---------------------------------------------------------
  # designmatrix = "ar0" automatically adds a constant intercept (mu).
  # This allows the model to learn the "Drift" (avg daily return).
  fit <- svsample(y, designmatrix = "ar0", quiet = FALSE)
  
  # ---------------------------------------------------------
  # 3. FORECASTING (Returns)
  # ---------------------------------------------------------
  # We simulate h steps forward. 
  # pred$y contains the future Returns (including drift + vol clustering + noise)
  pred <- predict(fit, steps = h)
  return_draws <- as.matrix(pred$y) # Matrix: Rows = 10,000 Draws, Cols = h Days
  
  # ---------------------------------------------------------
  # 4. CONVERT TO PRICE PATHS
  # ---------------------------------------------------------
  # We calculate the cumulative return for EVERY simulation path individually.
  # If a specific simulation had a "bad week", its price path will reflect that.
  cum_returns <- t(apply(return_draws, 1, cumsum))
  price_paths <- last_price * exp(cum_returns)
  
  # ---------------------------------------------------------
  # 5. SELECT "WIGGLY" SCENARIOS (The Magic Step)
  # ---------------------------------------------------------
  # Instead of averaging all 10,000 paths (which creates a smooth cone),
  # we look at the prices on the FINAL day (Day h).
  final_day_prices <- price_paths[, h]
  
  # Identify the 5%, 50%, and 95% target levels for the final day
  targets <- quantile(final_day_prices, probs = c(0.05, 0.5, 0.95))
  
  # Find the single simulation index (row) that ends closest to these targets.
  # This grabs a coherent "history" for that specific outcome.
  idx_lower  <- which.min(abs(final_day_prices - targets[1])) # The Bear Case Path
  idx_median <- which.min(abs(final_day_prices - targets[2])) # The Base Case Path
  idx_upper  <- which.min(abs(final_day_prices - targets[3])) # The Bull Case Path
  
  # ---------------------------------------------------------
  # 6. OUTPUT
  # ---------------------------------------------------------
  tibble(
    day_ahead = 1:h,
    price_lower  = price_paths[idx_lower, ],  
    price_medium = price_paths[idx_median, ], 
    price_upper  = price_paths[idx_upper, ]   
  )
}


#==============================================================================#
#==== 01 - Parameters =========================================================#
#==============================================================================#

##==============================##
## Manual Parameters.
##==============================##





##==============================##
## General Parameters.
##==============================##




#==============================================================================#
#==== 02 - Data ===============================================================#
#==============================================================================#

#==== 02A - Data Download =====================================================#

test <- DownloadYahooFinance(Tickers = "MSFT",
                             start_date = as.Date("2000-01-01"))
                             
test_return <- Return.calculate(test, method = "log")
combined_data <- merge(test, test_return)
test_tbl <- data.frame(Date = index(combined_data), coredata(combined_data)) %>%
  as_tibble() %>%
  setNames(c("Date", "Price", "ret")) %>% 
  na.omit()
# colnames(test_tbl) <- c("ret", "Date", "Price")

#==============================================================================#
#==== 03 - Code ===============================================================#
#==============================================================================#


#==== 03A - Test the function =================================================#

data <- test_tbl
vol_forecast <- forecast_stochvol(data = data,       ### Run the custom function.
                                  h = 100)

#==== 03B - Forecasted Stock Price ============================================#

price_forecast <- forecast_stochvol_price(data = data,
                                          h = 100)

## Chart.
ggplot(price_forecast, aes(x = day_ahead)) +
  geom_line(aes(y = price_medium), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = price_lower, ymax = price_upper), fill = "blue", alpha = 0.2) +
  labs(title = "100-Day MSFT Price Forecast (Volatility Cone)",
       y = "Stock Price", x = "Days Ahead") +
  theme_minimal()

#==============================================================================#
#==============================================================================#
#==============================================================================#