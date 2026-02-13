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
##==== Learn priors..
##==============================##

## Substitute MoM with MLE at some point.

learn_priors <- function(proxy_returns) {
  
  # 1. Fit the Standard Model to the Proxy
  message("Fitting model to proxy data to learn priors...")
  # We use default uninformative priors here to let the long history speak.
  fit_proxy <- svsample(proxy_returns, designmatrix = "ar0", quiet = FALSE)
  
  # =========================================================
  # FIX: Convert mcmc.list objects to standard matrices first
  # =========================================================
  sv_params   <- as.matrix(fit_proxy$para) # Contains: mu, phi, sigma
  beta_params <- as.matrix(fit_proxy$beta) # Contains: beta_0 (Intercept/Drift)
  
  # ---------------------------------------------------------
  # 2. Autolearn MU (Volatility Level)
  # NOTE: In stochvol, 'mu' is the mean of log-volatility, NOT price drift.
  # ---------------------------------------------------------
  mu_draws <- sv_params[, "mu"]
  
  # Robustness: Inflate SD to avoid being too dogmatic
  learned_mu <- c(mean(mu_draws), sd(mu_draws) * 2)
  
  # ---------------------------------------------------------
  # 3. Autolearn BETA (Price Drift / Mean Return)
  # This is the parameter that controls the long-term trend.
  # ---------------------------------------------------------
  # We extract the first column (the intercept)
  drift_draws <- beta_params[, 1] 
  
  # We define a Normal prior for the drift: c(mean, sd)
  learned_beta <- c(mean(drift_draws), sd(drift_draws) * 2)
  
  # ---------------------------------------------------------
  # 4. Autolearn SIGMA (Volatility of Volatility)
  # ---------------------------------------------------------
  sigma_draws <- sv_params[, "sigma"]
  learned_sigma <- mean(sigma_draws^2)
  
  # ---------------------------------------------------------
  # 5. Autolearn PHI (Persistence)
  # ---------------------------------------------------------
  phi_draws <- sv_params[, "phi"]
  
  # Transform phi (-1 to 1) to x (0 to 1) for Beta distribution
  x <- (phi_draws + 1) / 2
  
  # Method of Moments
  mean_x <- mean(x)
  var_x  <- var(x)
  
  # Handle numeric instability (if variance is extremely small)
  if (is.na(var_x) || var_x < 1e-8) var_x <- 1e-8
  
  term <- (mean_x * (1 - mean_x) / var_x) - 1
  a_learned <- mean_x * term
  b_learned <- (1 - mean_x) * term
  
  # Safety check for invalid Beta parameters
  if (is.na(a_learned) || a_learned <= 0 || b_learned <= 0) {
    warning("Method of moments failed for Phi. Reverting to default priors.")
    learned_phi <- c(5, 1.5)
  } else {
    learned_phi <- c(a_learned, b_learned)
  }
  
  list(
    mu    = learned_mu,    # Prior for Volatility Level
    beta  = learned_beta,  # Prior for Drift (Mean Return)
    phi   = learned_phi,   # Prior for Persistence
    sigma = learned_sigma  # Prior for Vol of Vol
  )
}

##==============================##
##==== Test function for the stock price.
##==============================##

forecast_stochvol_price <- function(data, h, 
                                    prior_nu = 0.1,  # 0.1 enables SV-t (Heavy Tails)
                                    prior_mu = c(0, 100),
                                    prior_phi = c(5, 1.5),
                                    prior_sigma = 1) {
  
  # ---------------------------------------------------------
  # 1. PREPARE DATA
  # ---------------------------------------------------------
  y <- data %>% pull(ret)
  last_price <- tail(data$Price, 1)
  
  # ---------------------------------------------------------
  # 2. ESTIMATION (With Drift + Heavy Tails)
  # ---------------------------------------------------------
  # priornu = 0.1 applies an exponential prior to the degrees of freedom (nu).
  # This tells the model: "Expect fat tails (low nu), but learn the exact shape."
  fit <- svsample(y, designmatrix = "ar0", 
                  priornu = prior_nu,       
                  priormu = prior_mu, 
                  priorphi = prior_phi, 
                  priorsigma = prior_sigma,
                  quiet = FALSE)
  
  # ---------------------------------------------------------
  # 3. FORECASTING
  # ---------------------------------------------------------
  # The prediction now draws from a t-distribution, generating
  # more extreme outliers than the Gaussian version.
  pred <- predict(fit, steps = h)
  return_draws <- as.matrix(pred$y) 
  
  # ---------------------------------------------------------
  # 4. CONVERT TO PRICE PATHS
  # ---------------------------------------------------------
  cum_returns <- t(apply(return_draws, 1, cumsum))
  price_paths <- last_price * exp(cum_returns)
  
  # ---------------------------------------------------------
  # 5. SELECT "WIGGLY" SCENARIOS
  # ---------------------------------------------------------
  final_day_prices <- price_paths[, h]
  targets <- quantile(final_day_prices, probs = c(0.05, 0.5, 0.95))
  
  idx_lower  <- which.min(abs(final_day_prices - targets[1])) 
  idx_median <- which.min(abs(final_day_prices - targets[2])) 
  idx_upper  <- which.min(abs(final_day_prices - targets[3])) 
  
  # ---------------------------------------------------------
  # 6. OUTPUT
  # ---------------------------------------------------------
  Price_forecast <- tibble(
    day_ahead = 1:h,
    price_lower  = price_paths[idx_lower, ],  
    price_medium = price_paths[idx_median, ], 
    price_upper  = price_paths[idx_upper, ]   
  )
  
  Out <- list(SVSample_fit = fit,
              Price_forecast = Price_forecast,
              target = targets)
  
  return(Out)
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
                             start_date = as.Date("2023-01-01"))
                             
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

# data <- test_tbl
# vol_forecast <- forecast_stochvol(data = data,       ### Run the custom function.
#                                   h = 100)

#==== 03B - Forecasted Stock Price ============================================#

data <- test_tbl
price_forecast_all <- forecast_stochvol_price(data = data,
                                          h = 100)

price_forecast <- price_forecast_all[["Price_forecast"]]

## Chart.
ggplot(price_forecast, aes(x = day_ahead)) +
  geom_line(aes(y = price_medium), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = price_lower, ymax = price_upper), fill = "blue", alpha = 0.2) +
  labs(title = "100-Day MSFT Price Forecast (Volatility Cone)",
       y = "Stock Price", x = "Days Ahead") +
  theme_minimal()

#==== 03C - Test the "learn" priors function ==================================#

proxy_data <- DownloadYahooFinance("SPY", start_date = "2000-01-01")
proxy_ret  <- Return.calculate(proxy_data, method="log") %>% na.omit() %>% as.vector()
proxy_ret_cutoff  <- proxy_ret[5800:length(proxy_ret)]

# 2. Learn the Priors
# This extracts the 'risk characteristics' of the asset class.
my_priors <- learn_priors(proxy_ret)
my_priors_cutoff <- learn_priors(proxy_ret_cutoff)

print("Learned Priors:")
print(my_priors)
print(my_priors_cutoff)

# 3. Apply to Your Limited Portfolio Data
# Your portfolio might be short, but the priors now enforce realistic risk behavior.
forecast_result_all <- forecast_stochvol_price(
  data = test_tbl, 
  h = 100,
  prior_nu = 0.1,                # Enable heavy tails
  prior_mu = my_priors$mu,       # Learned drift
  prior_phi = my_priors$phi,     # Learned persistence (clustering)
  prior_sigma = my_priors$sigma  # Learned vol-of-vol
)
forecast_result <- forecast_result_all[["Price_forecast"]]

forecast_result_cutoff_all <- forecast_stochvol_price(
  data = test_tbl, 
  h = 100,
  prior_nu = 0.1,                # Enable heavy tails
  prior_mu = my_priors_cutoff$mu,       # Learned drift
  prior_phi = my_priors_cutoff$phi,     # Learned persistence (clustering)
  prior_sigma = my_priors_cutoff$sigma  # Learned vol-of-vol
)
forecast_result_cutoff <- forecast_result_cutoff_all[["Price_forecast"]]

tail(forecast_result)
tail(forecast_result_cutoff)

## Chart.
ggplot(forecast_result, aes(x = day_ahead)) +
  geom_line(aes(y = price_medium), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = price_lower, ymax = price_upper), fill = "blue", alpha = 0.2) +
  labs(title = "100-Day MSFT Price Forecast (Volatility Cone)",
       y = "Stock Price", x = "Days Ahead") +
  theme_minimal()

ggplot(forecast_result_cutoff, aes(x = day_ahead)) +
  geom_line(aes(y = price_medium), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = price_lower, ymax = price_upper), fill = "blue", alpha = 0.2) +
  labs(title = "100-Day MSFT Price Forecast (Volatility Cone)",
       y = "Stock Price", x = "Days Ahead") +
  theme_minimal()


#==============================================================================#
#==== 04 - Mistr===============================================================#
#==============================================================================#

#==== 04A - Test the function =================================================#

data <- proxy_ret
gng_fit_obj <- GNG_fit(data)
dist_obj <- distribution(gng_fit_obj)

sample <- r(dist_obj, n = 500000)


#==============================================================================#
#==============================================================================#
#==============================================================================#