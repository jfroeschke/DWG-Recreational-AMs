# --- data ---
values <- c(97567, 70855, 62834, 119194, 136541, 131796, 18672, 22069, 48081, 59094)

# --- Step 1: Compute basic stats ---
mean_value <- mean(values)
pse <- 0.3
sd_val <- pse * mean_value    # proportional SD
n <- length(values)

# --- Step 2: Test autocorrelation ---
# Runs an autocorrelation function and plots
acf(values, main = "Autocorrelation of Original Data")

# Extract lag-1 autocorrelation
lag1_acf <- acf(values, lag.max = 1, plot = FALSE)$acf[2]
cat("Lag-1 autocorrelation:", lag1_acf, "\n")

# --- Step 3: Fit AR(1) model ---
ar_model <- arima(values, order = c(1, 0, 0))
phi <- ar_model$coef["ar1"]
cat("AR(1) coefficient (phi):", phi, "\n")

# --- Step 4: Simulate new time series ---
set.seed(123)
sim_values <- arima.sim(
  n = n, 
  list(ar = phi), 
  sd = sd_val
)

# Adjust to match the original mean
sim_values <- sim_values - mean(sim_values) + mean_value

# --- Step 5: Compare ---
ts.plot(values, sim_values, col = c("blue", "red"), lty = c(1, 2), main = "Original vs Simulated")
legend("topleft", legend = c("Original", "Simulated"), col = c("blue","red"), lty = c(1,2))

# Print simulated values
sim_values


# Original data
values <- c(97567, 70855, 62834, 119194, 136541, 131796, 18672, 22069, 48081, 59094)

# Fit AR(1) model
ar1_model <- arima(values, order = c(1, 0, 0))

# Fit white noise (no autocorrelation) model
wn_model <- arima(values, order = c(0, 0, 0))

# Compare AIC
cat("AR(1) AIC:", AIC(ar1_model), "\n")
cat("White Noise AIC:", AIC(wn_model), "\n")

# Which model is better?
if (AIC(ar1_model) < AIC(wn_model)) {
  cat("AR(1) model fits better than white noise.\n")
} else {
  cat("White noise model fits as well or better.\n")
}

# Optional: Compare residual autocorrelation
acf(residuals(ar1_model), main = "Residuals of AR(1) Model")
acf(residuals(wn_model), main = "Residuals of White Noise Model")

# Extract log-likelihoods
ll_ar1 <- as.numeric(logLik(ar1_model))
ll_wn  <- as.numeric(logLik(wn_model))

# Likelihood ratio test
LR <- 2 * (ll_ar1 - ll_wn)
p_value <- pchisq(LR, df = 1, lower.tail = FALSE)

cat("LogLik AR(1):", ll_ar1, "\n")
cat("LogLik White Noise:", ll_wn, "\n")
cat("Likelihood Ratio:", LR, "\n")
cat("p-value:", p_value, "\n")

if (p_value < 0.05) {
  cat("Significant: AR(1) model is better.\n")
} else {
  cat("Not significant: AR(1) model does not improve fit significantly.\n")
}
