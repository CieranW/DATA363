pdf("hw15.pdf")

# 3.1
# Given data
est_beta <- 17 / 418
rainfall_data <- c(3, 15, 1, 37, 5, 1, 8, 11, 6, 9, 12, 35, 22, 3, 38, 1, 2)

# Calculate the estimated probability
estimated_probability <- 1 - pgamma(25, shape = 1 / (2 * est_beta), scale = 1)

estimated_probability


# 3.2
rm(list = ls(all = TRUE))
data <- c(3, 15, 1, 35, 5, 1, 8, 11, 6, 9, 12, 35, 22, 3, 38, 1, 2)
n <- length(data)
xsrdata <- sort(data)
x_data <- seq(min(data), max(data))
beta_mm_est <- 1 / (2 * mean(data)) # Method of moment estimator
emp_cdf <- seq(1:n) / n
# Empirical cdf
gma_cdf <- pgamma(x_data, 1 / 2, beta_mm_est) # Gamma Cdf
plot(xsrdata, emp_cdf, cex = 10, type = "s", xlab = "Empirical CDF", ylab = "Fitted Gamma CDF")
lines(x_data, gma_cdf)

# 3.3
# Calculate 1/(2^β) using the estimator ^β
beta_hat <- 17 / 418
result <- 1 / (2^beta_hat)

# Check if the result is greater than 0
if (result > 0) {
    cat("The estimator is biased upward.")
} else {
    cat("The estimator is not biased upward.")
}

# 3.4
# Given data
est_beta <- 17 / 418
rainfall_data <- c(3, 15, 1, 37, 5, 1, 8, 11, 6, 9, 12, 35, 22, 3, 38, 1, 2)

# Calculate the estimated probability
estimated_probability <- 1 - pgamma(25, shape = 1 / (2 * est_beta), scale = 1)

# Manually calculate the derivative of g(μ) with respect to μ
mu <- 1 / (2 * est_beta) # Use the estimated value of μ
h <- 1e-5 # A small value for numerical differentiation

g_prime <- (pgamma(25 + h, shape = 1 / (2 * est_beta), scale = 1) - pgamma(25 - h, shape = 1 / (2 * est_beta), scale = 1)) / (2 * h)

# Estimate the variance using the delta method
var_X <- var(rainfall_data) # Variance of the rainfall data

var_alpha_hat <- (g_prime^2) * var_X

g_prime
var_X
var_alpha_hat


dev.off()
