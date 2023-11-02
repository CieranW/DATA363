pdf("hw12.pdf")

#1
#1.1
# Set a seed for reproducibility
set.seed(123)

# Simulate 1000 uniformly distributed variables between 0.9 and 1.1
x_values <- runif(1000, min = 0.9, max = 1.1)

# Calculate g(x) and g^*(x)
g_x <- x_values^2
g_star_x <- 2 * x_values - 1

# Calculate means and standard deviations
mean_g_x <- mean(g_x)
mean_g_star_x <- mean(g_star_x)

sd_g_x <- sd(g_x)
sd_g_star_x <- sd(g_star_x)

# Print the results
cat("Mean of g(x):", mean_g_x, "\n")
cat("Mean of g*(x):", mean_g_star_x, "\n")
cat("Standard Deviation of g(x):", sd_g_x, "\n")
cat("Standard Deviation of g*(x):", sd_g_star_x, "\n")

#1.2
# Set a seed for reproducibility
set.seed(123)

# Simulate 1000 normally distributed variables with specified mean and standard deviation
mean_value <- 1
sd_value <- 0.2 / sqrt(12/25)
x_values <- rnorm(1000, mean = mean_value, sd = sd_value)

# Calculate g(x) and g^*(x)
g_x <- x_values^2
g_star_x <- 2 * x_values - 1

# Calculate means and standard deviations
mean_g_x <- mean(g_x)
mean_g_star_x <- mean(g_star_x)

sd_g_x <- sd(g_x)
sd_g_star_x <- sd(g_star_x)

# Print the results
cat("Mean of g(x):", mean_g_x, "\n")
cat("Mean of g*(x):", mean_g_star_x, "\n")
cat("Standard Deviation of g(x):", sd_g_x, "\n")
cat("Standard Deviation of g*(x):", sd_g_star_x, "\n")

#3
#3.3
# Given values
L <- 1  # Length of the pendulum in meters
mu_T <- 5.40  # Mean of T
sigma_T <- 0.12  # Standard deviation of T

# Function to calculate g
g <- function(T, L) {
  return (4 * pi^2 * L / T^2)
}

# Calculate the mean and standard deviation of g
hat_g_mean <- g(mu_T, L)
hat_g_std_dev <- abs(-8 * pi^2 * L / mu_T^3) * sigma_T

hat_g_mean
hat_g_std_dev

dev.off()