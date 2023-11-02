pdf("hw11.pdf")
# 1
n <- c(1:100)
y <- abs(rcauchy(1000))
s <- cumsum(y)
plot(s / n, xlab = "n", ylim = c(0, 10), type = "l")

n <- c(1:100)
y <- abs(rcauchy(1000))
s <- cumsum(y)
plot(s / n, xlab = "n", ylim = c(0, 10), type = "l")

n <- c(1:100)
y <- abs(rcauchy(1000))
s <- cumsum(y)
plot(s / n, xlab = "n", ylim = c(0, 10), type = "l")

n <- c(1:100)
y <- abs(rcauchy(1000))
s <- cumsum(y)
plot(s / n, xlab = "n", ylim = c(0, 10), type = "l")

n <- c(1:100)
y <- abs(rcauchy(1000))
s <- cumsum(y)
plot(s / n, xlab = "n", ylim = c(0, 10), type = "l")

n <- c(1:100)
y <- abs(rcauchy(1000))
s <- cumsum(y)
plot(s / n, xlab = "n", ylim = c(0, 10), type = "l")

n <- c(1:100)
y <- abs(rcauchy(1000))
s <- cumsum(y)
plot(s / n, xlab = "n", ylim = c(0, 10), type = "l")

n <- c(1:100)
y <- abs(rcauchy(1000))
s <- cumsum(y)
plot(s / n, xlab = "n", ylim = c(0, 10), type = "l")

# 3
# 3.2
# Number of estimates
num_estimates <- 1000

# Sample size for each estimate
sample_size <- 240

# Initialize a vector to store the estimates
estimates <- numeric(num_estimates)

# Define the function g(x)
g <- function(x) {
    return(1 / sin(exp(x) - 1))
}

# Perform Monte Carlo integration for each estimate
for (i in 1:num_estimates) {
    # Generate a sample of uniform random variables
    sample_points <- runif(sample_size, min = 0, max = 9 / 16)

    # Evaluate the function g(x) at the sample points and calculate the mean
    estimate <- mean(g(sample_points))

    # Store the estimate
    estimates[i] <- estimate
}

# Calculate the mean and standard deviation of the estimates
mean_estimate <- mean(estimates)
sd_estimate <- sd(estimates)

# Display the mean and standard deviation
print("Mean estimate:")
print(mean_estimate)

print("Standard deviation of estimates:")
print(sd_estimate)

# 3.6
# Function g(x)
g <- function(x) 1 / sin(exp(x) - 1)

# Proposal density f_X(x) and its inverse
f_X <- function(x) 2 / (3 * sqrt(x))
w <- function(x) 3 * sqrt(x) / 2

# Number of samples
num_samples <- 1000

# Generate samples from the proposal density f_X(x)
samples <- runif(num_samples, 0, 9 / 16)

# Calculate importance sampling estimates
importance_estimates <- g(samples) / w(samples)

# Calculate mean and standard deviation of the estimates
mean_estimate <- mean(importance_estimates)
sd_estimate <- sd(importance_estimates)

# Print the results
cat("Mean Estimate:", mean_estimate, "\n")
cat("Standard Deviation of Estimates:", sd_estimate, "\n")

dev.off()
