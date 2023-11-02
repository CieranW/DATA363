# HW 10 - Examples of Mass and Density Functions
pdf("hw10.pdf")

# 1 - Plotting binomial distribution for different probabilities
# Plot binomial distribution for p = 0.25, 0.5, 0.75
p_values <- c(0.25, 0.5, 0.75)
for (p in p_values) {
    plot(c(0:12), dbinom(c(0:12), 12, p), type = "h", main = paste("Binomial Distribution (p =", p, ")"))
}

# 2 - Practice
# 2.1 - Negative Binomial
# Define the parameters
n <- 5
p <- 5 / 8

# Calculate the probabilities for x = 0, 1, ..., 12
probs <- dnbinom(0:12, size = n, prob = p)

# Create a bar plot of the probabilities
barplot(probs,
    names.arg = 0:12, xlab = "x", ylab = "P(X = x)",
    main = "Negative Binomial Probability Distribution",
    col = "blue", ylim = c(0, max(probs) * 1.2)
)

# 2.2 - Beta Random Variables
# Set the seed
set.seed(11)

# Simulate 1000 independent beta random variables
alpha <- 2
beta <- 4
n <- 1000
x <- rbeta(n, alpha, beta)

# Find the mean and variance of the sample
mean_x <- mean(x)
var_x <- var(x)

# Compare to the actual values
actual_mean <- alpha / (alpha + beta)
actual_var <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))

cat("Sample mean:", mean_x, "\n")
cat("Sample variance:", var_x, "\n")
cat("Actual mean:", actual_mean, "\n")
cat("Actual variance:", actual_var, "\n")

# 3 - More R Practice
# 3.1 - Standard Normal
p <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.85, 0.99)
q <- qnorm(p, 0, 1)
data <- data.frame(p, z = round(q, 3))

# Plotting using base R plot functions
plot(NA, xlim = c(-4, 4), ylim = c(0, 1), xlab = "z", ylab = "P(Z ≤ z)")
lines(data$z, data$p, col = "blue")
points(data$z, data$p, pch = 21, col = "red", bg = "red")
segments(data$z, data$p, rep(-0.1, length(p)), lty = 2)
title("Standard Normal Distribution Function")

# 3.2 - Chi-Square
# Degrees of freedom for chi-square
df <- 4

# Generate x values for the density function
x <- seq(0, 20, length = 1000)
density <- dchisq(x, df)

# Data for specific x values
probabilities <- c(0.10, 0.05, 0.01)
x_values <- qchisq(1 - probabilities, df) # 1 - P{X > x}
x_values_rounded <- round(x_values, 3)

# Create a plot
plot(x, density, type = "l", col = "blue", xlab = "x", ylab = "Density function f(X)", xlim = c(0, max(x_values_rounded) + 2))

# Add text for specific x values
text(x_values_rounded, dchisq(x_values_rounded, df), labels = paste("x =", x_values_rounded), col = "red", pos = 3)

# Add a title
title(main = "Density Function for Chi-Square(4) with Specific x Values Marked")

# Add a legend
legend("topright", legend = paste("x =", x_values_rounded), col = "red", lty = 1, cex = 0.8)

# 3.3 - Simulation
# Load the actuar package for rhyper function
library(actuar)

# Set the seed for reproducibility
set.seed(11)

# Simulate 1000 times drawing 10 marbles from the urn
num_simulations <- 1000
num_marbles_drawn <- 10
black_marbles_in_urn <- 5
white_marbles_in_urn <- 15

# Simulate the number of white marbles drawn in each simulation
white_marbles_drawn <- rhyper(num_simulations, white_marbles_in_urn, black_marbles_in_urn, num_marbles_drawn)

# Calculate mean and standard deviation
mean_white_marbles <- mean(white_marbles_drawn)
sd_white_marbles <- sd(white_marbles_drawn)

# Display the mean and standard deviation
cat("Mean number of white marbles:", mean_white_marbles, "\n")
cat("Standard deviation of white marbles:", sd_white_marbles, "\n")

# Save the plot to a PDF
dev.off()
