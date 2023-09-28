# HW 8 - Random Variables and Distribution Functions
# 3
# A Gumbel random variable X has the distribution function F_x(x) = exp(-e^-x)
pdf("hw8.pdf")
# 3.1 - Calculate the First Quartile (Solve for F_x(x) = 0.25)

# e^(-e^-x) = 0.25
# -e^-x = ln(0.25)
# e^-x = -ln(0.25)
# x = ln(ln(4))
# x ≈ 0.3266
x <- log(log(4))
print(round(x, 4))


# 3.2 - Distribution Function Graph
# Plot graph of F_x(x), indicating the first and third quartile, and median as points on the graph

# Define the cumulative distribution function F_X(x)
F_X <- function(x) exp(-exp(-x))

# Generate a sequence of x values
x_values <- seq(-5, 5, length.out = 1000)

# Compute the corresponding cumulative probabilities
F_values <- F_X(x_values)

# Find the quartiles
first_quartile <- qexp(0.25)
median <- qexp(0.5)
third_quartile <- qexp(0.75)

# Plot the cumulative distribution function
plot(x_values, F_values, type = "l", col = "blue", xlab = "x", ylab = "F_X(x)", main = "Cumulative Distribution Function")

# Add points for quartiles
points(first_quartile, F_X(first_quartile), col = "red", pch = 16, cex = 1.5)
points(median, F_X(median), col = "green", pch = 16, cex = 1.5)
points(third_quartile, F_X(third_quartile), col = "purple", pch = 16, cex = 1.5)

# Add legend
legend("topright", legend = c("First Quartile", "Median", "Third Quartile"), col = c("red", "green", "purple"), pch = 16, cex = 0.8)


# 3.3 - Probability Transform
# Determine the probability transform for this distribution, writing x as a function of u

# F_x^-1(u) = x(u)
# F_x(x) = u
# exp(-exp(-x)) = u
# -exp(-x) = ln(u)
# exp(-x) = -ln(u)
# -x = ln(-ln(u))
# x = -ln(-ln(u))


# 3.4 - Simulating Gumbel Random Variables
# Use the value found in 3.3 to create 80 samples with this distribution. Plot the graph that shows the true cumulative distribution function (in black) along with the emperical CDF of the data (different color)
# Load necessary libraries
library(ggplot2)

# Probability transform function
x_transform <- function(u) -log(-log(u))

# Generate 80 samples from the distribution using the probability transform
set.seed(123) # for reproducibility
samples <- x_transform(runif(80))

# Compute the empirical CDF
empirical_cdf <- ecdf(samples)

# Create a sequence of x values for plotting the true CDF
x_values <- seq(min(samples), max(samples), length.out = 1000)

# Compute the true CDF values
true_cdf <- exp(-exp(-x_values))

# Create a data frame for plotting
df <- data.frame(x_values, true_cdf)

# Plot the true CDF and the empirical CDF
ggplot(data = df, aes(x = x_values)) +
    geom_line(aes(y = true_cdf), color = "black", linewidth = 1) + # True CDF
    geom_step(stat = "ecdf", aes(color = "Empirical CDF"), linewidth = 1) + # Empirical CDF
    theme_minimal() +
    labs(
        title = "True and Empirical Cumulative Distribution Functions",
        x = "x", y = "CDF"
    ) +
    scale_color_manual(values = c("Empirical CDF" = "blue"))


# 3.5 - Density Function
# Determine the probability density function f_x(x) for the cumulative distribution function F_x(x)
# Define the probability density function f_X(x)
pdf_fx <- function(x) exp(-x) * exp(-exp(-x))

# Generate a sequence of x values
x_values <- seq(-5, 5, length.out = 1000)

# Compute the density values
density_values <- pdf_fx(x_values)

# Create a data frame for plotting
df <- data.frame(x = x_values, f_x = density_values)

# Plot the probability density function
ggplot(data = df, aes(x = x, y = f_x)) +
    geom_line(color = "blue", linewidth = 1) +
    theme_minimal() +
    labs(
        title = "Probability Density Function f_X(x)",
        x = "x", y = "f_X(x)"
    )


dev.off()
