pdf("hw4.pdf")

# 2 - Practice
semesters <- c(1, 2, 3, 4, 6)
mistakes <- c(8, 7, 4, 4, 1)
model <- lm(mistakes ~ semesters)
# To verify calculations later on
s2_data <- var(mistakes)
s2_fit <- var(predict(model))
s2_residual <- var(residuals(model))

data.frame("s^2 data" = s2_data, "s^2 fit" = s2_fit, "s^2 residual" = s2_residual)

# Plotting the above to see if there's any visual differences. Aside from one point as per the above commands, no change in the lines. Was not worth the effort
x_min <- 1
x_max <- 6
y_min <- 1
y_max <- 8

plot(s2_data, xlim = c(x_min, x_max), ylim = c(y_min, y_max))
abline(model, col = "red")

plot(s2_fit, xlim = c(x_min, x_max), ylim = c(y_min, y_max))
abline(model, col = "green")

plot(s2_residual, xlim = c(x_min, x_max), ylim = c(y_min, y_max))
abline(model, col = "blue")

# Actual data calculations for the various s^2 stuff.
s_sem <- sd(semesters)
s_mis <- sd(mistakes)
corr_sm <- cor(semesters, mistakes)
data.frame("sd_semester" = s_sem, "sd_mistakes" = s_mis, "correlation" = corr_sm)

s2_data <- s_mis^2 # s^2 data
s2_fit <- corr_sm^2 * s_mis^2 # s^2 fit
s2_residual <- s_mis^2 - (corr_sm^2 * s_mis^2) # s^2 residual

data.frame("s^2 data" = s2_data, "s^2 fit" = s2_fit, "s^2 residual" = s2_residual)

# 3 R practice
library("MASS")
data(mammals)
# 3.1 Data summary
mammals
summary(mammals)

# 3.2 Plotting
# Compute the correlation coefficient
correlation_coefficient <- cor(mammals$body, mammals$brain)

# Round the correlation coefficient
rounded_correlation <- round(correlation_coefficient, digits = 2)

# Create the scatter plot with the regression line
plot(mammals)
abline(lm(mammals$brain ~ mammals$body), col = "red")

# Add the correlation coefficient to the plot
text(x = 5000, y = 4000, labels = paste("r =", rounded_correlation))


# 3.4 R line and equation
rline <- lm(mammals)
intercept <- coef(rline)[1]
slope <- coef(rline)[2]
regression_equation <- paste("y =", round(slope, 3), "x +", round(intercept, 3))
rline
print(regression_equation)

# 3.5 Average? Double body size
brain <- (mammals$body * 2) * slope + intercept
brain
new_brain_avg <- mean(brain)
avg_dif <- mean(brain) - mean(mammals$brain)
data.frame("New average" = new_brain_avg, "Old average" = mean(mammals$brain), "Average difference" = avg_dif)

# 3.6 Analyzing data
mammals$residuals <- residuals(rline)
mammals_ordered <- mammals[order(mammals$residuals), ]
mammals_ordered

large_brain <- mammals_ordered[mammals_ordered$residuals > 60, ]
small_brain <- mammals_ordered[mammals_ordered$residuals < -150, ]

large <- large_brain$Animal
small <- small_brain$Animal
# # # Print the mammal species with unusually large brain sizes (positive residuals)
# cat("Mammal species with unusually large brain sizes:\n")
# print(large_brain)

# # # Print the mammal species with unusually small brain sizes (negative residuals)
# cat("\nMammal species with unusually small brain sizes:\n")
# print(small_brain)

# data.frame("Unusually Large Brain" = large_brain, "Unusually Small Brain" = small_brain)

# Extract the residuals from the dataset
residuals <- mammals_ordered$residuals

# Create a new data frame with just the residuals column
residuals_df <- data.frame(residuals)

# Order the new data frame by residuals in ascending order
residuals_df_ordered <- residuals_df[order(residuals_df$residuals), ]

# Print the ordered data frame
print(residuals_df_ordered)

# Create a new data frame with only the residuals column
residuals_only_df <- mammals_ordered[, "residuals", drop = FALSE]

# Print the new data frame
print(residuals_only_df)

dev.off()
