pdf("hw3.pdf")

# 1 Scatter Plot and Correlation calculation (by hand)
femur <- c(38, 56, 59, 64, 74)
humerus <- c(41, 63, 70, 72, 84)
correlation_1 <- cor(femur, humerus)
plot(femur, humerus, xlab = "Femur", ylab = "Humerus")

# 2 Finding correlation value to find linear regression equation by hand
fl <- c(5.8, 7, 7.5, 8.1, 8, 9)
h <- c(64, 65, 66, 67, 68, 69)
print(cor(fl, h))

# 3 More Scatter Plots
snowcover <- c(6.6, 5.9, 6.8, 7.7, 7.9, 7.8, 8.1, 16.6, 18.2, 15.2, 16.2, 17.1, 17.3, 18.1, 26.6, 27.1, 27.5, 28.4, 28.6, 29.6, 29.4)

windstress <- c(0.125, 0.160, 0.158, 0.155, 0.169, 0.173, 0.196, 0.111, 0.106, 0.143, 0.153, 0.155, 0.133, 0.130, 0.062, 0.051, 0.068, 0.055, 0.033, 0.029, 0.024)

regress_line <- lm(windstress ~ snowcover)

plot(snowcover, windstress, xlab = "Snowcover", ylab = "Windstress")
abline(regress_line, col = "red")
correlation <- cor(snowcover, windstress)
print(correlation)

# 3.4 Prediction
new_data <- c(12)
predictions <- predict(regress_line, new_data = data.frame(snowcover = 12))
data.frame(new_data, predictions)

snowcover_mean <- mean(snowcover)
windstress_mean <- mean(windstress)
snow_d <- snowcover - snowcover_mean
wind_d <- windstress - windstress_mean

slope_num <- sum(snow_d * wind_d)
slope_denom <- sum(snow_d^2)
slope <- slope_num / slope_denom
intercept <- windstress_mean - slope * snowcover_mean

# Test results to print
print(snowcover_mean)
print(windstress_mean)
print(slope)
print(intercept)

# 3.5 Residual, to check values
climate_residual <- resid(regress_line)
regress_line
climate_residual

dev.off()
