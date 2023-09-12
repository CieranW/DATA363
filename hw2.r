# Creates a pdf
pdf("hw2.pdf")

# Data for HW2
mosquitoes <- read.csv("http://math.arizona.edu/~jwatkins/mosquitoes.csv")

# 3.1. Summary of the data provided
summary(mosquitoes)

# 3.2. Plots data in a boxplot format with both types of mosquitoes present in the form of two separate box plots
boxplot(mosquitoes)

# 3.3. Empirical survival function of the mosquito data
attach(mosquitoes)

# Sort the survival times by type
wildtype <- sort(mosquitoes$wildtype)
transgenic <- sort(mosquitoes$transgenic)

# Each plot() is dedicated to one type of mosquito. par(new = TRUE) allows for both data plots to be combined into one
# Wildtype in blue, Transgenic in red
plot(wildtype, seq_along(wildtype) / length(wildtype), type = "p", xlab = "Time in Days", ylab = "Empirical Survival Probability", xlim = c(0, 55), ylim = c(0, 1), pch = 16, col = "blue")
par(new = TRUE)
plot(transgenic, seq_along(transgenic) / length(transgenic), type = "p", xlab = "Time in Days", ylab = "Empirical Survival Probability", xlim = c(0, 55), ylim = c(0, 1), pch = 16, col = "red")

# Legend to identify what's what
legend("topleft", c("Wildtype", "Transgenic"), fill = c("blue", "red"))

# 3.4. Q-Q plot of the two types of mosquitoes. Includes median and first/third quartiles.
# Q-Q plot with wildtype on x and transgenic on y
wildtype <- sort(wildtype)
transgenic <- sort(transgenic)
qqplot(wildtype, transgenic, xlim = c(0, 55), ylim = c(0, 55))
# Best fit line
abline(a = 0, b = 1)
# Median, and first and third quantiles. Marked in red
q <- c(0.25, 0.50, 0.75)
points(quantile(wildtype, q), quantile(transgenic, q), col = "red", pch = 19)

# Closes and saves pdf
dev.off()
