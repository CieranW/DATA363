# HW 9 - Expected Value
pdf("hw9.pdf")
# 2.2 - Graph
# Data for x, f(x), CDF, and SF
x <- c(1, 2, 3, 4, 5, 6)
x_SF <- c(1.0, 0.9, 0.7, 0.4, 0.1, 0)
y_SF <- c(0.8, 0.6, 0.4, 0.2, 0, 0)

# Create the plot
plot(x, x_SF, type = "b", pch = 19, col = "blue", ylim = c(0, 1.2), xlab = "x", ylab = "Probability")
lines(x, x_SF, type = "b", pch = 19, col = "green")
lines(x, y_SF, type = "b", pch = 19, col = "red")

# Add legend
legend("topright", legend = c("x SF", "y SF"), col = c("green", "red"), pch = 19)

# Add grid
grid()

# 3 - R Practice
# 3.4 - Simulation
set.seed(1234)
rpareto <- function(n, alpha, beta) {
    alpha / ((1 - runif(n))^(1 / beta))
}
x <- rpareto(1000, 1, 4)
mean(x)
var(x)

dev.off()
