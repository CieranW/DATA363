pdf("hw2_2.pdf")

lines <- c(0.7, -0.1, 1.3, -2.3, -0.3, -0.7, -1.0, 2.7, 0.6, 0.1)
scaled <- lines * -1.2
boxplot(lines, ylim = c(-5, 5), col = "red")
boxplot(scaled, col = "blue", ylim = c(-5, 5))
dev.off()
