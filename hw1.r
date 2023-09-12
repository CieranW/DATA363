# Create a bar chart with two bars, three segments
trees <- matrix(c(128, 119, 6, 78, 0, 4), ncol = 2)
colnames(trees) <- c("Flores", "Sumba")
rownames(trees) <- c("AA", "AE", "EE")
barplot(trees, legend = rownames(trees))

# Number stuff, first one is sequence, second is repeat
seq(25,1, -1)
rep(0,25)

n <- 1:25
samp <- sample(n, 30, replace = TRUE)
(A <- matrix(samp, ncol = 10))
A[1,]
A[,2:4]

data<-matrix(samp,nrow=2)
barplot(data)

x<-seq(200, 3000, 34)
curve(dnorm(x), -3,3)