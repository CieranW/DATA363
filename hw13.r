#3 - R Practice
# 1/r1 + 1/r2 = 1/f
# r1 is distance from lens to actual object, r2 is distance from lens to image of object
# r1 measured independently 36 times, r2 measured independently 40 times
# mean r1 = 10 cm, mean r2 = 18 cm
# SD r1 = 0.1 cm, SD r2 = 0.5 cm

# 3.1 - Law of Large Numbers
# R1 is the distribution of sample means of the 36 measurements of r1
# Use LLN to determine mean and SD of R1

# By LLN: mean of R1 is roughly the same as mean of r1
# So mean of R1 = 10 cm
# By LLN: SD of R1 is roughly SD of r1 divided by sqrt(36)
# So SD of R1 = 0.1 cm / sqrt(36) = 0.0167 cm

#3.2 - Central Limit Theorem
# R2 is the sample mean of the 40 measurements of r2.
# Estimate using CLT, P{R2 < 17.9 cm}

# By CLT: R2 is approximately normal with mean = mean of r2 = 18 cm and SD = SD of r2 / sqrt(40)
# So SD of R2 = 0.5 cm / sqrt(40) = 0.079 cm
# P{R2 < 17.9 cm} = P{Z < (17.9 - 18) / 0.079} = P{Z < -1.2658} = 0.1028
pnorm(-1.2658)

#3.3 - Estimator
# For measurements r11, r12,..., r136, r21, r22,..., r240 use the original formula to estimate the focal length
# To be done by hand


#3.4 - Comparison
# Using the Delta Method, mean of the estimator = 6.4286 and SD of the estimator = 0.01221
# Use R to simulate this protocal 10,000 times using the rnorm function and compare the results to the Delta Method
# Find the mean and standard deviation

# Set parameters
n_simulations <- 10000
mean_r1 <- 10
sd_r1 <- 0.1
mean_r2 <- 18
sd_r2 <- 0.5

# Initialize a vector to store simulated focal length estimates
simulated_f_values <- numeric(n_simulations)

# Perform the simulation
for (i in 1:n_simulations) {
  # Simulate the values of r1 and r2
  r1_simulated <- rnorm(36, mean = mean_r1, sd = sd_r1)
  r2_simulated <- rnorm(40, mean = mean_r2, sd = sd_r2)
  
  # Calculate the focal length estimate using the simulated r1 and r2
  f_hat_simulated <- 1 / ((1 / mean(r1_simulated)) + (1 / mean(r2_simulated)))
  
  # Store the simulated focal length estimate in the vector
  simulated_f_values[i] <- f_hat_simulated
}

# Calculate mean and standard deviation of simulated estimates
mean_simulation <- mean(simulated_f_values)
sd_simulation <- sd(simulated_f_values)

# Print the results
cat("Simulation - Mean:", mean_simulation, "\n")
cat("Simulation - Standard Deviation:", sd_simulation, "\n")

