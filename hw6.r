# 3 R Practice

# 3.1 Probability of recapturing 1 tagged tortoise
# Total number of tortoises in the population
total_population <- 300

# Number of tagged tortoises in the population
num_tagged <- 50

# Number of tortoises captured in the second capture
num_captured_second_capture <- 15

# Number of tagged tortoises among those captured in the second capture
num_tagged_in_second_capture <- 1

# Calculate the probability using the hypergeometric distribution
probability <- dhyper(num_tagged_in_second_capture, m = num_tagged, n = total_population - num_tagged, k = num_captured_second_capture)

# Round the probability to 4 decimal places
probability <- round(probability, 4)

# Print the probability
cat("The probability of exactly one of the 15 tortoises being tagged is:", probability, "\n")


# 3.2 Probability distribution
# Calculate the probabilities for each value of x
probabilities <- numeric(16) # Initialize an empty vector of length 16
for (x in 0:15) {
    probabilities[x + 1] <- dhyper(x, m = num_tagged, n = total_population - num_tagged, k = num_captured_second_capture)
}

# Round the probabilities to 4 decimal places
probabilities <- round(probabilities, 4)

# Create a data frame with probabilities
probabilities_table <- data.frame(x = 0:15, Probability = probabilities)

# Print the probabilities table
print(probabilities_table)

# Check that the sum of probabilities is 1
sum_of_probabilities <- sum(probabilities)
cat("Sum of probabilities:", sum_of_probabilities, "\n")


# 3.3 Simulation
# Number of simulations
num_simulations <- 10000

# Initialize a vector to store the outcomes
outcomes <- vector("list", length = num_simulations)

# Perform simulations
for (i in 1:num_simulations) {
    # Simulate the second capture
    captured_tortoises <- sample(1:total_population, num_captured_second_capture)

    # Count the number of tagged tortoises among the captured
    num_tagged_captured <- sum(captured_tortoises <= num_tagged)

    # Store the outcome in the outcomes vector
    outcomes[[i]] <- num_tagged_captured
}

# Create a table of outcomes
outcome_table <- table(unlist(outcomes))

# Create a data frame with labels
result_df <- data.frame("Tagged Tortoise Count" = as.numeric(names(outcome_table)), "Number of Simulations" = as.numeric(outcome_table))

# Print the table of outcomes
print(result_df)
