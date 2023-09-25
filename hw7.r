# 2 Alzheimer's Analysis
# 2.1 - Two Way Table
population_size <- 10000
# Given probabilities
prob_age <- c(0.87, 0.07, 0.042, 0.018) # Probabilities for each age group
prob_alzheimer <- 0.0184 # Probability of having Alzheimer's
prob_age_given_alzheimer <- c(0.03, 0.16, 0.44, 0.37) # Conditional probabilities of each age group given Alzheimer's

# Calculate actual counts for each group
count_age <- round(prob_age * population_size)
count_alzheimer <- round(prob_alzheimer * population_size)
count_age_given_alzheimer <- round(prob_age_given_alzheimer * count_alzheimer)

# Calculate the probability of each cell
prob_age_and_alzheimer <- count_age_given_alzheimer / population_size
prob_age_and_no_alzheimer <- (count_age - count_age_given_alzheimer) / population_size

# Create a matrix for the two-way table
two_way_table <- matrix(c(count_age_given_alzheimer, count_age - count_age_given_alzheimer, prob_age_and_alzheimer, prob_age_and_no_alzheimer), nrow = 2, ncol = 4, byrow = TRUE)

# Add row and column names
rownames(two_way_table) <- c("Alzheimer's", "No Alzheimer's")
colnames(two_way_table) <- c("Below 65", "65 to 74", "75 to 84", "85 or Older")

# Display the two-way table
print(two_way_table)

# 2.2 - Conditional Probability
# Calculating P(Alz|Age) for each age group
prevalence_alz_given_age <- two_way_table[1, ] / rowSums(two_way_table)

# Print the prevalence for each age group
cat("Prevalence of Alzheimer's for each age group:\n")
print(prevalence_alz_given_age)


# 3 Dengue problem
# Universal variables
false_pos <- 0.1
false_neg <- 0.01
true_pos <- 1 - false_neg

# 3.1 - True Positive Rate
# Print the probability of a positive test given dengue
cat("Probability of a positive test given dengue:", round(true_pos, 2), "\n")

# 3.2 - Total Positive Tests
# Given values
p_has_dengue <- 0.02

# Fraction of the population testing positive
fraction_testing_positive <- false_pos * (1 - p_has_dengue) + true_pos * p_has_dengue

# Print the fraction of the population testing positive
cat("Fraction of the population testing positive:", round(fraction_testing_positive, 4), "\n")

# 3.3 - Have Dengue Given a Positive Test. Use P{Has Dengue|Test+}
calculate_probability_has_dengue_given_test_positive <- function(TPR, TNR, prevalence_dengue) {
    # Calculate False Positive Rate (FPR)
    FPR <- 1 - TNR

    # Calculate P(Test+)
    P_Test_positive <- (TPR * prevalence_dengue) + (FPR * (1 - prevalence_dengue))

    # Calculate P(has dengue|Test+)
    P_has_dengue_given_Test_positive <- (TPR * prevalence_dengue) / P_Test_positive

    return(P_has_dengue_given_Test_positive)
}

# Given values
TPR <- 0.99 # True Positive Rate
TNR <- 0.9 # True Negative Rate
prevalence_dengue <- 0.02 # Prevalence of dengue in the population

# Calculate P(has dengue|Test+)
P_has_dengue_given_Test_positive <- calculate_probability_has_dengue_given_test_positive(TPR, TNR, prevalence_dengue)

# Print the result
cat("Probability that an individual has dengue given a positive test:", round(P_has_dengue_given_Test_positive, 4))
