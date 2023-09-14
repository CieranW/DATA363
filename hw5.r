pdf("hw5.pdf")

# 1
# Population size
total_population <- 60 # 20 women + 40 men

# Number of women in the population
num_women <- 20

# Create a function to generate a simple random sample and count women
generate_sample <- function(sample_size) {
    sample <- sample(c(rep("woman", num_women), rep("man", total_population - num_women)), size = sample_size)
    num_women_in_sample <- sum(sample == "woman")
    return(num_women_in_sample)
}

# Number of samples to generate
num_samples <- 10

# Sample size for each sample
sample_size <- 24

# Initialize a vector to store the number of women in each sample
women_in_samples <- numeric(num_samples)

# Generate samples and count women
for (i in 1:num_samples) {
    women_in_samples[i] <- generate_sample(sample_size)
}

# Calculate mean
mean_women_in_samples <- mean(women_in_samples)

# Calculate standard deviation
sd_women_in_samples <- sd(women_in_samples)

# Expected number of women in a sample of size 24
expected_mean <- num_women * (sample_size / total_population)

# Print results
cat("Mean number of women in samples:", mean_women_in_samples, "\n")
cat("Standard deviation of women in samples:", sd_women_in_samples, "\n")
cat("Expected mean based on population proportions:", expected_mean, "\n")

# Print the number of women in each sample
cat("Number of women in each of the 10 samples:\n")
print(women_in_samples)

# 3.1
# Set the seed for reproducibility
set.seed(123)

# Define the number of smokers and non-smokers to select
num_smokers_to_select <- 10
num_nonsmokers_to_select <- 10

# Create a vector of labels for smokers and nonsmokers
smokers <- paste("Smoker", 1:30)
nonsmokers <- paste("NonSmoker", 1:70)

# Perform stratified random sampling
selected_smokers <- sample(smokers, num_smokers_to_select)
selected_nonsmokers <- sample(nonsmokers, num_nonsmokers_to_select)

# Combine the selected subjects
selected_subjects <- c(selected_smokers, selected_nonsmokers)

# Extract the numeric part from the labels
numeric_part <- as.integer(gsub("\\D", "", selected_subjects))

# Sort based on the numeric part
selected_subjects <- selected_subjects[order(numeric_part)]

# Print the sorted selected subjects
print(selected_subjects)

# 3.2
# Set the random seed for reproducibility
set.seed(123)

# Create a vector of 30 smokers (assuming unique identifiers for each smoker)
population <- 1:30

# Number of samples to draw
num_samples <- 12

# Sample size for each draw
sample_size <- 10

# Initialize a matrix to record the samples
sample_matrix <- matrix(0, nrow = num_samples, ncol = sample_size)

# Loop through each sample
for (i in 1:num_samples) {
    # Randomly sample 10 smokers without replacement
    sample_matrix[i, ] <- sample(population, size = sample_size)
}

# Count how many times each smoker appears in the samples
smoker_counts <- table(sample_matrix)

# Print the results
print(smoker_counts)

# 3.3
mean(smoker_counts)
sd(smoker_counts)

# 3.4
# Convert the table to a numeric vector
smoker_counts_vector <- as.numeric(smoker_counts)

# Plot a histogram
hist(smoker_counts_vector, main = "Histogram of Smoker Counts", xlab = "Number of Appearances", ylab = "Frequency", col = "lightgreen")

dev.off()
