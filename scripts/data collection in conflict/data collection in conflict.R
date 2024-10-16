# MENA MEL Workshop
# Data collection in conflict

# Randomized response ---- 

# Set the probability for the randomization step
# p = probability of answering the sensitive question truthfully
# 1-p = probability of answering "Yes" regardless of the truth

p <- 0.7  # You can adjust the probability

# Create a function to apply the randomized response technique
randomized_response <- function(truth, p) {
  # Generate a random number for each respondent
  random_num <- runif(length(truth))
  
  # If the random number is less than p, they answer truthfully
  # Otherwise, they answer "Yes" regardless of the truth
  response <- ifelse(random_num < p, truth, 1)
  
  return(response)
}

# Simulate data
# Assume we have a vector of true responses (1 = Yes, 0 = No)
set.seed(123)  # Set seed for reproducibility
n <- 100  # Number of respondents
true_responses <- rbinom(n, 1, 0.4)  # 40% of respondents say Yes

# Apply the randomized response technique
reported_responses <- randomized_response(true_responses, p)

# Let's estimate the proportion of 'Yes' responses in the population
# The formula for estimation based on the randomized response method is:
# Estimated True Proportion = (Observed Proportion - (1 - p)) / p

observed_proportion <- mean(reported_responses)
estimated_true_proportion <- (observed_proportion - (1 - p)) / p

# Print the results
cat("Observed proportion of 'Yes' responses:", observed_proportion, "\n")
cat("Estimated true proportion of 'Yes' responses:", estimated_true_proportion, "\n")




# Endorsement ---- 

# Set random seed for reproducibility
set.seed(123)

# Define a sample of respondents
n <- 100  # Adjust the sample size as needed

# Simulate respondent IDs
respondents <- data.frame(ID = 1:n)

# Define experimental conditions (endorsement)
endorsements <- c("Politician A", "Politician B", "No Endorsement")

# Randomly assign endorsements to respondents
respondents$endorsement <- sample(endorsements, n, replace = TRUE)

# Simulate responses (1 = support, 0 = no support)
# For simplicity, we assume that endorsements influence support probability
respondents$response <- ifelse(respondents$endorsement == "Politician A", rbinom(n, 1, 0.7),
                               ifelse(respondents$endorsement == "Politician B", rbinom(n, 1, 0.5), 
                                      rbinom(n, 1, 0.4)))

# View the first few rows of the data
head(respondents)


# Summarize support by endorsement condition
summary_by_endorsement <- respondents %>%
  group_by(endorsement) %>%
  summarise(support_rate = mean(response))

print(summary_by_endorsement)

# Run a logistic regression to assess the effect of endorsements on support
model <- glm(response ~ endorsement, data = respondents, family = binomial)

# View the model summary
summary(model)


# Predict probabilities from the model
predicted_probs <- predict(model, type = "response")

# Add the predicted probabilities to the data
respondents$predicted_probs <- predicted_probs

# View the first few rows
head(respondents)

# Summary of predicted probabilities by endorsement
predicted_summary <- respondents %>%
  group_by(endorsement) %>%
  summarise(mean_pred_prob = mean(predicted_probs))

print(predicted_summary)





# List ---- 


# Sample data generation: Control group and Treatment group
set.seed(123) # For reproducibility
n <- 200  # Sample size

# Generate control group data (non-sensitive items)
control_group <- data.frame(
  item1 = rbinom(n, 1, 0.5),
  item2 = rbinom(n, 1, 0.4),
  item3 = rbinom(n, 1, 0.3),
  sensitive_item=NA,
  group = rep("control", n)
)

head(control_group)

# Generate treatment group data (includes sensitive item)
treatment_group <- data.frame(
  item1 = rbinom(n, 1, 0.5),
  item2 = rbinom(n, 1, 0.4),
  item3 = rbinom(n, 1, 0.3),
  sensitive_item = rbinom(n, 1, 0.2),  # Sensitive item
  group = rep("treatment", n)
)

head(treatment_group)

# Combine the datasets

lst <- rbind(control_group, treatment_group)

head(lst)

# Calculate total number of items selected in each group

lst$selected_items <- rowSums(lst[,1:3]) # Control group: sum non-sensitive items

# For the treatment group, include the sensitive item
lst$selected_items <- ifelse(lst$group == "treatment", rowSums(lst[,1:4]), lst$selected_items)


# Calculate mean number of items selected in each group
aggregate(selected_items ~ group, lst, mean)

# Difference in means is the estimate of the proportion endorsing the sensitive item
t.test(selected_items ~ group, data = lst)


