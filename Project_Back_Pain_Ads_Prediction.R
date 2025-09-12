

# Phase 7: Prediction

# Prediction 1: T-test for the ad campaign

# Step 1: Simulate the A/B test data
# (In reality, this data would come from your advertising platform)

# Daily CTR of the old ad (generic)
old_ad_ctr <- c(0.012, 0.011, 0.015, 0.013, 0.010) # Average around 1.2%

# Daily CTR of the new ad (empathetic)
new_ad_ctr <- c(0.018, 0.022, 0.019, 0.025, 0.021) # Average around 2.1%


# Step 2: Perform the T-test to compare the two groups
test_result <- t.test(new_ad_ctr, old_ad_ctr)


# Step 3: Show results
print(test_result)

# The p-value is 0.0005584 which is very good !



# Prediction 2: Linear Regression for content engagement 

# Step 1: Simulate the data
# (In reality, this data would come from your analytics platform, e.g., Google Analytics or social media insights)

# Create a dataframe to hold the data
content_data <- data.frame(
 
  content_type = factor(c(rep("Old_Content", 10), rep("New_Content", 10))),
  engagement_rate = c(
    rnorm(10, mean = 0.02, sd = 0.005), 
    rnorm(10, mean = 0.045, sd = 0.008) 
  )
)

# Step 2: Perform the linear regression
engagement_model <- lm(engagement_rate ~ content_type, data = content_data)

# Step 3: Display the summary of the model
summary(engagement_model)

# The p-value is 2.665e-08 which is Excellent !


# Step 4: Ensure the simulated data exists
content_data <- data.frame(
  content_type = factor(c(rep("Old_Content", 10), rep("New_Content", 10))),
  engagement_rate = c(
    rnorm(10, mean = 0.02, sd = 0.005),
    rnorm(10, mean = 0.045, sd = 0.008)
  )
)

# Step 5: Load libraries
library(scales)

# Step 6: Creation of the boxplot 
ggplot(data = content_data, aes(x = content_type, y = engagement_rate, fill = content_type)) +
  geom_boxplot(alpha = 0.7) + 
  geom_jitter(width = 0.1, alpha = 0.6) + 
  
  scale_fill_manual(values = c("Old_Content" = "grey", "New_Content" = "orange")) +
  
  labs(
    title = "Engagement Rate by Content Type",
    subtitle = "The new reassurance content is significantly more engaging",
    x = "Content Type",
    y = "Engagement Rate"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme_classic() +
  theme(legend.position = "none")




# Prediction 3: Clustering to identify high-value customers 

# Step 1: Simulate customer data
# (In reality, this data would come from your CRM or sales database)
set.seed(123)
customer_data <- data.frame(
  purchase_frequency = sample(1:20, 100, replace = TRUE),
  average_spend = rnorm(100, mean = 50, sd = 15)
)

# Step 2: Scale the data
customer_data_scaled <- scale(customer_data)

# Step 3: Perform k-means clustering
kmeans_result <- kmeans(customer_data_scaled, centers = 3, nstart = 25)

# Step 4: Add the cluster assignment back to the original data
customer_data$cluster <- as.factor(kmeans_result$cluster)


# Step 5: Visualize the clusters
library(ggplot2)
ggplot(customer_data, aes(x = purchase_frequency, y = average_spend, color = cluster)) +
  geom_point(size = 4) +
  labs(
    title = "Customer Segments based on Behavior",
    subtitle = "Identifying the high-value customer cluster",
    x = "Purchase Frequency (per year)",
    y = "Average Spend ($)"
  ) +
  theme_classic()













