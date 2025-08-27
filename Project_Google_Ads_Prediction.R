
# Phase 3: Prediction


library(tidyverse)
library(readr)

# Import data
Flat_Table <- read_csv("Google_Ads_Flat_Table.csv")

# Recommendation 1: Immediate Action impact (to be taken within 1 to 2 weeks)

# 1. Prepare the data for the model

model_data <- Flat_Table %>%
  filter(product_category == "Electronics" & source_medium == "Google / CPC" & ad_spend > 0)

# 2. Build the linear regression model
# We want to predict 'revenue' based on 'ad_spend'
revenue_model <- lm(revenue ~ ad_spend, data = model_data)

# Show the result
summary(revenue_model)

# 3. Prepare the data for prediction
# Creation of the values
current_budget <- sum(model_data$ad_spend)
double_budget <- current_budget * 2

# Create a new table with the future budget
future_data <- data.frame(ad_spend = c(current_budget, double_budget))

# 4. Make predictions
predictions <- predict(revenue_model, newdata = future_data)

# Show result
print(paste("Current budget:", "$",round(current_budget, 2), "-> Predicted income:", "$", round(predictions[1], 2)))
print(paste("Bouble Budget:", "$",round(double_budget, 2), "-> Predicted income:", "$", round(predictions[2], 2)))


# 5. Create graphic for the prediction of the Electronics
ggplot(model_data, aes(x = ad_spend, y = revenue)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(
    title = "Relationship between Advertising Expenditure and Revenue",
    subtitle = "For the Electronics category on Google/CPC",
    x = "Advertising Expenditure ($)",
    y = "Revenue ($)") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  )


# Recommendation 2: Optimization Action Impact (to be launched within 1 to 2 months)


# 1. Prepare the data for the model

model_house_data <- Flat_Table %>%
  filter(product_category == "House" & ad_spend > 0)

# 2. Build the linear regression model for the House category
house_model <- lm(revenue ~ ad_spend, data = model_house_data)

# Show result
summary(house_model)

# 3. We're going to train the model
# Let's create scenarios with hypothetical advertising budgets
future_data_home <- data.frame(ad_spend = c(5000, 10000, 20000))

# 4. Let's predict
home_predictions <- predict(house_model, newdata = future_data_home)

# Show result
print("Revenue predictions for the 'Home' category based on the budget invested:")
print(paste("For a budget of", "$",future_data_home$ad_spend[1], "-> Predicted revenue:", "$", round(home_predictions[1], 2)))
print(paste("For a budget of", "$",future_data_home$ad_spend[2], "-> Predicted revenue:", "$", round(home_predictions[2], 2)))
print(paste("For a budget of", "$",future_data_home$ad_spend[3], "-> Predicted revenue:", "$", round(home_predictions[3], 2)))


# 5. Create graphic for the predition of the House
ggplot(model_house_data, aes(x = ad_spend, y = revenue)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relationship between Expenses and Income for the 'Home' Category",
    subtitle = "The model predicts a negative return on investment, justifying a review of the strategy before investing.",
    x = "Advertising Expenditures ($)",
    y = "Revenues ($)") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  )
  

# Recommendation 3: Preventive Action impact (2-3 month strategy)


# 1. Prepare the data for the model

tiktok_model_data <- Flat_Table %>%
  filter(str_detect(source_medium, "TikTok"))

# 2. Build the linear regression model TikTok
tiktok_model <- lm(revenue ~ clicks, data = tiktok_model_data)

# Show result
summary(tiktok_model)

# 3. Create graphic for the model
ggplot(tiktok_model_data, aes(x = clicks, y = revenue)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Predictive Impact of TikTok Activity on Revenue",
    subtitle = "The analysis shows no evidence that TikTok clicks generate revenue, invalidating investment in this channel.",
    x = "Clicks",
    y = "Revenue ($)" ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  )

# Done :) 







