

# Phase 2: Diagnostic Analysis

# Analysis of Hypothesis 1: The Offer is More Competitive

library(tidyverse)
library(readr)

# Import dataset
Flat_Table <- read_csv("Google_Ads_Flat_Table.csv")

# Step 1:

# Aggregation to calculate the average price per item
average_price_per_category <- Flat_Table %>%
  
  filter(revenue > 0 & quantity > 0) %>%
  
  group_by(product_category) %>%
  summarise(
    total_revenue = sum(revenue),
    total_quantity = sum(quantity)) %>%
  
  mutate(average_price_item = total_revenue / total_quantity) %>%
  arrange(desc(average_price_item))

# Show result
print(average_price_per_category)

# colors creation
colors_7 <- c("orange", "grey", "grey", "grey", "grey", "grey","grey")

# Create graphic
ggplot(average_price_per_category, aes(x = reorder(product_category, -average_price_item), y = average_price_item)) +
  geom_col(fill = colors_7) +
  labs(
    title = "Average Price per Item Sold: Electronics Have the Highest Price",
    subtitle = "Contrary to the assumption, the average price of an electronic item is the highest, disproving the idea of 'attractive prices'.",
    x = NULL,
    y = "Average Price per Item ($)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )

# Step 2:

# Aggregation of revenue during promotional periods
revenus_in_promo <- Flat_Table %>%
 
  filter(str_detect(tolower(campaign_name), "promo|solde|black|cyber")) %>%
  filter(revenue > 0) %>%
  
  group_by(product_category) %>%
  summarise(
    total_revenue = sum(revenue)) %>%
  
  arrange(desc(total_revenue))

# show result
print(revenus_in_promo)

# colors creation
colors_8 <- c("orange", "grey", "grey", "grey", "grey", "grey","grey")

# Create graphic for promotional sales
ggplot(revenus_in_promo, aes(x = reorder(product_category, -total_revenue), y = total_revenue)) +
  geom_col(fill = colors_8) +
  labs(
    title = "Revenue during sales periods: electronics accounts for almost all of it",
    subtitle = "The Electronics category dominates sales during key promotional campaigns.",
    x = NULL,
    y = "Promotional Sales ($)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )

# Analysis of Hypothesis 2: Targeted Advertising is More Effective


# Aggregation to calculate the Conversion Rate by category for Google/CPC
conversion_per_category <- Flat_Table %>%
  
  filter(source_medium == "Google / CPC") %>%
  
  group_by(product_category) %>%
  summarise(
    total_sessions = n_distinct(session_id),
    total_transactions = n_distinct(transaction_id[revenue > 0]) ) %>%
  mutate(conversion_rate = total_transactions / total_sessions) %>%
 
  filter(conversion_rate > 0) %>%
  arrange(desc(conversion_rate))

# Show result
print(conversion_per_category)

# colors creation
colors_9 <- c("orange", "grey", "grey", "grey", "grey", "grey")

# Create graphic for conversion rate
ggplot(conversion_per_category, aes(x = reorder(product_category, -conversion_rate), y = conversion_rate)) +
  geom_col(fill = colors_9) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Targeting Effectiveness: Conversion Rate on Google/CPC",
    subtitle = "The 'Electronics' category converts paid traffic from Google much more effectively.",
    x = NULL,
    y = "Conversion Rate"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )

# Analysis of Hypothesis 3: The Impact of Content on TikTok

# Aggregation to calculate CTR by paid social channel
performance_engagement_social <- Flat_Table %>%
  
  filter(str_detect(source_medium, "TikTok|Facebook|Instagram") & ad_spend > 0) %>%
  
  group_by(source_medium) %>%
  summarise(
    total_clicks = sum(clicks),
    total_impressions = sum(impressions)) %>%
  mutate(ctr = total_clicks / total_impressions) %>%
  
  arrange(desc(ctr))

# Show result
print(performance_engagement_social)

# Colors creation
colors_10 <- c("grey", "grey", "orange")

# Create Graphic for the CTR
ggplot(performance_engagement_social, aes(x = reorder(source_medium, -ctr), y = ctr)) +
  geom_col(fill = colors_10) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Engagement Analysis: Click-Through Rate (CTR) by Social Channel",
    subtitle = "TikTok shows a lower click-through rate, suggesting less direct engagement than other platforms.",
    x = NULL,
    y = "Click-Through Rate (CTR)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )


# Analysis of Hypothesis 4: The Average Basket is Structurally Higher


# Aggregation to calculate the Average Basket (AOV) by category
average_basket_by_category <- Flat_Table %>%
  
  filter(revenue > 0) %>%
  
  group_by(product_category) %>%
  summarise(
    total_revenue = sum(revenue),
    total_transactions = n_distinct(transaction_id) ) %>%
  mutate(average_basket = total_revenue / total_transactions) %>%
  
  arrange(desc(average_basket))

# show result
print(average_basket_by_category)

# Colors creation
colors_11 <- c("orange", "grey", "grey", "grey", "grey", "grey", "grey")

# Create graphic for average basket (AOV)
ggplot(average_basket_by_category, aes(x = reorder(product_category, -average_basket), y = average_basket)) +
  geom_col(fill = colors_11) +
  labs(
    title = "Analysis of the Average Basket by Category",
    subtitle = "The average basket for electronic products is significantly higher than that of other categories.",
    x = NULL,
    y = "Average Basket ($)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )

# Analysis of Hypothesis 5: Customer Loyalty

# Aggregation to calculate the average number of transactions per customer

loyalty_by_category <- Flat_Table %>%
  
  filter(revenue > 0) %>%
  
  group_by(product_category) %>%
  summarise(
    total_transactions = n_distinct(transaction_id),
    total_clients = n_distinct(customer_id)) %>%
  mutate(rate_per_customer = total_transactions / total_clients) %>%
  
  arrange(desc(rate_per_customer))

# Show result
print(loyalty_by_category)

# Colors creation
colors_12 <- c("orange", "grey", "grey", "grey", "grey", "grey", "grey")

# Create graphic for loyalty by category
ggplot(loyalty_by_category, aes(x = reorder(product_category, -rate_per_customer), y = rate_per_customer)) +
  geom_col(fill = colors_12) +
  labs(
    title = "Loyalty Analysis: Transactions per Customer",
    subtitle = "Customers in the 'Electronics' category purchase slightly more frequently than those in other categories.",
    x = NULL,
    y = "Average Number of Transactions per Customer"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )


library(lubridate)

# Analysis of Hypothesis 6: The Impact of Seasonality

# Aggregation to compare monthly revenues
seasonality_by_category <- Flat_Table %>%
  
  filter(revenue > 0) %>%

 
  mutate(group_category = ifelse(product_category == "Electronics", "Electronics", "Other Categories")) %>%
  
  mutate(transaction_date = as.Date(transaction_date)) %>%
  mutate(month = floor_date(transaction_date, "month")) %>%
  
  group_by(month, group_category) %>%
  
  summarise(
    total_revenue = sum(revenue))

# Show result
print(seasonality_by_category)

# Colors creation
colors_13 <- c("Electronics" = "orange", "Other Categories" = "grey")

# Create graphic with for seasonality by category
ggplot(seasonality_by_category, aes(x = month, y = total_revenue, color = group_category)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3)+
  
  scale_color_manual(values = colors_13) +
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  
  labs(
    title = "Impact of Seasonality on Revenue",
    subtitle = "The peak in electronics revenue at the end of the year is significantly more pronounced than that of other categories.",
    x = NULL,
    y = "Monthly Revenue ($)",
    color = "Category") +
  
  theme_classic() +
  theme(
    legend.position = "top",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )

# Analysis of Hypothesis 7: The Impact of Competition

# Aggregation to calculate the average CPC per category

cpc_per_category <- Flat_Table %>%
 
  filter(ad_spend > 0 & clicks > 0) %>%
  
  group_by(product_category) %>%
  summarise(
    total_ad_spend = sum(ad_spend),
    total_clicks = sum(clicks)) %>%
  mutate(cpc = total_ad_spend / total_clicks) %>%
  
  arrange(desc(cpc))

# Show result
print(cpc_per_category)

# Colors creation
colors_14 <- c("grey", "grey", "grey", "grey", "grey", "grey", "orange")

# Create graphic with cpc by category
ggplot(cpc_per_category, aes(x = reorder(product_category, -cpc), y = cpc)) +
  geom_col(fill = colors_14) +
  
  labs(
    title = "Competition Analysis: Cost Per Click (CPC) by Category",
    subtitle = "The CPC for Electronics is relatively low, suggesting less competition or more effective campaigns.",
    x = NULL,
    y = "Average Cost Per Click ($)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )


# Now go check the file: Project_Google_Ads_Prediction.R
# in order to see the prediction based on the recommendations! :)






















