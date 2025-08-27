

library(tidyverse)
library(dplyr)
library(lubridate)

# Import data

Flat_Table <- read.csv("//Users//john//Downloads//Project_Google_Ads_Flat_Table.csv")

Flat_Table %>%

  
# Phase 1: Descriptive Analysis

# Step 1

# Agregation by country (Where)
  
performance_by_country <- Flat_Table %>%
  group_by(country) %>%
  summarise(total_revenue = sum(revenue)) %>%
  arrange(desc(total_revenue))

# Show result
print(performance_by_country)

# colors creation
colors <- c("orange", "grey", "grey") 


# Create graphic by country
ggplot(performance_by_country, aes(x = reorder(country, -total_revenue), y = total_revenue)) +
  geom_col(fill = colors) +
  labs(
    title = "Performance by country",
    subtitle = "Revenue is overwhelmingly concentrated in France.",
    x = NULL,
    y = "Total_Revenue ($)") +
  theme_classic() +
theme(
  legend.position = "none",
  plot.subtitle = element_text(size = 8),
  axis.text.x = element_text(angle = 45, hjust = 1),
  axis.title.y = element_text(margin = margin(r = 20))
)

# Agregation by category of product 
performance_by_category <- Flat_Table %>%
  filter(revenue > 0) %>%
  group_by(product_category) %>%
  summarise(total_revenue = sum(revenue)) %>%
  arrange(desc(total_revenue))

# Show result
print(performance_by_category)

# colors creation
colors_2 <- c("orange", "grey", "grey", "grey", "grey", "grey","grey")

# Create graphic by category
ggplot(performance_by_category, aes(x = reorder(product_category, -total_revenue), y = total_revenue)) +
  geom_col(fill = colors_2) +
  labs(
    title = "Performance by Product Category",
    subtitle = "The 'Electronics' category generates the majority of revenue.",
    x = NULL,
    y = "Total_Revenue ($)") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )


# Step 2

# Agregation by month (When)
performance_by_month <- Flat_Table %>%
  mutate(month = floor_date(ymd(transaction_date), "month")) %>%
  group_by(month) %>%
  summarise(total_revenue = sum(revenue)) %>%
  arrange(month) 

# Show result
print(performance_by_month)

# colors creation
colors_3 <- c("grey", "grey", "grey", "grey", "grey", "grey", 
            "grey", "grey", "grey", "grey","grey","grey","grey","grey",
            "grey", "orange", "orange")


# Create graphic by month
ggplot(performance_by_month, aes(x = month, y = total_revenue)) +
  geom_col(fill = colors_3) +
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  
  labs(
    title = "Monthly Sales Performance",
    subtitle = "A peak in revenue is visible at the end of the year (holiday season).",
    x = NULL,
    y = "Total_Revenue ($)") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )

# Step 3

# Agregation by source of trafic 
performance_by_source <- Flat_Table %>%
  group_by(source_medium) %>%
  summarise(
    total_revenue = sum(revenue),
    total_ad_spend = sum(ad_spend),
    roas = total_revenue / total_ad_spend) %>%
  filter(is.finite(roas)) %>%
  arrange(desc(total_revenue))

# Show result
print(performance_by_source)

# colors creation
colors_4 <- c("orange", "orange", "grey", "grey", "grey", "grey")

# Create graphic by source
ggplot(performance_by_source, aes(x = reorder(source_medium, -total_revenue), y = total_revenue)) +
  geom_col(fill = colors_4) +
  labs(
    title = "Performance by Traffic Source",
    subtitle = "Google / CPC and TikTok / Social  are the main revenue-generating channels.",
    x = NULL,
    y = "Total_Revenue ($)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )


# Step 4

library(scales)
library(dplyr)

# Aggregation by category to calculate percentages
performance_in_pourcentage <- Flat_Table %>%
  filter(revenue > 0) %>%
  group_by(product_category) %>%
  summarise(total_revenue = sum(revenue)) %>%
  mutate(percentage_numeric = total_revenue / sum(total_revenue)) %>%
  mutate(percentage_display = percent(percentage_numeric, accuracy = 0.01)) %>%
  arrange(desc(percentage_numeric))

# Show result
print(performance_in_pourcentage)

# colors creation
colors_5 <- c("orange", "grey", "grey", "grey", "grey", "grey","grey")

# Create graphic in percentage
ggplot( performance_in_pourcentage, aes(
                                    y = percentage_numeric, 
                                   x = reorder(product_category, -percentage_numeric))) +
  geom_col(fill = colors_5) +
  
  geom_text(aes(label = percentage_display), vjust = -0.5, size = 3) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Share of Each Category in Total Revenue",
    subtitle = "The 'Electronics' category accounts for more than half of sales.",
    x = NULL,
    y = "Share of Total Revenue (%)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )

# Step 5

# No need for agregation with Boxplot graphic (How much)
# colors creation
colors_6 <- c("Electronics" = "orange", 
            "Clothing" = "grey", 
            "House" = "grey", 
            "Beauty" = "grey", 
            "Books" = "grey", 
            "Accessories" = "grey",
            "High-Tech " = "grey")

# Create Boxplot graphic
ggplot(Flat_Table %>% 
         
  filter(revenue > 0), aes(x = product_category, y = revenue, fill = product_category)) +
  geom_boxplot() +
  
  scale_fill_manual(values = colors_6) +
  
  labs(
    title = "How much do customers spend per category?",
    subtitle = "Orders in 'Electronics' have the highest median value and the widest price range.",
    x = NULL,
    y = "Revenue per Transaction ($)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )

# Step 6

# Aggregation by campaign to calculate ROAS (What)
performance_per_campagne <- Flat_Table %>%
  
  filter(ad_spend > 0) %>%
  group_by(campaign_name) %>%
  summarise(
    total_revenue = sum(revenue),
    total_ad_spend = sum(ad_spend)) %>%
  mutate(
    roas = total_revenue / total_ad_spend,
    rentabilite = ifelse(roas >= 1, "Positive", "Negative")) %>%
  filter(total_revenue > 0) %>%
  arrange(desc(roas))

# Show result
print(performance_per_campagne)


# Create graphic by campaign
ggplot(performance_per_campagne, aes(x = reorder(campaign_name, -roas), y = roas, fill = rentabilite)) +
  geom_col() +
  
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  
  scale_fill_manual(values = c("Positive" = "orange", "Negative" = "grey")) +
  
  labs(
    title = "Performance by advertising campaign",
    subtitle = "The majority of campaigns are profitable (ROAS > 1), but some perform below the threshold.",
    x = NULL,
    y = "ROAS (Return on Advertising Spend)") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20))
  )



# Now go check the file: Project_Google_Ads_Diagnostic_Analysis.R
# in order to understand why the category Electronics perform very well ! :)







