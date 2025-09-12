
# Phase 1: Preparation
# Step 1: Load the library

library(sentimentr)
library(data.table)
library(plyr)
library(tidyverse)


# Step 2: loading file
data <- readLines(file('//Users//john//Downloads//Back_pain - dataset_reddit-scraper-lite_2025-09-02_12-47-09-684.csv'))
comment_text <- get_sentences(data)

# Phase 2: Cleaning comments

# Step 1: Convert text to lowercase
data <- tolower(data)

# Show result
print(data)

# Step 2: Remove punctuation
cleaned_data <- gsub("[[:punct:]]", " ", data)

# Show result
print(cleaned_data)

# Step 3: Removal of unnecessary words

install.packages("tm")
library(tm)

cleaned_data_2 <- removeWords(cleaned_data, stopwords("english"))

# Show result
print(cleaned_data_2)

# Step 4: Remove numbers
cleaned_data_3 <- gsub("[[:digit:]]", "",cleaned_data_2)

# Show result
print(cleaned_data_3)

# Phase 3: Sentiment Analysis
# Step 1: Calculating the sentiment score
sentiments <- sentiment_by(cleaned_data_3)

# Show result
print(sentiments)

# Step 2: Create sentiment class

sentiment_df <- as.data.frame(sentiments)

# Step 3: Creation of the classification function
get_sentiment_class <- function(sentiment_score) {
 
  if (sentiment_score < -0.3) {
    sentiment_class <- "Negative"
    
  } else if (sentiment_score < 0.3) {
    sentiment_class <- "Neutral"
    
  } else {
    sentiment_class <- "Positive"
  }
  return(sentiment_class)
}

# We put the result in the column 
sentiment_df$sentiment_class <- sapply(sentiment_df$ave_sentiment, get_sentiment_class)

# Show result
print(sentiment_df$sentiment_class)

print("Creation of the classification function")
head(sentiment_df[, c("ave_sentiment", "sentiment_class")])

# Step 4 : Aggregation

library(plyr)
library(ggplot2)

# We count the number of comments for each sentiment class.
sentiment_summary <- count(sentiment_df, sentiment_class)

# Show result
print("Summary of feelings:")
print(sentiment_summary)

# Step 5: Creation of a graphic bar
ggplot(data = sentiment_summary, aes(x = sentiment_class, y = n, fill = sentiment_class)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of Sentiments in Comments",
    subtitle = "Analysis of discussions on back pain",
    x = "Sentiment Class",
    y = "Number of Comments"
  ) +
  theme_minimal() +
  scale_fill_manual(values=c("Negative"="red", "Neutral"="grey", "Positive"="mediumseagreen"))+
  theme_classic() +
  theme(
       legend.position = "none",
       plot.subtitle = element_text(size = 8),
       axis.title.x = element_text(margin = margin(t = 20)),
       axis.title.y = element_text(margin = margin(r = 20)))


# Phase 4: Emotional analysis

# Step 1: Preparing emotion
emotion_df <- emotion_by(cleaned_data_3)

# Step 2: Aggregation for each emotion
emotion_summary <- subset(
  aggregate(emotion_count ~ emotion_type, emotion_df, sum),
  emotion_count > 0
)

# Show result
print("Summary of detected emotions:")
print(emotion_summary)

# Step 3: Creation colors
color_1 <- c("grey", "grey", "grey", "grey", "grey", "grey","orange","grey","grey","grey","orange","grey","grey","grey","orange","grey")

# Step 4: Creation of a graph for emotions
ggplot(data = emotion_summary, aes(x = reorder(emotion_type, -emotion_count), y = emotion_count)) +
  geom_bar(stat = "identity", fill = color_1) +
  labs(
    title = "Distribution of Emotions in Comments",
    subtitle = "What are the most common emotions?",
    x = "Type of Emotion",
    y = "Number of Occurrences"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  )

# Phase 5: Emotional hypothesis 

# Step 1: Preparation for analysis

if (!"sentiment_class" %in% names(data)) {
  sentiment_df <- as.data.frame(sentiment_by(data))
  
  
  get_sentiment_class <- function(sentiment_score) {
    if (sentiment_score < -0.3) { sentiment_class <- "Negative" }
    else if (sentiment_score < 0.3) { sentiment_class <- "Neutral" }
    else { sentiment_class <- "Positive" }
    return(sentiment_class)
  }
  sentiment_df$sentiment_class <- sapply(sentiment_df$ave_sentiment, get_sentiment_class)
  data <- cbind(data, sentiment_df)
}

# Step 2:  Filter only negative comments
negative_comments <- data %>%
  dplyr::filter(sentiment_class == "Negative")

# Step 3: Analyze emotions on this subset
negative_emotions <- emotion_by(negative_comments$data)

# Step 4: Aggregate the results
negative_emotion_summary <- subset(
  aggregate(emotion_count ~ emotion_type, negative_emotions, sum),
  emotion_count > 0
)

# Step 5: Creation of colors
color_2 <- c("orange", "grey", "grey", "orange", "grey", "orange","grey","grey","orange","grey","grey","grey")


# Step 6: Creation of the graphic bar
ggplot(data = negative_emotion_summary, aes(x = reorder(emotion_type, - emotion_count), y = emotion_count)) +
  geom_bar(stat = "identity", fill = color_2) +
  labs(
    title = "Dominant Emotions within NEGATIVE Comments",
    subtitle = "What emotions are associated with pain?",
    x = "Emotion Type",
    y = "Number of Occurrences"
  ) +
  theme_classic() +
  theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.subtitle = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20))
  )

# Conclusion: Emotional hypothesis verified

# Phase 6: competitors hypothesis


# Step 1:  Define a list of keywords related to solutions and competitors
competitor_keywords <- c("advil", "tylenol", "ibuprofen", "doctor", "physio", "chiro", "medication", "pills")

# Step 2: Create a search pattern with these keywords
pattern <- paste(competitor_keywords, collapse = "|")

# Step 3: Filter the comments that contain at least one of these keywords from the original 'body' column
competitor_comments <- data %>%
  dplyr::filter(grepl(pattern, data, ignore.case = TRUE))

# Step 4: Count the sentiment distribution for these specific comments
competitor_sentiment_summary <- plyr::count(competitor_comments, "sentiment_class")

# Show result
print("Sentiment distribution when a competitor is mentioned:")
print(competitor_sentiment_summary)

# Step 5: Creation of the graphic bar
ggplot(data = competitor_sentiment_summary, aes(x = sentiment_class, y = freq, fill = sentiment_class)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Sentiment Towards Competing Solutions",
    subtitle = "Are people satisfied with existing solutions?",
    x = "Sentiment Class",
    y = "Number of Comments"
  ) +
  theme_classic() +
  scale_fill_manual(values=c("Negative" = "grey", "Neutral" = "orange", "Positive" = "mediumseagreen")) +
  theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      plot.subtitle = element_text(size = 8),
      axis.title.x = element_text(margin = margin(t = 20)),
      axis.title.y = element_text(margin = margin(r = 20)))

# Conclusion: Partially verified

# Phase 6: Offer hypothesis

# Step 1: Load ALL necessary libraries for the word cloud
install.packages("wordcloud")
install.packages("RColorBrewer")
library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)

# Step 2: Filter to get only the "Positive" comments
positive_comments <- data %>%
  dplyr::filter(sentiment_class == "Positive")

# Step 3: Create a "corpus", which is a collection of text for text mining
corpus <- Corpus(VectorSource(positive_comments$data)) 

# Step 4: Cleaning text
corpus <- tm_map(corpus, content_transformer(tolower))     
corpus <- tm_map(corpus, removeNumbers)                  
corpus <- tm_map(corpus, removePunctuation)              
corpus <- tm_map(corpus, removeWords, stopwords("english")) 
corpus <- tm_map(corpus, stripWhitespace)                

# Step 5: Creation of the table
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing=TRUE)
df_freqs <- data.frame(word = names(word_freqs), freq=word_freqs)

# Step 5: Colors creation
orange_palette <- c("darkorange", "orange", "coral", "orangered", "sandybrown")

# Step 6: Create word cloud
wordcloud(words = df_freqs$word, 
          freq = df_freqs$freq, 
          max.words = 100, 
          random.order = FALSE, 
          colors = orange_palette)

# Conclusion: We now have our advertising slogan generator thanks to the word cloud.




