

install.packages("sentimentr")
install.packages("data.table")
install.packages("plyr", type = "source")


library(sentimentr)
library(data.table)
library(plyr)



# step 1: loading file
movie_reviews <- readLines(file("/Users/john/Downloads/The-Tinder-Swindler.txt"))

review_text <- get_sentences(movie_reviews)

# step 2: See sentiments for each line

sentiment(review_text)

# step 3: sentiment by each review

sentiments <- sentiment_by(review_text)

# step 4: Convert sentiment data.table to data frame

sentiment_df <- setDF(sentiments)

# step 5: Function that generates a sentiment class based on sentiment score

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

# step 6: Add a sentiment_class attribute

  sentiment_df$sentiment_class <- sapply(sentiment_df$ave_sentiment, get_sentiment_class)
            
# step 7: Show result sentiment
      
            sentiment_df[ , 4:5]
      
# step 8: Draw a pie chart
            
      sentiment_summary <- count(sentiment_df, "sentiment_class")
      
      pie(sentiment_summary$freq,
          sentiment_summary$sentiment_class,
          col=c("Red", "Blue", "Green"))
      
# Analyzing Emotions
# step 1:Create a dataframe for emotions by review
      
      install.packages("EMOTIONS")
      library(EMOTIONS)
      
      emotion_df <- setDF(emotion_by(review_text))
      emotion_df
      
# step 2: Aggregation by emotion types and remove 0 values
      
      emotion_summary = subset (
              aggregate(emotion_count ~ emotion_type,
                        emotion_df, sum),
                        emotion_count > 0)
            
# step 3: Draw a pie chart for emotion summary

pie(emotion_summary$emotion_count, emotion_summary$emotion_type,
    col = c("Red", "Green", "Blue", "orange", "Brown", "Purple", "Yellow", "Grey", "Pink", "White", "Maroon", "Beige", "Black"))
            
            




