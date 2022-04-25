#Getting Libraries
library("tidyverse")
library("rvest")
library("tuber")
library("tidytext")
library("plyr")
library("dplyr")
library("textdata")
library("ggplot2")
library("wordcloud")
library("wordcloud2")
library("RColorBrewer")

set.seed(1234)

#Using this link as reference
# https://cran.r-project.org/web/packages/tuber/vignettes/tuber-ex.html
# Creating Project on API Console
# Enabling the YouTube Data API V3
# Creating OAuth Credentials and adding users to verify

#Authenticating my Details
yt_oauth("754004588089-j155ksbuqlnrkgnvrd5l31kh5ugf6qob.apps.googleusercontent.com", "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", token="")

# Video Details
#https://www.youtube.com/watch?v=N708P-A45D0

#Getting all details about a video with id as "N708P-A45D0"
get_stats(video_id="N708P-A45D0")

#Getting Video Details
get_video_details(video_id="N708P-A45D0")

#Searching all videos with a specific topic
res1 <- yt_search("Barack Obama")
head(res1[, 1:3])

#Getting comments from a specific video comment section
res2 <- get_comment_threads(c(video_id="N708P-A45D0"), max_results = 101)
head(res2)

#Creating df for only specific columns
df <- res2 %>% select(videoId, authorDisplayName, textDisplay)
head(df)
summary(df)
glimpse(df)

#adding comment number to each comment
df <- df %>%
  mutate(comment_number = row_number())
head(df)

# taking each word as a token to clean it
text_df <- df %>%
  unnest_tokens(word, textDisplay)
head(text_df,15)

# removing stop words:
tidy_comments <- text_df %>%
  anti_join(stop_words)
head(tidy_comments)

#we can add plot to count words and make a bar plot

# getting sentiments:
get_sentiments("bing")

# joining tidy_comments with sentiments:
comment_sentiments <- tidy_comments %>%
  inner_join(get_sentiments("bing"))
glimpse(comment_sentiments)
head(comment_sentiments,20)

# change of sentiments after every 15 comments:
comment_sentiments <- comment_sentiments %>%
  mutate(index = comment_number %/% 15)
head(comment_sentiments,30)

# count of sentiment at an interval of every 15 comments:
comment_sentiments <- comment_sentiments %>%
  count(index, sentiment)
head(comment_sentiments,30)

# using pivot_wider to make column wise table:
comment_sentiment_wider <- comment_sentiments %>%
  pivot_wider(names_from = "sentiment", values_from = "n")
head(comment_sentiment_wider, 15)

# Calculating sentiment for each section of 15 comments:
comment_sentiment_wider <- comment_sentiment_wider %>%
  mutate(sentiment = positive - negative)
head(comment_sentiment_wider, 15)

# Mutating Sentiment Positive, Negative or Neutral on the basis of final sentiment value obtained above
comment_sentiment_wider_final <- comment_sentiment_wider %>%
  mutate(Sentiment_Category = ifelse(sentiment > 0, "Positive",
                                     ifelse(sentiment < 0, "Negative",
                                            "Neutral")))
sample_n(comment_sentiment_wider_final,10)
plot1 <- ggplot(comment_sentiment_wider_final, aes(x= Sentiment_Category)) + geom_bar()
plot1

# Plotting the change of sentiments over comments:
plot2 <- ggplot(comment_sentiment_wider, aes(index, sentiment)) + 
  geom_bar(stat="identity") + 
  geom_smooth()
plot2

# Changing some stuff in comment_sentiments

# Finding comment_sentiments:
comment_sentiments_word_cloud <-  tidy_comments %>%
  inner_join(get_sentiments("bing"))
head(comment_sentiments_word_cloud)

# Finding count of each word:
comment_sentiments_word_cloud_count <- comment_sentiments_word_cloud %>%
  group_by(word) %>% arrange(word) %>% select(c(4))
unique(comment_sentiments_word_cloud_count)


word_freq <- comment_sentiments_word_cloud_count %>% 
  dplyr::summarise(Freq = n())
word_freq %>% arrange(-Freq)
# WordCloud of Comments:
word_cloud_plot <- wordcloud(words = word_freq$word, freq = word_freq$Freq, min.freq = 1, max.words=200, 
                             random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))