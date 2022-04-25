#Getting Libraries
library("tidyverse")
library("rvest")
library("tuber")
library("tidytext")
library("dplyr")
library("textdata")
library("ggplot2")
library("tm")
library("topicmodels")

#Using this link as reference
# https://cran.r-project.org/web/packages/tuber/vignettes/tuber-ex.html
# Creating Project on API Console
# Enabling the YouTube Data API V3
# Creating OAuth Credentials and adding users to verify

#Authenticating my Details
yt_oauth("192603887469-rasooci3vu64kfroe901cnsmnp0lghhe.apps.googleusercontent.com", "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", token = "")

#Getting comments from a specific video comment section
res2 <- get_comment_threads(c(video_id="EGcXF0iG-2s"), max_results = 101)
head(res2)

#Creating df for only specific columns
df <- res2 %>% select(videoId, authorDisplayName, textDisplay)
head(df)
summary(df)
glimpse(df)
df2 <- res2 %>% select(textDisplay)

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
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  mutate(across(everything(), replace_na, 0))
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

#Creating a DTM
myCorpus = Corpus(VectorSource(df2$textDisplay))
mydtm = DocumentTermMatrix(myCorpus)
inspect(mydtm)

#Eliminating zero document DTM
row_sum <- apply(mydtm, 1, FUN = sum)
mydtm=mydtm[row_sum!=0,]

#Applying LDA VEM
mydtm_lda_vem = LDA(mydtm, k = 2)
mydtm_tidy_vem = tidy(mydtm_lda_vem)
mydtm_tidy_vem = rename(mydtm_tidy_vem, word = term)
mydtm_tidy_vem = mydtm_tidy_vem %>%
  anti_join(stop_words)
mydtm_tidy_vem = rename(mydtm_tidy_vem, term = word)

#Top 10 words in each topic VEM
mydtm_tidy_vem %>% filter(topic==1) %>%
  mutate(beta_rank = min_rank(desc(beta))) %>% arrange(beta_rank) %>% head(10)
mydtm_tidy_vem %>% filter(topic==2) %>%
  mutate(beta_rank = min_rank(desc(beta))) %>% arrange(beta_rank) %>% head(10)

#Plotting graphs VEM
mydtm_tidy_vem %>% filter(topic==1) %>%
  mutate(beta_rank = min_rank(desc(beta))) %>%
  arrange(beta_rank) %>%
  head(10) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(beta, term)) +
  geom_col(show.legend = FALSE)

mydtm_tidy_vem %>% filter(topic==2) %>%
  mutate(beta_rank = min_rank(desc(beta)))%>%
  arrange(beta_rank) %>%
  head(10) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(beta, term)) +
  geom_col(show.legend = FALSE)

#Applying LDA Gibbs
mydtm_lda_gibbs = LDA(mydtm, k = 2, method = 'Gibbs')
mydtm_tidy_gibbs = tidy(mydtm_lda_gibbs)
mydtm_tidy_gibbs = rename(mydtm_tidy_gibbs, word = term)
mydtm_tidy_gibbs = mydtm_tidy_gibbs %>%
  anti_join(stop_words)
mydtm_tidy_gibbs = rename(mydtm_tidy_gibbs, term = word)

#Top 10 words in each topic Gibbs
mydtm_tidy_gibbs %>% filter(topic==1) %>%
  mutate(beta_rank = min_rank(desc(beta))) %>% arrange(beta_rank) %>% head(10)
mydtm_tidy_gibbs %>% filter(topic==2) %>%
  mutate(beta_rank = min_rank(desc(beta))) %>% arrange(beta_rank) %>% head(10)

#Plotting graphs Gibbs
mydtm_tidy_gibbs %>% filter(topic==1) %>%
  mutate(beta_rank = min_rank(desc(beta)))%>%
  arrange(beta_rank) %>%
  head(10) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(beta, term)) +
  geom_col(show.legend = FALSE)

mydtm_tidy_gibbs %>% filter(topic==2) %>%
  mutate(beta_rank = min_rank(desc(beta)))%>%
  arrange(beta_rank) %>%
  head(10) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(beta, term)) +
  geom_col(show.legend = FALSE)