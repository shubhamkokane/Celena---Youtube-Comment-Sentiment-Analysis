library("shiny")
library("tidyverse")
library("rvest")
library("tuber")
library("tidytext")
library("dplyr")
library("textdata")
library("ggplot2")
library("tm")
library("topicmodels")
library("wordcloud")
library("wordcloud2")
library("RColorBrewer")

yt_oauth("192603887469-rasooci3vu64kfroe901cnsmnp0lghhe.apps.googleusercontent.com", "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", token = "")
#yt_oauth("754004588089-j155ksbuqlnrkgnvrd5l31kh5ugf6qob.apps.googleusercontent.com", "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", token="")


#https://statisticsglobe.com/extract-substring-before-or-after-pattern-in-r
#to extract pecific pattern after a pattern in r
ui <- fluidPage(tags$h1("Data Wrangling and Husbandry 16:954:597:01 Project"),
                tags$p("Celina is an", tags$strong("application/smart assistant")," which finds the ", tags$strong(" overall sentiment")," of any video on YouTube and ", tags$strong("classifies")," the topic of video based on the User Comments."),
                tags$hr(),
                navbarPage(title = "CELINA",
                           tabPanel("About Us", tags$h2("Team Introduction : "),
                                    tags$li("Rajesh Bhat : rmb317"),
                                    tags$li("Rutu Desai : rmd228"),
                                    tags$li("Shubham Kokane : ssk203")
                           ),
                           tabPanel("Project Details", 
                                    tags$h2("Details About Project :"),
                                    tags$p(tags$h3(tags$li("Project Aim : ")), "The main aim of the project is to analyse the sentiments of the comments
                                           from different YouTube and to find the topic based on the
                                           classification of the comments using Latent Dirichlet Allocation (LDA) method
                                           and try to present the sentiment score for each video by establishing a colour
                                           grading for each sentiment on public opinions.",
                                           tags$h3(tags$li("Abstract : ")), "We know that data is generated tremendously every second of the day and
                                           YouTube being one of the most favourite video streaming platform which has
                                           a lot of data generated on each video which can have different types of
                                           context (semantically). So each and every video has lots of comments and it can be sometimes difficult to go through
                                           the whole video to understand what the video is about. To minimize that effort, this application summarizes the 
                                           overall sentiment in the comment sections and finds the top words used in the comments to get a better understanding of
                                           what is the topic of the video.",
                                           tags$h3(tags$li("About Data and Process : ")),"We get the data everytime from YouTube Comments sections with the help of Video Id using 
                                           Tuber Package in R. We then use the data and process it to make it in more readbale and further usable format. We Also create Document Term Matrix
                                           for each video so that we can then apply Latent Dirichlet Allocation to get Topics from the Comments of that specific Video. 
                                           With the help of Shiny Library we then show all our data in the form of a Website with in built tags for html and css.",
                                           tags$h3(tags$li("Methodology Used : ")), "We start by scraping data from YouTube comments.
                                           Use different functions to clean the data, for instance using regex to
                                           find specific patterns, removing stop words, making it in the format which can be further used,
                                           renaming column names so that specific joins can also be performed.
                                           Then we create a COrpus and apply Document Term Matrix to that corpus to get the
                                           terms in every documents. We then Use LDA to find the topics from the comments of the Video and using 
                                           different libraries and functions to plots bar charts and word clouds to compare the results with different approach/method used.
                                           User can then try to find/guess topics from the model created above.")),
                           navbarMenu(title = "Application",
                                      tabPanel("Search with Video Link",
                                               textInput(inputId = "title",
                                                         label = "Enter the url",
                                                         value = "https://www.youtube.com/watch?v=fUpChfNN5Uo"),
                                               #value = "https://www.youtube.com/watch?v=hSRoKK4ZeRE"),
                                               verbatimTextOutput("video"),
                                               verbatimTextOutput("stats"),
                                               verbatimTextOutput("comments"),
                                               tags$h2("Word Clouds"),
                                               tags$p("We compare the Word Cloud before and after cleaning the data we collect."),
                                               fluidRow(
                                                 column(6,
                                                        plotOutput("word_cloud_before")),
                                                 column(6, 
                                                        plotOutput("word_cloud_after"))),
                                               tags$h2("Overall Sentiment in Comments"),
                                               tags$p("We take data, clean and find the sentiments and classify the Overall Sentiments in 3 parts : Positive, Negative and Neutral."),
                                               plotOutput("overall_sentiment"),
                                               tags$h2("Overall Sentiment from Comments"),
                                               plotOutput("sentiment_bar"),
                                               tags$h2("Topics after LDA"),
                                               tags$p("We compare 2 Methods of Latent Dirichlet Allocation : 1) VEM and 2) Gibbs, to see how topics are classified."),
                                               tags$h2("VEM Methd-2 Topics"),
                                               fluidRow(
                                                 column(6,
                                                        plotOutput("topic_1_vem")),
                                                 column(6,
                                                        plotOutput("topic_2_vem"))),
                                               tags$h2("Gibbs Methd-2 Topics"),
                                               fluidRow(
                                                 column(6,
                                                        plotOutput("topic_1_gibbs")),
                                                 column(6,
                                                        plotOutput("topic_2_gibbs")))),
                                      tabPanel("Search Video Based on a Topic and get Video Link",
                                               textInput(inputId = "title_search",
                                                         label = "Enter the text to be searched",
                                                         value = "Barack Obama"),
                                               verbatimTextOutput("search")))
                )
                
)
server <- function(input, output){
  video_id <- reactive({
    sub(".*v=", "", input$title)
  })
  
  output$video <- renderPrint({
    print("The Video Id is : ") 
    video_id()
  })
  
  output$stats <- renderPrint({
    print("Video Statistics are : ")
    get_stats(video_id())
  })
  
  comments <- reactive({
    print("Comments Table : ")
    get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
      select(videoId, authorDisplayName, textDisplay) %>%
      mutate(comment_number = row_number()) %>%
      unnest_tokens(word, textDisplay) %>%
      anti_join(stop_words)  %>%
      inner_join(get_sentiments("bing")) %>%
      mutate(index = comment_number %/% 15) %>%
      count(index, sentiment) %>%
      pivot_wider(names_from = "sentiment", values_from = "n") %>%
      mutate(across(everything(), replace_na, 0)) %>%
      mutate(sentiment = positive - negative) %>%
      mutate(Sentiment_Category = ifelse(sentiment > 0, "Positive",
                                         ifelse(sentiment < 0, "Negative",
                                                "Neutral")))
  })
  
  output$comments <- renderPrint({
    print("The Extracted and Processed Data is : ") 
    comments()
  })
  
  output$word_cloud_before <- renderPlot({
    get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
      select(videoId, authorDisplayName, textDisplay) %>%
      unnest_tokens(word, textDisplay) %>%
      group_by(word) %>% 
      select(c(3)) %>% 
      dplyr::summarise(frequency = n()) %>% 
      with(wordcloud(words = word, freq = frequency, min.freq = 1, max.words=300, 
                     random.order=FALSE, rot.per=0.35 ,colors=brewer.pal(8, "Dark2")))
  })
  
  output$word_cloud_after <- renderPlot({
    get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
      select(videoId, authorDisplayName, textDisplay) %>%
      mutate(comment_number = row_number()) %>%
      unnest_tokens(word, textDisplay) %>%
      inner_join(get_sentiments("bing")) %>%
      anti_join(stop_words)  %>%
      group_by(word) %>% 
      select(c(4)) %>% 
      dplyr::summarise(frequency = n()) %>% 
      with(wordcloud(words = word, freq = frequency, min.freq = 1, max.words=300, 
                     random.order=FALSE, rot.per=0.35 ,colors=brewer.pal(8, "Dark2")))
  })
  
  output$overall_sentiment <- renderPlot({
    #get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
    #  select(videoId, authorDisplayName, textDisplay) %>%
    #  mutate(comment_number = row_number()) %>%
    #  unnest_tokens(word, textDisplay) %>%
    #  anti_join(stop_words)  %>%
    #  inner_join(get_sentiments("bing")) %>%
    #  mutate(index = comment_number %/% 15) %>%
    #  count(index, sentiment) %>%
    #  pivot_wider(names_from = "sentiment", values_from = "n") %>%
    #  mutate(across(everything(), replace_na, 0)) %>%
    #  mutate(sentiment = positive - negative) %>%
    #  mutate(Sentiment_Category = ifelse(sentiment > 0, "Positive",
    #                                     ifelse(sentiment < 0, "Negative",
    #                                            "Neutral"))) %>%
    comments() %>%
      ggplot(aes(index, sentiment)) + 
      geom_bar(stat="identity", fill='lightblue') + 
      geom_smooth() +
      ggtitle("Bar Plot to Sentiments in Comments Section per every 15 comments") + 
      theme(plot.title = element_text(size = 10, hjust=0.5)) +
      xlab("Comment Numbers") + ylab("Count of Sentiment Cateory")
  })
  
  output$sentiment_bar <- renderPlot({
    #get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
    #  select(videoId, authorDisplayName, textDisplay) %>%
    #  mutate(comment_number = row_number()) %>%
    #  unnest_tokens(word, textDisplay) %>%
    #  anti_join(stop_words)  %>%
    #  inner_join(get_sentiments("bing")) %>%
    #  mutate(index = comment_number %/% 15) %>%
    #  count(index, sentiment) %>%
    #  pivot_wider(names_from = "sentiment", values_from = "n") %>%
    #  mutate(across(everything(), replace_na, 0)) %>%
    #  mutate(sentiment = positive - negative) %>%
    #  mutate(Sentiment_Category = ifelse(sentiment > 0, "Positive",
    #                                     ifelse(sentiment < 0, "Negative",
    #                                            "Neutral"))) %>%
    comments() %>% 
      ggplot(aes(x = Sentiment_Category, fill = Sentiment_Category)) + geom_bar() + 
      ggtitle("Bar Plot to compare Sentiments in Comments Section") + 
      theme(plot.title = element_text(size = 10, hjust=0.5)) +
      xlab("Sentiment Category") + ylab("Count of Sentiment") + scale_fill_brewer(palette = "Reds")
  })
  
  output$topic_1_vem <- renderPlot({
    get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
      select(textDisplay) %>%
      VectorSource(.) %>%
      Corpus(.) %>%
      DocumentTermMatrix(.) %>%
      .[apply(. , 1, sum) !=0, ] %>%
      LDA(., k = 2) %>%
      tidy(.) %>%
      as_tibble() %>%
      rename(word = term) %>%
      anti_join(stop_words) %>%
      rename(term = word) %>%
      filter(topic == 1) %>%
      mutate(beta_rank = min_rank(desc(beta))) %>%
      arrange(beta_rank) %>%
      head(10) %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(beta, term)) +
      geom_col(show.legend = FALSE, fill = 'green') +
      ggtitle("Bar Plot to Show Terms in Topic 1 after LDA Classification") + 
      theme(plot.title = element_text(size = 10, hjust=0.5)) +
      xlab("Beta Score") + ylab("Word")
  }) 
  
  #      as_tibble(tidy(LDA(.[apply(DocumentTermMatrix(Corpus(VectorSource(.$textDisplay))), 1, sum)!=0, ], k=2)))
  
  output$topic_2_vem <- renderPlot({
    get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
      select(textDisplay) %>%
      VectorSource(.) %>%
      Corpus(.) %>%
      DocumentTermMatrix(.) %>%
      .[apply(. , 1, sum) !=0, ] %>%
      LDA(., k = 2) %>%
      tidy(.) %>%
      as_tibble() %>%
      rename(word = term) %>%
      anti_join(stop_words) %>%
      rename(term = word) %>%
      filter(topic == 2) %>%
      mutate(beta_rank = min_rank(desc(beta))) %>%
      arrange(beta_rank) %>%
      head(10) %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(beta, term)) +
      geom_col(show.legend = FALSE, fill = 'yellow') +
      ggtitle("Bar Plot to Show Terms in Topic 2 after LDA Classification") + 
      theme(plot.title = element_text(size = 10, hjust=0.5)) +
      xlab("Beta Score") + ylab("Word")
  })
  
  output$topic_1_gibbs <- renderPlot({
    get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
      select(textDisplay) %>%
      VectorSource(.) %>%
      Corpus(.) %>%
      DocumentTermMatrix(.) %>%
      .[apply(. , 1, sum) !=0, ] %>%
      LDA(., k = 2, method = "Gibbs") %>%
      tidy(.) %>%
      as_tibble() %>%
      rename(word = term) %>%
      anti_join(stop_words) %>%
      rename(term = word) %>%
      filter(topic == 1) %>%
      mutate(beta_rank = min_rank(desc(beta))) %>%
      arrange(beta_rank) %>%
      head(10) %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(beta, term)) +
      geom_col(show.legend = FALSE, fill = 'orange') +
      ggtitle("Bar Plot to Show Terms in Topic 1 after LDA Classification") + 
      theme(plot.title = element_text(size = 10, hjust=0.5)) +
      xlab("Beta Score") + ylab("Word") + scale_fill_brewer(palette = "Greens")
  }) 
  
  #      as_tibble(tidy(LDA(.[apply(DocumentTermMatrix(Corpus(VectorSource(.$textDisplay))), 1, sum)!=0, ], k=2)))
  
  output$topic_2_gibbs <- renderPlot({
    get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
      select(textDisplay) %>%
      VectorSource(.) %>%
      Corpus(.) %>%
      DocumentTermMatrix(.) %>%
      .[apply(. , 1, sum) !=0, ] %>%
      LDA(., k = 2, method = "Gibbs") %>%
      tidy(.) %>%
      as_tibble() %>%
      rename(word = term) %>%
      anti_join(stop_words) %>%
      rename(term = word) %>%
      filter(topic == 2) %>%
      mutate(beta_rank = min_rank(desc(beta))) %>%
      arrange(beta_rank) %>%
      head(10) %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(beta, term)) +
      geom_col(show.legend = FALSE, fill = 'blue') +
      ggtitle("Bar Plot to Show Terms in Topic 2 after LDA Classification") + 
      theme(plot.title = element_text(size = 10, hjust=0.5)) +
      xlab("Beta Score") + ylab("Word")
  })
  output$search <- renderPrint({
    print("Searched Results are : ")
    yt_search(input$title_search) %>% select(video_id, title, channelTitle, description) %>% head(5)
  })
  
}

shinyApp(ui = ui, server = server)

#Sharing App
