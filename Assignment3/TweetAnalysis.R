

library(rtweet)
library(sentimentr)
library(SentimentAnalysis)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(tm)
library(wordcloud)

Twitter <- search_tweets("vaccine", n =3000, include_rts = FALSE,lang="en")
#View(Twitter)
#class(Twitter)

Twitter$stripped_text <- gsub("http.*","",Twitter$text)
Twitter$stripped_text <- gsub("https.*","",Twitter$stripped_text)
Twitter$stripped_text <- gsub('[[:punct:]]',"" ,Twitter$stripped_text)
Twitter$stripped_text <- gsub('[[:cntrl:]]', "" ,Twitter$stripped_text)
Twitter$stripped_text <- gsub('\\d+',"" ,Twitter$stripped_text)
Twitter$stripped_text <- tolower(Twitter$stripped_text)
tweet_list <- Twitter$stripped_text
View(tweet_list)

clean <- function(x){
  x <- gsub("http.*","",x)
  x <- str_replace_all(x, "[^[:alnum:]]", " ")
  x <- tolower(x)
  x <-removeWords(x,stopwords('en'))
  x <-removeWords(x,c('americans','Americans'))
  x <-removePunctuation(x)
  x <-stripWhitespace(x)
  return(x) }

cleaned_tweets <- clean(tweet_list)
#View(cleaned_tweets)
length(cleaned_tweets)

sentiment <- analyzeSentiment(cleaned_tweets)
View(sentiment)
plotSentiment(sentiment, x = NULL, cumsum = FALSE, xlab = "",
              ylab = "Sentiment")

location_names <- unique(unlist(Twitter$location),use.names =FALSE)
location_count <- tabulate(match(Twitter$location, unique(Twitter$location)))
location_df <- data.frame("names" = unlist(location_names),"count" = unlist(location_count))
location_df <- location_df %>%
  arrange(desc(location_count)) %>% top_n(20)
location_df <- location_df[2:20,]
maxcount <- max(location_df$count)

ggplot(data = location_df,aes(x =names, y=count))+
  geom_col(col='dark green', fill ='dark blue', size = 2)+  theme(legend.position = "none")+
  ylim(0,maxcount)+
  xlab("Places from where most of the Tweets are Recorded")+ ylab("Number of Tweets")


ggplot(data = Twitter,aes(x = Twitter$created_at))+
  geom_histogram(aes(fill = ..count..))+
  theme(legend.position = "none")+
  xlab("Time at which the tweets are recorded")+ ylab("Number of Tweets")+
  scale_fill_gradient(low = 'pink',high ='red')

source_uniq <- unique(unlist(Twitter$source),use.names =FALSE)
source_count <- tabulate(match(Twitter$source, unique(Twitter$source)))
source_uniq <- source_uniq[1:10]
source_count <- source_count[1:10]
max_count <- max(source_count)
source_df <- data.frame("names" = unlist(source_uniq),"count" = unlist(source_count))



ggplot(data = source_df,aes(x =source_uniq, y=source_count))+
  geom_col(col='dark green', fill ='dark blue', size = 2)+  theme(legend.position = "none")+
  ylim(0,max_count)+
  xlab("Devices from which the Tweets are Recorded")+ ylab("Number of Tweets")


sentiment_range <- sentiment(cleaned_tweets)
count_tweets <- table(sign(sentiment_range$sentiment))
count_tweets


ggplot(sentiment_range, aes(x=sentiment_range$word_count, y=sentiment_range$sentiment)) + 
  geom_bar(stat='identity', aes(fill=sentiment_range$sentiment), width=.5)+ 
  coord_flip()


emotion_range <- emotion(cleaned_tweets)
emotion_range[1:20]
emotion_range <- emotion_range %>% group_by(element_id) %>% filter(emotion_count == max(emotion_count)) %>% slice(1)
Twitter$sentiment <- emotion_range$emotion_type

emotion_unique <- unique(unlist(emotion_range$emotion_type),use.names =FALSE)
emotion_count <- tabulate(match(emotion_range$emotion_type, unique(emotion_range$emotion_type)))
emotion_df <- data.frame("names" = unlist(emotion_unique),"count" = unlist(emotion_count))


ggplot(data = emotion_df, aes(names, count)) +
  geom_bar(aes(fill = names),stat = "identity")+
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Sentiment score based on each Tweets")

#Words related to positive and negative tweets or based on emotions

for(un in unique(unlist(Twitter$sentiment)))
{
  Tweet_sentiment <- data.frame()
  sentiment_text <- data.frame()
  
  Tweet_sentiment <- Twitter[Twitter$sentiment == un,]
  sentiment_text <- Tweet_sentiment$text
  sentiment_text <- clean(sentiment_text)
  
  line_by <- function(x){
    words_list <-  tibble("word"= unlist(strsplit(x," ")))
    freq_words <- words_list %>%
      group_by(word) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>% top_n(200)
    return (freq_words)
  }
  
  freq <- line_by(sentiment_text)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, un)
  wordcloud(freq$word,freq = freq$count,color = rainbow(10),scale = c(3,1),min.freq = 0, random.color = FALSE,main = "Title")
  
}
