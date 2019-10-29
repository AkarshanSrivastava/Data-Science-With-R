library(rvest)
library(magrittr)
library(readr)
library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)
library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)
#install.packages(c('ROAuth','RCurl'))
require('ROAuth')
require('RCurl')


cred <- OAuthFactory$new(consumerKey='2vBpHneUggqA3g6UJLRbdfC4B', # Consumer Key (API Key)
                         consumerSecret='RCliA6thGAa8J91i4lNL0zMyKSXcFwcSXWbFKtjOZz8RySIBLJ', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
setwd("C://Users//Sanchi//Desktop//Assignment//Complete//R//NLP")
getwd()

save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")



#install.packages("twitteR", "RCurl", "RJSONIO", "stringr")

setup_twitter_oauth("2vBpHneUggqA3g6UJLRbdfC4B", # Consumer Key (API Key)
                    "RCliA6thGAa8J91i4lNL0zMyKSXcFwcSXWbFKtjOZz8RySIBLJ", #Consumer Secret (API Secret)
                    "891539595652976640-rHXrnL2L2Tql69LQVXZipjAp7IV1s18",  # Access Token
                    "50SbSHaZeQoclnvixDZJgF3nCZyPZlL8YgYz9kOLNUZ7h")  #Access Token Secret


#registerTwitterOAuth(cred)

Tweets <- userTimeline('OfficialKalam', n = 1000, includeRts = T)
Tweets
TweetsDF <- twListToDF(Tweets)
twee
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Tweets123.csv",row.names = F)

twt <- read.csv("Tweets123.csv")
twt.txt <- as.character(twt$text)
View(twt.txt)

## creating corpus data
data <- Corpus(VectorSource(twt.txt))

clean_data <- tm_map(data, tolower)
clean_data <- tm_map(clean_data, removeNumbers)
clean_data <- tm_map(clean_data, removePunctuation)
clean_data <- tm_map(clean_data, removeWords, stopwords())
clean_data <- tm_map(clean_data, removeWords, c("rt","dicapriofdn","https...","..."))
clean_data <- tm_map(clean_data, removeWords, c("rt","..."))
clean_data <- tm_map(clean_data, stripWhitespace)
clean_data <- tm_map(clean_data, stemDocument)
inspect(clean_data[1:10])

## creating Term Document Matrix

data_tdm <- TermDocumentMatrix(clean_data)
inspect(data_tdm[1:10,1:10])

a <- NULL
for (i in 1:ncol(data_tdm)){ 
  if (sum(data_tdm[, i]) == 0){ 
    a <-  c(a,i)
  } }
## creating word cloud
m <- as.matrix(data_tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d <- d[-1,]  ## removing "..."
head(d, 10)

windows()
wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# lOADING +VE AND -VE words  
pos_words <- scan("C://Users//Sanchi//Desktop//Assignment//Complete//R//NLP//positive-words.txt", what="character", comment.char=";")	
neg_words <- scan("C://Users//Sanchi//Desktop//Assignment//Complete//R//NLP//negative-words.txt", what="character", comment.char=";") 	
#pos.words=c(pos.words,"wow", "kudos", "hurray") 			# including our own positive words to the existing list
#neg.words = c(neg.words)

############### SENTIMENT ANALYSIS ##################

## Positive words bar plot
poswordbar = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), pos_words)
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  df <- data.frame(names,freq_pos)
  windows()
  ggplot(head(df,30), aes(reorder(names,freq_pos), freq_pos)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Positive words") + ylab("Frequency") +
    ggtitle("Most frequent positive words")
}

poswordbar(data_tdm)

## Positive word cloud
poswordc = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), pos_words)
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  windows()
  wordcloud(names,freq_pos,sacle= c(4,0.5),min.freq = 1, colors = brewer.pal(8,"Dark2"))
}

poswordc(data_tdm)

## Negative word bar plot
negwordbar = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  neg.matches = match(names(freq), neg_words)
  neg.matches = !is.na(neg.matches)
  freq_neg <- freq[neg.matches]
  names <- names(freq_neg)
  df <- data.frame(names,freq_neg)
  windows()
  ggplot(head(df,30), aes(reorder(names,freq_neg), freq_neg)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Negative words") + ylab("Frequency") +
    ggtitle("Most frequent negative words")
}

negwordbar(data_tdm)

## Negative word cloud
negwordc = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  neg.matches = match(names(freq), neg_words)
  neg.matches = !is.na(neg.matches)
  freq_neg <- freq[neg.matches]
  names <- names(freq_neg)
  windows()
  wordcloud(names,freq_neg,scale = c(4,0.5), min.freq = 1, colors = brewer.pal(8,"Dark2"))
}

negwordc(data_tdm)

