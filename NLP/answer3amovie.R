library(NLP)
library(XML)
library(rvest)
library(magrittr)
library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)

url <- "https://www.imdb.com/title/tt7485048/reviews?sort=helpfulnessScore&dir=desc&ratingFilter"
review <- NULL
for(i in 1:10)
{
  murl <- read_html(as.character(paste(url,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%  ## starting of review 
    html_text()
  review <- c(review,rev)
}
length(review)
setwd("C://Users//Acer//Desktop//Assignment//pending//NLP")
getwd()
write.table(review,"Super30.txt")
super30 <- readLines("C://Users//Acer//Desktop//Assignment//pending//NLP//Super30.txt")
View(super30)
super30


## creating corpus data 
corpus <- Corpus(VectorSource(super30))


## cleaning of data
clean_data <- tm_map(corpus,toupper)
inspect(clean_data[1:6])
clean_data <- tm_map(clean_data,removePunctuation)
clean_data<- tm_map(clean_data,stripWhitespace)

clean_data <- tm_map(clean_data,removeNumbers)
clean_data <- tm_map(clean_data,removeWords,stopwords('english')) 
stopword <- readLines(file.choose())
clean_data <- tm_map(clean_data,removeWords,stopword)


# Remove URL's from corpus
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)

clean_data<- tm_map(clean_data, content_transformer(removeURL))
inspect(clean_data[1:7])


#####EDA

## tdm or dtm 

dtm <- TermDocumentMatrix(clean_data)
inspect(dtm[1:10,1:10])

dim(dtm)
dtm$nrow
dtm$ncol
dtm$dimnames
dtm$i
dtm$v
dtm$j
dtm
dtm1 <- as.matrix(dtm)
dtm1

## bag oe word 
ro <- apply(dtm1,1,sum)
ro


dtmnew <-dtm1[ro>0,]
View(dtmnew)


library(topicmodels)
lda <- LDA(dtmnew,5)
terms <- terms(lda,3)
terms



tops <- terms(lda)
tops

ta <- table(names(tops),unlist(tops))
ta

ta <- data.frame(ta)
ta


############clustering
cls <- hclust(dist(ta),method = "ward.D2")
par(family="HiraKakuProN-W3") 
plot(cls)



##### emotion Mining
library(syuzhet)
myfile <- readLines(file.choose())
sentence <- get_sentences(myfile)
class(sentence)

sentiment <- get_sentiment(sentence,method = "bing")
nrc_vector <- get_sentiment(sentence,method = "nrc")
head(nrc_vector)

class(sentiment)
plot(sentiment)
sum(sentiment)
mean(sentiment)
head(sentiment)
head(sentence)
summary(sentiment)


plot(sentiment,type = "l",main = "plot Trajectory",
     xlab = "Narrative Time",ylab = "Emotion Valence")
abline(h=0,col="Green")


###extract positive and negative emotion

neg <- sentence[which.min(sentiment)]
neg
pos <- sentence[which.max(sentiment)]
pos


### in  more depth
poa_v <- myfile
poa_sent <- get_sentiment(poa_v, method = "bing")
plot(poa_sent,type = "h", main = "LOTR using transformed Values",
     xlab = "Narrative Time", ylab = "Emotinal Valence",col="red")

ft_values <- get_transformed_values(poa_sent,
                                    low_pass_size = 3,
                                    x_reverse_len = 100,
                                    scale_vals = TRUE,
                                    scale_range = FALSE)

plot(ft_values, type = "h", main = "LOTR using Transformed values",
     xlab = "Narrative time", ylab = "Emotional Valence",
     col="red")

nrc_data <- get_nrc_sentiment(sentence)
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment('grim')

#subset
sad_items <- which(nrc_data$sadness>0)
head(sentence[sad_items])

barplot(sort(colSums(prop.table(nrc_data[,1:8]))), horiz = T, cex.names = 0.7,
        las= 1, main = "Emotions", xlab="Percentage",col = 1:8)


#### 20+% people trust on this movie 
## 18 approx anticipation
## 17 16 approx joy 
