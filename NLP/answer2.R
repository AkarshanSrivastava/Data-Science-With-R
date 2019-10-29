### we are going to extract the review from amazon  also known as web scripting
#install.packages("rvest")  ## for web scripting
library(rvest)
#install.packages("XML")  ## we know that web are in html format so
library(XML)
#install.packages("magrittr") ### increase performance 
library(magrittr)
library("tm")
library("slam")
library(topicmodels)
library(NLP)

##amazon review
aurl <- "https://www.amazon.in/Xiaomi-Mi-A2-Gold-Storage/product-reviews/B07DJHWT5V/ref=cm_cr_dp_d_show_all_top?ie=UTF8&reviewerType=all_reviews"
amazon_review <- NULL
for(i in 1:50)
  {
murl <- read_html(as.character(paste(aurl,i,sep="=")))
rev <- murl %>%
  html_nodes(" .review-text-content") %>%  ## starting of review 
  html_text()
amazon_review <- c(amazon_review,rev)
}
length(amazon_review)
setwd("C://Users//Acer//Desktop//Assignment//pending//NLP")
write.table(amazon_review,"M20.txt",row.names = F)
getwd()
###### NLP ###



moto_review <- readLines("C://Users//Acer//Desktop//Assignment//pending//NLP//M20.txt")
moto_review
length(moto_review)
## corpus of data 
review<- Corpus(VectorSource(moto_review))
review

######cleaning

review <- tm_map(review,toupper)
inspect(review[10])
review <- tm_map(review,removePunctuation)
inspect(review[10])
review <- tm_map(review,stripWhitespace)
inspect(review[10])
review <- tm_map(review,removeNumbers)
inspect(review[10])
review <- tm_map(review,removeWords,stopwords('english')) 
inspect(review[10])
stopword <- readLines(file.choose())
review <- tm_map(review,removeWords,stopword)
inspect(review[12])

# Remove URL's from corpus
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)

review<- tm_map(review, content_transformer(removeURL))
inspect(review[14])


#####EDA

## tdm or dtm 

dtm <- DocumentTermMatrix(review)
dim(dtm)
dtm$nrow
dtm$ncol
dtm$dimnames
dtm$i
dtm$v
dtm$j
dtm1 <- as.matrix(dtm)


## bag oe word 
ro <- apply(dtm1,1,sum)
ro


dtmnew <-dtm1[ro>0,]
View(dtmnew)


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
######  trust is maximum in the review
## disgust is min or least according to review 
## we can say that maximum number of coustmer are satisfied with product 
## so we can say that product is good 



