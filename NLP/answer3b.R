library(rvest)
library(XML)
library(magrittr)


###review
aurl <- "https://www.amazon.in/OnePlus-Pro-Nebula-Blue-256GB/product-reviews/B07HGJK535/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber="
one_plus_review <- NULL
murl <- read_html(as.character(paste(aurl,sep="=")))
rev <- murl %>%
  html_nodes(".review-text-content") %>%
  html_text()
one_plus_review <- c(one_plus_review,rev)
setwd("C://Users//Acer//Desktop//Assigment//NLP")
write.table(one_plus_review,"one_plus.txt",row.names = F)
getwd()


##
library(tm)
library(NLP)
library(syuzhet)
library(topicmodels)
library(slam)


seven_pro_review <- readLines("C:/Users/Acer/Desktop/Assignment/NLP/one_plus.txt")
View(seven_pro_review)
length(seven_pro_review)

review <- Corpus(VectorSource(seven_pro_review))

##cleaning of data
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


####EDA
tdm <- TermDocumentMatrix(review)
dim(tdm)
tdm$nrow
tdm$dimnames
tdm1 <- as.matrix(tdm)
View(tdm1)


#### bag of word

bago <- apply(tdm1,1,sum)
View(bago)

tdmnew <- tdm1[bago>0]
View(tdmnew)
table(tdmnew)
##

lda <- LDA(tdmnew,5)
