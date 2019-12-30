#Author: ZhuZheng(Iverson) ZHOU
#z-ing.net

if(!require(installr)) {
  install.packages("installr"); require(installr)}
updateR()

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(SnowballC)
library(wordcloud)
library(tm)
library(tidytext)
library(graph)
library(Rgraphviz)
library(topicmodels)


#library(rvest)
setwd("C:/Users/Zing/OneDrive/R/TextMining")
data <- read.csv('./wesearchdotcodotnz.csv', as.is=T, header=F,colClasses="character")


data1=data[,c(1,5:8)]
data1$ads=do.call(paste, c(data1[c("V7", "V8")], sep = "")) 
#data1$ads = paste(data1$V7, data1$V8, sep="_")
df <- data.frame(data1)

df %>%
  group_by(V5) %>%
  summarize(N_posts = n_distinct(V1)) %>%
  ggplot(aes(V5, N_posts)) +
  geom_col() +
  coord_flip()

#clean=gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$V8)
#clean=gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$V7)
#removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", df$ads) 




wesearchnz_Corpus <- Corpus(VectorSource(df$ads))
wesearchnz_Corpus <- tm_map(wesearchnz_Corpus, content_transformer(tolower))

for (i in c(1:2, 320)) {
  cat(paste0("[", i, "] "))
  writeLines(strwrap(as.character(wesearchnz_Corpus[[i]]), 60))
}


removeBR <-  function(x) gsub("(\n|<br />)", " ", x)
wesearchnz_Corpus <- tm_map(wesearchnz_Corpus, content_transformer(removeBR))

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
#removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
wesearchnz_Corpus <- tm_map(wesearchnz_Corpus, content_transformer(removeURL))

#wesearchnz_Corpus <- tm_map(wesearchnz_Corpus, removePunctuation) 
#wesearchnz_Corpus <- tm_map(wesearchnz_Corpus, removeNumbers)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
wesearchnz_Corpus <- tm_map(wesearchnz_Corpus, content_transformer(removeNumPunct))


for (i in c(1:2, 320)) {
  cat(paste0("[", i, "] "))
  writeLines(strwrap(as.character(wesearchnz_Corpus[[i]]), 60))
}
wesearchnz_CorpusCopy <- wesearchnz_Corpus





# stem words
 # wesearchnz_Corpus <- tm_map(wesearchnz_Corpus, stemDocument)
#for (i in c(1:2, 320)) {
#  cat(paste0("[", i, "] "))
#  writeLines(strwrap(as.character(wesearchnz_Corpus[[i]]), 60))
#}


stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

wesearchnz_Corpus <- lapply(wesearchnz_Corpus, stemCompletion2, dictionary=wesearchnz_CorpusCopy)
wesearchnz_Corpus <- Corpus(VectorSource(wesearchnz_Corpus))

for (i in c(1:2, 320)) {
  cat(paste0("[", i, "] "))
  writeLines(strwrap(as.character(wesearchnz_Corpus[[i]]), 60))
}



#Create a Term Document Matrix of our documents. Which reflects the number of times each term in the corpus is found in each of the documents. And add some readable columnnmes.
wesearchnz_tdm <- TermDocumentMatrix(wesearchnz_Corpus, control = list(wordLengths = c(1, Inf)))
wesearchnz_tdm


idx <- which(dimnames(wesearchnz_tdm)$Terms == "lost")
inspect(wesearchnz_tdm[idx + (0:5), 101:110])

(freq.terms <- findFreqTerms(wesearchnz_tdm, lowfreq = 30))
term.freq <- rowSums(as.matrix(wesearchnz_tdm))
term.freq <- subset(term.freq, term.freq >= 30)
df <- data.frame(term = names(term.freq), freq = term.freq)



# add extra stop words 
myStopwords <- c(stopwords('english'), "fontsize", "body", "accent", "also", "mm","us", "th", "can","d", "de", "fontsiz", "get", "grid", "href", "lenght"
                 , "lockedfals", "px", "p", "relnofollow", "sansserif", "year", "will", "wlsdexcept", "sinc", "semihiddenfals")
# remove "r" and "big" from stopwords
#myStopwords <- setdiff(myStopwords, c("r"))
# remove stopwords from corpus
wesearchnz_Corpus <- tm_map(wesearchnz_Corpus, removeWords, myStopwords)



wesearchnz_tdm <- TermDocumentMatrix(wesearchnz_Corpus, control = list(wordLengths = c(1, Inf)))
wesearchnz_tdm

(freq.terms <- findFreqTerms(wesearchnz_tdm, lowfreq = 30))
term.freq <- rowSums(as.matrix(wesearchnz_tdm))
term.freq <- subset(term.freq, term.freq >= 30)
df <- data.frame(term = names(term.freq), freq = term.freq)



#library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()






#df = gsub("[[:punct:]]", "", df)
#df= gsub("[[:digit:]]", "", df)
#df = gsub("[[:punct:]]", "", df)
#df = gsub("[[:digit:]]", "", df)
#df = gsub("http\\w+", "", df)
#df = gsub("[ \t]{2,}", "", df)
#df = gsub("^\\s+|\\s+$", "", df)

dtm <- TermDocumentMatrix(wesearchnz_Corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 20,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




#find freq
findFreqTerms(dtm, lowfreq = 30)
# words are associated with 'reward'?
findAssocs(dtm, terms = "reward", corlimit = 0.3)




#library(graph)
#library(Rgraphviz)

plot(dtm, term = freq.terms, corThreshold = 0.3, weighting = T)


plot(dtm,
     terms=findFreqTerms(dtm, lowfreq=30) ,
     corThreshold=0.3)


m <- as.matrix(wesearchnz_tdm)
word.freq <- sort(rowSums(m), decreasing = T)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]


wordcloud(words = names(word.freq), freq = word.freq, min.freq = 15,
          random.order = F, colors = pal)
n 


# remove sparse terms
wesearchnz_tdm2 <- removeSparseTerms(wesearchnz_tdm, sparse = 0.95)
m2 <- as.matrix(wesearchnz_tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward")
plot(fit)
rect.hclust(fit, k = 6) 

m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 6 # number of clusters
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3)



#library(topicmodels)
#topic modelling
dtm <- as.DocumentTermMatrix(wesearchnz_tdm)

lda <- LDA(dtm, k = 8) # find 8 topics
(term <- terms(lda, 6))
