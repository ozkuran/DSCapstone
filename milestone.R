
library(stringi)    # Character String Processing Facilities (general data information, word count etc..)
library(ggplot2)    # An Implementation of the Grammar of Graphics (visualization of data)
library(NLP)        # NLP infrastructure
library(tm)         # Text Mining Package (text mining tools)
library(RWeka)      # R/Weka interface (machine learning algorithms)

twitterFile <- './final/en_US/en_US.twitter.txt'
blogFile <- './final/en_US/en_US.blogs.txt'
newsFile <- './final/en_US/en_US.news.txt'

twitterData <- readLines(twitterFile)
blogsData <- readLines(blogFile)
newsData <- readLines(newsFile)

twitterStats <- stri_stats_general(twitterData)
blogsStats <- stri_stats_general(blogsData)
newsStats <- stri_stats_general(newsData)

twitterWords <- stri_count_words(twitterData)
blogsWords <- stri_count_words(blogsData)
newsWords <- stri_count_words(newsData)

twitterStats
blogsStats
newsStats

summary(twitterWords)
summary(blogsWords)
summary(newsWords)

sum(twitterWords)
sum(blogsWords)
sum(newsWords)

qplot(twitterWords, binwidth = 2)  + xlim(0,50) + ylab("Count") + xlab("Word Count per Tweet")
qplot(blogsWords, binwidth = 2)  + xlim(0,250) + ylab("Count") + xlab("Word Count per Blog Posts")
qplot(newsWords, binwidth = 2)  + xlim(0,250) + ylab("Count") + xlab("Word Count per News Article")

set.seed(54321)

sampleDataTwitter <- sample(twitterData, 50000) 
sampleDataBlogs <- sample(blogsData, 25000) 
sampleDataNews <- sample(newsData, 2000) 

data.raw <- c(sampleDataTwitter, sampleDataBlogs, sampleDataNews)
#data.raw <- c(twitterData, blogsData, newsData)
data.badwords <- readLines("badwords.txt")
data.stopwords <- stopwords('english')

cleanData <- function (d)
{
  clean <- Corpus(VectorSource(d))
  clean <- tm_map(clean, content_transformer(tolower))
  clean <- tm_map(clean, removePunctuation)
  clean <- tm_map(clean, removeNumbers)
  clean <- tm_map(clean, stripWhitespace)
#  clean <- tm_map(clean, stemDocument) steming causes errors
#  clean <- tm_map(clean, removeWords, c(data.stopwords,data.badwords)) stop words could be meanful
  clean <- tm_map(clean, removeWords, c(data.badwords))
  return (clean) 
}

data.clean <- cleanData(data.raw)



#unigram <-function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min=1, max=1))
bigram <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min=2, max=2))
trigram <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min=3, max=3))
quadrigram <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min=4, max=4))


options(mc.cores=1)
#data.unigrams <- TermDocumentMatrix(data.clean , control=list(tokenize=unigram))
data.bigrams <- TermDocumentMatrix(data.clean , control=list(tokenize=bigram))
data.trigrams <- TermDocumentMatrix(data.clean , control=list(tokenize=trigram))
data.quadrigrams <- TermDocumentMatrix(data.clean , control=list(tokenize=quadrigram))

#wordlist <- unique(data.unigrams$dimnames$Terms)

#save(data.unigrams, data.bigrams, data.trigrams, data.quadrigrams, wordlist, file = "nGram.RData")
save(data.bigrams, data.trigrams, data.quadrigrams, file = "nGram.RData")


#data.unigramsDF <- data.frame(table(data.unigrams))
#data.unigramsDF <- data.unigramsDF[order(-data.unigramsDF$Freq), ]
#head(data.unigramsDF,10)

#data.bigramsRS <- rowSums(as.matrix(data.bigrams))
#data.trigramsRS <- rowSums(as.matrix(data.trigrams))
#data.quadrigramsRS <- rowSums(as.matrix(data.quadrigrams))



