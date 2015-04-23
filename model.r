
library(stringi)    # Character String Processing Facilities (general data information, word count etc..)
library(ggplot2)    # An Implementation of the Grammar of Graphics (visualization of data)
library(NLP)        # NLP infrastructure
library(tm)         # Text Mining Package (text mining tools)
library(RWeka)      # R/Weka interface (machine learning algorithms)

## load n-gram data from where we left off at milestone
load("nGram.RData")



## Frequencies 
#data.unigramsF <- rowSums(as.matrix(data.unigrams))
#data.unigramsT <- terms(data.unigrams)
data.bigrams <- removeSparseTerms(data.bigrams, 0.9999)
data.bigramsF <- rowSums(as.matrix(data.bigrams))
data.trigrams <- removeSparseTerms(data.trigrams, 0.9999)
data.trigramsF <- rowSums(as.matrix(data.trigrams))
data.quadrigrams <- removeSparseTerms(data.quadrigrams, 0.9999)
data.quadrigramsF <- rowSums(as.matrix(data.quadrigrams))
#colnames(data.quadrigramsF) <- c("nGram","Count")

#data.trigramsF <- names(rep(times = data.trigramsF))
#data.trigramsF <- do.call(rbind, Filter( length(3), data.trigramsF ))


start <- function(ngrams, n) { # ngrams=ngrams, n="n" in ngram, 
  termvector <- Terms(ngrams)
  wordvector <- strsplit(termvector, split=" ")
  beginning  <- character(length(termvector))
  for (i in 1:length(wordvector)) {
    beginning[i] <- paste(wordvector[[i]][1:(n-1)], sep="", collapse=" ")
  }
  return(beginning)
}

##Get the last word in each ngram.
end <- function(ngrams, n) { # ngrams=ngrams, n="n" in ngram, 
  termvector <- Terms(ngrams)
  wordvector <- strsplit(termvector, split=" ")
  last       <- character(length(termvector))
  for (i in 1:length(wordvector)) {
    last[i] <- wordvector[[i]][n]
  }
  return(last)
}


data.bigramsT <- data.bigrams$dimnames$Terms
data.bigramsS  <- start(data.bigrams,  2)
data.bigramsE   <- end(data.bigrams,  2)
data.trigramsT <- data.trigrams$dimnames$Terms
data.trigramsS  <- start(data.trigrams,  3)
data.trigramsE   <- end(data.trigrams,  3)
data.quadrigramsT <- data.quadrigrams$dimnames$Terms
data.quadrigramsS  <- start(data.quadrigrams,  4)
data.quadrigramsE   <- end(data.quadrigrams,  4)

rowf <- function(terms, rowsums) {
  rowsum <- numeric(length(terms))
  for (i in 1:length(terms)) {
    rowsum[i] <- rowsums[[i]]
  }
  return(rowsum)
}

data.bigramSum <- rowf(data.bigramsT, data.bigramsF)
data.trigramSum <- rowf(data.trigramsT, data.trigramsF)
data.quadrigramSum <- rowf(data.quadrigramsT, data.quadrigramsF)

# Square Matrix Too little info at quadrigrams may be leftover

data.bigramsB <- unique(c(data.bigramsS, data.bigramsE, "<end>"))
data.trigramsB <- unique(c(data.trigramsS, data.trigramsE, "<end>"))

data.bigramsAll <- cbind(data.bigramsS, data.bigramsE, data.bigramsF)
data.trigramsAll <- cbind(data.trigramsS, data.trigramsE, data.trigramsF)
data.quadrigramsAll <- cbind(data.quadrigramsS, data.quadrigramsE, data.quadrigramsF)

data.bigramsAll <- data.bigramsAll[order(-rank(data.bigramsF)),]
data.trigramsAll <- data.trigramsAll[order(-rank(data.trigramsF)),]
data.quadrigramsAll <- data.quadrigramsAll[order(-rank(data.quadrigramsF)),]

save(data.bigramsAll, data.trigramsAll, data.quadrigramsAll, file = "model.RData")

# ufirst  <- unique(data.bigramsS)
# usecond <- unique(data.bigramsE)
# first   <- data.bigramsS
# second  <- data.bigramsE
# uboth <- unique(c(ufirst, usecond, "<end>"))
# 
# # Square matrix
# 
# squarematf <- function(uboth, begin, last, rowsums) {
#   m <- matrix(0, nrow=length(uboth), ncol=length(uboth))
#   ufirst  <- unique(begin)
#   usecond <- unique(last)
#   for (i in 1:length(ufirst)) { ## Square array is needed to work with markovchain
#     rowword = ufirst[i]
#     ipositions = grep(paste("^", rowword, "$", sep=""), begin)
#     denominator = sum(rowsums[ipositions])
#     row = grep(paste("^", rowword, "$", sep=""), uboth)
#     for (j in 1:length(ipositions)) {
#       searchword <- last[ipositions[j]]
#       column <- grep(paste("^", searchword, "$", sep=""), uboth)
#       m[row, column] <- rowsums[ipositions[j]] / denominator
#     }
#   }
#   for (i in (length(ufirst) + 1):length(uboth)) {
#     m[i, length(uboth)] <- 1
#   }
#   return(m)	
# }
# 
# 
# mbi <- squarematf(data.bigramsB, data.bigramsS, data.bigramsE, data.bigramsF)
# 
# mrowsums <- apply(mbi, 1, sum)
# 
# mvector <- c(mbi)
# nrows = dim(mbi)[1]
# mc <- new("markovchain", transitionMatrix=matrix(c(mbi), nrow=nrows, byrow=FALSE, dimnames=list(uboth, uboth))) 
# 
# 
# 
# ## Predictions ##
# predict(mc, newdata="for")
# 
# 
# predict3 <- function(matrix, uboth, begin, last,  rowsums, input="<start>") { #rowsums for the ngrams not the matrix!  
#   if (is.null(input)) { input = "<start>" }
#   ufirst  <- unique(begin)
#   usecond <- unique(last)
#   row = grep(paste("^", input, "$", sep=""), uboth)
#   ipositions = grep(paste("^", input, "$", sep=""), begin)
#   frequencies = rowsums[ipositions]
#   words = last[ipositions]
#   nindex1 = which.max(frequencies); option1 = words[nindex1]; frequencies = frequencies[-nindex1]; words = words[-nindex1]
#   nindex2 = which.max(frequencies); option2 = words[nindex2]; frequencies = frequencies[-nindex2]; words = words[-nindex2]
#   nindex3 = which.max(frequencies); option3 = words[nindex3]
#   return(c(option1, option2, option3))
# }
# 
# 
# 
# predict3(mc, data.bigramsB, ufirst, usecond, data.bigramsF, "roses")
# 
