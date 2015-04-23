library(shiny)
library(stringi)    # Character String Processing Facilities (general data information, word count etc..)
library(ggplot2)    # An Implementation of the Grammar of Graphics (visualization of data)
library(NLP)        # NLP infrastructure
library(tm)         # Text Mining Package (text mining tools)
library(RWeka)      # R/Weka interface (machine learning algorithms)

## load n-gram data from where we left off at milestone
load("model.RData")

colnames(data.bigramsAll) <- c("Start","End","Frequency") 
colnames(data.trigramsAll) <- c("Start","End","Frequency") 
colnames(data.quadrigramsAll) <- c("Start","End","Frequency") 

data.bigramsAll = as.data.frame(data.bigramsAll)
data.trigramsAll = as.data.frame(data.trigramsAll)
data.quadrigramsAll = as.data.frame(data.quadrigramsAll)

predict <-function(input) {
  sw <- stopwords(kind = "en")
  input <- removePunctuation(input)
  input <- removeNumbers(input)
  input <- rev(unlist(strsplit(input," ")))
  input <- input[grepl('[[:alpha:]]',input)]
  input <- tolower(input) 
  linput <- paste(input[2],input[1],sep = ' ')
  if(linput == ''|linput == "na na") return('No string to predict!')
  if (length(input) == 1){
    uW <- data.bigramsAll[data.bigramsAll$Start == input[1],]
    return(uW)
  }
  else if(length(input) == 2){
    uW <- head(data.bigramsAll[data.bigramsAll$Start == input[2],],5)
    biW <- head(data.trigramsAll[data.trigramsAll$Start == paste(input[2],input[1],sep = ' '),],5)
    return(rbind(biW,uW))
  }
  else {
    uW <- head(data.bigramsAll[data.bigramsAll$Start == input[2],],5)
    biW <- head(data.trigramsAll[data.trigramsAll$Start == paste(input[2],input[1],sep = ' '),],5)
    tW <- head(data.quadrigramsAll[data.quadrigramsAll$Start == paste(input[3],input[2],input[1],sep = ' '),],5)
    return(rbind(tW,biW,uW))
  }
}

upcoming <- function(input){
  result <- predict(input)
  x <- head(unique(subset(result, select=c("End"))),5)
  return(x)
}


shinyServer(
  function(input, output) {
    observe({

      prediction <- upcoming(input$word)
      output$first <- renderText({ 
        paste("First Possiblity:    ", prediction[1,])
      })
      output$second <- renderText({ 
        paste("Second Possiblity: ", prediction[2,])
      })
      output$third <- renderText({ 
        paste("Third Possiblity:  ", prediction[3,])
      })
      
    })    
  }
)