## ************************* Cleaning Functions ************************
library(tm)

### Remove empty words
remove_empty <- function(doc){
        aa <- doc == " "
        doc_ <- doc[!aa]
        bb <- doc_ == ""
        doc_ <- doc_[!bb]
        return(doc_)
}

textClean <- function(doc1){
        library(tm)
        
        ###**************** Creating corpus and Cleaning data  *********************
        corpus <- Corpus(VectorSource(doc1))
        
        ### Preparing data
        corpus<- tm_map(corpus, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))
        
        ### Remove punctuation
        corpus <- tm_map(corpus, removePunctuation)
        
        ### Remove numbers
        corpus <- tm_map(corpus, removeNumbers)
        
        ### lower case letters
        corpus <- tm_map(corpus, content_transformer(tolower))
        
        ### Remove url
        remURL <- function(x) gsub('http\\S+\\s*',"", x)
        corpus <- tm_map(corpus, content_transformer(remURL))
        
        # Eliminate extra white spaces
        corpus <- tm_map(corpus, stripWhitespace)
        
        ###************* Converting Final Corpus as data Frame  **********************
        Dtext <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
        text <- unlist(Dtext)
        
        
}