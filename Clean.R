## ************************* Cleaning Functions ************************
### Remove empty words
remove_empty <- function(doc){
        aa <- doc == " "
        doc_ <- doc[!aa]
        bb <- doc_ == ""
        doc_ <- doc_[!bb]
        return(doc_)
}

textClean <- function(doc1){
        ### Lowercase words
        doc1 <- tolower(doc1)
        
        ### Remove punctuation : "." "," "!" "?" "@" "/" "|" "(" ")" ";" -->web pages are kept
        doc1 <- gsub("\\. +|\\...|,|!|\\?|@|/|\\||\\(|\\)|;|\\.$", " ", doc1)
        
        ##Removing Na values
        jj <- is.na(doc1)
        doc1 <- doc1[!jj]
        
        ### Splitting web pages links
        ## doc1 <- unlist(strsplit(doc1, "(?=[.])", perl=TRUE))  ## splitting by "." keeping "."
        doc1 <- (strsplit(doc1, "(?=[.])", perl=TRUE))  ## splitting by "." keeping "."
        ## creating .es  .com words
        tt <- which(doc1 %in% ".")
        doc1[tt+1] <- paste(doc1[tt], doc1[tt+1],  sep="")
        
        doc1 <- gsub("[\\b.\\b]$", "", doc1) ## remove isolate "."
        
        
        ### Remove numbers
        doc1 <- gsub(" *([0-9]) *"," ", doc1)
        
        ### Remove special symbols : any characters followed by at least one symbol followed by any character
        doc1 <- gsub("[$]+", " ", doc1)
        
        # Eliminate extra white spaces
        doc1 <- gsub("[  ]+", " ", doc1)
        
        
        ### Keep only normal characters
        doc1 <- grep("([a-zA-Z]+)", doc1, value=TRUE)
        
        ### Remove stange symbols
        doc1 <- gsub("â???|o", "", doc1)
        doc1 <- gsub("+|-", "", doc1)
        
        
        # Eliminate extra white spaces
        doc1 <- gsub("[  ]+", " ", doc1)
        doc1 <- gsub(" ", "", doc1)
        
        ### Remove empty words
        doc1 <- remove_empty(doc1)
        
}