



## ************  week 1: Task 0 *************************


##  This exercise uses the files named LOCALE.blogs.txt where LOCALE 
##  is the each of the four locales en_US, de_DE, ru_RU and fi_FI

### Articles:
## http://journal.code4lib.org/articles/11626


### Loading libraries
library(tm)

### Defining path to files
path <- "./final"

rep1 <- "de_DE"
rep2 <- "en_US"
rep3 <- "fi_FI"
rep4 <- "ru_RU"

dir1 <- paste(path, rep1, sep="/")
dir2 <- paste(path, rep2, sep="/")
dir3 <- paste(path, rep3, sep="/")
dir4 <- paste(path, rep4, sep="/")


name1 <- "de_DE.blogs.txt"
name2 <- "en_US.blogs.txt"
name3 <- "fi_FI.blogs.txt"
name4 <- "ru_RU.blogs.txt"

file_dir1 <- paste(dir1, name1, sep="/")
file_dir2 <- paste(dir2, name2, sep="/")
file_dir3 <- paste(dir3, name3, sep="/")
file_dir4 <- paste(dir4, name4, sep="/")


### Regrouping text files in a Corpus directory
#### Creating a new directory, if it exists it will not be crashed
corp <- "corpus"
dir.create(file.path(path, corp))

#### Copying files
new_dir <- paste(path, corp, sep="/")

file.copy(file_dir1, new_dir, overwrite = FALSE)
file.copy(file_dir2, new_dir, overwrite = FALSE)
file.copy(file_dir3, new_dir, overwrite = FALSE)
file.copy(file_dir4, new_dir, overwrite = FALSE)


### Reading files using scan
text_1 <- scan(file_dir1, what = "character")
text_2 <- scan(file_dir2, what = "character")


### Reading files using "tm" package
text <- Corpus(DirSource(new_dir))
save(text, file="./text_backup.dat")
load(file="./text_backup.dat")
#### read documents 
writeLines(as.character(text[[1]]))


##---------  Quizz week 1 --------------------


# q2 : 
path <- "./final"
rep2 <- "en_US"
dir2 <- paste(path, rep2, sep="/")
name <- "en_US.twitter.txt"
file_dir2 <- paste(dir2, name, sep="/")
q2 <- readLines(file_dir2)


# q3 :
path <- "./final"
rep2 <- "en_US"
dir2 <- paste(path, rep2, sep="/")

name1 <- "en_US.blogs.txt"
name2 <- "en_US.news.txt"
name3 <- "en_US.twitter.txt"

file_dir1 <- paste(dir2, name1, sep="/")
file_dir2 <- paste(dir2, name2, sep="/")
file_dir3 <- paste(dir2, name3, sep="/")

file1 <- readLines(file_dir1)
file2 <- readLines(file_dir2)
file3 <- readLines(file_dir3)

max(nchar(file1))
max(nchar(file2))
max(nchar(file3))



# q4 :
# using file3 from previous question

# Number of lines where "love" occurs:
love <- grep(pattern = "love", file3 )
length(love)

# Number of lines where "hate" occurs:
hate <- grep(pattern = "hate", file3 )
length(hate)



# q5 :
line_bio <- grep(pattern = "biostats", file3)
file3[line_bio]


# q6:
## How many tweets have those characters:
## "A computer once beat me at chess, but it was no match for me at kickboxing"


q5 <- grep(pattern= "A computer once beat me at chess, but it was no match for me at kickboxing", file3)

file3[q5]

## -------------------  ENd QUIZ 1 ----------------------------------------------------

## --------------------- END Week 1 ---------------------------------------------------



##  -------------------- WEEk 2  ------------------------------------------------

## ********************* 1) Exploratory data analysis ***************************

# https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know#step-2-install-and-load-the-required-packages

# https://thachtranerc.wordpress.com/2016/04/12/katzs-backoff-model-implementation-in-r/

## ENGLISH data set
### Defining path to files
library(tm)
library(SnowballC)
library(ngramrr)
library(data.table)
library(tidyr)

path <- "./final"

rep2 <- "en_US"
dir2 <- paste(path, rep2, sep="/")

name1 <- "en_US.blogs.txt"
name2 <- "en_US.news.txt"
name3 <- "en_US.twitter.txt"

file_dir1 <- paste(dir2, name1, sep="/")
file_dir2 <- paste(dir2, name2, sep="/")
file_dir3 <- paste(dir2, name3, sep="/")

file1 <- readLines(file_dir1, encoding = "UTF-8", skipNul = TRUE)
file2 <- readLines(file_dir2, encoding = "UTF-8", skipNul = TRUE)
file3 <- readLines(file_dir3, encoding = "UTF-8", skipNul = TRUE)

### Subsampling
set.seed(5231)

perc <- 0.10 ##10% of lines
sfile1 <- sample(file1, size=length(file1)*perc, replace = FALSE)
sfile2 <- sample(file2, size=length(file2)*perc, replace = FALSE)
sfile3 <- sample(file3, size=length(file3)*perc, replace = FALSE)

sfile1 <- strsplit(sfile1, split=" ")

sTotal <- c(sfile1, sfile2, sfile3)
remove(file1, file2, file3,sfile1,sfile2,sfile3)
gc()

###**************** Creating corpus and Cleaning data  *********************
corpus <- Corpus(VectorSource(sTotal))

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


### Save Plain text and corpus
save(corpus, file="./corpus.RData")



###************* Converting Final Corpus as data Frame  **********************
Dtext <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
text <- unlist(Dtext)
remove(Dtext,corpus,sTotal)
gc()

## ******** Cleaning functions *************

## this function removes rows with empty spaces
bigram_clean <- function(bigram){
        ## removing empty rows
        aa <- which(bigram[,1] == "" | bigram[,2] == "")
        if (length(aa)>0){
                mat <- bigram[-aa,]
        }
        
        ## removing row when word1 == word2
        aa <- which(mat[,1] == mat[,2])
        if (length(aa)>0){
                mat <- mat[-aa,]
        }
        
        ## removing NA values
        aa <- which(is.na(mat[,1])|is.na(mat[,2]))
        if (length(aa)>0){
                mat <- mat[-aa,]
        }
        return(mat)
}


## this function removes rows with empty spaces
trigram_clean <- function(trigram){
        ## removing empty rows
        aa <- which(trigram[,1] == "" | trigram[,2] == ""| trigram[,3] == "")
        if (length(aa)>0){
                mat <- trigram[-aa,]
        }
        
        ## removing row when word1 == word2
        aa <- which(mat[,1] == mat[,2] | mat[,2] == mat[,3] | mat[,1] == mat[,3])
        if (length(aa)>0){
                mat <- mat[-aa,]
        }
        
        ## removing NA values
        aa <- which(is.na(mat[,1])|is.na(mat[,2])|is.na(mat[,3]))
        if (length(aa)>0){
                mat <- mat[-aa,]
        }
        return(mat)
}



quadgram_clean <- function(quadgram){
        ## removing empty rows
        aa <- which(quadgram[,1] == "" | quadgram[,2] == ""| quadgram[,3] == ""| quadgram[,4] == "")
        if (length(aa)>0){
                mat <- quadgram[-aa,]
        }
        
        ## removing row when word1 == word2
        aa <- which(mat[,1] == mat[,2] | mat[,2] == mat[,3] | mat[,1] == mat[,3] | mat[,3] == mat[,4])
        if (length(aa)>0){
                mat <- mat[-aa,]
        }
        
        ## removing NA values
        aa <- which(is.na(mat[,1])|is.na(mat[,2])|is.na(mat[,3])|is.na(mat[,4]))
        if (length(aa)>0){
                mat <- mat[-aa,]
        }
        return(mat)
}



## This function selects the 4000 first words most used
bigram_sel <- function(doc, words=4000) {
        
        stpw <- stopwords("english")
        noun <- 5
        
        ## One of the 2 words must not be a stop word
        list1 <- which(doc[,1] %in% stpw)
        list2 <- which(doc[,2] %in% stpw)
        match <- intersect(list1, list2)
        doc <- doc[-match,]
        
        numw <- min(words, nrow(doc))
        
        ## Getting 5 bigrams for each unique first word
        uni <- unique(doc[,1])
        #trob <- function(i){
        #        indi <- which(doc[,1] %in% uni[i])
        #        mw <- min(length(indi), noun)
        #        if (length(indi)>0){
        #                fin <- c(indi[1:mw])  
        #        }
        #}
        
        fin <- NULL
        words <- min(words, length(uni))
        
        #i <- seq(1:words)
        #llista <- lapply(i , function(x) append(fin,trob(x)))  ##4000 values ->9 min
        
        #start.time <- Sys.time()
        for (i in  1 :words){
                indi <- which(doc[,1] %in% uni[i])
                mw <- min(length(indi), noun)
                if (length(indi)>0){
                        fin <- c(fin, indi[1:mw])  
                }
        }
        
        #end.time <- Sys.time()
        #time.taken <- end.time - start.time
        #time.taken
        
        doc <- doc[fin,]
        return(doc)
        
}




## This function selects the 4000 first words most used
trigram_sel <- function(doc, words=600, words2=100) {
        
        stpw <- stopwords("english")
        noun <- 5
        
        ## One of the 3 words must not be a stop word
        list1 <- which(doc[,1] %in% stpw)
        list3 <- which(doc[,3] %in% stpw)
        match <- intersect(list1, list3)
        doc <- doc[-match,]
        list3 <- which(doc[,3] %in% stpw)
        doc <- doc[-list3,]
        
        numw <- min(words, nrow(doc))
        
        ## Getting 5 bigrams for each unique first word
        uni <- unique(doc[,1])
        fin <- NULL
        ws <- min(words, length(uni))
        uni <- uni[1:words]
        
        a2 <- which(doc[,1] %in% uni)
        doc<- doc[a2,]
        doc2 <- NULL
        
        for (i in  1 :ws){
                print(i)
                indi <- which(doc[,1] %in% uni[i])
                pard <- unique(doc[indi,2])
                w2 <- min(length(pard), words2)
                pard <- pard[1:w2]
                for (j in 1:w2){
                        yindi <- which(doc[,1] %in% uni[i] & doc[,2] %in% pard[j])
                        mw <- min(length(yindi), noun)
                        if (mw>0){
                                doc2 <- rbind(doc2, doc[yindi[1:mw],])  
                        }
                }
                doc <- doc[-indi,]
                
        }
        #doc <- doc[fin,]
        return(doc2)
        #return(doc)
        
}




## This function selects the 4000 first words most used
quadgram_sel <- function(doc, words=400, words2=80, words3=25) {
        
        stpw <- stopwords("english")
        noun <- 5
        
        ## One of the 3 words must not be a stop word
        list2 <- which(doc[,2] %in% stpw)
        list4 <- which(doc[,4] %in% stpw)
        match <- intersect(list2, list4)
        doc <- doc[-match,]
        list4 <- which(doc[,4] %in% stpw)
        doc <- doc[-list4,]
        
        numw <- min(words, nrow(doc))
        
        ## Getting 5 bigrams for each unique first word
        uni <- unique(doc[,1])
        fin <- NULL
        ws <- min(words, length(uni))
        uni <- uni[1:words]
        
        a2 <- which(doc[,1] %in% uni)
        doc<- doc[a2,]
        doc2 <- NULL
        
        for (i in  1 :ws){
                print(i)
                indi <- which(doc[,1] %in% uni[i])
                pard <- unique(doc[indi,2])
                w2 <- min(length(pard), words2)
                pard <- pard[1:w2]
                for (j in 1:w2){
                        yindi <- which(doc[,1] %in% uni[i] & doc[,2] %in% pard[j])
                        pard2 <- unique(doc[yindi,3])
                        w3 <- min(length(pard2), words3)
                        pard2 <- pard[1:w3]
                        for (k in 1:w3){
                                yindi2 <- which(doc[,1] %in% uni[i] & doc[,2] %in% pard[j] & doc[,3] %in% pard2[k])
                                mw <- min(length(yindi2), noun)
                                if (mw>0){
                                        doc2 <- rbind(doc2, doc[yindi2[1:mw],])  
                                }
                        }
                }
                doc <- doc[-indi,]
                
        }
        #doc <- doc[fin,]
        return(doc2)
        #return(doc)
        
}

## *******************************************************************



###************* TOKENIZATION ***********************
## Tokenization = read text to break it into words and sentences and turn it into N-grams

## bigram 3min
Context <- ""
finword <- paste(Context, text, collapse = " ")
finword <- unlist(strsplit(finword, split=" "))
finword <- finword[-1]
save(finword, file="./finwords.RData")
remove(text)
gc()

bigram <- lapply(ngrams(finword, 2), paste, collapse= " " )
bigram <- unlist(bigram)

save(bigram, file= "./big_tm.RData")
remove(bigram)
gc()

## Trigram
trigram <- lapply(ngrams(finword, 3), paste, collapse= " " )
trigram <- unlist(trigram)

save(trigram, file= "./tri_tm.RData")
remove(trigram)
gc()

##QuadGram
quadgram <- lapply(ngrams(finword, 4), paste, collapse= " " )
quadgram <- unlist(quadgram)

save(quadgram, file= "./quad_tm.RData")
remove(quadgram)
gc()

remove(finword)
gc()

## Converting into Data frames: word1, word2, word3, freq
### Bigram
load(file= "./big_tm.RData")
bigram <- table(as.character(bigram))       ## Getting unique bigrams count
bigram <- sort(bigram, decreasing = TRUE)   ## Sorting unique bigrams 
save(bigram, file = "./big_sort.RData")


bigram <- data.frame(bigram, stringsAsFactors = FALSE)   ##Creating data frame
bigram <- data.frame(word = as.character(bigram[,1]), Freq = bigram[,2], stringsAsFactors = FALSE)
bigram <- separate(bigram, col="word", into = paste("word", 1:2, sep=" "))   ##Separating each bigram in 2 cols
colnames(bigram) <- c("word1", "word2", "Freq")

bigram <- bigram_clean(bigram)
print(object.size(bigram), units="Mb")
start.time <- Sys.time()
bigram <- bigram_sel(bigram) ## 6min
end.time <- Sys.time()
time.taken1 <- end.time - start.time
time.taken1
save(bigram, file = "./big_sel.RData")
remove(bigram)
gc()

### trigram
load( file= "./tri_tm.RData")
trigram <- table(as.character(trigram))       ## Getting unique bigrams count
trigram <- sort(trigram, decreasing = TRUE)   ## Sorting unique bigrams 
save(trigram, file = "./tri_sort.RData")

trigram <- data.frame(trigram, stringsAsFactors = FALSE)   ##Creating data frame
trigram <- data.frame(word = as.character(trigram[,1]), Freq = trigram[,2], stringsAsFactors = FALSE)
trigram <- separate(trigram, col="word", into = paste("word", 1:3, sep=" "))   ##Separating each bigram in 2 cols
colnames(trigram) <- c("word1", "word2","word3", "Freq")

trigram <- trigram_clean(trigram)
save(trigram, file="./tri_prim.RData")
print(object.size(trigram), units="Mb")
start.time <- Sys.time()
trigram <- trigram_sel(trigram)  ## 32min(600words + 100 words2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
save(trigram, file = "./tri_sel.RData")
remove(trigram)
gc()

### quadgram
load(file= "./quad_tm.RData")
quadgram <- table(as.character(quadgram))       ## Getting unique bigrams count
quadgram <- sort(quadgram, decreasing = TRUE)   ## Sorting unique bigrams 
save(quadgram, file = "./quad_sort.RData")

quadgram <- data.frame(quadgram, stringsAsFactors = FALSE)   ##Creating data frame
quadgram <- data.frame(word = as.character(quadgram[,1]), Freq = quadgram[,2], stringsAsFactors = FALSE)
quadgram <- separate(quadgram, col="word", into = paste("word", 1:4, sep=" "))   ##Separating each bigram in 2 cols
colnames(quadgram) <- c("word1", "word2","word3", "word4", "Freq")

quadgram <- quadgram_clean(quadgram)
save(quadgram, file="./quad_prim.RData")
print(object.size(quadgram), units="Mb")
start.time <- Sys.time()
quadgram <- quadgram_sel(quadgram)  ## 3.65h(400words + 80words2+ 25words3)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
save(quadgram, file = "./quad_sel.RData")


## **************  Preparing prediction model  ***********************

load(file = "./big_sel.RData")
load(file = "./tri_sel.RData")

source("Katz_Back-off_1.0.R")

parc <- discount(bigram, trigram)

bigram <- parc[[1]]
trigram <- parc[[2]]

save(bigram, file="./bigram_fin.RData")
save(trigram, file="./trigram_fin.RData")

















## *************  Cleaning using common fucntions & tm package ********************
Clean_string <- function(string){
        
        ## Lowercase
        temp <- tolower(string)
        
}

file1 <- Clean_string(file1)
file2 <- Clean_string(file2)
file3 <- Clean_string(file3)

corpus1 <- Corpus(VectorSource(file1))
corpus2 <- Corpus(VectorSource(file2))
corpus3 <- Corpus(VectorSource(file3))

### Remove punctuation
corpus1 <- tm_map(corpus1, removePunctuation)
corpus2 <- tm_map(corpus2, removePunctuation)
corpus3 <- tm_map(corpus3, removePunctuation)


### Remove numbers
corpus1 <- tm_map(corpus1, removeNumbers)
corpus2 <- tm_map(corpus2, removeNumbers)
corpus3 <- tm_map(corpus3, removeNumbers)


### Removing common words
corpus1 <- tm_map(corpus1, removeWords, stopwords("english"))
corpus2 <- tm_map(corpus2, removeWords, stopwords("english"))
corpus3 <- tm_map(corpus3, removeWords, stopwords("english"))

# Eliminate extra white spaces
corpus1 <- tm_map(corpus1, stripWhitespace)
corpus2 <- tm_map(corpus2, stripWhitespace)
corpus3 <- tm_map(corpus3, stripWhitespace)

# Simple transforms
toSpace <- content_transformer(function(x,pattern) gsub(pattern, "  ", x))

corpus1 <- tm_map(corpus1, toSpace, "/|@|\\|")  ## Removing "/" "@" and "|"
corpus2 <- tm_map(corpus2, toSpace, "/|@|\\|")
corpus3 <- tm_map(corpus3, toSpace, "/|@|\\|")

save(corpus1, file="./corpus1.RData")
save(corpus2, file="./corpus2.RData")
save(corpus3, file="./corpus3.RData")

# Stemming : SnowBallC package needed : removes common word endings (ed, es, s)
#corpus1 <- tm_map(corpus1, stemDocument)
#corpus2 <- tm_map(corpus2, stemDocument)
#corpus3 <- tm_map(corpus3, stemDocument)

save(corpus1, file="./corpus1.RData")
save(corpus2, file="./corpus2.RData")
save(corpus3, file="./corpus3.RData")
## **********************************************************************


### Treat the processed doc as text document
docs <- tm_map(docs, PlainTextDocument )

## ********************************************************************



## ************ Build a term-document matrix **************************
dtm1 <- TermDocumentMatrix(corpus1)
dtm2 <- TermDocumentMatrix(corpus2)
dtm3 <- TermDocumentMatrix(corpus3)

save(dtm1, file="./dtm1.RData")
save(dtm2, file="./dtm2.RData")
save(dtm3, file="./dtm3.RData")

load(file="./dtm1.RData")
load(file="./dtm2.RData")
load(file="./dtm3.RData")


## Removing Sparse Terms
test1 <- removeSparseTerms(dtm1, 0.1)

temp1 <- inspect(dtm1)
freqMat1 <- data.frame(NAMES = rownames(temp1), FREQ = rowSums(temp1))
row.names(freqMat1) <- NULL
freqMat1 <- order(freqMat1, decreasing = FALSE)
## ************* ENDING :  Cleaning using common fucntions & tm package ********************



## *************  Cleaning using common fucntions ********************

### Lowercase words
doc1 <- tolower(file1)

### Remove punctuation : "." "," "!" "?" "@" "/" "|" "(" ")" ";"
doc1 <- gsub("\\.|,|!|\\?|@|/|\\||\\(|\\)|;", " ", doc1)

### Remove numbers
doc1 <- gsub(" *([0-9]) *"," ", doc1)

### Remove special symbols : any characters followed by at least one symbol followed by any character
doc1 <- gsub("[$]+", " ", doc1)

# Eliminate extra white spaces
doc1 <- gsub("[  ]+", " ", doc1)


### Keep only normal characters
doc1 <- grep("([a-zA-Z]+) +", doc1, value=TRUE)


### removing common words
paste_list <- function(list){
        for (i in 1:length(list)){
                if (i == 1) {
                        final <- paste("\\b",cwords[i], "\\b", sep="")
                } else {
                        final <- paste(final,"|\\b",cwords[i], "\\b", sep="")
                }
                
        }
        return(final)
}

cwords <- stopwords("english")
list <- paste_list(cwords)
# doc1 <- gsub(list, "", doc1 )  ## ens interessa


### Remove stange symbols
doc1 <- gsub("â???|o", "", doc1)
doc1 <- gsub("+|-", "", doc1)


# Eliminate extra white spaces
doc1 <- gsub("[  ]+", " ", doc1)
doc1 <- gsub(" ", "", doc1)

### Remove empty words
remove_empty <- function(doc){
        aa <- doc == " "
        doc_ <- doc[!aa]
        bb <- doc_ == ""
        doc_ <- doc_[!bb]
        return(doc_)
}

doc1 <- remove_empty(doc1)


## Saving
save(doc1, file="./doc1.RData")





##------------------------ This one ----------------------------
## */*******************************************************************/

### Defining path to files
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(caret)
library(data.table)
library(tidyr)


path <- "./final"

rep2 <- "en_US"
dir2 <- paste(path, rep2, sep="/")

name1 <- "en_US.blogs.txt"
name2 <- "en_US.news.txt"
name3 <- "en_US.twitter.txt"

file_dir1 <- paste(dir2, name1, sep="/")
file_dir2 <- paste(dir2, name2, sep="/")
file_dir3 <- paste(dir2, name3, sep="/")

file1 <- scan(file_dir1, what = "character")
file2 <- scan(file_dir2, what = "character")
file3 <- scan(file_dir3, what = "character")


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
        #doc1 <- grep("([a-zA-Z]+)", doc1, value=TRUE)
        
        ### Remove stange symbols
        doc1 <- gsub("â???|o", "", doc1)
        doc1 <- gsub("+|-", "", doc1)
        
        
        # Eliminate extra white spaces
        doc1 <- gsub("[  ]+", " ", doc1)
        doc1 <- gsub(" ", "", doc1)
        
        ### Remove empty words
        doc1 <- remove_empty(doc1)
        
}

## This function removes repeated values: ex: in in ; at at ; etc
Pair_clean <- function(doc){
        k <- NULL
        for (i in 1:length(doc)){
                aa <- unlist(strsplit(as.character(doc[i]), split=" " ))
                if (aa[1] == aa[2]){
                        k <- c(k,i)
                }
        }
        doc <- doc[-k]
        return(doc)
        
}

## This function removes repeated values: ex: in in at ; at the at ;on the the ,  etc
Pair_clean3 <- function(doc){
        k <- NULL
        for (i in 1:length(doc)){
                aa <- unlist(strsplit(as.character(doc[i]), split=" " ))
                if (aa[1] == aa[2] | aa[2] == aa[3] | aa[1] == aa[3]){
                        k <- c(k,i)
                }
        }
        doc <- doc[-k]
        return(doc)
        
}

#Keep the top 6 paired for each starter word :
## endword condition : 2 highest frequent words + 2 nouns/words out of the list of stopwords
Pair_unique <- function(doc, words=2000) {
        stpw <- stopwords("english")
        art <-2
        noun <- 6
        
        #un <- sapply(strsplit(as.character(doc[,1]), split = " ", fixed = TRUE), head, 1) #split and take the 1st word
        #uni <- unique(un)
        pard <- which(doc[,2] %in% stpw)
        doc <- doc[-pard,]
        uni <- unique(doc[,1])
        fin <- NULL
        words <- min(words, length(uni))  ## we keep min between 2000 words and length(unique words)
        #for (i in  1 :words){
        #        indi <- which(doc[,1] %in% uni[i])
        #        mw <- min(length(indi), art)
        #        if (length(indi)>0){
        #                fin <- c(fin, indi[1:mw])  
        #        }
        #}
        #doc <- doc[fin,]
        
        ##2ond part, we take 6 nouns
        list <- which(doc[,1] %in% uni[1:words])
        doc <- doc[list,]
        
        for (i in  1 :words){
                list <- which(doc[,1] %in% uni[i])
                list <- list[1:noun]
                #doc2 <- 
                fin <- c(fin, list)
        }
        doc <- doc[fin,]
        return(doc)

}

##******************************************************************

## We get the 30% of the beginning of each file
file1 <- file1[1:(0.3*length(file1))]
file2 <- file2[1:(0.3*length(file2))]
file3 <- file3[1:(0.3*length(file3))]

doc1 <- textClean(file1)
doc2 <- textClean(file2)
doc3 <- textClean(file3)

endoc <- c(doc1, doc2, doc3)
save(endoc, file="./endoc.RData")
## Unlisting for frequency counting
unlist <- unlist(endoc)
freq <- table(unlist1)
freq <- sort(freq, decreasing = TRUE)

## creating a word cloud
pal2 <- brewer.pal(8,"Dark2")
wordcloud(names(freq), freq, min.freq=2000, max.words=Inf, random.order=FALSE, 
          colors=pal2)

## plotting barplot
freqPlot <- data.frame(freq[1:15])
plot_freq <- ggplot(freqPlot,aes(x=unlist1, y=Freq))+
        geom_bar(stat="identity", aes(fill=as.factor(unlist1)))

##****************************************************


## ******* 2 ngrams  ******************
## Pairs of words
pair_list <- lapply(ngrams(doc1, 2), paste, collapse= " " ) 
freqPair <- sort(table(unlist(pair_list)), decreasing = TRUE)
## Plotting
pairPlot <- data.frame(freqPair[1:15])
plot_pair <- ggplot(pairPlot,aes(x=Var1, y=Freq))+
        geom_bar(stat="identity", aes(fill=as.factor(Var1)))
## *******************************


## ******* 3 ngrams  ******************
## Pairs of words
pair_list2 <- lapply(ngrams(doc1, 3), paste, collapse= " " ) 
freqPair2 <- sort(table(unlist(pair_list2)), decreasing = TRUE)
## Plotting
pairPlot2 <- data.frame(freqPair2[1:15])
plot_pair2 <- ggplot(pairPlot2,aes(x=Var1, y=Freq))+
        geom_bar(stat="identity", aes(fill=as.factor(Var1)))
## *******************************

##***********  Unique words to cover 50%  & 90% ?  **********************
sum <- 0
tabFreq <- data.frame(freq)
for (i in 1:nrow(tabFreq)){
        sum <- sum + tabFreq$Freq[i]
        ind <- i
        if (sum > 0.5*length(doc1)){
                break()
        }
}
ind
##  -->We need 1219 unique words to cover the 50% of the  whole text


sum <- 0
for (i in 1:nrow(tabFreq)){
        sum <- sum + tabFreq$Freq[i]
        ind <- i
        if (sum > 0.9*length(doc1)){
                break()
        }
}
ind
##  -->We need 26179 unique words to cover the 90% of the  whole text
##C **********************************************************************



## ***************  Words coming from foreign language  ********************
library(textcat)
test <- textcat(names(freq[1:50]))


#---------------------------------------------------------------------------
#---------------------------------------------------------------------------











## ************************** TASK 3 **************************************

## ************* Creating train and test sets ***************
load(file="./endoc.RData")
set.seed(4562)
inTrain <- sample(length(endoc), size=0.8*length(endoc), replace = FALSE)
train <- endoc[inTrain]
test <- endoc[-inTrain]



## Unlisting for frequency counting
unlist1 <- unlist(train)
unigram <- table(unlist1)
unigram <- sort(unigram, decreasing = TRUE)
unigram <- data.frame(unigram)
unigram <- data.frame(word1= as.character(unigram[,1]), Freq= unigram[,2], stringsAsFactors = FALSE )
unigram <- unigram[1:100,]
print(object.size(unigram), units= "Mb")
save(unigram, file="./unigram.RData")

## ******* 2 ngrams  ******************
## Pairs of words
pair_list <- lapply(ngrams(train, 2), paste, collapse= " " ) 
pair_list <- Pair_clean(pair_list)
freqPair <- sort(table(unlist(pair_list)), decreasing = TRUE)
save(freqPair, file="./freqPair.RData")

freqPair <- data.frame(freqPair, stringsAsFactors = FALSE)
freqPair <- data.frame(words= as.character(freqPair[,1]), Freq = freqPair[,2], stringsAsFactors = FALSE)
freqPair <- separate(freqPair, col="words", into = paste("word", 1:2, sep=" "))
freqPair <- Pair_unique(freqPair) ### the most used 2000 words 
freqPair <- data.frame(as.character(freqPair$Var1), Freq= freqPair$Freq, stringsAsFactors = FALSE)
print(object.size(freqPair), units= "Mb")
save(freqPair, file="./freqPair_red.RData")


c1 <- NULL
c2 <- NULL
for (i in 1: nrow(freqPair)){
        aa <- sapply(X=as.character(freqPair[i,1]), FUN= unlist(strsplit), split= " ", perl = TRUE)
        c1 <- c(c1, aa[[1]][1])
        c2 <- c(c2, aa[[1]][2])
}
## Creating data frame with 1st word, 2nd word and 2-gram frequency
bigram <- data.frame(word1 = c1, word2 = c2, freq= freqPair$Freq , stringsAsFactors = FALSE)

save(bigram, file="./bigram.RData")


## ******* 3 ngrams  ******************
## Pairs of words
pair_list2 <- lapply(ngrams(train, 3), paste, collapse= " " )
pair_list2 <- Pair_clean3(pair_list2)
freqPair2 <- sort(table(unlist(pair_list2)), decreasing = TRUE)
save(freqPair2, file="./freqPair2.RData")

doc2 <- data.table(freqPair2)
doc2 <- separate(doc2, col="V1", into = paste("word", 1:3, sep=" "))

freqPair2 <- data.frame(freqPair2, stringsAsFactors = FALSE)
freqPair2 <- Pair_unique(freqPair2) ### the most used 2000 words 
freqPair2 <- data.frame(as.character(freqPair2$Var1), Freq= freqPair2$Freq)
print(object.size(freqPair2), units= "Mb")
save(freqPair2, file="./freqPair2_red.RData")

c1 <- NULL
c2 <- NULL
c3 <- NULL
for (i in 1: nrow(freqPair2)){
        aa <- sapply(X=as.character(freqPair2[i,1]), FUN= unlist(strsplit), split= " ", perl = TRUE)
        c1 <- c(c1, aa[[1]][1])
        c2 <- c(c2, aa[[1]][2])
        c3 <- c(c3, aa[[1]][3])
}
## Creating data frame with 1st word, 2nd word and 2-gram frequency
trigram <- data.frame(word1 = c1, word2 = c2, word3=c3, freq= freqPair2$Freq , stringsAsFactors = FALSE)

save(trigram, file="./trigram.RData")

load(file="./trigram.RData")
load(file="./bigram.RData")

source("Katz_Back-off_1.0.R")

parc <- discount(bigram, trigram)

bigram <- parc[[1]]
trigram <- parc[[2]]

save(bigram, file="./bigram_fin.RData")
save(trigram, file="./trigram_fin.RData")

