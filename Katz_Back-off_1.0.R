

## -------------------------------------------------------
##            Katz's Back-off implementation
## -------------------------------------------------------



## ********************   References:  *********************

## https://thachtranerc.wordpress.com/2016/04/12/katzs-backoff-model-implementation-in-r/
## https://github.com/ThachNgocTran/KatzBackOffModelImplementationInR

## ********************************************************



## *******************  Notes  ***************************

## This implementation considers only 2-grams and 3-grams







## -----------------------------------------------------
##                  Discount coefficients 
## -----------------------------------------------------

discount <- function(TwoGram, ThreeGram){
        ## input : the text input with 2 words : input[1:2] : 1col= 1st words; 2col=2nd word; 
        ## d= (r+1)/r * (N_r+1)/N_r
        ## r: frequency of the gram ; Nr = number of times frequency "r" appears in the list
        

        
        ## --------------------
        ##  Treating 3 gram list 
        ## --------------------
        
        ## Tgram : list of 3grams containing the 2 words input (3rd col: freq)
        un <- unique(ThreeGram[,4])
        if ((TRUE %in% is.na(un)) == TRUE){
                print("There are frequencies = NA, please check your input 3-GRAM")
                stop()
        }
        
        ## Defining the discount values for each different frequency
        if (length(un) >0){
                disc <- vector(mode="numeric", length = length(un))  
                nm <- min(length(un),6)  ## If frequency is higher than 6-1=5 --> d=1
                for (i in 1:length(un)){
                        if (i < nm){
                                freq <- un[i]
                                freq2 <- freq+1
                                Nfreq <- nrow(ThreeGram[ThreeGram[,4]==freq,])
                                Nfreq2 <- nrow(ThreeGram[ThreeGram[,4]==freq2,])
                                dis <- freq2 / freq * Nfreq2 / Nfreq
                                if (dis ==0){dis<-1}
                                disc[i] <- dis
                        } else {
                                dis <- 1
                                disc[i] <- dis
                        }
                }
        }
        
        ## disc --> discount values for each unique frequency
        ma <- match(ThreeGram$freq, un)
        val <- NULL
        for (j in 1: nrow(ThreeGram)){
                f <- disc[ma[j]]
                val <- c(val,f)
        }
        ThreeGram$disc <- round(val,3)  # Add discount values to 3-gram data frame
        max3 <- max(ThreeGram$disc)
        ThreeGram$disc <- ThreeGram$disc / max3 # Normalizing to 1
        
        
        ## --------------------
        ##  Treating 2-gram list 
        ## --------------------
        
        un <- unique(TwoGram[,3])
        if ((TRUE %in% is.na(un)) == TRUE){
                print("There are frequencies = NA, please check your input 2-GRAM")
                stop()
        }
        
        ## Defining the discount values for each different frequency
        if (length(un) >0){
                disc <- vector(mode="numeric", length = length(un))  
                nm <- min(length(un),7)  ## If frequency is higher than 7-1=6 --> d=1
                for (i in 1:length(un)){
                        if (i < (nm-1)){
                                freq <- un[i]
                                freq2 <- freq+1
                                Nfreq <- nrow(TwoGram[TwoGram[,3]==freq,])
                                Nfreq2 <- nrow(TwoGram[TwoGram[,3]==freq2,])
                                dis <- freq2 / freq * Nfreq2 / Nfreq
                                if (dis ==0){dis<-1}
                                disc[i] <- dis
                        } else {
                                dis <- 1
                                disc[i] <- dis
                        }
                }
        }
        
        ## disc --> discount values for each unique frequency
        ma <- match(TwoGram[,3], un)
        val <- NULL
        for (j in 1: nrow(TwoGram)){
                f <- disc[ma[j]]
                val <- c(val,f)
        }
        TwoGram$disc <- round(val,3)  # Add discount values to 3-gram data frame
        max2 <- max(TwoGram$disc)
        TwoGram$disc <- TwoGram$disc / max2 # Normalizing to 1
        
        return(list(TwoGram, ThreeGram))
        
        
        
}





## -----------------------------------------------------
##                  Prediction algorithm
## -----------------------------------------------------

source("./Clean.R")


## gets an input and return the 5 most probable words as  output
predic_katz <- function(input){
        input <- unlist(strsplit(input, split= " ")) ## Splitting in different words
        input <- textClean(input) ## Cleaning the input data
        load( file="./unigram.RData")
        load( file="./bigram_fin.RData")
        load( file="./trigram_fin.RData")
        
        Nwords <- length(input)
        
        ## if input is larger than 2 words we take the 2 last words
        if (Nwords >2){
                input2 <-  input[(length(input)-1):length(input)]
                input <- input2
        }

        ## 1 word input : using bigram
        if (Nwords == 1){ 
                find <- which(bigram[,1] == input[1])
                if (length(find) == 0){
                        output <- unigram[1:5,1]
                } else {
                        bisel <- bigram[find,2]
                        output <- bisel[1:5] 
                }
        }
        
        
        ## 2 input words or more --> trigram or Bigram -->Katz Back off
        if (Nwords >= 2){
                ## Selecting rows containing the last 2 words from trigram of the input
                sel1 <- which(trigram[,1] %in% input[1] & trigram[,2] %in% input[2])
                if (length(sel1) >0){
                        sel1 <- trigram[sel1,]
                        sel2 <- which(sel1[,2] %in% input[2])
                        sel2 <- sel1[sel2,]
                        
                        ## Defining pbeta
                        pbeta <- 1-(sum(sel2[,4]*sel2[,5]) / sum(sel2[,4]))
                        
                        ## Selecting bigram rows where the 1st word is equal to the last word
                        usel <- which(bigram[,1] %in% input[2])
                        usel <- bigram[usel,]
                        
                        ## Removing those having word 2 = word 3 of the trigram
                        rsel <- which(usel[,2] %in% sel2[,3])
                        usel <- usel[-rsel,]
                        
                        ## for each word left, we calculate its probability 2-gram
                        usel$prob <- usel$freq * usel$disc * pbeta /(sum(usel$freq*usel$disc))
                        
                        ## Calculate the probability for each 3-gram end word
                        sel2$prob <- sel2$freq * sel2$disc / sum(sel2$freq * sel2$disc)
                        
                        ##Subseting each end word in bigram and trigram with its probability and sorting
                        bisel <- data.frame(endword=usel$word2, prob=usel$prob, stringsAsFactors = FALSE)
                        trisel <- data.frame(endword=sel2$word3, prob=sel2$prob, stringsAsFactors = FALSE)
                        final <- rbind(bisel, trisel)
                        yy <- order(final$prob, decreasing = TRUE)
                        final <- final[yy,]
                        output <- final[1:5,1]
                } else { 
                        ## Applying bigram
                        find <- which(bigram[,1] %in% input[2])
                        if (length(find)>0){
                                bisel <- bigram[find,]
                                output<- bisel[1:5,2]
                        } else{## Applying unigram
                                output <- unigram[1:5,1]
                        }
                        
                        
                }
                
                
        }
        
        return(output)
        
        
        
        
}







