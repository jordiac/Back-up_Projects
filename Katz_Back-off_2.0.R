

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

source("./Clean_2.0.R")


## gets an input and return the 5 most probable words as  output
predic_text <- function(input){
        input <- textClean(input) ## Cleaning the input data
        input <- unlist(strsplit(input, split= " ")) ## Splitting in different words
        
        load( file="./big_sel.RData") ##bigram
        load( file="./tri_sel.RData") ##trigram
        load( file="./quad_sel.RData") ##quadgram
        
        Nwords <- length(input)
        
        # Defining the input texts for each Ngram
        if (Nwords >2){ 
                qtext <- input[(length(input)-2):length(input)] ##input for quadgram
                ttext <- input[(length(input)-1):length(input)] ##input for trigram
                btext <- input[length(input)]                   ##input for bigram
                
        } else if (Nwords==2){
                ttext <- input[1:2]
                btext <- input[2]
        } else if (Nwords==1) {
                btext <- input[1]
        }
        
        output <- NULL
        ## Predicting
        if (Nwords >2){
                qlist <- which(quadgram[,1] == qtext[1] & quadgram[,2] == qtext[2] & quadgram[,3] == qtext[3])
                tlist <- which(trigram[,1] == ttext[1] & trigram[,2] == ttext[2])
                blist <- which(bigram[,1] == btext[1])
                
                if (length(qlist) >0){
                        output4 <-quadgram[qlist,4:5]
                        colnames(output4) <- c("Predic", "Freq")
                        output <- rbind(output, output4)
                }
                
                if (length(tlist) >0){
                        output3 <- trigram[tlist,3:4]
                        colnames(output3) <- c("Predic", "Freq")
                        output <- rbind(output, output3)
                }
                
                if (length(blist) >0){
                        output2 <- bigram[blist,2:3]
                        colnames(output2) <- c("Predic", "Freq")
                        output <- rbind(output, output2)
                }
                num <- min(5, nrow(output))
                output <- output[1:num,]
                
        }else if (Nwords ==2) {
                tlist <- which(trigram[,1] == ttext[1] & trigram[,2] == ttext[2])
                blist <- which(bigram[,1] == btext[1])
                
                if (length(tlist) >0){
                        output3 <- trigram[tlist,3:4]
                        colnames(output3) <- c("Predic", "Freq")
                        output <- rbind(output, output3)
                }
                
                if (length(blist) >0){
                        output2 <- bigram[blist,2:3]
                        colnames(output2) <- c("Predic", "Freq")
                        output <- rbind(output, output2)
                }
                num <- min(5, nrow(output))
                output <- output[1:num,]
                
                
        } else if (Nwords==1){
                blist <- which(bigram[,1] == btext[1])
                if (length(blist) >0){
                        output <- rbind(output, bigram[blist,])
                        colnames(output) <- c("Predic", "Freq")
                }
                num <- min(5, nrow(output))
                output <- output[1:num,]
        }
                
        return(output)
        
}







