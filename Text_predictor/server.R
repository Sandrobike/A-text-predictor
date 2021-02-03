#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(data.table)

# Define server logic 



shinyServer(function(input, output) {
    
    prediction <- data.table(word = c("...","...","..."),
                             probability = c(0,0,0), 
                             type = c("waiting","waiting","waiting"))
    d2 <- 0
    d3 <- 2.5
    
    # Model data subset
    unigram <- fread(file = "funigram2.csv")
    bigram <- fread(file = "fbigram2.csv")
    trigram <- fread(file = "ftrigram2.csv")
    quadgram <- fread(file = "fquadgram2.csv")
    pengram <- fread(file = "fpengram2.csv")
    
    # Combined SBO-KBO model prediction function
    # inputs: 1-gram to 5-gram data tables; unigr,bigr,trigr,quadgr,pengr
    #         each data table has two column: n_gram and freq (including n_gram counts)
    # input: prefix; the piece of sentence for which you want to predict the next word
    # output: a list including:
    #           observed: a data table with two columns: n_gram and prob including all 
    #                     the observed predicted n-grams ordered by probabilities (prob)
    #                     that are present in input xxxgr data.table for the
    #                     correspondent prefix
    #           unobserved: a data table with two columns: n_gram and prob including all 
    #                     the unobserved predicted n_grams ordered by probabilities (prob)
    #                     according KBO calculation, that are not present in input trigr 
    #                     data.table for the correspondent prefix (bigram)
    #           unobs-l: a logic value (TRUE or FALSE). it is TRUE when an unobserved table
    #                    is present
    #           prefix: a character string reporting the effective prefix used for prediction 
    #                   (generally shorter than input prefix)
    my_model <- function(unigr,bigr,trigr,quadgr,pengr,prefix,d2,d3) {
        n_char <- length(str_split(prefix," ")[[1]])
        
        if(n_char == 1) {
            obs_2g <- data.table(n_gram=vector(mode = 'character', length = 0),
                                 freq=vector(mode = 'integer', length = 0))
            regex <- paste("^", prefix," ",sep = "")
            index <- grep(regex, bigr$n_gram)
            if(length(index) > 0) {
                obs_2g <- bigr[index, ]
            } else {
                return(NULL)
            }
            obsCount <- unigr[unigr$n_gram==prefix,]$freq[1]
            qbo_obs_2 <- obs_2g[,prob := freq/obsCount][,c(1,3)]
            
            out <- list(observed = qbo_obs_2, unobserved = NA, 
                        unobs_l = FALSE, prefix = prefix)
            return(out)
        } else if(n_char == 2) {
            # katz back off model
            obs_3g <- data.table(n_gram=vector(mode = 'character', length = 0),
                                 freq=vector(mode = 'integer', length = 0))
            regex <- paste("^", prefix," ",sep = "")
            index <- grep(regex, trigr$n_gram)
            if(length(index) > 0) {
                obs_3g <- trigr[index, ]
            }
            
            obs_3g_tails <- str_split_fixed(obs_3g$n_gram, " ", 3)[, 3]
            unobs_3g_tails <- unigr[!(unigr$n_gram %in% obs_3g_tails), ]$n_gram
            
            # if(nrow(Obs_3g) < 1) return(NULL)
            obsCount <- bigr[bigr$n_gram==prefix,]$freq[1]
            qbo_obs_3 <- obs_3g[,prob := (freq -d3)/obsCount][,c(1,3)]
            
            w_i_minus1 <- str_split(prefix, " ")[[1]][2]
            boBigram <- paste(w_i_minus1, unobs_3g_tails, sep = " ")
            obsBoBigram <- bigr[bigr$n_gram %in% boBigram, ]
            
            unobs_bo_bi <- boBigram[!(boBigram %in% obsBoBigram$ngram)]
            
            first_words <- str_split_fixed(obsBoBigram$n_gram, " ", 2)[, 1]
            first_word_freqs <- unigr[unigr$n_gram %in% first_words, ]
            obsBigProbs <- (obsBoBigram$freq -d2)/ first_word_freqs$freq
            qbo_obs_2 <- data.table(n_gram=obsBoBigram$n_gram, prob=obsBigProbs)
            
            unig <- str_split(prefix," ")[[1]][2]
            unig <- unigr[unigr$n_gram == unig,]
            if (dim(unig)[1] == 0) return(NULL)
            
            regex <- paste("^", unig$n_gram[1]," ",sep = "")
            bigsThatStartWithUnig <- bigr[grep(regex, bigr$n_gram),]
            if(nrow(bigsThatStartWithUnig) < 1) {
                alpha2 <- 1
            } else {
                alpha2 <- 1 - (sum(bigsThatStartWithUnig$freq - d2) / unig$freq)
            }
            
            big_row <- bigr[bigr$n_gram %in% prefix,]
            
            if(nrow(obs_3g) < 1) {
                alpha3 <- 1
            } else {
                alpha3 <- 1 - sum((obs_3g$freq - d3) / big_row$freq[1])
            }
            # get the unobserved bigram tails
            qboUnobsBigs <- str_split_fixed(unobs_bo_bi, " ", 2)[, 2]
            
            # convert to data.table with counts
            qboUnobsBigs <- unigr[unigr$n_gram %in% qboUnobsBigs, ]
            denom <- sum(qboUnobsBigs$freq)
            # converts counts to probabilities
            qbo_unobs_2 <- data.table(n_gram=unobs_bo_bi,
                                      prob=(alpha2 * qboUnobsBigs$freq / denom))
            
            qboBigrams <- rbind(qbo_obs_2, qbo_unobs_2)
            qboBigrams <- qboBigrams[order(-qboBigrams$prob), ]
            sumQboBigs <- sum(qboBigrams$prob)
            first_prefix_word <- str_split(prefix, " ")[[1]][1]
            unobsTrigNgrams <- paste(first_prefix_word, qboBigrams$n_gram, sep=" ")
            unobsTrigProbs <- alpha3 * qboBigrams$prob / sumQboBigs
            qbo_unobs_3 <- data.table(n_gram=unobsTrigNgrams, prob=unobsTrigProbs)
            
            
            out <- list(observed = qbo_obs_3, unobserved = qbo_unobs_3, 
                        unobs_l = TRUE, prefix = prefix)
            return(out)
        } else if (n_char == 3) {
            
            obs_4g <- data.table(n_gram=vector(mode = 'character', length = 0),
                                 freq=vector(mode = 'integer', length = 0))
            regex <- paste("^", prefix," ",sep = "")
            index <- grep(regex, quadgr$n_gram)
            if(length(index) > 0) {
                obs_4g <- quadgr[index, ]
            } else {
                prefix2 <- str_c(str_split(prefix," ")[[1]][2:3],collapse = " ")
                return(my_model(unigr,bigr,trigr,quadgr,pengr,prefix2,d2,d3))
            }
            
            obsCount <- trigr[trigr$n_gram==prefix,]$freq[1]
            qbo_obs_4 <- obs_4g[,prob := freq/obsCount][,c(1,3)]
            
            out <- list(observed = qbo_obs_4, unobserved = NA, 
                        unobs_l = FALSE, prefix = prefix)
            return(out)
        } else if (n_char == 4) {
            
            obs_5g <- data.table(n_gram=vector(mode = 'character', length = 0),
                                 freq=vector(mode = 'integer', length = 0))
            regex <- paste("^", prefix," ",sep = "")
            index <- grep(regex, pengr$n_gram)
            if(length(index) > 0) {
                obs_5g <- pengr[index, ]
            } else {
                prefix2 <- str_c(str_split(prefix," ")[[1]][2:4],collapse = " ")
                return(my_model(unigr,bigr,trigr,quadgr,pengr,prefix2,d2,d3))
            }
            
            obsCount <- quadgr[quadgr$n_gram==prefix,]$freq[1]
            qbo_obs_5 <- obs_5g[,prob := freq/obsCount][,c(1,3)]
            
            out <- list(observed = qbo_obs_5, unobserved = NA, 
                        unobs_l = FALSE, prefix = prefix)
            return(out)
        } else if (n_char > 4) {
            prefix2 <- str_c(str_split(prefix," ")[[1]][(n_char-3):n_char],collapse = " ")
            
            obs_5g <- data.table(n_gram=vector(mode = 'character', length = 0),
                                 freq=vector(mode = 'integer', length = 0))
            regex <- paste("^", prefix2," ",sep = "")
            index <- grep(regex, pengr$n_gram)
            if(length(index) > 0) {
                obs_5g <- pengr[index, ]
            } else {
                prefix2 <- str_c(str_split(prefix," ")[[1]][(n_char-2):n_char],collapse = " ")
                return(my_model(unigr,bigr,trigr,quadgr,pengr,prefix2,d2,d3))
            }
            
            obsCount <- quadgr[quadgr$n_gram==prefix2,]$freq[1]
            qbo_obs_5 <- obs_5g[,prob := freq/obsCount][,c(1,3)]
            
            out <- list(observed = qbo_obs_5, unobserved = NA, 
                        unobs_l = FALSE, prefix = prefix2)
            return(out)
        } else {
            print("Too few characters for prefix")
            return(NULL)
        }
    }
    
    # This function select the three highest probable predicted words ordering them  
    # from the highest to the lower probable starting from observed and unobserved tables
    # in input outlist
    my_prediction <- function(outlist,prefix)  {
        out_data <- data.frame(word = c(NA,NA,NA), 
                               probability = c(NA,NA,NA), 
                               type = c("Not found","Not found","Not found"))
        outmsg <- list(paste("Given the text prefix >>> ", prefix, " <<<"),
                       "No matches found >>>",
                       out_data[1:3,],prefix)
        
        if(is.null(outlist)) return(outmsg)
        first <- str_split(outlist$observed$n_gram[1]," ")[[1]]
        n_char <- length(first)
        first <- first[n_char]
        second <- str_split(outlist$observed$n_gram[2]," ")[[1]]
        second <- second[n_char]
        third <- str_split(outlist$observed$n_gram[3]," ")[[1]]
        third <- third[n_char]
        tails <- c(first,second,third)
        probs <- outlist$observed$prob[1:3]
        out_data <- data.frame(word = tails, probability = probs, type = "Observed")
        
        if (outlist$unobs_l) {
            first <- str_split(outlist$unobserved$n_gram[1]," ")[[1]]
            n_char <- length(first)
            first <- first[n_char]
            second <- str_split(outlist$unobserved$n_gram[2]," ")[[1]]
            second <- second[n_char]
            third <- str_split(outlist$unobserved$n_gram[3]," ")[[1]]
            third <- third[n_char]
            tails <- c(first,second,third)
            probs <- outlist$unobserved$prob[1:3]
            unobs_data <- data.frame(word = tails, probability = probs, type = "Unobserved")
            if(is.na(out_data$word[1])) {
                out_data <- unobs_data
            } else {
                out_data <- rbind(out_data,unobs_data)
            }
            out_data <- out_data[order(-out_data$probability),]
        }
        if(is.na(out_data$word[2])) {
            out_data$type[2] <- "Not found"
        }
        if(is.na(out_data$word[3])) {
            out_data$type[3] <- "Not found"
        }
        outmsg <- list(paste("Given the text prefix >>> ", prefix, " <<<"),
                       "highets probability prediction are >>>",
                       out_data[1:3,],outlist$prefix)
        return(outmsg)
    }
    
    res <- ""
    output$mywords <- renderPrint({ t(prediction)})
    output$prefix <- renderPrint({""})
    observeEvent(input$text, { 
        output$value <- renderPrint({ lower <- tolower(input$text)
             lower
             })
    })
    observeEvent(input$text, { 
        res <<- str_trim(tolower(input$text))
        })
    
    observeEvent(input$act1, { 
        res <<- str_c(tolower(res),prediction$word[1],sep = " ")
        output$value <- renderPrint({ tolower(res)})
         })
   
    observeEvent(input$act2, { 
    res <<- str_c(tolower(res),prediction$word[2],sep = " ")
    output$value <- renderPrint({ tolower(res)})
    })
    observeEvent(input$act3, { 
        res <<- str_c(tolower(res),prediction$word[3],sep = " ")
        output$value <- renderPrint({ tolower(res)})
    })
    
    observeEvent(input$act0, { 
        # output$pred <- renderPrint({str_trim(res)})
        prefix <- str_trim(res)
        outlist <- my_model(unigram,bigram,trigram,quadgram,pengram,prefix,d2,d3)
        pred_list <- my_prediction(outlist,prefix)
        prediction <<- pred_list[[3]]
        output$mywords <- renderPrint({ t(prediction)})
        output$prefix <- renderPrint({pred_list[[4]]})
    })
})

