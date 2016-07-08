# The purpose of this script is to give accuracy ratings to the models.

library(data.table)

# Load a set of test data from the Twitter entries if they already exist
dir <- paste0(getwd(), "/kneser_ney_smoothed_2")
if(all(file.exists(paste0(dir, "/test_questions.txt"), paste0(dir, "/test_answers.txt")))){
        test_phrases <- data.table(questions = readLines(con = paste0(dir, "/test_questions.txt"), encoding = "UTF-8"),
                             answers = readLines(con = paste0(dir, "/test_answers.txt"), encoding = "UTF-8"))
} else{
        # If the documents aren't found, create them.
        dir <- paste0(getwd(),"/final/en_US")
        
        texts <- readLines(con = paste0(dir, "/en_US.twitter.txt"),
                           encoding = "UTF-8")
        
        # Remove the samples that were included in the training set
        pcnt <- 10
        set.seed(6282016)
        ind <- sample.int(n = length(texts), size = ceiling(pcnt*length(texts)/100))
        texts <- texts[-ind]
        
        # Take half (~45%) of the unused data as a test set
        pcnt <- 50
        ind <- sample.int(n = length(texts), size = ceiling(pcnt*length(texts)/100))
        texts <- texts[ind]
        
        # Convert the texts into a list
        texts <- as.list(texts)
        
        # Tokenize the words
        texts <- lapply(X = texts, FUN = function(x){unlist(strsplit(as.character(x), "[[:space:]]+"))})
        
        # Use the preceding words as the input to the function, and the last word as the answer.
        questions <- lapply(texts, FUN = function(x){x[1:(length(x) - 1)]})
        answers <- lapply(texts, FUN = function(x){x[length(x)]})
        
        # Put the sentences back together again for the questions
        questions <- lapply(questions, FUN = function(x){paste0(x, collapse = " ")})
        
        # Remove any answers that our model can't predict due to the data cleaning.
        # Namely, swear words and web addresses, which we don't want to predict.
        answers <- lapply(X = answers, FUN = gsub, pattern = "[^[:alpha:][:space:]'-]",
                        replacement = " ", ignore.case = TRUE)
        
        ## Remove all webaddresses
        answers <- lapply(X = answers, FUN = gsub, pattern = "((http|w{3}).*?(com|edu|org| ))",
                        replacement = "", ignore.case = TRUE)
        
        ## Remove all swearwords (https://en.wiktionary.org/wiki/Category:English_swear_words)
        
        ### Remove all instances of the words ass/arsehole
        answers <- lapply(X = answers, FUN = gsub, pattern = "(ass|arse).*hole",
                        replacement = "", ignore.case = TRUE)
        
        ### Remove all singlet ass tokens
        answers <- lapply(X = answers, FUN = gsub, pattern = "ass([^[:alnum:]]|$)",
                        replacement = "", ignore.case = TRUE)
        
        ### Remove all variants of goddamn
        answers <- lapply(X = answers, FUN = gsub, pattern = "god.*?damn",
                        replacement = "", ignore.case = TRUE)
        
        ### Remove all bastards from the text
        answers <- lapply(X = answers, FUN = gsub, pattern = "bastard[s]*",
                        replacement = "", ignore.case = TRUE)
        
        ### Remove all instances of bitch
        answers <- lapply(X = answers, FUN = gsub,
                        pattern = "(son[s]*)*.*?(of)*.*(a*).*(bitch)+(es| )*",
                        replacement = "", ignore.case = TRUE)
        
        ### Remove the word cunt
        answers <- lapply(X = answers, FUN = gsub, pattern = "cunt", replacement = "",
                        ignore.case = TRUE)
        
        ### Remove the word damn
        answers <- lapply(X = answers, FUN = gsub, pattern = "damn", replacement = "",
                        ignore.case = TRUE)
        
        ### Remove the phrase mother fucker
        answers <- lapply(X = answers, FUN = gsub, pattern = "mother.*?fucker",
                        replacement = "", ignore.case = TRUE)
        
        ### Remove the word fuck and its variants
        answers <- lapply(X = answers, FUN = gsub, pattern = "fuck[deginr]*", replacement = "",
                        ignore.case = TRUE)
        
        ### Remove the phrase Holy Shit
        answers <- lapply(X = answers, FUN = gsub, pattern = "holy.*?shit",
                        replacement = "", ignore.case = TRUE)
        
        ### Remove the phrase shitass
        answers <- lapply(X = answers, FUN = gsub, pattern = "shit.*?ass",
                        replacement = "", ignore.case = TRUE)
        
        ### Remove the word shit and its variants
        answers <- lapply(X = answers, FUN = gsub, pattern = "shit[deginrty]*", replacement = "",
                        ignore.case = TRUE)
        
        ### Remove all instances of whore
        answers <- lapply(X = answers, FUN = gsub,
                        pattern = "(son[s]*)*.*?(of)*.*(a*).*(whore)+(s| )*",
                        replacement = "", ignore.case = TRUE)
        
        # Find the indices of the answers that are completely removed by cleaning
        inds <- sapply(X = answers, FUN = function(x){x == ""}, USE.NAMES = FALSE)
        inds <- which(inds)
        
        # Remove these invalid indices
        questions <- unlist(questions, use.names = FALSE)[-inds]
        answers <- unlist(answers, use.names = FALSE)[-inds]
        
        # Store the phrases inside of a data table
        test_phrases <- data.table(questions, answers)
        
        # Save the answer set
        dir <- paste0(getwd(), "/kneser_ney_smoothed_2")
        my_con <- file(description = paste0(getwd(), "/kneser_ney_smoothed_2",
                                            "/test_answers.txt"),
                       encoding = "UTF-8")
        writeLines(answers, con = my_con)
        close(my_con)
        
        # Save the question set
        dir <- paste0(getwd(), "/kneser_ney_smoothed_2")
        my_con <- file(description = paste0(getwd(), "/kneser_ney_smoothed_2",
                                            "/test_questions.txt"),
                       encoding = "UTF-8")
        writeLines(questions, con = my_con)
        close(my_con)
        
        # Remove all of the extraneous variables
        rm(dir, my_con, questions, answers, inds, texts, pcnt)
}

# Test the accuracy of a given model
acc_test <- function(model = function(){"The"}){
        
}
