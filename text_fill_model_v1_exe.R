# This file actually carries out the model using tables generated earlier.
# Notes:
        ## Only n-grams with frequencies > 200 were used.
        ## n ranged from 1 to 3
        ## Laplace smoothing was used to deal with 0 probability entries.
        ## Table rows are monograms, whereass columns are preceding n-grams

# This is a function that gets the probability of the next word using a 2-gram model.
bigram_model <- function(phrase){
#         # Find the index of the last word in the phrase, if present
#         ind <- grep(pattern = phrase[length(phrase)], x = monograms[[1]],
#                     fixed = TRUE)
        
        ind <- which(monograms[[1]] %in% phrase[length(phrase)])
        
        if(length(ind) < 1){
                # If not found, return 0 so that the most probable mono gram is chosen
                monogramprobs[[1]][which.max(monogramprobs[[1]])]
        }
        else{
                # Return the word probabilities for each continuation
                monogramprobs[[1]][ind] + bigramprobs[,ind]
        }
}

# This is a function that gets the probability of the next word using a 3-gram model
trigram_model <- function(phrase){
        # Find the index of the last words in the phrase, if present
        pat <- paste(phrase[length(phrase) - 1:length(phrase)])
        
#         ind <- grep(pattern = phrase[length(phrase)], x = bigrams[[1]],
#                     fixed = TRUE)
        
        ind <- which(bigrams[[1]] %in% pat)
        
        if(length(ind) < 1){
                # If not found, return 0 so that the most probable mono gram is chosen
                0
        }
        else{
                # Return the word probabilities for each continuation
                trigramprobs[,ind]
        }        
}

# Create a next word predictor helper function
guessword <- function(phrase = character(length = 0L)){
        # Import the tables generated earlier
        monograms <- read.table(file = paste0(getwd(), "/saved_models/laplace_smoothed/monograms.txt"),
                                colClasses = "character")
        
        bigrams <- read.table(file = paste0(getwd(), "/saved_models/laplace_smoothed/bigrams.txt"),
                                colClasses = "character")
        
        trigrams <- read.table(file = paste0(getwd(), "/saved_models/laplace_smoothed/trigrams.txt"),
                                colClasses = "character")
        
        monogramprobs <- read.table(file = paste0(getwd(), "/saved_models/laplace_smoothed/monogramprobs.txt"),
                                  colClasses = "numeric")
        
        bigramprobs <- read.table(file = paste0(getwd(), "/saved_models/laplace_smoothed/bigramprobs.txt"),
                                colClasses = "numeric")
        
        trigramprobs <- read.table(file = paste0(getwd(), "/saved_models/laplace_smoothed/trigramprobs.txt"),
                                  colClasses = "numeric")
        
        # Remove any special characters from phrase
        phrase <- gsub(pattern = "[^[:alpha:][:space:]'-]", replacement = " ",
                       x = phrase)
        
        # Break up phrase into its component words
        phrase <- strsplit(phrase, "[[:space:]]+")
        
        if(length(phrase[[1]]) < 1)
        {
                "Invalid Input: input phrase must contain at least 1 word."
        }
        else
        {
                if(length(phrase[[1]]) > 1){
                        # Since we are using a trigram model, we only need the last two words
                        phrase <- c(phrase[[1]][(length(phrase[[1]]) - 1):length(phrase[[1]])])
                        
                        wordprobs <- bigram_model(phrase) + trigram_model(phrase)
                        
                        monograms[[1]][which.max(wordprobs)]
                }
                else{
                        phrase <- phrase[[1]]
                        
                        wordprobs <- bigram_model(phrase)
                        
                        monograms[[1]][which.max(wordprobs)]
                }
        }
}
