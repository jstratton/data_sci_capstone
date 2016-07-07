# The purpose of this script is to give accuracy ratings to the models.

library(data.table)

# Load a set of test data from the Twitter entries
dir <- paste0(getwd(),"/final/en_US")

texts <- list(twit = readLines(con = paste0(dir, "/en_US.twitter.txt"),
                               encoding = "UTF-8")
)

texts <- unlist(texts, use.names = FALSE)

# Remove the samples that were included in the training set
pcnt <- 10
set.seed(6282016)
ind <- sample.int(n = length(texts), size = ceiling(pcnt*length(texts)/100))
texts <- texts[-ind]

# Take half (~45%) of the unused data as a test set
pcnt <- 50
ind <- sample.int(n = length(texts), size = ceiling(pcnt*length(texts)/100))
texts <- texts[ind]

# Convert the texts back into a list
texts <- as.list(texts)

# Tokenize the words
texts <- lapply(X = texts, FUN = function(x){unlist(strsplit(as.character(x), "[[:space:]]+"))})

# Use the preceding words as the input to the function, and the last word as the answer.
questions <- lapply(texts, FUN = function(x){x[1:(length(x) - 1)]})
answers <- lapply(texts, FUN = function(x){x[length(x)]})

# Put the sentences back together again for the questions
questions <- lapply(questions, FUN = function(x){paste0(x, collapse = " ")})

# Test the accuracy of a given model
acc_test <- function(model = function(){"The"}){
        
}
