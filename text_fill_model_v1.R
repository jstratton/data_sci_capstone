# This file contains the code for a prototype text filling model.
# I decided to use Laplace smoothing for this model due to its simplicity.
# For the sake of processing time, I only used 2, 3, and 4-grams with frequencies greater than 2.

# Load req libraries
library(tm)
library(RWeka)
library(SnowballC)

# Load the sample corpus using the tm package
docs <- DirSource(directory = paste0(getwd(), "/samples/compiled/"))
texts <- VCorpus(docs)

# Strip the whitespace from the documents
texts <- tm_map(texts, stripWhitespace)

# Find the 1-gram frequencies
strsplit_space_tokenizer <- function(x)
        unlist(strsplit(as.character(x), "[[:space:]]+"))

# Set the parameters for termFreq
ctrl <- list(tokenize = strsplit_space_tokenizer, tolower = FALSE)

freq_1 <- termFreq(doc = texts[[1]], control = ctrl)

# Find the 2-gram frequencies
bigramtokenizer <- function(x){NGramTokenizer(x, Weka_control(min = 2, max = 2, 
                                                              delimiters = " \n"))}

ctrl <- list(tokenize = bigramtokenizer, tolower = FALSE)

freq_2 <- termFreq(doc = texts[[1]], control = ctrl)

# Find the 3-gram frequencies
trigramtokenizer <- function(x){NGramTokenizer(x, Weka_control(min = 3, max = 3,
                                                               delimiters = " \n"))}

ctrl <- list(tokenize = trigramtokenizer, tolower = FALSE)

freq_3 <- termFreq(doc = texts[[1]], control = ctrl)

# Find the 4-gram frequencies
tetragramtokenizer <- function(x){NGramTokenizer(x, Weka_control(min = 4, max = 4, 
                                                                 delimiters = " \n"))}

ctrl <- list(tokenize = tetragramtokenizer, tolower = FALSE)

freq_4 <- termFreq(doc = texts[[1]], control = ctrl)

# Sort the frequencies
freq_1 <- sort(x = freq_1, decreasing = TRUE)
freq_2 <- sort(x = freq_2, decreasing = TRUE)
freq_3 <- sort(x = freq_3, decreasing = TRUE)
freq_4 <- sort(x = freq_4, decreasing = TRUE)

# Reduce memory load by removing all elements with frequency < 40
freq_1 <- freq_1[freq_1 > 40]
freq_2 <- freq_2[freq_2 > 40]
freq_3 <- freq_3[freq_3 > 40]
freq_4 <- freq_4[freq_4 > 40]

# Build word log-probability tables

## 1-gram word log-probability table
prob_1 <- log(freq_1) - log(sum(freq_1))

## 2-gram word log-probability table

### Set up a connection to create a text file with the bigram log probs
my_con <- file(description = paste0(getwd(), "/saved_models/laplace_smoothed/bigrams.txt"),
               open = "a")

### Find every entry of freq_2 beginning with the i-th ngram

hits <- lapply(X = rownames(freq_1), FUN = function(k){grep(pattern = paste0("^", k, "[[:space:]]+"), 
                                                  x = rownames(freq_2))})

### Compute the log frequency for each continuation, and then save
bigramconts <- matrix(data = 0, nrow = length(freq_1), ncol = length(freq_1))

for(i in 1:length(freq_1)){
        
        bigramconts[i] <- sapply(X = rownames(freq_1), FUN = function(k){
                # Temporarily store a subset of the frequency counts
                temp_freq <- freq_2[hits[[i]]]
                
                # Find the location of the matches for a given word
                loc <- grep(pattern = paste0("[[:space:]]+", k, "$"),
                     x = rownames(temp_freq))
                
                # Pull the frequency for that word if found, 0 otherwise
                if(length(loc) > 0) output <- temp_freq[loc]
                else output <- 0
        }, USE.NAMES = FALSE)
}

close(my_con)
