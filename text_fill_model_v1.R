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

### Store the rownames of the frequencies in their own vectors to make the code more efficient
monograms <- rownames(freq_1)
bigrams <- rownames(freq_2)
trigrams <- rownames(freq_3)
tetragrams <- rownames(freq_4)

### Find every entry of freq_2 beginning with the i-th ngram

hits <- lapply(X = monograms, FUN = function(k){grep(pattern = paste0("^", k, "[[:space:]]+"), 
                                                  x = bigrams)})

### Compute the log frequency for each continuation, and then save
bigramconts <- matrix(data = 0, nrow = length(monograms), ncol = length(monograms))

for(i in 1:length(monograms)){
        bigramconts[i, hits[[i]]] <- sapply(X = hits[[i]], FUN = function(k){
                length(grep(pattern = paste0("[[:space:]]+", monograms[k], "$"), x = bigrams))
        }, USE.NAMES = FALSE)
}

close(my_con)
