# This file contains code to build a text filling model.
# Simple Kneser-Ney smoothing with D = 0.75 was used.
# For the sake of processing time, I chose V : Freq(w_i) > 200
# I decided to build a trigram model

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

# Sort the frequencies
freq_1 <- sort(x = freq_1, decreasing = TRUE)
freq_2 <- sort(x = freq_2, decreasing = TRUE)
freq_3 <- sort(x = freq_3, decreasing = TRUE)

# For the sake of processing time, I only use 1-grams with freq > 200
freq_1 <- freq_1[freq_1 > 200]

# Only include n-grams composed of the terms in our vocabulary
V <- rownames(freq_1)

### Toeknize the bigram and trigram strings
bigrams <- unlist(strsplit(as.character(rownames(freq_2)), "[[:space:]]+"))
trigrams <- unlist(strsplit(as.character(rownames(freq_3)), "[[:space:]]+"))

### Determine whether the components of an n-gram appear in V
validate_ngrams <- function(ngrams){
        sapply(X = ngrams, FUN = function(words){
                # Return false if even one word is outside of V
                all(words %in% V)
        })
}

### Eliminate any 2 and 3-grams with OOV words
freq_2 <- freq_2[validate_ngrams(bigrams)]
freq_3 <- freq_3[validate_ngrams(trigrams)]


