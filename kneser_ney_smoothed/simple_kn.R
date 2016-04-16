# This file contains code to build a text filling model.
# Simple Kneser-Ney smoothing with D = 0.75 was used.
# For the sake of processing time, I chose V : Freq(w_i) > 45
# I decided to build a trigram model

# Load req libraries
library(tm)
library(RWeka)
library(SnowballC)
library(data.table)

# Load the sample corpus using the tm package
docs <- DirSource(directory = paste0(getwd(), "/samples/compiled/"), encoding = "UTF-8")
texts <- VCorpus(docs)

# Strip the whitespace from the documents
texts <- tm_map(texts, stripWhitespace)

# Find the 1-gram frequencies
strsplit_space_tokenizer <- function(x){
        unlist(strsplit(as.character(x), "[[:space:]]+"))
}

ctrl <- list(tokenize = strsplit_space_tokenizer, tolower = FALSE,
             wordLengths = c(1, Inf))

freq_1 <- termFreq(doc = texts[[1]], control = ctrl)

# Find the 2-gram frequencies
bigramtokenizer <- function(x){NGramTokenizer(x, Weka_control(min = 2, max = 2, 
                                                              delimiters = " \n"))}

ctrl <- list(tokenize = bigramtokenizer, tolower = FALSE,
             wordLengths = c(1, Inf))

freq_2 <- termFreq(doc = texts[[1]], control = ctrl)

# Find the 3-gram frequencies
trigramtokenizer <- function(x){NGramTokenizer(x, Weka_control(min = 3, max = 3,
                                                               delimiters = " \n"))}

ctrl <- list(tokenize = trigramtokenizer, tolower = FALSE,
             wordLengths = c(1, Inf))

freq_3 <- termFreq(doc = texts[[1]], control = ctrl)

# Store the term frequencies in data tables
monograms <- data.table(word = rownames(freq_1), mono_freq = freq_1)

# Split apart the phrases and store them in the bigram data table
phrases <- sapply(X = strsplit(as.character(rownames(freq_2)), "[[:space:]]+"),
                  FUN = unlist)

bigrams <- data.table(big_first = phrases[1,], big_second = phrases[2,],
                      big_freq = freq_2)

# Split apart the trigram phrases and store them in the trigram data table
phrases <- sapply(X = strsplit(as.character(rownames(freq_3)), "[[:space:]]+"),
                  FUN = unlist)

trigrams <- data.table(tri_first = phrases[1,], tri_second = phrases[2,],
                       tri_third = phrases[3,], tri_freq = freq_3)

# Create a numeric word key to monograms
setkey(monograms, mono_freq)
monograms[,map := 1:length(freq_1)]

# Map the constituent words onto the numeric key
setkey(monograms, word) # Set monogram's key to words to enable binary searching

# Replace each word with the corresponding key
bigrams[, `:=` (big_first = monograms[big_first, map],
                big_second = monograms[big_second, map])]

trigrams[, `:=` (tri_first = monograms[tri_first, map],
                 tri_second = monograms[tri_second, map],
                 tri_third = monograms[tri_third, map])]

# For the sake of processing time, I only use 1-grams with freq > 50
freq_1 <- freq_1[freq_1 > 50]
# Remove any infrequent entries from the bigrams and trigams
freq_2 <- freq_2[freq_2 > 5]
freq_3 <- freq_3[freq_3 > 5]

# Only include n-grams composed of the terms in our vocabulary
V <- rownames(freq_1)

### Toeknize the bigram and trigram strings
bigrams <- sapply(X = strsplit(as.character(rownames(freq_2)), "[[:space:]]+"),
                  FUN = unlist)

trigrams <- sapply(X = strsplit(as.character(rownames(freq_3)), "[[:space:]]+"),
                  FUN = unlist)

### Determine whether the components of an n-gram appear in V
validate_ngrams <- function(ngrams){
        apply(X = ngrams, MARGIN = 2, FUN = function(words){
                # Initialize a logical value to determine whether a ngram is valid
                in_vocabulary <- TRUE
                i <- 1
                
                # Check each word but return false once an OoV word is found
                while(in_vocabulary & i <= length(words) ){
                        in_vocabulary <- words[i] %in% V
                        i <- i + 1
                }
                
                in_vocabulary
        })
}

### Eliminate any 2 and 3-grams with OOV words
freq_2 <- freq_2[validate_ngrams(bigrams)]
freq_3 <- freq_3[validate_ngrams(trigrams)]

# Save the character strings for all of the surviving n-grams
write.table(rownames(freq_1), 
            file = paste0(getwd(), "/saved_models/simple_kneser_ney/monograms.txt"))

write.table(rownames(freq_2), 
            file = paste0(getwd(), "/saved_models/simple_kneser_ney/bigrams.txt"))

write.table(rownames(freq_3), 
            file = paste0(getwd(), "/saved_models/simple_kneser_ney/trigrams.txt"))

# Build count tables for bigrams and trigrams for later use
# Table structure: columns are the preceding words, rows are the completions
### Tokenize the n-grams (again)
bigrams <- sapply(X = strsplit(as.character(rownames(freq_2)), "[[:space:]]+"),
                  FUN = unlist)

trigrams <- sapply(X = strsplit(as.character(rownames(freq_3)), "[[:space:]]+"),
                   FUN = unlist)

### Map the bigram counts into a VxV matrix
### Find the bigrams whose first term matches a given vocabulary term
hits <- lapply(X = V, FUN = function(first){
        which(bigrams[1,] %in% first, arr.ind = TRUE)
})

bigram_counts <- sapply(X = hits, FUN = function(h){
        # Limit the bigram counts to the ones that match the first term
        freq_2_lim <- freq_2[h]
        # Subset the bigrams to only include ones that had a first term match
        temp_completions <- bigrams[2,h]
        
        sapply(X = V, function(second){
                # Find the index of the word that completes the bigram, if any
                loc <- which(temp_completions %in% second, arr.ind = TRUE)
                
                # Return the frequency for that bigram, if detected
                ifelse(length(loc) > 0, yes = freq_2_lim[loc], no = 0)
        }, USE.NAMES = FALSE)
}, USE.NAMES = FALSE)

### Map the trigram counts into another table
hits <- apply(X = bigrams, MARGIN = 2, FUN = function(bi){
        # We only need to find the matches for the first 2 words in the trigram
        apply(X = trigrams, MARGIN = 2, FUN = function(tri){
                (tri[1] %in% bi[1]) & (tri[2] %in% bi[2])
        })
})

trigram_counts <- apply(X = hits, MARGIN = 2, FUN = function(h){
        # Temporarily store a subset of the frequency counts
        temp_freq <- freq_3[h]
        # We only want the last word of each trigram here
        temp_trigrams <- trigrams[3,h]
        
        sapply(X = V, FUN = function(k){
                # Find the location of the matches for a given continuation
                loc <- which(temp_trigrams %in% k, arr.ind = TRUE)
                
                # Pull the frequency for that word if found, 0 otherwise
                if(length(loc) > 0) output <- temp_freq[loc]
                else output <- 0
        }, USE.NAMES = FALSE)
        
})

# Simple Kneser Ney Smoothing
## Monograms
### Find the number of bigrams actually observed in the data
N_bigrams <- sum(bigram_counts > 0)
pkn_monograms <- apply(X = bigram_counts, MARGIN = 1, function(x){
        sum(x > 0)/N_bigrams
})

## Bigrams
D = 0.75

### Find the number of counts each preceding word has associated with it
proceding_counts <- apply(X = bigram_counts, MARGIN = 2, FUN = sum)

### Find the number of possible completions for a given history
num_completions <- apply(X = bigram_counts, MARGIN = 2, FUN = function(x){
        sum(x > 0)})
        
# pkn_bigrams <- apply(X = bigram_counts, MARGIN = 1, function(completion){
#         psmooth <- sum(completion > 0)/N_bigrams
#                 
#         sapply(X = completion, function(history){
#                 alpha <- max(history - D,0)/
#                 gamma <- D
#                         
#                 alpha + gamma*psmooth
#         })
# })

### Build the pkn bigram table
pkn_bigrams <- matrix(data = 0, nrow = length(V), ncol = length(V))
for(i in 1:length(V)){
        p_smooth <- pkn_monograms[i]
        
        helper <- cbind(bigram_counts[i,], num_completions, proceding_counts)
        
        pkn_bigrams[i,] <- apply(X = helper, MARGIN = 1, function(x){
                max(x[1] - D, 0)/x[3] + D*x[2]*p_smooth/x[3]
        })
        
#         for(j in 1:length(V)){
#                 alpha <- max(bigram_counts[i,j] - D, 0)/proceding_counts[j]
#                 gamma <- D*num_completions[j]/proceding_counts[j]
#                 
#                 pkn_bigrams[i,j] <- alpha + gamma*p_smooth
#         }
}
