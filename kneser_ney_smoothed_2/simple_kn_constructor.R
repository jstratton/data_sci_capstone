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
docs <- DirSource(directory = paste0(getwd(), "/kneser_ney_smoothed_2",
                                     "/twitter_corpus/"), encoding = "UTF-8")
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

# Find the 4-gram frequencies
tetragramtokenizer <- function(x){NGramTokenizer(x, Weka_control(min = 4, max = 4,
                                                               delimiters = " \n"))}

ctrl <- list(tokenize = tetragramtokenizer, tolower = FALSE,
            wordLengths = c(1, Inf))

freq_4 <- termFreq(doc = texts[[1]], control = ctrl)

# Store the term frequencies in data tables
monograms <- data.table(word = rownames(freq_1), mono_freq = freq_1)

# Split apart the phrases and store them in the bigram data table
phrases <- sapply(X = strsplit(as.character(rownames(freq_2)), "[[:space:]]+"),
                  FUN = unlist)

bigrams <- data.table(big_first = phrases[1,],
                      big_second = phrases[2,],
                      big_freq = freq_2)

# Split apart the trigram phrases and store them in the trigram data table
phrases <- sapply(X = strsplit(as.character(rownames(freq_3)), "[[:space:]]+"),
                  FUN = unlist)

trigrams <- data.table(tri_first = phrases[1,],
                       tri_second = phrases[2,],
                       tri_third = phrases[3,],
                       tri_freq = freq_3)

# Split apart the tetragram phrases and store them in the tetragram DT
phrases <- sapply(X = strsplit(as.character(rownames(freq_4)), "[[:space:]]+"),
                  FUN = unlist)

tetragrams <- data.table(tetra_first = phrases[1,],
                         tetra_second = phrases[2,],
                         tetra_third = phrases[3,],
                         tetra_fourth = phrases[4,],
                         tetra_freq = freq_4)

# Create a numeric word key to monograms
setkey(monograms, mono_freq)
monograms[,map := 1:length(freq_1)]

# Map the constituent words onto the numeric key
setkey(monograms, word) # Set monogram's key to words to enable binary searching

# Replace each word with the corresponding key
bigrams[, `:=` (big_first = monograms[big_first, map],
                big_second = monograms[big_second, map])
        ]

trigrams[, `:=` (tri_first = monograms[tri_first, map],
                 tri_second = monograms[tri_second, map],
                 tri_third = monograms[tri_third, map])
         ]

tetragrams[, `:=` (tetra_first = monograms[tetra_first, map],
                 tetra_second = monograms[tetra_second, map],
                 tetra_third = monograms[tetra_third, map],
                 tetra_fourth = monograms[tetra_fourth, map])
         ]

# For the sake of processing time, I only use 1-grams with freq > 5
# setkey(monograms, mono_freq)
# monograms <- monograms[mono_freq > 5]

# Sort the bigram and trigram tables by word
setkey(bigrams, big_first, big_second)
setkey(trigrams, tri_first, tri_second, tri_third)
setkey(tetragrams, tetra_first, tetra_second, tetra_third, tetra_fourth)

# Restrict n-grams to terms in our vocabulary
# bigrams <- bigrams[big_first > monograms[,min(map) - 1]
#                         & big_second > monograms[,min(map) - 1]
#                   ]
# 
# trigrams <- trigrams[tri_first > monograms[,min(map) - 1]
#                         & tri_second > monograms[,min(map) - 1]
#                         & tri_third > monograms[,min(map) - 1]
#                     ]
# 
# tetragrams <- tetragrams[tetra_first > monograms[,min(map) - 1]
#                      & tetra_second > monograms[,min(map) - 1]
#                      & tetra_third > monograms[,min(map) - 1]
#                      & tetra_fourth > monograms[,min(map) - 1]
#                      ]

# Save the n-gram tables
write.table(monograms, 
            file = paste0(getwd(), "/kneser_ney_smoothed_2/KN_monograms.txt"),
            row.names = FALSE)

write.table(bigrams, 
            file = paste0(getwd(), "/kneser_ney_smoothed_2/KN_bigrams.txt"),
            row.names = FALSE)

write.table(trigrams, 
            file = paste0(getwd(), "/kneser_ney_smoothed_2/KN_trigrams.txt"),
            row.names = FALSE)

write.table(tetragrams,
            file = paste0(getwd(), "/kneser_ney_smoothed_2/KN_tetragrams.txt"),
            row.names = FALSE)
