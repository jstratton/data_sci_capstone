# This is a file where I explore the data some more!

# Load req libraries
library(tm)
library(RWeka)
library(SnowballC)

# Load the documents using the tm package
docs <- DirSource(directory = paste0(getwd(), "/samples/clean/"))
texts <- VCorpus(docs)

# Strip the white space of the documents.
texts <- tm_map(texts, stripWhitespace)

# Find the 1-gram frequencies
strsplit_space_tokenizer <- function(x)
        unlist(strsplit(as.character(x), "[[:space:]]+"))

# Set the parameters for termFreq
ctrl <- list(tokenize = strsplit_space_tokenizer, tolower = FALSE)

freq_1 <- lapply(X = texts, FUN = termFreq, control = ctrl)

# Find the 2-gram frequencies
bigramtokenizer <- function(x){NGramTokenizer(x, Weka_control(min = 2, max = 2, 
                                                              delimiters = " \n"))}

ctrl <- list(tokenize = bigramtokenizer, tolower = FALSE)

freq_2 <- lapply(X = texts, FUN = termFreq, control = ctrl)

# Find the 3-gram frequencies
trigramtokenizer <- function(x){NGramTokenizer(x, Weka_control(min = 3, max = 3,
                                                               delimiters = " \n"))}

ctrl <- list(tokenize = trigramtokenizer, tolower = FALSE)

freq_3 <- lapply(X = texts, FUN = termFreq, control = ctrl)

# Find the 4-gram frequencies
tetragramtokenizer <- function(x){NGramTokenizer(x, Weka_control(min = 4, max = 4, 
                                                                 delimiters = " \n"))}

ctrl <- list(tokenize = tetragramtokenizer, tolower = FALSE)

freq_4 <- lapply(X = texts, FUN = termFreq, control = ctrl)

# Sort the frequencies
freq_1 <- lapply(X = freq_1, FUN = sort, decreasing = TRUE)
freq_2 <- lapply(X = freq_2, FUN = sort, decreasing = TRUE)
freq_3 <- lapply(X = freq_3, FUN = sort, decreasing = TRUE)
freq_4 <- lapply(X = freq_4, FUN = sort, decreasing = TRUE)


# Plot the frequencies
par(mfcol = c(1,3))

plot(x = 1:length(freq_1[[1]]), y = freq_1[[1]], type = "l", main = "Blog 1-gram Frequencies", xlab = "Word Rank", ylab = "Word Frequency(Counts)", lwd = 5)
plot(x = 1:length(freq_1[[2]]), y = freq_1[[2]], type = "l", main = "News 1-gram Frequencies", xlab = "Word Rank", ylab = "Word Frequency(Counts)", lwd = 5)
plot(x = 1:length(freq_1[[3]]), y = freq_1[[3]], type = "l", main = "Twitter 1-gram Frequencies", xlab = "Word Rank", ylab = "Word Frequency(Counts)", lwd = 5)

par(mfcol = c(1,3))

plot(x = 1:length(freq_2[[1]]), y = freq_2[[1]], type = "l", main = "Blog 2-gram Frequencies", xlab = "Word Rank", ylab = "Word Frequency(Counts)", lwd = 5)
plot(x = 1:length(freq_2[[2]]), y = freq_2[[2]], type = "l", main = "News 2-gram Frequencies", xlab = "Word Rank", ylab = "Word Frequency(Counts)", lwd = 5)
plot(x = 1:length(freq_2[[3]]), y = freq_2[[3]], type = "l", main = "Twitter 2-gram Frequencies", xlab = "Word Rank", ylab = "Word Frequency(Counts)", lwd = 5)

par(mfcol = c(1,3))

plot(x = 1:length(freq_3[[1]]), y = freq_3[[1]], type = "l", main = "Blog 3-gram Frequencies", xlab = "Word Rank", ylab = "Word Frequency(Counts)", lwd = 5)
plot(x = 1:length(freq_3[[2]]), y = freq_3[[2]], type = "l", main = "News 3-gram Frequencies", xlab = "Word Rank", ylab = "Word Frequency(Counts)", lwd = 5)
plot(x = 1:length(freq_3[[3]]), y = freq_3[[3]], type = "l", main = "Twitter 3-gram Frequencies", xlab = "Word Rank", ylab = "Word Frequency(Counts)", lwd = 5)

par(mfcol = c(1,3))

plot(x = 1:length(freq_4[[1]]), y = freq_4[[1]], type = "l", main = "Blog 4-gram Frequencies", xlab = "Word Rank", ylab = "Word Frequency(Counts)", lwd = 5)
plot(x = 1:length(freq_4[[2]]), y = freq_4[[2]], type = "l", main = "News 4-gram Frequencies", xlab = "Word Rank", ylab = "Word Frequency(Counts)", lwd = 5)
plot(x = 1:length(freq_4[[3]]), y = freq_4[[3]], type = "l", main = "Twitter 4-gram Frequencies", xlab = "Word Rank", ylab = "Word Frequency(Counts)", lwd = 5)

# View the head of the 1-gram frequency table
monograms_table <- cbind(1:10, rownames(freq_1[[1]][1:10]), freq_1[[1]][1:10], rownames(freq_1[[2]][1:10]), freq_1[[2]][1:10], rownames(freq_1[[3]][1:10]), freq_1[[3]][1:10])

rownames(monograms_table) <- rep(x = "", times = 10)
colnames(monograms_table) <- c("Rank", "Blogs", "Counts", "News", "Counts", "Twitter", "Counts")

monograms_table

# View the head of the 2-gram frequency table
bigrams_table <- cbind(1:10, rownames(freq_2[[1]][1:10]), freq_2[[1]][1:10], rownames(freq_2[[2]][1:10]), freq_2[[2]][1:10], rownames(freq_2[[3]][1:10]), freq_2[[3]][1:10])

rownames(bigrams_table) <- rep(x = "", times = 10)
colnames(bigrams_table) <- c("Rank", "Blogs", "Counts", "News", "Counts", "Twitter", "Counts")

bigrams_table

# View the head of the 3-gram freq table
trigrams_table <- cbind(1:10, rownames(freq_3[[1]][1:10]), freq_3[[1]][1:10], rownames(freq_3[[2]][1:10]), freq_3[[2]][1:10], rownames(freq_3[[3]][1:10]), freq_3[[3]][1:10])

rownames(trigrams_table) <- rep(x = "", times = 10)
colnames(trigrams_table) <- c("Rank", "Blogs", "Counts", "News", "Counts", "Twitter", "Counts")

trigrams_table

# View the head of the 4-gram freq table
tetragrams_table <- cbind(1:10, rownames(freq_4[[1]][1:10]), freq_4[[1]][1:10], rownames(freq_4[[2]][1:10]), freq_4[[2]][1:10], rownames(freq_4[[3]][1:10]), freq_4[[3]][1:10])

rownames(tetragrams_table) <- rep(x = "", times = 10)
colnames(tetragrams_table) <- c("Rank", "Blogs", "Counts", "News", "Counts", "Twitter", "Counts")

tetragrams_table
