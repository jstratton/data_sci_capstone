# This script contains code for constructing a corpus from the Twitter data

# Load Required Libraries
library(tm)
library(RWeka)
library(SnowballC)

# Load the txt files
dir <- paste0(getwd(),"/final/en_US")

texts <- list(blogs = readLines(con = paste0(dir, "/en_US.blogs.txt"),
                                encoding = "UTF-8")
              )

# I've chosen to use a sample size of 5% for the sake of processing time
pcnt <- 5
set.seed(3252016)
texts <- lapply(X = texts, function(x){sample(x, size = ceiling(pcnt*length(x)/100))})

# Start cleaning the texts
## Remove all non-Eng chars, digits, and punctuation except for ' and -
texts <- lapply(X = texts, FUN = gsub, pattern = "[^[:alpha:][:space:]'-]",
                replacement = " ", ignore.case = TRUE)

## Remove all webaddresses
texts <- lapply(X = texts, FUN = gsub, pattern = "((http|w{3}).*?(com|edu|org| ))",
                replacement = "", ignore.case = TRUE)

## Remove all swearwords (https://en.wiktionary.org/wiki/Category:English_swear_words)

### Remove all instances of the words ass/arsehole
texts <- lapply(X = texts, FUN = gsub, pattern = "(ass|arse).*hole",
                replacement = "", ignore.case = TRUE)

### Remove all singlet ass tokens
texts <- lapply(X = texts, FUN = gsub, pattern = "ass([^[:alnum:]]|$)",
                replacement = "", ignore.case = TRUE)

### Remove all variants of goddamn
texts <- lapply(X = texts, FUN = gsub, pattern = "god.*?damn",
                replacement = "", ignore.case = TRUE)

### Remove all bastards from the text
texts <- lapply(X = texts, FUN = gsub, pattern = "bastard[s]*",
                replacement = "", ignore.case = TRUE)

### Remove all instances of bitch
texts <- lapply(X = texts, FUN = gsub,
                pattern = "(son[s]*)*.*?(of)*.*(a*).*(bitch)+(es| )*",
                replacement = "", ignore.case = TRUE)

### Remove the word cunt
texts <- lapply(X = texts, FUN = gsub, pattern = "cunt", replacement = "",
                ignore.case = TRUE)

### Remove the word damn
texts <- lapply(X = texts, FUN = gsub, pattern = "damn", replacement = "",
                ignore.case = TRUE)

### Remove the phrase mother fucker
texts <- lapply(X = texts, FUN = gsub, pattern = "mother.*?fucker",
                replacement = "", ignore.case = TRUE)

### Remove the word fuck and its variants
texts <- lapply(X = texts, FUN = gsub, pattern = "fuck[deginr]*", replacement = "",
                ignore.case = TRUE)

### Remove the phrase Holy Shit
texts <- lapply(X = texts, FUN = gsub, pattern = "holy.*?shit",
                replacement = "", ignore.case = TRUE)

### Remove the phrase shitass
texts <- lapply(X = texts, FUN = gsub, pattern = "shit.*?ass",
                replacement = "", ignore.case = TRUE)

### Remove the word shit and its variants
texts <- lapply(X = texts, FUN = gsub, pattern = "shit[deginrty]*", replacement = "",
                ignore.case = TRUE)

### Remove all instances of whore
texts <- lapply(X = texts, FUN = gsub,
                pattern = "(son[s]*)*.*?(of)*.*(a*).*(whore)+(s| )*",
                replacement = "", ignore.case = TRUE)

### Add the start of phrase and end of phrase tokens to the file(s).
texts <- lapply(X = texts, FUN = gsub, pattern = "^", replacement = "<s> <s> ")
texts <- lapply(X = texts, FUN = gsub, pattern = "$", replacement = "</s>")

# Save the text file
output <- c(texts$blogs, texts$news, texts$twit)
my_con = file(description = paste0(getwd(), "/samples/compiled/fulltext.txt"),
              encoding = "UTF-8")
writeLines(output, con = my_con)

close(my_con)
