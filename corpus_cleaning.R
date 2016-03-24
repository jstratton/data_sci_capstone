# This script contains code for importing and cleaning the corpus.

# Load Required Libraries

# ## Remove all punctuation except for ' and -
# sample_text <- tm_map(x = sample_text, FUN = gsub, 
#                       pattern = "[][!#$%&()*+,./:;<=>?@^_`{|}~]", 
#                       replacement = "", ignore.case = TRUE)
# 
# ## Remove all numerals
# sample_text <- tm_map(x = sample_text, FUN = removeNumbers)
# 
# ## Remove all webaddresses
# sample_text <- tm_map(x = sample_text, FUN = gsub,
#                       pattern = "((http|w{3}).*?(com|edu|org| ))",
#                       replacement = "", ignore.case = TRUE)
# 
# ## Remove all swearwords (https://en.wiktionary.org/wiki/Category:English_swear_words)
# 
# ### Remove arse/asshole
# sample_text <- tm_map(x = sample_text, FUN = gsub, pattern = "(ass|arse).*hole", 
#                                         replacement = "", ignore.case = TRUE)
# 
# ### Remove all singlet "ass" tokens
# sample_text <- tm_map(x = sample_text, FUN = gsub, pattern = "ass[^[:alnum:]]",
#                                         replacement = "", ignore.case = TRUE)
# 
# ### Remove all variants of goddamn
# sample_text <- tm_map(x = sample_text, FUN = gsub, pattern = "god.*?damn",
#                                         replacement = "", ignore.case = TRUE)
# 
# ### Remove all bastards from the text
# sample_text <- tm_map(x = sample_text, FUN = gsub, pattern = "bastard[s]*", replacement = "", ignore.case = TRUE)
# 
# ### Remove all instances of bitch
# sample_text <- tm_map(x = sample_text, FUN = gsub,
#                       pattern = "(son[s]*)*.*?(of)*.*(a*).*bitch(es| )",
#                       replacement = "", ignore.case = TRUE)
# 
# ### Remove the word cunt
# sample_text <- tm_map(x = sample_text, FUN = gsub, pattern = "cunt", replacement = "",
#                       ignore.case = TRUE)
# 
# ### Remove the word damn
# sample_text <- tm_map(x = sample_text, FUN = gsub, pattern = "damn",
#                       replacement = "", ignore.case = TRUE)
# 
# ### Remove the phrase mother fucker
# sample_text <- tm_map(x = sample_text, FUN = gsub, pattern = "mother.*?fucker",
#                       replacement = "", ignore.case = TRUE)
# 
# ### Remove fuck
# sample_text <- tm_map(x = sample_text, FUN = gsub, pattern = "fuck",
#                       replacement = "", ignore.case = TRUE)
# 
# ### Remove the phrase Holy Shit
# sample_text <- tm_map(x = sample_text, FUN = gsub, pattern = "holy.*?shit",
#                       replacement = "", ignore.case = TRUE)
# 
# ### Remove the phrase shitass
# sample_text <- tm_map(x = sample_text, FUN = gsub, pattern = "shit.*?ass",
#                       replacement = "", ignore.case = TRUE)
# 
# ### Remove the word shit
# sample_text <- tm_map(x = sample_text, FUN = gsub, pattern = "shit", replacement = "", 
#                       ignore.case = TRUE)
# 
# ### Remove the phrase son of a whore
# sample_text <- tm_map(x = sample_text, FUN = gsub, 
#                       pattern = "(son[s]*)*.*?(of)*.*(a*).*whore(s| )", 
#                       replacement = "", ignore.case = TRUE)
# 
# sample_text <- tm_map(x = sample_text, FUN = stripWhitespace)
# 
# # Take a random 5% sample from the corpus
# pcnt <- 5
# set.seed(3232016)
# sample_text <- lapply(X = texts, FUN = function(x){sample(x[[1]], size = ceiling(pcnt*length(x[[1]])/100))})
