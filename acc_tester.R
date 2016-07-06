# The purpose of this script is to give accuracy ratings to the models.

library(data.table)

# Load a set of test data from the Twitter entries
dir <- paste0(getwd(),"/final/en_US")

texts <- list(twit = readLines(con = paste0(dir, "/en_US.twitter.txt"),
                               encoding = "UTF-8")
)

# Remove the samples that were included in the training set

