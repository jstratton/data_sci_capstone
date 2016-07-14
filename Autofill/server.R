#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)

loc <- "C:/Users/James/data_sci_capstone/kneser_ney_smoothed_2/"
monograms <- fread(paste0(loc, "KN_monograms.txt"))
bigrams <- fread(paste0(loc, "KN_bigrams.txt"))
trigrams <- fread(paste0(loc, "KN_trigrams.txt"))
tetragrams <- fread(paste0(loc, "KN_tetragrams.txt"))

# Set the smoothing discount for the model
D = 0.75

# This function makes a prediction using a binary model based on the history
bigram_model <- function(history){
        # Extract the final word from the history
        final <- history[nrow(history), grams]
        # Check if the previous word is w/in our observed bigrams
        setkey(bigrams, big_first)
        if(bigrams[list(final), all(is.na(big_freq))]){
                # Compute the Kneser-Ney probabilities for each unigram
                P_KN0 <- copy(monograms)
                
                # Set the keys to make the process go faster
                setkey(P_KN0, map)
                setkey(bigrams, big_second)
                
                # Find the Kneser-Ney (log) probabilities for each continuation
                P_KN0[, P0 := bigrams[, length(big_freq), keyby = big_second]$V1]
                P_KN0[, P0 := P0/nrow(bigrams)]
                
                P_KN0[, P0 := log(P0)]
                
                # Sort the table by rank
                rank <- order(P_KN0[, P0], decreasing = TRUE)
                P_KN0 <- P_KN0[rank,]
                
                # Return the word corresponding to the top entry
                setkey(monograms, map)
                monograms[P_KN0[1, map], word]
        }
        else
        {
                # Sort the bigram table by history for later use
                setkey(bigrams, big_first)
                
                # Initialize a data table with an entry for every possible continuation
                P_KN2 <- data.table(completion = monograms[, min(map)]:monograms[, max(map)],
                                    count = 0L)
                setkey(P_KN2, completion)
                
                # Fill the data table with any counts that we have seen
                P_KN2[list(bigrams[list(final), big_second]), 
                      count := bigrams[list(final), big_freq]
                      ]
                
                # Compute the total number of counts for the given history
                total_counts <- P_KN2[,sum(count)]
                # Find the number of unique completions associated with the history
                unique_counts <- P_KN2[,sum(count > 0)]
                
                # Find the first order probabilities
                setkey(bigrams, big_second)
                P_KN1 <- data.table(mono_word = monograms[,map],
                                    P1 = bigrams[, max(sum(big_freq > 0) - D, 0),
                                                 by = big_second]$V1)
                P_KN1[, P1 := P1/nrow(bigrams)] # Normalize by the no. of bigrams
                P_KN1[, P1 := P1 + D/nrow(bigrams)] # Include the zero order term
                
                # Find the KN smoothed probability for each combination
                P_KN2[, P2 := max(count - D, 0)/total_counts, by = completion]
                P_KN2[, P2 := P2 + D*total_counts*P_KN1[, P1]/unique_counts]
                
                P_KN2[, P2 := log(P2)] # Convert to log probs
                
                # Return the top completion
                rank <- order(P_KN2[, P2], decreasing = TRUE)
                P_KN2 <- P_KN2[rank,]
                
                # Return the word corresponding to the top entry
                setkey(monograms, map)
                monograms[P_KN2[1, completion], word]
        }
}

# This function makes a prediction using a trigram model
trigram_model <- function(history){
        # Check to see if the history is w/in our observed trigrams
        setkey(trigrams, tri_first, tri_second) # Sort trigrams to allow binary searching
        
        first <- history[nrow(history) - 1, grams] # Store the penultimate word
        second <- history[nrow(history), grams] # store the final word of the history
        
        if(trigrams[list(first,second), all(is.na(tri_freq))]){
                # If the history wasn't observed, switch to the bigram model
                bigram_model(history)
        }
        else{
                # Initialize a data table with an entry for every possible continuation
                P_KN3 <- data.table(completion3 = monograms[, min(map)]:monograms[, max(map)],
                                    count = 0L,
                                    P3 = 0.0)
                setkey(P_KN3, completion3)
                
                # Fill the data table with any counts that we have seen
                P_KN3[list(trigrams[list(first, second), tri_third]), 
                      count := trigrams[list(first, second), tri_freq],
                      ]
                
                # Compute the total number of counts for the given history
                total_counts <- P_KN3[,sum(count)]
                # Find the number of unique completions associated with the history
                unique_counts <- P_KN3[,sum(count > 0)]
                
                # Find the probability for each completion based on counts
                P_KN3[,P3 := max(count - D, 0)/total_counts, by = completion3]
                
                # Find the 1st order probs
                setkey(bigrams, big_second) # Sort the bigram table properly
                P_KN1 <- data.table(completion1 = monograms[,map],
                                    P1 = bigrams[, max(sum(big_freq > 0) - D, 0),
                                                 by = big_second]$V1)
                setkey(P_KN1, completion1)
                
                P_KN1[, P1 := P1/nrow(bigrams)] # Normalize by the no. of bigrams
                P_KN1[, P1 := P1 + D/nrow(bigrams)] # Include the zero order term
                
                # Find the 2nd order probs
                # Initialize a data table to store the 2nd order probs
                P_KN2 <- data.table(completion2 = monograms[, min(map)]:monograms[, max(map)],
                                    P2 = 0.0)
                setkey(P_KN2, completion2) # set P_KN2's key to allow bin sorting
                
                # Order the trigrams to allow quick subsetting
                setkey(trigrams, tri_second)
                
                # Find N1+(*,w_i-1,w_i), note that w_i-1 is fixed
                N1 <- trigrams[list(second), length(tri_freq), by = tri_third]
                N1_total <- N1[,sum(V1)] # Find the total number of phrases containing w_i-1
                
                # Find N1+(w_i-1, *)
                setkey(bigrams, big_first)
                N1_preceding <- bigrams[list(second), sum(big_freq > 0)]
                
                # Map each N1+ value onto its corresponding word
                P_KN2[list(N1[,tri_third]), P2 := N1[,V1]]
                # Find the first term of the probability
                P_KN2[, P2 := max(P2 - D, 0)/N1_total, by = completion2]
                # Interpolate with the first order term
                P_KN2[, P2 := P2 + D*N1_preceding*P_KN1[,P1]/N1_total]
                
                # Interpolate P3 with P2 (and by extension P1)
                P_KN3[, P3 := P3 + D*unique_counts*P_KN2[,P2]/total_counts]
                
                P_KN3[, P3 := log(P3)] # Convert to log probs
                
                # Rank the different words based on probability
                rank <- order(P_KN3[, P3], decreasing = TRUE)
                P_KN3 <- P_KN3[rank,]
                
                # Return the word corresponding to the top entry
                setkey(monograms, map)
                monograms[P_KN3[1, completion3], word]
        }
}

tetragram_model <- function(history){
        # Sort the tetragram table to allow binary searching
        setkey(tetragrams, tetra_first, tetra_second, tetra_third)
        
        # Store the relevant terms from history for ease of use
        first <- history[nrow(history) - 2, grams]
        second <- history[nrow(history) - 1, grams]
        third <- history[nrow(history), grams]
        
        # Back off to the trigram model if this history wasn't observed
        if(tetragrams[list(first, second, third), all(is.na(tetra_freq))]){
                trigram_model(history)
        }
        else{
                # Find the 1st order probs
                ### Find N1+(*,w_i), sum(N1+(*,w_i))
                setkey(bigrams, big_second) # Sort the bigram table properly
                P_KN1 <- data.table(completion1 = monograms[,map],
                                    P1 = bigrams[, max(sum(big_freq > 0) - D, 0),
                                                 by = big_second]$V1)
                setkey(P_KN1, completion1)
                
                P_KN1[, P1 := P1/nrow(bigrams)] # Normalize by the no. of bigrams
                P_KN1[, P1 := P1 + D/nrow(bigrams)] # Include the zero order term
                
                # Find the 2nd order probs
                ## Initialize a data table to store the 2nd order probs
                P_KN2 <- data.table(completion2 = monograms[, min(map)]:monograms[, max(map)],
                                    P2 = 0.0)
                setkey(P_KN2, completion2) # set P_KN2's key to allow bin sorting
                
                ## Order the trigrams to allow quick subsetting
                setkey(trigrams, tri_second)
                
                ## Find N1+(*,w_i-1,w_i), note that w_i-1 is fixed
                N1 <- trigrams[list(second), length(tri_freq), by = tri_third]
                N1_total <- N1[,sum(V1)] # Find the total number of phrases containing w_i-1
                
                ## Find N1+(w_i-1, *)
                setkey(bigrams, big_first)
                N1_preceding <- bigrams[list(second), length(big_freq)]
                
                ## Map each N1+ value onto its corresponding word
                P_KN2[list(N1[,tri_third]), P2 := N1[,V1]]
                ## Find the first term of the probability
                P_KN2[, P2 := max(P2 - D, 0)/N1_total, by = completion2]
                ## Interpolate with the first order term
                P_KN2[, P2 := P2 + D*N1_preceding*P_KN1[,P1]/N1_total]
                
                # Find the 3rd order probs
                ## Initialize a table to store the 3rd order probs
                P_KN3 <- data.table(completion3 = monograms[, min(map)]:monograms[, max(map)],
                                    P3 = 0.0)
                setkey(P_KN3, completion3)
                
                ## Find N1+(*, w_i-2, w_i-1, *)
                setkey(tetragrams, tetra_second, tetra_third) # Sort the tetragrams
                tri_completions <- tetragrams[list(second, third),
                                              length(tetra_freq),
                                              by = tetra_fourth]
                
                ## Find sum(N1+(*, w_i-2, w_i-1, *)), over all words
                tri_completion_totals <- tri_completions[,sum(V1)]
                
                ## Map the completion statistics into P_KN3
                P_KN3[list(tri_completions[,tetra_fourth]),
                      P3 := tri_completions[,V1]]
                
                ## Compute the probability based on the completion stats
                P_KN3[, P3 := max(P3 - D, 0)/tri_completion_totals,
                      by = completion3]
                
                ## Find N1+(w_i-2, w_i-1, *)
                setkey(trigrams, tri_first, tri_second)
                tri_preceding <- nrow(tetragrams[list(first, second)])
                
                ## Interpolate with the 2nd order terms
                P_KN3[, P3 := P3 + D*tri_preceding*P_KN2[,P2]/tri_completion_totals]
                
                # Find the 4th order probs
                ## Create a DT to hold the probabilities
                P_KN4 <- data.table(completion4 = monograms[, min(map)]:monograms[, max(map)],
                                    count = 0L,
                                    P4 = 0.0)
                setkey(P_KN4, completion4)
                
                ## Sort the tetragrams properly
                setkey(tetragrams, tetra_first, tetra_second, tetra_third)
                
                ## Fill the data table with any counts that we have seen
                P_KN4[list(tetragrams[list(first, second, third), tetra_fourth]), 
                      count := tetragrams[list(first, second, third), tetra_freq],
                      ]
                
                ## Compute the total number of counts for the given history
                total_counts <- P_KN4[,sum(count)]
                ## Find the number of unique completions associated with the history
                unique_counts <- P_KN4[,sum(count > 0)]
                
                ## Find the probability for each completion based on counts
                P_KN4[,P4 := max(count - D, 0)/total_counts, by = completion4]
                
                ## Interpolate with the third order terms
                P_KN4[, P4 := P4 + D*unique_counts*P_KN3[,P3]/total_counts]
                
                P_KN4[, P4 := log(P4)] # Convert to log probs
                
                ## Rank the words
                rank <- order(P_KN4[, P4], decreasing = TRUE)
                P_KN4 <- P_KN4[rank,]
                
                # Return the word corresponding to the top entry
                setkey(monograms, map)
                monograms[P_KN4[1, completion4], word]
        }
}

# This function takes user input and returns a list of the most probable words
word_guesser <- function(history = ""){
        # Tokenize the history
        history <- unlist(strsplit(history, split = "[[:space:]]+"))
        # Convert the history into a data table
        history <- data.table(grams = history)
        # Remove any punctuation or special characters not included in my model
        history[, grams := gsub(x = grams, pattern = "[^[:alpha:][:space:]'-]", 
                                replacement = "", ignore.case = TRUE)]
        
        # Add the start of sentence markers to the history
        history <- data.table(grams = c("<S>", "<S>", "<S>", history[, grams]))
        
        # Convert each word to a numeric using the map
        setkey(monograms, word)
        history[, grams := monograms[list(grams), map]]
        
        # Sort the monograms based on the map
        setkey(monograms, map)
        
        # Try the tetragram model to start
        tetragram_model(history)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        output$answer <- renderText({word_guesser(input$history)})
})
