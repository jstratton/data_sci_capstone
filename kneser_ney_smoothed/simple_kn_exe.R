# This file actually makes a prediction based on the saved n-gram tables

# Load required libraries
library(data.table)

# Load the n-gram data tables
loc <- "/saved_models/simple_kneser_ney/"
monograms <- fread(paste0(getwd(), loc, "KN_monograms.txt"))
bigrams <- fread(paste0(getwd(), loc, "KN_bigrams.txt"))
trigrams <- fread(paste0(getwd(), loc, "KN_trigrams.txt"))

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
                
                # Return the zero order probabilities
                P_KN0[, P0 := log(P0)]
                P_KN0[order(-P0)]
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
                
                # Return the top completions
                P_KN2 <- P_KN2[order(-P2)]
                monograms[list(P_KN2[,head(completion)])]
                #P_KN2[,head(P2)]
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
                N1 <- trigrams[list(second), sum(tri_freq > 0), by = tri_third]
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
                
                # Output a prediction
                P_KN3 <- P_KN3[order(-P3)]
                monograms[list(P_KN3[,head(completion3)])]
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
        
        # Convert each word to a numeric using the map
        setkey(monograms, word)
        history[, grams := monograms[list(grams), map]]
        
        # Sort the monograms based on the map
        setkey(monograms, map)
        
        # Use a bigram model if we only have one word in the history
        if(nrow(history) < 2){
                # Run the bigram model
                bigram_model(history)
        }
        else{
                # Try a trigram model if we have more than one word in history.
                trigram_model(history)
        }
}
