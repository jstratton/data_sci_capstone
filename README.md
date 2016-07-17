# Data Science Capstone
This is the repository for my Data Science Specialization Capstone project. The project involves building an app to predict a user's next word based on their input. Text completion is useful for speeding up text entry and reducing repetitive motion injuries, among other things.

## Getting and Cleaning Data

Text data was provided by SwiftKey. I chose to use the Twitter posts as my corpus because they are short sentences. This makes it possible to include a lot more entries than the news and blog posts. I assume that most users will only enter short phrases on the smartphones. Twitter posts will be a good match for such data due to their brevity.

I took several steps to clean the data. First, I removed all repeats from the Twitter corpus. Second, I removed all special characters from the Twitter posts. I went and removed most of the characters from different alphabets. This was mainly to prevent my model from predicting symbols such as emojis. I then went and removed all webaddresses from the corpus. Most webaddresses aren't common enough to be worth trying to predict. After that, I went and removed all swear words according to Wiktionary's list. Finally, I added a start of post string "<S> <S> <S> " to the beginning of each post to make it easier to predict words at the start of a sentence.

## Word Prediction Model

I analyzed my Twitter corpus to create term frequency tables. These are required to determine the probabilities of a given word. In several of the references I found on NLP, the frequencies were stored in two dimensional tables. One axis corresponds to the word history with the other corresponding to the completed word. I found that these take up too much memory and are hard to work with. The problem is that a lot of storage space is devoted to words that were never observed. Also, character vectors take up a lot more space than numerics do.

To solve this problem, I stored the frequency tables as NxL data tables. L is the number of distinct N-grams observed. Any N-gram continuation not observed in the data set is ommitted. No permanent memory is wasted on the zero entries in this structure. During word prediction, the table for the input history is temporarily filled with zeroes corresponding to the blank spaces. Mapping each word to a numeric also helped reduce the file size. See the files under Autofill/data for an example of these structures.

As for the actual model, I used a 4-gram Kneser-Ney model as my top level model. I chose the unmodified Kneser-Ney model because it was more straightforward to implement. In a nutshell, Kneser-Ney models work by discounting some of the probability mass from the observed N-gram completions to unobserved ones. Kneser-Ney models also use interpolation with lower order models to make words that appear in many contexts more probable than ones that occur in limited contexts. For more information about Kneser-Ney models, consult the references at the end of this Read Me.

A problem I encountered when working with the KN model is the fact that it has singularities if a given history was never observed. I implemented a stupid backoff approach to resolve this issue. If a given history was not observed, I would switch to the (N - 1)-gram model. Succesively lower order models would be used until a model with an observed history is found. Otherwise, the Kneser-Ney probability for a single word is calculated. Meaning that the word that completes the most phrases will be returned.

## Results

My model achieved an accuracy of ~15% when it attempted to predict the last word of a series of test phrases. Meaning that my model was able to exactly guess what the user's next word was 15% of the time. However, this performance evaluation is clearly an underestimate. For example, one of the test phrases was "I love ya", but my model predicts "you" for the phrase "I love". In actual use, mistakes like that wouldn't be a problem.

I decided to manually evaluate the results to see how well the model would perform for its intended purpose. To this end, I compared the model's output with the test input. If the model came up with a natural sounding completion, I marked that as a good result. Otherwise, I would mark it as a bad answer. After carrying out this evaluation, I found that my model came up with valid completions ~80% of the time. To view the results of my evaluation, open test_results.txt or enter the following commands in your R environment:

dir <- "./kneser_ney_smoothed_2"
test_results <- data.table(questions = readLines(con = paste0(dir, "/test_questions.txt"), encoding = "UTF-8"), answers = readLines(con = paste0(dir, "/test_answers.txt"), encoding = "UTF-8"), model_answers = readLines(con = paste0(dir, "/verifier.txt"), encoding = "UTF-8"), good_completion = readLines(con = paste0(dir, "/good_completions.txt"), encoding = "UTF-8"))

## File Locations

My repo is a bit of a mess. Here are the locations of specific files of interest:

- ./kneser_ney_smoothed_2/twit_cleaning.R -> the R script used to clean the Twitter corpus used to train/test my model

- ./kneser_ney_smoothed_2/simple_kn_constructor.R -> a R script that builds the N-gram frequency table needed by the model.

- ./kneser_ney_smoothed_2/simple_kn_exe_2.R -> the implementation of the KN model that was later copied into the server's code.

- ./acc_tester.R -> automatically tests the accuracy of a model. Accuracy is determined by how many exact matches the model obtains.

- ./Autofill -> all of the files used by my app are located here.

## Works Cited

- Good introduction to text completion: https://web.stanford.edu/~jurafsky/slp3/4.pdf

- Detailed explanation of Kneser-Ney models: http://www.speech.sri.com/projects/srilm/manpages/pdfs/chen-goodman-tr-10-98.pdf

- Swear Words List: https://en.wiktionary.org/wiki/Category:English_swear_words