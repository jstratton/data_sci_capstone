Text Completion App
========================================================
author: James Stratton
date: July 16, 2016
autosize: true

Introduction
========================================================

- People enter text on their phones on a daily basis.
- You can only enter one character at a time on touchscreen keyboards.
- Repetitive motion injuries can become a problem after extended use.
- Text completion algorithms can fill in words for users.

My Approach
========================================================

- My app uses three word histories to predict future words.
- I used a simple Kneser-Ney model to determine word probabilities.
- Twitter data was used to train the model because the app is intended to complete short phrases.
- If a given history was never observed in training, my model backs off to successively shorter histories.
- If none of the history was observed, the model returns the word observed to complete the most histories.

Results
========================================================

- The model predicts the user's exact word 15% of the time.
- However, the app predicts logical continuations 80% of the time.
- Word history tables take up 136.6 MB of memory, or 100 MB when implemented as data tables.
- For comparison, smartphones have RAM on the order of 2 to 4 GB.
- Adapting this model to a smartphone shouldn't be a problem.
