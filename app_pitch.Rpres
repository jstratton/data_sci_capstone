Text Completion App
========================================================
author: James Stratton
date: July 16, 2016
autosize: true

https://github.com/jstratton/data_sci_capstone.git

Introduction
========================================================

- People enter text on their phones on a daily basis.
- You can only enter one character at a time on touchscreen keyboards.
- Repetitive motion injuries can become a problem after extended use.
- Text completion algorithms can fill in words for users.

My Approach
========================================================

- My app uses three word histories to predict future words.
- I used a 3-gram Kneser-Ney model to determine word probabilities.
- Stupid back off to lower order models was used to handle unobserved histories.
- If none of the history was observed, the model returns the word observed to complete the most histories.
- Twitter data was used for training because the app is intended for short phrases.

Results
========================================================

- Natural continuations are predicted 80% of the time.
- Exact words are predicted 15% of the time.
- Word history tables take up 136.6 MB of memory, or 100 MB when implemented as data tables.
- For comparison, smartphones have about 2 to 4 GB of RAM.

![alt text](./app_pitch-figure/app_pic.png)

Conclusion
===

- User typing demands could be reduced by %80.
- This reduction should significantly reduce repetitive motion injuries.
- Adapting and optimizing the app for a smartphone environment are the main tasks required for implementation.
