Next Word Predictor Project
========================================================
author: Fernanda Sanchez 
date: 2020-10-23
autosize: true

The Project
========================================================
This project involves Natural Language Processing.  
The task is to take a user's input phrase and to output a recommendation of a predicted next word.  
It uses english language only.

Parts of the project:  
- Next Word Prediction Model, as basis for an app
- Next Word Prediction App hosted at shinyapps.io
- Presentation hosted at R pubs


The Model
========================================================
The model was trained on data provided by SwiftKey on blogs, news and twitter. 
The english language data provided was 556MB and the model was trained using 1% of the sample.

The model adapts a set of n-grams (a contiguous sequence of n words from a given sample of text) to make a prediction on the next likely word, if no results were found then it will display a question mark *?*.


The Steps
========================================================
The prediction model creates a *tidy data* using text mining in R. 

The model steps are: 

1. Read the raw text files for model training.
2. Clean training data and separate it into 2 word, 3 word, 4 word and n grams.
3. Sort n grams by frequency
4. Prediction function uses a reverse type prediction model
  - user supplies an input phrase
  - model uses last 3, 2, or 1 words to predict the best 4th, 3rd, or 2nd match 
5. Shows the next word prediction as an output


The App
========================================================
The next word prediction app provides a simple user interface to the next word prediction model.  

*Features:*  

1. Text box for user input  
2. Predicted next word outputs dynamically below user input  
3. Tabs with plots of most frequent n grams in the data-set
4. Tab panel with user instructions  


