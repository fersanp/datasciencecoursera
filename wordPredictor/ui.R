#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})

source("./predictor.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Predictor Application"),
    mainPanel(
    # Show a plot of the generated distribution
      tabsetPanel(
        tabPanel("Predictor",
                textInput("user_input", h3("Enter some words:"), 
                         value = "Input"),
               h3("Predicted Next Word:"),
               h4(em(span(textOutput("ngram_output"))))
        ),
        tabPanel("Top Sextgrams",
                 br(),
                 img(src = "./sextgrams.png", height = 550, width = 550)),
        
        tabPanel("Top Quintgrams",
                 br(),
                 img(src = "./quintgrams.png", height = 550, width = 550)),
        
        tabPanel("Top Quadgrams",
                 br(),
                 img(src = "./quadgrams.png", height = 550, width = 550)),
        
        tabPanel("Top Trigrams",
                 br(),       
                 img(src = "./trigrams.png", height = 550, width = 550)),
        
        tabPanel("Top Bigrams",
                 br(),
                 img(src = "./bigrams.png", height = 550, width = 550)),
        
        tabPanel("Top Monograms",
                 br(),
                 img(src = "./monograms.png", height = 550, width = 550)),
        
        tabPanel(
            "Instructions", 
            h5("1. Enter words in the text box."),
            h5("2. The predicted next word is printed below."),
            h5("3. A question mark means no prediction"),
            h5("4. The tool works in english only.")
      )
  ))
))
