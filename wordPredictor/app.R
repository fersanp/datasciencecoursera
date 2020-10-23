#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#suppressPackageStartupMessages({
#  library(tidyverse)
#  library(stringr)
#})

source("./predictor.R")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Predictor Application"),
  mainPanel(
    # Show a plot of the generated distribution
      tabsetPanel(
        tabPanel("Predictor",
                h3("Predicted Next Word:"),
                textInput("user_input", h3("Enter some words:"), 
                         value = "Input"),
               h4(em(span(textOutput("ngram_output"))))
        ),
        tabPanel("Top Sextgrams",
                 br(),
                 img(src = "./sextgrams.png", height = 500, width = 700)),
        
        tabPanel("Top Quintgrams",
                 br(),
                 img(src = "./quintgrams.png", height = 500, width = 700)),
        
        tabPanel("Top Quadgrams",
                 br(),
                 img(src = "./quadgrams.png", height = 500, width = 700)),
        
        tabPanel("Top Trigrams",
                 br(),       
                 img(src = "./trigrams.png", height = 500, width = 700)),
        
        tabPanel("Top Bigrams",
                 br(),
                 img(src = "./bigrams.png", height = 500, width = 700)),
        
        tabPanel("Top Monograms",
                 br(),
                 img(src = "./monograms.png", height = 500, width = 700)),
        

        tabPanel(
            "Instructions", 
            h5("1. Enter a word or words in the text box."),
            h5("2. The predicted next word is printed below."),
            h5("3. No need to hit enter of submit."),
            h5("4. A question mark means no prediction"),
            h5("5. The tool works in english only."))
      )
    )
  )
)
  



server <- function(input, output, session) {
  output$ngram_output <- reactive({
    if (input$user_input == "")
      return(NULL)
    ngrams(input$user_input)
  })
}


shinyApp(ui, server)