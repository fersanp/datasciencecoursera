#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(
  fluidPage(
    titlePanel(HTML("Car Consumption: <br><small> Relationship between variables and miles per gallon (MPG)")
    ),
    mainPanel(
      selectInput("variable", "Variable:",
                  c("Number of cylinders" = "cyl",
                    "Horsepower" = "hp",
                    "Weight (lb/1000)" = "wt",
                    "Transmission" = "am",
                    "Number of gears" = "gear",
                    "Number of carburetors" = "carb"
                    )),
      h3("Regression model: "), 
      h3(textOutput("caption")),
      plotOutput("mpgPlot"),
      verbatimTextOutput("fit")
      )
    )
  )


