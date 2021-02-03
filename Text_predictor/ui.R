#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(data.table)

# Define UI for application that perform a text predictor
shinyUI(fluidPage(

    
    # Application title
    titlePanel("Text predictor for Coursera Capstone project"),
    h5("by Alessandro Vasta"),

    # Sidebar with a text input for next word prediction
    sidebarLayout(

        sidebarPanel(
            h6("Enter your text in 'Text input' bar below and press the button 'Run prediction'
               to get the next predicted word"),
            textInput("text", label = h5("Text input"), value = ""),
            actionButton("act0", label = "Run prediction"),
            h6("Press one of the following buttons to add the correspondent 
               predicted word to the 'Prefix text for prediction' bar, or type a new
               sentence in the above 'Text input' bar"),
            actionButton("act1", label = "word 1"),
            actionButton("act2", label = "word 2"),
            actionButton("act3", label = "word 3"),
        ),
        

        # Show up to three possible predicted words in probability order
        mainPanel(
            hr(),
            h4("Prefix text for prediction"),
            fluidRow(column(6, verbatimTextOutput("value"))),
            h4("Predicted words"),
            h6("Note: possible unobserved type words are the outcome of a Katz Back Off  
               model (KBO) when prefix length = 2"),
            fluidRow(column(6, verbatimTextOutput("mywords"))),
            h4("based on effective prefix"),
            fluidRow(column(6, verbatimTextOutput("prefix")))
        )
    )
))
