#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
        
        # Title
        headerPanel("Text Filling App"),
        
        # Text Entry Field
        sidebarPanel(
                # Create a text input
                textInput("history", "Enter your Phrase Here:", ""),
                
                # Add a submit button
                submitButton(text = "Submit")
        ),
        
        # Output result.
        mainPanel(
                h3("Predicted Word:"),
                
                verbatimTextOutput("answer")
        )
))
