library(shiny)

shinyUI(fluidPage(
  titlePanel("TextPredictor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Type a one or two word input into the input box (all lowercase) and press 'Enter'."),
      
      textInput("word", "Input", value="Data Science Capstone"),
      textOutput("first"),
      textOutput("second"),
      textOutput("third"),
      textOutput("word")
    ),
    
    mainPanel( #no main panel
    )
  )
))