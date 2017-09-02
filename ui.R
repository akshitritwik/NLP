

library(shiny)
shinyUI(fluidPage(
  textInput("text", label = h2("Cardiology Next Word Predictor"), value = "myocardial"),
  submitButton(text = "Predict next word..."),
  hr(),
  fluidRow((verbatimTextOutput("value")))
  
))
