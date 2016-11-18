shinyUI(fluidPage(
    titlePanel("Movie Review Scorer"),
    
    textInput("text","Enter Review...",""),
    fluidRow(column(5,verbatimTextOutput("value")))
    
)
)
