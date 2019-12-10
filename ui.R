library(shiny)
shinyUI(fluidPage(
    titlePanel("This app will predict the next word if you
                type something. A chart and wordcloud will
                also show other likely words."),
    sidebarLayout(
        sidebarPanel(
            textInput("usertext", "Please Erase & Type Here",value="is going to be a"),
            submitButton("Submit"),
            plotOutput("cloudcloud")
        ),
        mainPanel(style="font-size:75px;font-weight:bold;color:blue",
            textOutput("predicted"),
            plotOutput("plotplot")
        )
    )
))