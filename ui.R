library(shiny)
library(datasets)
library(dplyr)
library(ggplot2)
library(mice)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Ozone in New York City from May to September 1973"),
    
    fluidRow(
        column(width = 2, 
               radioButtons("method", "Estimation Method",
                     choices = c(LinearModel = "lm",
                                 Imputation = "imp"),
                     selected = "lm")
               ),
        column(width = 6, offset = 1,
               sliderInput("period", 
                           "Select period to be analized",
                           min = as.Date("1973-05-01"), max = as.Date("1973-09-30"),
                           value = c(as.Date("1973-05-01"), as.Date("1973-09-30")),
                           timeFormat = "%F",
                           width = "100%",
                           step = 1)
               ),
        column(width = 2, offset = 1,
               actionButton("help1", label = "Show Instructions")
               )
    ),
    fluidRow(
        column(width = 6, 
               plotlyOutput("showPlot1")
        ),
        column(width = 6, 
               plotlyOutput("showPlot2")
        )
    ),
    fluidRow(
        column(width = 6, 
               plotlyOutput("showPlot3")
        ),
        column(width = 6, 
               plotlyOutput("showPlot4")
        )
    )
    )
)



