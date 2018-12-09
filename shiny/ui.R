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
shinyUI(fluidPage(
  
  # Application title
  titlePanel("migrEn data view"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tags$h3(textOutput("patname")),
      numericInput(inputId = "patient", label = "display curve for patient ID", min=0, max=5, step=1, value=4),

      sliderInput(inputId = "amplitude", label = "amplitude", min=0, max=1, value=0.6),
      sliderInput(inputId = "period", label = "period", min=1, max=50, value=31),
      sliderInput(inputId = "offset", label = "offset", min=1, max=50, value=12),
      sliderInput(inputId = "threshold", label = "threshold", min=0, max=0.5, value=0.2),
      checkboxInput(inputId = "line", label = "display threshold line", value = TRUE),
  
      tags$br(),
      checkboxInput(inputId = "values", label = "show intensity values", value = FALSE),
      tags$h3("date range:"),
      checkboxInput(inputId = "autodate", label = "auto", value = TRUE),
      dateRangeInput(inputId = "daterange", label = NULL, language = "en", separator = " to ")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("intensity", height=700)
     # verbatimTextOutput("stats")
    )
  )
))
