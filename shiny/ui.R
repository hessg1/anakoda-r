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
      checkboxInput(inputId = "u1headache", label = "Kopfschmerzen anzeigen", value = TRUE),
      checkboxInput(inputId = "u1photophobia", label = "Lichtempfindlichkeit anzeigen", value = FALSE),
      checkboxInput(inputId = "u1nausea", label = "Übelkeit anzeigen", value = FALSE),
      checkboxInput(inputId = "etc", label = "...", value = FALSE),
      tags$br(),
      tags$h3("Datumsbereich:"),
      checkboxInput(inputId = "autodate", label = "automatisch", value = TRUE),
      dateRangeInput(inputId = "daterange", label = NULL, language = "de", separator = " bis ")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("intensity"),
      verbatimTextOutput("stats")
    )
  )
))
