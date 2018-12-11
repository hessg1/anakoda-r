#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)


#shinyUI(fluidPage(

# Application title
# titlePanel("migrEn data view"),

#Navbar begin
navbarPage("migrEn Data Viewer",
        
           #first Tab -> Headache data
           tabPanel("Headache", selected=TRUE,
                    
                    # Sidebar with a slider input for number of bins 
                    sidebarLayout(
                      sidebarPanel(
                        tags$h3(textOutput("patname")),
                        numericInput(inputId = "patient", label = "display curve for patient ID", min=0, max=5, step=1, value=0),
                        
                        conditionalPanel(
                          inputId = "cond1",
                          condition = "input.patient > 0",
                          sliderInput(inputId = "amplitude", label = "amplitude", min=0, max=1, value=0.6),
                          sliderInput(inputId = "period", label = "period", min=1, max=50, value=31),
                          sliderInput(inputId = "offset", label = "offset", min=0, max=50, value=12),
                          sliderInput(inputId = "threshold", label = "threshold", min=0, max=0.5, value=0.2),
                          checkboxInput(inputId = "line", label = "display threshold line", value = TRUE)
                        ),
                        
                        checkboxGroupInput(inputId = "symptoms", label="display findings", 
                                           choiceNames = c("Nausea (N)", "Photophobia (P)", "Phonophobia (O)", "Hyperosmia (H)", "Menstruation (M)", "Stress (S)", "Weather influence (W)"),
                                          choiceValues = c("422587007",  "409668002",       "313387002",       "45846002",       "276319003",        "73595000",   "45893009")),
                        
        
                        
                        tags$br(),
                        checkboxInput(inputId = "values", label = "show headache intensity values", value = FALSE),
                        
                        tags$h3("date range:"),
                        checkboxInput(inputId = "autodate", label = "auto", value = TRUE),
                        dateRangeInput(inputId = "daterange", label = NULL, language = "en", separator = " to ")
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        plotOutput("intensity", height=700)
                         # , verbatimTextOutput("stats")
                      )
                    )
           ),
           
           #second Tab -> more information
           tabPanel("Summary",
                    fluidRow(
                      column(6,
                             #includeMarkdown("introduction.md")
                             helpText("Hallo")
                      ),
                      column(3,
                             img(class="img-polaroid",
                                 src="/img/bfh_logo_de.png"),
                             tags$small(
                               "Source: Photographed at the Bay State Antique ",
                               "Automobile Club's July 10, 2005 show at the ",
                               "Endicott Estate in Dedham, MA by ",
                               a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                 "User:Sfoskett")
                             )
                      )
                    )
           )
           
)
