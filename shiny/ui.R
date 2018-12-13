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
# titlePanel("migraine data viewer"),

#Navbar begin
navbarPage("migraine data viewer",
        
           #first Tab -> Headache data
           tabPanel("All patients", selected=TRUE,
                    
                    sidebarLayout(
                      sidebarPanel(
                        tags$h3(textOutput("patname")),
                        # TODO: dynamically generate choices list with actual number of patients
                        selectInput("patient", "display curve for patient ID", choices = c('none'=0, 'Patient 1'=1,'Patient 2'=2, 'Patient 3'=3,'Patient 4'=4,'Patient 5'=5,'Patient 6'=6)),
                        
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
                        dateRangeInput(inputId = "daterange", label = NULL, language = "en", separator = " to ", start="2018-10-17", end="2018-11-16")
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        plotOutput("intensity", height=700)
                         # , verbatimTextOutput("stats")
                      )
                    )
           ),
           
           #second Tab -> patient detail
           tabPanel("Patient detail",
                    sidebarLayout(
                      sidebarPanel(
                        # TODO: dynamically generate choices list with actual number of patients
                        selectInput("uid", "Choose Patient", choices = c('Patient 1'='1','Patient 2'='2', 'Patient 3'='3','Patient 4'='4','Patient 5'='5','Patient 6'='6')),
                        
                        checkboxGroupInput(inputId = "symptoms2", label="display findings", 
                                           choiceNames = c("Nausea (N)", "Photophobia (P)", "Phonophobia (O)", "Hyperosmia (H)", "Menstruation (M)", "Stress (S)", "Weather influence (W)"),
                                           choiceValues = c("422587007",  "409668002",       "313387002",       "45846002",       "276319003",        "73595000",   "45893009")),
                        
                        checkboxInput(inputId = "asdfsad", label = "show migraine curve", value = TRUE),
                        
                        tags$h3("date range:"),
                        checkboxInput(inputId = "autodate2", label = "auto", value = TRUE),
                        dateRangeInput(inputId = "daterange2", label = NULL, language = "en", separator = " to ", start="2018-10-17", end="2018-11-16")
                        
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        plotOutput("patientDetail", height=700)
                        # , verbatimTextOutput("stats")
                      )
                    )
           )
           
)
