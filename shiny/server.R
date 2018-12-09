#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source('midata-helper.R')
source('graphics.R')

# import and prepare data
  # midata: load, extract, prepare
    client <- setupMidata(forceLogin = FALSE)
    conditions <- extractData(queryMidata(client))
    conditions <- prepareData(conditions)

  # split off headache
    headaches <- data.frame(day = subset(conditions, conditions$intensity > 0)$day, intensity = subset(conditions, conditions$intensity > 0)$intensity, duration = subset(conditions, conditions$intensity > 0)$duration, uid = subset(conditions, conditions$intensity > 0)$uid)
    headaches <- colourize(headaches, c("darkolivegreen4", "yellow", "red"))

# / import and prepare data



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$intensity <- renderPlot({
    
    title <- "Kopfschmerz-Intensität nach Tagen"
    alldates <- c()
    
    alldates <- c(alldates, headaches$day)
    
    if(input$autodate){
      if(length(alldates) == 0){
        startDate <- Sys.Date #as.Date("2018-11-13", origin="1970-01-01")
        endDate <- Sys.Date   #as.Date("2018-11-13", origin="1970-01-01")
      }
      else{
        startDate <- as.Date(min(alldates), origin="1970-01-01") # find earliest date
        endDate <- as.Date(max(alldates), origin="1970-01-01")  # find latest date
      }
    }
    else{
      startDate <- as.Date(input$daterange[1], origin="1970-01-01")
      endDate <- as.Date(input$daterange[2], origin="1970-01-01")
    }
    
    dayFrame <- preparePlot(from=startDate, to=endDate,label="UID", yLim=c(1,5))
    plotFrame <- merge(x=dayFrame,y=headaches, all.x=TRUE)
    if(input$patient > 0){
     drawCurve(amplitude = input$amplitude, zero = input$patient, threshold = input$threshold, offset = input$offset, period = input$period, showLine = input$line)
    }
    #drawCurve(amplitude = 0.25, zero = 4, threshold = 0.15, offset = -7.5, period = 10*pi, showLine = FALSE)
    lines(plotFrame$day, plotFrame$uid, type="p", col=plotFrame$col, pch = 16, cex = (plotFrame$duration)/6 )

    

  })
  output$stats <- renderPrint({
    headaches
  })
  output$patname <- renderText("Migraine curve")
  
})
