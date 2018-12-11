#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# import libraries, custom functions and snomed dictionary
  library(shiny)
  source('midata-helper.R')
  source('graphics.R')
  coding <- read.csv("codingSCT.csv", sep=",", header=T, row.names = "code")
# / import libraries, custom functions and snomed dictionary
  
# import and prepare data
  # midata: load, extract, prepare
    client <- setupMidata(forceLogin = FALSE)
    conditions <- extractData(queryMidata(client))
    conditions <- prepareData(conditions)

  # split off headache
    headaches <- data.frame(day = subset(conditions, conditions$intensity > 0)$day, intensity = subset(conditions, conditions$intensity > 0)$intensity, duration = subset(conditions, conditions$intensity > 0)$duration, uid = subset(conditions, conditions$intensity > 0)$uid)
    headaches <- colourize(headaches, c("darkolivegreen4", "orange", "red3"))

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
    
    dayFrame <- preparePlot(from=startDate, to=endDate,label="UID", yLim=c(0.5,6.2))
    plotFrame <- merge(x=dayFrame,y=headaches, all.x=TRUE)
    if(input$patient > 0 && is.numeric(input$patient)){
      # draw migraine curve
      drawCurve(amplitude = input$amplitude/2, zero = input$patient, threshold = input$threshold, offset = input$offset, period = input$period, showLine = input$line)
    }
    # draw data points
    lines(plotFrame$day, plotFrame$uid, type="p", col=plotFrame$col, pch = 16, cex = (plotFrame$duration)/3 )
    
    # draw intensity values if requested
    if(input$values){
      text(plotFrame$day, plotFrame$uid, plotFrame$intensity, cex=0.6)
    }
    
    # draw all checked symptoms
    i <- 2
    if(!is.null(input$symptoms)){
      for(code in input$symptoms){
        addToPlot(code, colour="wheat4", symbol=as.character(coding[code,'symbol']), days = dayFrame, conditions = conditions, offset = i)
        i <- i+1
      }
    }
    
    

  })
  
  output$patientDetail <- renderPlot({
    user <- subset(conditions, (conditions$uid == input$uid))
    user <- subset(user, user$intensity > 0)
    user <- merge(data.frame(intensity = user$intensity, time = user$startTime), data.frame(intensity = c(0), time = user$endTime), all=TRUE)
    firstDate <- min(user$time) - 1 
    str(firstDate)
    user <- rbind(user, data.frame(time=firstDate, intensity=0))
    user <- user[order(user$time),]
    
    plot(user$time, user$intensity, type="s", col="mediumblue")
  })
  
   # output$stats <- renderPrint({
   #   conditions$findingText
   # })
  output$patname <- renderText("Migraine curve")
  
})
