#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source('functions0.3.R')

# import and prepare data
  # set up RonFHIR client (with new authentification every time)
  client <- setupMidata("http://test.midata.coop", FALSE)
  
  # query resources
  resources <- queryMidata(client)

  # extract data (for compatibility reasons we have to do special case for claudia at the moment)
  if(resources$entry$resource$subject$display[1] == "Claudia Zimmermann"){
    conditions <- extractData(resources)
  } else {
    conditions <- extractDataOld(resources)
  }
  
  # do some factorizing
  conditions$findingText <- factor(conditions$findingText)
  conditions$bodysiteText <- factor(conditions$bodysiteText) 
  
  # convert timestamp to date
  #conditions$duration = conditions$endTime - conditions$startTime
  #conditions$startTime <- as.POSIXct(conditions$startTime, origin="1970-01-01")
  conditions$day <- as.Date(conditions$startTime)
  
  headache <- data.frame(day = subset(conditions, conditions$intensity > 0)$day, intensity = subset(conditions, conditions$intensity > 0)$intensity)
  colours <- colorRampPalette(c('red','green'))
  headache$col <- colours(10)[as.numeric(cut((headache$intensity)/10,breaks = 10))]
  # this does not work as intended, see https://stackoverflow.com/questions/9946630/colour-points-in-a-plot-differently-depending-on-a-vector-of-values
  
# / import and prepare data



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$intensity <- renderPlot({
    
    title <- "Kopfschmerz-Intensität nach Tagen"
    alldates <- c()
    
    alldates <- c(alldates, headache$day)
    
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
    
    dayFrame <- data.frame(day = seq(startDate, endDate, 1), pain_intensity=c(-1))
    plot(dayFrame, ylim=c(0,10), type="n", xlab="Tag", ylab="Intensität Kopfschmerzen")
    abline(v=startDate:endDate,lty=2,col="lightgrey")
    curve(4*sin(0.5*x)+5, n = 1000, col = "salmon", add = TRUE)
    
    plotFrame <- merge(x=dayFrame,y=headache, all.x=TRUE)
    lines(plotFrame$day, plotFrame$intensity, col = headache$col, type="p", pch=15)

    

  })
  output$stats <- renderPrint({
    headache
  })
  output$patname <- renderText({paste("Hallo,", conditions$name[1])})
  
})
