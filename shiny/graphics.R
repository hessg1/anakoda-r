## Plots the actual headache intensity over time, or adds an additional finding to a plot of headache intensity.
##
## Parameters:
## - userid*: the id of the patient whose data should be drawn
## - sct: the snomed CT code of the finding to be plotted (default: all headache codes)
## - conditions*: the dataset with the headache data and findings
## - colour: the colour of the plot (default: "steelblue3")
## - daterange: the data range of the plot, as a vector with start and enddate, as POSIXct.
##
## Return value:
## - nothing (a plot is drawn)
##
## Author: hessg1@bfh.ch  Date: 2018-12-12
##


plotBySCT <- function(userid, sct = NULL, conditions, colour = "steelblue3", daterange, description, ypos){
  user <- subset(conditions, (conditions$uid == userid))
  # if no SCT code is given, we will later draw all headache codes:
  if(is.null(sct)){
    user <- subset(user, (user$findingSCT == "162308004" | user$findingSCT == "162307009" | user$findingSCT== "162309007"))
    size <- 3
    typ <- "s"
    line <- "solid"
  }
  else if(sct == "276319003" | sct == "73595000"){
    user <- subset(user, (user$findingSCT == sct))
    size <- 10
    typ <- "p"
    line <- "blank"
    par(new=TRUE)
  }
  # else we can extract the wanted code
  else{
    user <- subset(user, (user$findingSCT == sct))
    size <- 3
    line <- "dotted"
    typ <- "s"
    par(new=TRUE)
  }
  if(length(user$uid) > 0){
    user <- merge(data.frame(intensity = user$intensity, time = user$startTime), data.frame(intensity = c(0), time = user$endTime), all=TRUE)
   
    # add a zero value before the first entry, so the curve starts at the bottom of the graph
    mydate <- min(user$time) - 1 
    user <- rbind(user, data.frame(time=mydate, intensity=0))
    
    #define data range or plot
    startdate <- daterange[1]
    enddate <- daterange[2]
    
    user <- rbind(user, data.frame(time=startdate, intensity=0))
    user <- rbind(user, data.frame(time=enddate, intensity=0))
    
    # make sure data is plotted in chronological order
    user <- user[order(user$time),]
    
    plot(user$time, user$intensity, type=typ, lty= line, col=colour, ylab="intensity", xlab="day", lwd = size, ylim = c(0,10))
    if(line == "blank"){
      line <- "solid"
    }
    legend(startdate, ypos , legend = description , col=colour, lty=line, cex=0.8, box.lty=0)
  }
}



## This function draws a sinus curve on a plot. This function needs an existing 
## plot to draw on.
##
## Parameters:
## - amplitude: the amplitude of the curve ("height") (default: 1)
## - period: the period of the curve ("width") (default: 1)
## - zero: the zeropoint of the sinus curve ("y-position") (default: 0)
## - offset: the offset of the curve ("x-position") (default: 0)
## - threshold: the cutoff-value (default: 0)
## - showLine: boolean, if TRUE a dotted horizontal line is plotted at the threshold (default: FALSE)
##
## Return value:
## - nothing (a plot is drawn)
##
## Author: hessg1@bfh.ch  Date: 2018-12-09
##
drawCurve <- function(amplitude = 1, period = 1, zero = 0, offset = 0, threshold = 0, showLine = FALSE){
  curve(zero-threshold + (amplitude  * sin(2 * pi * (x-offset)/period)), n = 1000/period, col = "grey", add = TRUE)
  
  if(showLine){
    curve(zero + 0*x, n= 2, col="grey", lty=3, add=TRUE)
  }
  
}

## This function adds symptoms to an existing plot. The plot has already to be drawn.
##
## Parameters:
## - sct*: the snomed CT code of the symptom to be drawn (has to be in "conditions", of course)
## - symbol: the symbol that will be drawn on the graph (one character or integer) (default: 8)
## - colour: what colour will the symbol be drawn (default: "darkgrey")
## - offset: by how many positions the symbol will be offset from the users base line (default: 1)
## - days: the data frame for the x-axis of the plot (usually dayFrame)
## - conditions: the data frame with the actual data
## 
## Return value:
## - nothing (a plot is drawn)
##
## Author: hessg1@bfh.ch  Date: 2018-12-10
##
addToPlot <- function(sct, symbol = 8, colour = "darkgrey", offset = 1, days = dayFrame, conditions = conditions){
  if(is.character(sct)){
    data <- data.frame(day = subset(conditions, conditions$findingSCT == sct)$day, uid = subset(conditions, conditions$findingSCT == sct)$uid, intensity = subset(conditions, conditions$findingSCT == sct)$intensity)
  }
  frame <- merge(x=days,y=data, all.x=TRUE)
  lines(frame$day, as.numeric(frame$uid)-(offset/10), type="p", col = colour, pch = symbol, cex = 0.8)
}


## This function adds a 'col' column to a dataset, with corresponding fading colour code 
## for headache intensity.
##
## Parameters:
## - dataset*: the dataset, has to have a row 'intensity'.
## - colours: a vector of the colours: first is the colour for the least intensity,
##            second is the colour for hightest intensity. in-between intensities are
##            matched with calculated colours. default: from a dark green to yellow to red
##
## Return value:
## - A dataset with an added colour column
##
## Author: hessg1@bfh.ch  Date: 2018-12-09
##
colourize <- function(dataset, colours = c("darkolivegreen4", "yellow", "red")){
  rbPal <- colorRampPalette(colours)
  dataset$col <- rbPal(10)[as.numeric(cut(dataset$intensity,breaks = 10))]
  return(dataset)
}



## not for plotting negative y-values
## TODO: doku
## TODO: see how description of x-axis could work out
##       https://stackoverflow.com/questions/4843969/plotting-time-series-with-date-labels-on-x-axis
##
## Author: hessg1@bfh.ch  Date: 2018-12-09
##
preparePlot <- function(from = "2018-10-15", to="2018-11-20", label = "Intensity", yLim = c(0,10)){
  startDate <- as.Date(from)
  endDate <- as.Date(to)
  
  dayFrame <- data.frame(day = seq(startDate, endDate, 1), y=c(-1))
  plot(dayFrame, ylim=yLim, type="n", xlab="day", ylab=label)
  abline(v=startDate:endDate,lty=2,col="whitesmoke")
  
  return(dayFrame)
  
}

## DEPRECATED
##
## This function draws a sinus curve on a plot, with a cutoff-value above which 
## the curve is colored in a specified colour. This function needs an existing 
## plot to draw on.
##
## Parameters:
## - range: the x-range the curve will be drawn on (default: 0-10)
## - amplitude: the amplitude of the curve ("height") (default: 1)
## - period: the period of the curve ("width") (default: 1)
## - zeropoint: the zeropoint of the sinus curve ("y-position") (default: 0)
## - offset: the offset of the curve ("x-position") (default: 0)
## - threshold: the cutoff-value: values bigger than the threshold are coloured in
##   another colour (default: 0)
## - showLine: boolean, if TRUE a dotted horizontal line is plotted at the threshold (default: FALSE)
##
## Author: hessg1@bfh.ch  Date: 2018-12-08
##
# drawCurveOld <- function(range = c(0, 10), amplitude = 1, period = 1, zeropoint = 0, offset = 0, threshold = 0, showLine = FALSE){
#   # setting the colors
#   .normCol <- "grey"
#   .highCol <- "orange"
#   
#   start <- range[1]
#   end <- range[2]
#   smoothing <- 190 + 10 * (amplitude / period)
#   
#   x <- (start:(end*smoothing))/smoothing
#   curve <- data.frame(x = x, y = zeropoint + (amplitude  * sin(2 * pi * (x-offset)/period)))
#   curve$col <- sapply(curve$y, function(value){
#     return(if(value > threshold) .highCol else .normCol)
#   })
#   
#   lines(curve$x,curve$y, type="p", pch=".", col = curve$col)
#   if(showLine){
#     abline(h=threshold, col="lightgrey", lty=3)
#   }
# }