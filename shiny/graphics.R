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
## Author: hessg1@bfh.ch  Date: 2018-12-09
##
drawCurve <- function(amplitude = 1, period = 1, zero = 0, offset = 0, threshold = 0, showLine = FALSE){
  curve(zero-threshold + (amplitude  * sin(2 * pi * (x-offset)/period)), n = 1000/period, col = "grey", add = TRUE)
  
  if(showLine){
    curve(zero + 0*x, n= 2, col="grey", lty=3, add=TRUE)
  }
  
}



## This function adds a 'col' column to a dataset, with corresponding fading colour code 
## for headache intensity.
##
## Parameters:
## - dataset*: the dataset, has to have a row 'intensity'.
## - colours: a vector of the colours: first is the colour for the least intensity,
##            second is the colour for hightest intensity. in-between intensities are
##            matched with calculated colours. default: from 'gold' to 'red'
##
## Author: hessg1@bfh.ch  Date: 2018-12-09
##
colourize <- function(dataset, colours = c('gold','red')){
  rbPal <- colorRampPalette(colours)
  dataset$col <- rbPal(10)[as.numeric(cut(dataset$intensity,breaks = 10))]
  return(dataset)
}



## not for plotting negative y-values
## TODO: doku
## TODO: see how description of x-axis could work out
##
preparePlot <- function(from = "2018-10-15", to="2018-11-20", label = "Intensität", yLim = c(0,10)){
  startDate <- as.Date(from)
  endDate <- as.Date(to)
  
  dayFrame <- data.frame(day = seq(startDate, endDate, 1), y=c(-1))
  plot(dayFrame, ylim=yLim, type="n", xlab="Tag", ylab=label)
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
drawCurveOld <- function(range = c(0, 10), amplitude = 1, period = 1, zeropoint = 0, offset = 0, threshold = 0, showLine = FALSE){
  # setting the colors
  .normCol <- "grey"
  .highCol <- "orange"
  
  start <- range[1]
  end <- range[2]
  smoothing <- 190 + 10 * (amplitude / period)
  
  x <- (start:(end*smoothing))/smoothing
  curve <- data.frame(x = x, y = zeropoint + (amplitude  * sin(2 * pi * (x-offset)/period)))
  curve$col <- sapply(curve$y, function(value){
    return(if(value > threshold) .highCol else .normCol)
  })
  
  lines(curve$x,curve$y, type="p", pch=".", col = curve$col)
  if(showLine){
    abline(h=threshold, col="lightgrey", lty=3)
  }
}