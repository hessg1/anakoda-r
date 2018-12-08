## This function draws a sinus curve on a plot, with a cutoff-value above which 
## the curve is colored in a specified colour
##
##    Author: hessg1@bfh.ch  Date: 2018-12-08
##
.normCol <- "grey"
.highCol <- "orange"

drawCurve <- function(amplitude, period, zeropoint, cutoff){
  # setting the colors

  start <- 0
  end <- 50
  smoothing <- 200
  
  #amplitude <- 1.2
  #period <- 4
  #zeropoint <- 0
  #cutoff <- 0.7
  
  x <- (start:(end*smoothing))/smoothing
  curve <- data.frame(x = x, y = zeropoint + (amplitude * sin(x/period)))
  curve$col <- sapply(curve$y, function(value){
    return(if(value > cutoff) .highCol else .normCol)
    })
  
  lines(curve$x,curve$y, type="p", pch=".", col = curve$col)
  
  
}
