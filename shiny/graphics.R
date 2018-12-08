## This function draws a sinus curve on a plot, with a cutoff-value above which 
## the curve is colored in a specified colour. This function needs an existing 
## plot to draw on.
##
## Parameters:
## - amplitude: the amplitude of the curve ("height")
## - period: the period of the curve ("widht")
## - zeropoint: the zeropoint of the sinus curve ("y-position")
## - offset: the offset of the curve ("x-position")
## - threshold: the cutoff-value: values bigger than the threshold are coloured in
##   another colour
## - showLine: boolean, if TRUE a dotted horizontal line is plotted at the threshold
##
##    Author: hessg1@bfh.ch  Date: 2018-12-08
##
.normCol <- "grey"
.highCol <- "orange"

drawCurve <- function(amplitude, period, zeropoint, offset, threshold, showLine){
  # setting the colors

  start <- 0
  end <- 50
  smoothing <- 200
  
  x <- (start:(end*smoothing))/smoothing
  curve <- data.frame(x = x, y = zeropoint + (amplitude * sin((x-offset)/period)))
  curve$col <- sapply(curve$y, function(value){
    return(if(value > threshold) .highCol else .normCol)
    })
  
  lines(curve$x,curve$y, type="p", pch=".", col = curve$col)
  if(showLine){
    abline(h=threshold, col="lightgrey", lty=3)
  }
}
