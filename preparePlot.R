# file for offline optimizing plot for R Shiny
# hessg1, 2018-12-08

source('./shiny/midata-helper.R')
source('./shiny/graphics.R')

# prepare DATA as usual
# midata
client <- setupMidata(forceLogin = FALSE)
conditions <- extractData(queryMidata(client))
conditions <- prepareData(conditions)

headaches <- data.frame(day = subset(conditions, conditions$intensity > 0)$day, intensity = subset(conditions, conditions$intensity > 0)$intensity, duration = subset(conditions, conditions$intensity > 0)$duration, uid = subset(conditions, conditions$intensity > 0)$uid)
headaches <- colourize(headaches, c("darkolivegreen4", "yellow", "red"))


# plot setup
dayFrame <- preparePlot(label="UID", yLim=c(1,5))
plotFrame <- merge(x=dayFrame,y=headaches, all.x=TRUE)
drawCurve(amplitude = 0.25, zero = 4, threshold = 0.15, offset = -13, period = 31.592, showLine = TRUE)
lines(plotFrame$day, plotFrame$uid, type="p", col=plotFrame$col, pch = 16, cex = (plotFrame$duration)/6 )

