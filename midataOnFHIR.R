# import helper functions
source('./shiny/functions0.4.R')

# Setting up the fhirClient
client <- setupMidata("http://test.midata.coop", TRUE)

# make a query
resources <- queryMidata(client)

data2 <- extractData(resources)
str(data$intensity)

colours <- colorRampPalette(c('red','blue'))
myColours <- colours(10)[as.numeric(cut(data$intensity,breaks = 10))]
