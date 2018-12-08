# import helper functions
source('./shiny/functions0.4.R')

# Setting up the fhirClient
client <- setupMidata("http://test.midata.coop", TRUE)

# make a query
resources <- queryMidata(client)

data <- extractData(resources)
