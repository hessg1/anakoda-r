# this script is an example for how to retrieve and extract data from midata,
# using the midata-helper functions.

# import helper functions
source('./shiny/midata-helper.R')

# Setting up the fhirClient
client <- setupMidata("http://test.midata.coop", TRUE)

# make a query
resources <- queryMidata(client)

# extract data
data <- extractData(resources)

str(data)

