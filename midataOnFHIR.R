# this script is an example for how to retrieve and extract data from midata,
# using the midata-helper functions.

# import helper functions
source('./shiny/midata-helper.R')

# Setting up the fhirClient
client <- setupMidata("http://test.midata.coop", TRUE)

# make queries
res <- client$search("Observation", "date=ge2019-04-01")
med <- client$search("MedicationStatement", "status=active")



# extract data
observations <- extractObservation(res)
medications <- extractMedication(med)


# wie viele Datensätze sind verkehrt?
summary(observations$wrongDate)
summary(observations$name)

# wann wurde gespeichert?
obsTimestamps <- observations[,c("name", "timestamp")]
medTimestamps <- medications[,c("name", "timestamp")]
saveTime <- rbind(obsTimestamps, medTimestamps)
saveTime$timestamp <- as.POSIXct(saveTime$timestamp, format="%Y-%m-%dT%H:%M:%S")
saveTime$hour <- as.numeric(format(saveTime$timestamp, format="%H"))

hist(saveTime$timestamp, breaks = 'days', main="Verlauf der Einträge aller Teilnehmer")
hist(saveTime$hour,  main="Zu welchen Uhrzeiten wurden die Einträge gespeichert")
#

