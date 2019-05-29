# This script is the entry point, and is used to build a connection to MIDATA and call functions from other scripts

# import helper functions
source('./midata-helper.R')
source('./thesis-helper.R')
source('./demographics.R')

# Setting up the fhirClient
client <- setupMidata("http://ch.midata.coop", TRUE)

# make queries
res <- client$search("Observation", "date=ge2019-04-19")
med <- client$search("MedicationStatement", "status=active")

# extract data
observations <- extractObservation(res)
medications <- extractMedication(med)

observations <- removeTesters(observations)
medications <- removeTesters(medications)

# wie viele Datensätze sind verkehrt?
summary(observations$wrongDate)
observations <- prepareData(observations)

# einige erste Auswertungen
str(observations$name)
summary(observations$name)
summary(medications$name)
summary(factor(observations$app))

# zeichne charts für Kohorte
byCohort(observations, medications)
# und die einzelnen User:
byUser(observations)

emptyHeadache <- headaches[which(is.na(headaches$bodysiteSCT)),]
paste(nlevels(factor(emptyHeadache$name)), "User haben leere Kopfschmerzen persistiert", sep=" ")

