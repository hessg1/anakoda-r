# this script is an example for how to retrieve and extract data from midata,
# using the midata-helper functions.

# import helper functions
source('./shiny/midata-helper.R')

# Setting up the fhirClient
client <- setupMidata("http://ch.midata.coop", TRUE)

# make queries
res <- client$search("Observation", "date=ge2019-04-30")
med <- client$search("MedicationStatement", "status=active")

# extract data
observations <- extractObservation(res)
medications <- extractMedication(med)

# separate tester data from participant data
tester <- c("P-QSGN-AMCP", "P-MQ05-Q3HM", "P-G8OR-RDUB", "P-POBP-PROB", "P-MLHK-K1L5", "P-UUFN-JHBI", "P-REDP-9UUJ", "P-P5VV-4H3R", "P-HGVD-TODT", "P-VHD4-51A9")

tester_obs <- observations[ which(observations$name %in% tester), ]
tester_med <- medications[which(medications$name %in% tester),]
observations <- observations[ which(!observations$name %in% tester), ]
medications <- medications[which(!medications$name %in% tester),]
observations <- as.data.frame(lapply(observations, function(x) if(is.factor(x)) factor(x) else x)) # ungenutzte Faktor-Ausprägungen entfernen
medications <- as.data.frame(lapply(medications, function(x) if(is.factor(x)) factor(x) else x)) # ungenutzte Faktor-Ausprägungen entfernen
rm(tester)

# wie viele Datensätze sind verkehrt?
summary(observations$wrongDate)
str(observations$name)
summary(observations$name)
summary(medications$name)

# wann wurde gespeichert?
obsTimestamps <- observations[,c("name", "timestamp")]
medTimestamps <- medications[,c("name", "timestamp")]
saveTime <- rbind(obsTimestamps, medTimestamps)
saveTime$timestamp <- as.POSIXct(saveTime$timestamp, format="%Y-%m-%dT%H:%M:%S")
saveTime$hour <- as.numeric(format(saveTime$timestamp, format="%H"))

hist(saveTime$timestamp, breaks = 'days', main="Verlauf der Einträge aller Teilnehmer")
hist(saveTime$hour,  main="Zu welchen Uhrzeiten wurden die Einträge gespeichert")
hist(summary(observations$name), main="Wie viele Beiträge haben verschiedene User?")
