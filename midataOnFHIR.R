# this script is an example for how to retrieve and extract data from midata,
# using the midata-helper functions.

# import helper functions
source('./shiny/midata-helper.R')

# Setting up the fhirClient
client <- setupMidata("http://ch.midata.coop", TRUE)

# make queries
res <- client$search("Observation", "date=ge2019-04-19")
med <- client$search("MedicationStatement", "status=active")

# extract data
observations <- extractObservation(res)
medications <- extractMedication(med)

# separate tester data from participant data
tester <- c("P-QSGN-AMCP", "P-MQ05-Q3HM", "P-G8OR-RDUB", "P-POBP-PROB", "P-MLHK-K1L5", "P-UUFN-JHBI", "P-REDP-9UUJ", "P-P5VV-4H3R", "P-HGVD-TODT", "P-VHD4-51A9")

#tester_obs <- observations[ which(observations$name %in% tester), ]
#tester_med <- medications[which(medications$name %in% tester),]
observations <- observations[ which(!observations$name %in% tester), ]
medications <- medications[which(!medications$name %in% tester),]
observations <- as.data.frame(lapply(observations, function(x) if(is.factor(x)) factor(x) else x)) # ungenutzte Faktor-Ausprägungen entfernen
medications <- as.data.frame(lapply(medications, function(x) if(is.factor(x)) factor(x) else x)) # ungenutzte Faktor-Ausprägungen entfernen
rm(tester)

# wie viele Datensätze sind verkehrt?
summary(observations$wrongDate)
observations <- prepareData(observations)



# einige erste Auswertungen
str(observations$name)
summary(observations$name)
summary(medications$name)
summary(factor(observations$app))

plot(observations$type, col="#0a967a", sub="Welche Kategorien wurden am meisten gespeichert?", main="Arten von Einträgen", ylab="Anzahl Einträge")

# wann wurde gespeichert?
saveTime <- rbind(observations[,c("name", "timestamp")], medications[,c("name", "timestamp")])
saveTime$timestamp <- as.POSIXct(saveTime$timestamp, format="%Y-%m-%dT%H:%M:%S")
saveTime$hour <- as.numeric(format(saveTime$timestamp, format="%H"))

hist(saveTime$timestamp, breaks = 'days', main="Verlauf der Einträge", sub="Wie viele Einträge wurden pro Tag gemacht?", col="#0a967a", ylab = "Anzahl Einträge", freq = TRUE, xlab = "Datum")

hist(saveTime$hour,  main="Uhrzeiten", sub="Zu welcher Tageszeit erfolgen die Einträge?", xlab = "Tageszeit", ylab = "Anzahl Einträge", breaks = 24, col="#0a967a", labels=TRUE)
axis(side= 1, at=0:23)

hist(summary(observations$name), main="Wie viele Beiträge haben verschiedene User?", xlab= "Anzahl Einträge", ylab="Anzahl User", breaks = 10, col="#0a967a")

# wir werfen einen Blick auf die Kopfschmerzen
headaches <- observations[ which(observations$type == 'headache'),]
hist(summary(headaches$name), main="Kopfschmerz-Einträge", sub="Wie viele Kopfschmerzen haben die User bisher persistiert?", ylab="Anzahl Einträge", xlab="Anzahl User", breaks = 5, labels=TRUE, col="#0a967a")


# und die einzelnen User:
users <- split(observations, observations$name)

nbrUsers <- 0
usersNbrDays <- vector()
minNbrDays <- 10 # an wie vielen Tagen sollen die User mindestens gespeichert haben, um geplottet zu werden
for(i in 1:length(users)){
  nbrDays <- nlevels(factor(users[[i]]$day))
  usersNbrDays[i] <- nbrDays
  if(nbrDays > minNbrDays){
    # Plot aufsetzen
    par(mfrow=c(1,2))
    # Zeitverlauf plotten
    nbrUsers <- nbrUsers + 1
    saveTime <- users[[i]][,c("name", "timestamp")]
    saveTime$timestamp <- as.POSIXct(saveTime$timestamp, format="%Y-%m-%dT%H:%M:%S")
    saveTime$hour <- as.numeric(format(saveTime$timestamp, format="%H"))
    hist(saveTime$timestamp, breaks = 'days', main=paste("Einträge von", users[[i]]$name[1], sep = " "), sub="Wie viele Einträge wurden pro Tag gemacht?\n(ohne MedicationStatement)", col="#0a967a", ylab = "Anzahl Einträge", freq = TRUE, xlab = "")
  
    # Arten von Einträgen plotten
    plot(users[[i]]$type, col="#0a967a", sub=paste("Welche Eintrag-Arten hat", users[[i]]$name[1], "gespeichert?", sep=" "), main="Arten von Einträgen", ylab="Anzahl Einträge")
  }
}
print(paste(nbrUsers, "Nutzer haben an mindestens", minNbrDays, "Tagen Daten gespeichert", sep= " "))
par(mfrow=c(1,1))
hist(usersNbrDays, main="Speicher-Disziplin einzelner User", sub="An wie vielen verschiedenen Tagen haben User gespeichert? (ohne MedicationStatement)", ylab= "Anzahl User", xlab="", breaks = 30, col="#0a967a")
axis(side= 1, at=1:25)
