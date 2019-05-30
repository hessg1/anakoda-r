# This script is the entry point, and is used to build a connection to MIDATA and call functions from other scripts

# import helper functions
source('./midata-helper.R')
source('./thesis-helper.R')
source('./demographics.R')
source('./prediction.R')

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
print(paste( nlevels(observations$name), "Nutzer haben Observations persistiert", sep =" "))
print(paste( nlevels(medications$name), "Nutzer haben MedicationStatements persistiert", sep =" "))
print(paste( count(observations$app == 'anakoda')[2,2], "Einträge sind von anakoda,", count(observations$app == 'anakoda')[1,2], "von heMIgrania", sep=" "))

# zeichne charts für Kohorte
byCohort(observations, medications)
# und die einzelnen User:
byUser(observations)

# Machine-Learning / prediction:
  # etwas Datenaufbereitung
  users <- split(observations, observations$name) # Datensatz nach User aufteilen
  usersByDay <- lapply(users, function(x){splitIntoDays(x)}) # User-Datensätze nach Tagen aufteilen

  # als erstes versuchen wir es mit einem modell pro user
  nb_results <- nb_byUser(usersByDay)
  plot(nb_results$kappa ~ nb_results$nDays, main="Naive Bayes (Modell für jeden User einzeln)", col = rainbow(30)[10 * nb_results$accuracy], pch=16, xlab="Anzahl Tage", ylab="Kappa (in Farbe: Genauigkeit)", ylim=c(-0.5, 1), xlim=c(5,30))  
  abline(lm(nb_results$kappa ~ nb_results$nDays), col="grey")
  # wir sehen, mit einzelnen usern ist es noch nicht so gut (viel zu wenig Daten)
  
  
  # also probieren wir mal ein Modell über alle user zu machen
  nb_overall_results <- nb_byCohort(users, usersByDay)
  plot(nb_overall_results$kappa ~ nb_overall_results$nDays, col = rainbow(30)[10 * nb_overall_results$accuracy], pch=16, main="Naive Bayes (Modell über alle User)", xlab="Anzahl Tage", ylab="Kappa (in Farbe: Genauigkeit)", ylim=c(-0.5, 1), xlim=c(5,30))
  abline(lm(nb_overall_results$kappa ~ nb_overall_results$nDays), col="grey")

  
  # noch ein Versuch mit Random Forest
  # TODO: daten trennen: user raussuchen mit mehr als n tagen, diese aus trainingsdaten entfernen
  rf_overall_results <- rf_byCohort(users, usersByDay)
  plot(rf_overall_results$kappa ~ rf_overall_results$nDays, col = rainbow(30)[10 * rf_overall_results$accuracy], pch=16, main="Random Forest (Modell über alle User)", xlab="Anzahl Tage", ylab="Kappa (in Farbe: Genauigkeit)", ylim=c(-0.5, 1), xlim=c(5,30))
  abline(lm(rf_overall_results$kappa ~ rf_overall_results$nDays), col="grey")
  
  
  