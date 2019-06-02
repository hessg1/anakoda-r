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
descriptiveStat(observations)

# und die einzelnen User:
byUser(observations)



# Machine-Learning / prediction:
  # etwas Datenaufbereitung
  users <- split(observations, observations$name) # Datensatz nach User aufteilen
  usersByDay <- lapply(users, function(x){splitIntoDays(x)}) # User-Datensätze nach Tagen aufteilen

  # als erstes versuchen wir es mit einem modell pro user
  nb_results <- nb_byUser(usersByDay,  threshold = 10, log = F)
  plot(nb_results$kappa ~ nb_results$nDays, main="Naive Bayes (Modell für jeden User einzeln)", col = rainbow(30)[(10 * (nb_results$accuracy+0.1))], pch=16, xlab="Anzahl erfasste Tage", ylab="Kappa", ylim=c(-0.5, 1), xlim=c(5,30))  
  legend("topright", title="Genauigkeit", title.col="black", text.col = rainbow(30)[c(11,9,7,5,3,1)],lwd=0,legend=c("100%","80%","60%", "40%", "20%", "0%"), xjust=0.5, cex=0.7, bty="n")
  abline(lm(nb_results$kappa ~ nb_results$nDays), col="grey")
  # wir sehen, mit einzelnen usern ist es noch nicht so gut (viel zu wenig Daten)
  
  
  # also probieren wir mal ein Modell über alle user zu machen
  nb_overall_results <- nb_byCohort(users, usersByDay, threshold = 7, log = F)
  plot(nb_overall_results$kappa ~ nb_overall_results$nDays, col = rainbow(30)[(10 * (nb_overall_results$accuracy+0.1))], pch=16, main="Naive Bayes (Modell über alle User)", xlab="Anzahl erfasste Tage", ylab="Kappa", ylim=c(-0.5, 1), xlim=c(5,30))
  legend("topright", title="Genauigkeit", title.col="black", text.col = rainbow(30)[c(11,9,7,5,3,1)],lwd=0,legend=c("100%","80%","60%", "40%", "20%", "0%"), xjust=0.5, cex=0.7, bty="n")
  abline(lm(nb_overall_results$kappa ~ nb_overall_results$nDays), col="grey")

  
  # noch ein Versuch mit Random Forest
 
  rf_overall_results <- rf_byCohort(users, usersByDay, threshold = 7, log = F)
  plot(rf_overall_results$kappa ~ rf_overall_results$nDays, col = rainbow(30)[(10 * (rf_overall_results$accuracy+0.1))], pch=16, main="Random Forest (Modell über alle User)", xlab="Anzahl erfasste Tage", ylab="Kappa", ylim=c(-0.5, 1), xlim=c(5,30))
  legend("topright", title="Genauigkeit", title.col="black", text.col = rainbow(30)[c(11,9,7,5,3,1)],lwd=0,legend=c("100%","80%","60%", "40%", "20%", "0%"), xjust=0.5, cex=0.7, bty="n")
  abline(lm(rf_overall_results$kappa ~ rf_overall_results$nDays), col="grey")
 
  # jetzt teilen wir das Datenset auf
  threshold <- 20
  highUsers <- usersByDay[sapply(usersByDay, function(x){nrow(x) > threshold})] # nur die User mit Einträgen an mehr als in threshold definierten Tagen
  lowUsers <- users[sapply(users, function(x){nlevels(factor(x$day)) <= threshold})] # der Rest, aber im Format wie "users"
  
  #rf_divided_results <- rf_byCohort(lowUsers, highUsers, threshold = threshold, log = T)
  #plot(rf_divided_results$kappa ~ rf_divided_results$nDays, col = rainbow(30)[(10 * (rf_divided_results$accuracy+0.1))], pch=16, main="Random Forest (trainiert mit LowUser, getestet mit highUser)", xlab="Anzahl erfasste Tage", ylab="Kappa", ylim=c(-0.5, 1), xlim=c(5,30))
  #legend("topright", title="Genauigkeit", title.col="black", text.col = rainbow(30)[c(11,9,7,5,3,1)],lwd=0,legend=c("100%","80%","60%", "40%", "20%", "0%"), xjust=0.5, cex=0.7, bty="n")
  #abline(lm(rf_divided_results$kappa ~ rf_divided_results$nDays), col="grey")

  nb_divided_results <- nb_byCohort(lowUsers, highUsers, threshold = 7, log = F)
  plot(nb_divided_results$kappa ~ nb_divided_results$nDays, col = rainbow(30)[(10 * (nb_divided_results$accuracy+0.1))], pch=16, main="Naive Bayes (geteilt)", sub=paste("trainiert mit LowUser, getestet mit highUser (threshold=", threshold, ")", sep=""), xlab="Anzahl erfasste Tage", ylab="Kappa", ylim=c(-0.5, 1), xlim=c(5,30))
  legend("topright", title="Genauigkeit", title.col="black", text.col = rainbow(30)[c(11,9,7,5,3,1)],lwd=0,legend=c("100%","80%","60%", "40%", "20%", "0%"), xjust=0.5, cex=0.7, bty="n")
  abline(lm(nb_divided_results$kappa ~ nb_divided_results$nDays), col="grey")
  
  # zum Vergleich testen wir noch zwei dumme "Algorithmen"
  alwaysNo_results <- alwaysNo_byCohort(usersByDay, threshold = 5, log = F) 
  plot(alwaysNo_results$kappa ~ alwaysNo_results$nDays, col = rainbow(30)[(10 * (alwaysNo_results$accuracy+0.1))], pch=16, main="Vorhersage: immer nein", xlab="Anzahl erfasste Tage", ylab="Kappa", ylim=c(-0.5, 1), xlim=c(5,30))
  legend("topright", title="Genauigkeit", title.col="black", text.col = rainbow(30)[c(11,9,7,5,3,1)],lwd=0,legend=c("100%","80%","60%", "40%", "20%", "0%"), xjust=0.5, cex=0.7, bty="n")
  abline(lm(alwaysNo_results$kappa ~ alwaysNo_results$nDays), col="grey")
  
  same_results <- same_byCohort(usersByDay, threshold = 5, log = F)
  plot(same_results$kappa ~ same_results$nDays, col = rainbow(30)[(10 * (same_results$accuracy+0.1))], pch=16, main="Vorhersage: wie am jetzigen Tag", xlab="Anzahl erfasste Tage", ylab="Kappa", ylim=c(-0.5, 1), xlim=c(5,30))
  legend("topright", title="Genauigkeit", title.col="black", text.col = rainbow(30)[c(11,9,7,5,3,1)],lwd=0,legend=c("100%","80%","60%", "40%", "20%", "0%"), xjust=0.5, cex=0.7, bty="n")
  abline(lm(same_results$kappa ~ same_results$nDays), col="grey")
  