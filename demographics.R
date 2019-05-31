# This function analyzes the demographics of the study population and draws some charts

removeTesters <- function(observations){
  tester <- c("P-QSGN-AMCP", "P-MQ05-Q3HM", "P-G8OR-RDUB", "P-POBP-PROB", "P-MLHK-K1L5", "P-UUFN-JHBI", "P-REDP-9UUJ", "P-P5VV-4H3R", "P-HGVD-TODT", "P-VHD4-51A9")
  #tester_obs <- observations[ which(observations$name %in% tester), ]
  #tester_med <- medications[which(medications$name %in% tester),]
  observations <- observations[ which(!observations$name %in% tester), ]
  observations <- as.data.frame(lapply(observations, function(x) if(is.factor(x)) factor(x) else x)) # ungenutzte Faktor-Ausprägungen entfernen
  rm(tester)
  return(observations)
}

byCohort <- function(observations, medications){
  plot(observations$type, col="#0a967a", sub="Welche Kategorien wurden am meisten gespeichert?", main="Arten von Einträgen", ylab="Anzahl Einträge")
  
  # wann wurde gespeichert?
  medications$timestamp <- as.POSIXct(medications$timestamp, format="%Y-%m-%dT%H:%M:%S")
  
  saveTime <- rbind(observations[,c("ID", "name", "timestamp")], medications[,c("ID", "name", "timestamp")])
  saveTime$posix <- saveTime$timestamp
  saveTime$posix <- as.POSIXct(saveTime$posix, format="%Y-%m-%dT%H:%M:%S")
  saveTime$hour <- as.numeric(format(saveTime$posix, format="%H"))
  
  hist(saveTime$timestamp, breaks = 'days', main="Verlauf der Einträge", sub="Wie viele Einträge wurden pro Tag gemacht?", col="#0a967a", ylab = "Anzahl Einträge", freq = TRUE, xlab = "Datum")
  
  summary(factor(saveTime$hour))
  
  saveTime$hour <- saveTime$hour + 1 # alles um 1 nach rechts schieben
  # weil R die ersten beiden Werte aus einem seltsamen Grund im Histogramm zusammen nimmt, machen wir aus allen 1 eine 0 
  #(damit 0 und 1 (jetzt leer) zusammengefasst werden statt 1 und 2)
  saveTime$hour[saveTime$hour == 1] <- 0 
  
  hist(saveTime$hour, right=T, main="Uhrzeiten", sub="Zu welcher Tageszeit erfolgen die Einträge?", xlab = "Tageszeit", ylab = "Anzahl Einträge", breaks = 25, col="#0a967a", labels=TRUE)
  axis(side= 1, at=0:24)
  
  hist(summary(observations$name), main="Wie viele Einträge haben verschiedene User?", xlab= "Anzahl Einträge", ylab="Anzahl User", breaks = 10, col="#0a967a")
  
  # wir werfen einen Blick auf die Kopfschmerzen
  headaches <- observations[ which(observations$type == 'headache'),]
  hist(summary(headaches$name), main="Kopfschmerz-Einträge", sub="Wie viele Kopfschmerzen haben die User bisher persistiert?", xlab="Anzahl Einträge", ylab="Anzahl User", breaks = 5, labels=TRUE, col="#0a967a")
  
  par(mfrow=c(2,1))
  headaches$posix <- as.POSIXct(headaches$startTime, format="%Y-%m-%dT%H:%M:%S")
  headaches$hourStart <- as.numeric(format(headaches$posix, format="%H"))
  headaches$hourStart <- headaches$hourStart + 1
  headaches$hourStart[headaches$hourStart == 1] <- 0
  hist(headaches$hourStart, right=T, main="Uhrzeiten", sub="Zu welcher Tageszeit hatten die User Kopfschmerzen? (Start)", xlab = "Tageszeit", ylab = "Anzahl Kopfschmerzen", breaks = 25, col="#0a967a", labels=TRUE)
  axis(side= 1, at=0:24)
  
  headaches$delay <- headaches$timestamp - headaches$startTime
  
  headaches$posix <- as.POSIXct(headaches$endTime, format="%Y-%m-%dT%H:%M:%S")
  headaches$hourEnd <- as.numeric(format(headaches$posix, format="%H"))
  headaches$hourEnd <- headaches$hourEnd + 1
  headaches$hourEnd[headaches$hourEnd == 1] <- 0
  hist(headaches$hourEnd, right=T, main="Uhrzeiten", sub="Zu welcher Tageszeit hatten die User Kopfschmerzen? (Ende)", xlab = "Tageszeit", ylab = "Anzahl Kopfschmerzen", breaks = 25, col="#0a967a", labels=TRUE)
  axis(side= 1, at=0:24)
  par(mfrow=c(1,1))
  plot((headaches$duration / 60) ~ headaches$hourStart, main="Kopfschmerzdauer zu Startzeit", ylab="Dauer (Minuten)", xlab="Tageszeit", pch=4, col = rainbow(30)[headaches$intensity])
  axis(side= 1, at=0:24)
  plot((headaches$duration / 60) ~ headaches$hourEnd, main="Kopfschmerzdauer zu Endzeit", ylab="Dauer (Minuten)", xlab="Tageszeit", pch=4, col = rainbow(30)[headaches$intensity])
  axis(side= 1, at=0:24)
}

byUser <- function(observations){
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
  par(mfrow=c(1,1))
  print(paste(nbrUsers, "Nutzer haben an mindestens", minNbrDays, "Tagen Daten gespeichert", sep= " "))
  par(mfrow=c(1,1))
  hist(usersNbrDays, main="Speicher-Disziplin einzelner User", sub="An wie vielen verschiedenen Tagen haben User gespeichert? (ohne MedicationStatement)", ylab= "Anzahl User", xlab="", breaks = 30, col="#0a967a")
  axis(side= 1, at=1:30)
}

