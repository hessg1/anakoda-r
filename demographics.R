# This functions analyze the demographics of the study population and draws some charts
source('./thesis-helper.R')

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
  
  par(mfrow=c(2,2))
  headaches$posix <- as.POSIXct(headaches$startTime, format="%Y-%m-%dT%H:%M:%S")
  

  headaches$hourStart <- as.numeric(format(headaches$posix, format="%H"))
  headaches$hourStart <- headaches$hourStart + 1
  headaches$hourStart[headaches$hourStart == 1] <- 0
  hist(headaches$hourStart, right=T, main="Uhrzeiten",xlim=c(0,24),  sub="Zu welcher Tageszeit hatten die User Kopfschmerzen? (Start)", xlab = "Tageszeit", ylab = "Anzahl Kopfschmerzen", breaks = 25, col="#0a967a", labels=TRUE)
  axis(side= 1, at=0:24)
  
  headaches$delay <- headaches$timestamp - headaches$startTime
  
  headaches$posix <- as.POSIXct(headaches$endTime, format="%Y-%m-%dT%H:%M:%S")
  headaches$hourEnd <- as.numeric(format(headaches$posix, format="%H"))
  headaches$hourEnd <- headaches$hourEnd + 1
  headaches$hourEnd[headaches$hourEnd == 1] <- 0
  hist(headaches$hourEnd, right=T, main="Uhrzeiten",xlim=c(0,24),  sub="Zu welcher Tageszeit hatten die User Kopfschmerzen? (Ende)", xlab = "Tageszeit", ylab = "Anzahl Kopfschmerzen", breaks = 25, col="#0a967a", labels=TRUE)
  axis(side= 1, at=0:24)
  
  headaches2 <- headaches[which(headaches$duration != 60*180),]
  hist(headaches2$hourStart, right=T, xlim=c(0,24),  main="Uhrzeiten (ohne 'default' Kopfschmerzen)", sub="Zu welcher Tageszeit hatten die User Kopfschmerzen? (Start)", xlab = "Tageszeit", ylab = "Anzahl Kopfschmerzen", breaks = 25, col="#0a967a", labels=TRUE)
  axis(side= 1, at=0:24)
  hist(headaches2$hourEnd, right=T, xlim=c(0,24), main="Uhrzeiten (ohne 'default' Kopfschmerzen)", sub="Zu welcher Tageszeit hatten die User Kopfschmerzen? (Ende)", xlab = "Tageszeit", ylab = "Anzahl Kopfschmerzen", breaks = 25, col="#0a967a", labels=TRUE)
  axis(side= 1, at=0:24)
  
  par(bg = '#606060')
 
  plot((headaches$duration / 60) ~ headaches$hourStart, main="Kopfschmerzdauer zu Startzeit",xlim=c(0,24),  ylab="Dauer (Minuten)", xlab="Tageszeit", pch=4, col = rainbow(10, start=0, end=0.3)[11-headaches$intensity])
  legend("topleft", title="Intensität", title.col="black", text.col = rainbow(10, start=0, end=0.3)[c(1,3,5,7,9,10)],lwd=0,legend=c("10","8","6", "4", "2", "1"), xjust=0.5, cex=0.7, bty="n")
  axis(side= 1, at=0:24)
  plot((headaches$duration / 60) ~ headaches$hourEnd, main="Kopfschmerzdauer zu Endzeit", xlim=c(0,24), ylab="Dauer (Minuten)", xlab="Tageszeit", pch=4, col = rainbow(10, start=0, end=0.3)[11-headaches$intensity])
  #legend("topleft", title="Intensität", title.col="black", text.col = rainbow(10, start=0, end=0.3)[c(1,3,5,7,9,10)],lwd=0,legend=c("10","8","6", "4", "2", "1"), xjust=0.5, cex=0.7, bty="n")
  axis(side= 1, at=0:24)
  
  plot((headaches2$duration / 60) ~ headaches2$hourStart, xlim=c(0,24),  main="Kopfschmerzdauer zu Startzeit (ohne 'default')", ylab="Dauer (Minuten)", xlab="Tageszeit", pch=4, col = rainbow(10, start=0, end=0.3)[11-headaches2$intensity])
  #legend("topleft", title="Intensität", title.col="black", text.col = rainbow(10, start=0, end=0.3)[c(1,3,5,7,9,10)],lwd=0,legend=c("10","8","6", "4", "2", "1"), xjust=0.5, cex=0.7, bty="n")
  axis(side= 1, at=0:24)
  plot((headaches2$duration / 60) ~ headaches2$hourEnd, xlim=c(0,24), main="Kopfschmerzdauer zu Endzeit (ohne 'default')", ylab="Dauer (Minuten)", xlab="Tageszeit", pch=4, col = rainbow(10, start=0, end=0.3)[11-headaches2$intensity])
  #legend("topleft", title="Intensität", title.col="black", text.col = rainbow(10, start=0, end=0.3)[c(1,3,5,7,9,10)],lwd=0,legend=c("10","8","6", "4", "2", "1"), xjust=0.5, cex=0.7, bty="n")
  axis(side= 1, at=0:24)
  
  par(mfrow=c(1,1), bg='white')
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
  axis(side= 1, at=1:40)
  
 
}

descriptiveStat <- function(observations){
  observations$findingText <- tolower(observations$findingText) # because hemigrania and anakoda have some difference in case setting
  compcond <- observations[observations$type == 'complaint' | observations$type == 'condition',]
  barchart(factor(compcond$findingText), col="#0a967a", main="Welche Auffälligkeiten treten am häufigsten auf?", xlab="Anzahl Nennungen (alle User)")

  headaches <- observations[observations$type == 'headache',]
  barchart(factor(headaches$findingText), col="#0a967a", main="Welche Kopfschmerzen treten am häufigsten auf?", xlab="Anzahl Nennungen (alle User)")
  barchart(factor(headaches$bodysiteText), col="#0a967a", main="Auf welcher Seite treten Kopfschmerzen am häufigsten auf?", xlab="Anzahl Nennungen (alle User)")

  usersByDay <- lapply(split(observations, observations$name), function(x){splitIntoDays(x)}) # split dataset into users and days
  
  hdDays <- data.frame(matrix(ncol=3,nrow=length(usersByDay)))
  names(hdDays) <- c("name", "headacheDays", "numberOfDays")
  # iterate through users for detecting
  for(i in 1:length(usersByDay)){
    headache <- factor(usersByDay[[i]]$headache)
    levels(headache) <- c(FALSE, TRUE)
    headacheDays <- summary(headache)['TRUE']
    numberDays <- length(usersByDay[[i]]$headache)
    name <- names(usersByDay)[[i]]
    hdDays[i,] <- c(name, headacheDays, numberDays)
  }
  hdDays <- hdDays[hdDays$headacheDays > 0,] # throw out the users without headaches
  hdDays$percent <- apply(hdDays, 1, function(x){as.numeric(x[2]) / as.numeric(x[3])})
  par(mfrow=c(1,2))
  hist(as.numeric(hdDays$headacheDays), col="#0a967a", main="Verteilung der Kopfschmerz-Tage", xlab="Anzahl Kopfschmerztage", ylab="Anzahl User", breaks = 10)
  hist(100*hdDays$percent, col="#0a967a", main="Verteilung der Kopfschmerz-Tage (relativ)", xlab="Anteil Kopfschmerztage (%)", ylab="Anzahl User", xlim=c(0,100))
}
