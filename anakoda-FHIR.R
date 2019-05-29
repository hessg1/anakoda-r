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
print(paste( nlevels(observations$name), "Nutzer haben Observations persistiert", sep =" "))
print(paste( nlevels(medications$name), "Nutzer haben MedicationStatements persistiert", sep =" "))
print(paste( count(observations$app == 'anakoda')[2,2], "Einträge sind von anakoda,", count(observations$app == 'anakoda')[1,2], "von heMIgrania", sep=" "))

# zeichne charts für Kohorte
byCohort(observations, medications)
# und die einzelnen User:
byUser(observations)

# Machine-Learning / prediction:
  library(e1071) # für naive bayes
  library(caret) # für die confusion matrix
  

  users <- split(observations, observations$name) # Datensatz nach User aufteilen
  usersByDay <- lapply(users, function(x){splitIntoDays(x)}) # User-Datensätze nach Tagen aufteilen

  nb_results <-  data.frame(matrix(ncol=13,nrow=0))
  # durch user iterieren, aber wir machen ML nur wenn über 15 Datensätze vorhanden sind
  for(user in usersByDay){
    user$nextDayHeadache <- factor(user$nextDayHeadache)
    user$prevDayHeadache <- factor(user$prevDayHeadache)
    if(dim(user)[1] > 10 & ('TRUE' %in% user$nextDayHeadache == TRUE)){
      model <- naiveBayes(nextDayHeadache ~ ., user[,2:ncol(user)], laplace=3)
      predicted <- predict(model, user[,2:ncol(user)]) 
      conf <- confusionMatrix((predicted), factor(user$nextDayHeadache))
      nb_results <- rbind(nb_results,  c(conf$table,dim(user)[1],table(user$nextDayHeadache)['TRUE'], conf$overall))
      print("")
      print(as.logical(predicted))
      print(as.logical(user$nextDayHeadache))
      print(conf$overall)
    }
  }
  names(nb_results) <- c("FF", "FT", "TF", "TT", "nDays","nTrue", "accuracy" , "kappa", "accu_lower", "accu_upper", "accu_null", "accuP", "Pmnemar")
  
  # wir sehen, mit einzelnen usern ist es noch nicht so gut (viel zu wenig Daten)
  # also probieren wir mal ein Modell über alle user zu machen
  
  # verschiebe die Tage aller User um jeweils 10 Jahre, um Überlappungen zu vermeiden
  for(i in 1:length(users)){
    userFactor <- 10 * i * 365
    users[[i]]$day <- as.Date(users[[i]]$day) - userFactor
  }
  
  # mache ein einziges Dataframe aus allen usern
  userdf <- rbind.fill(users)
  # und teile es in tage auf
  users <- splitIntoDays(userdf)
  
  # faktorisieren
  # TODO: noch weitere Variablen faktorisieren
  users$nextDayHeadache <- factor(users$nextDayHeadache)
  users$prevDayHeadache <- factor(users$prevDayHeadache)
  users <- users[,2:ncol(users)] # die day-Spalte erklärt nichts, die lassen wir weg
  overallModel <-  naiveBayes(nextDayHeadache ~ ., users, laplace=0)
  predicted <- predict(overallModel, users) 
  confusionMatrix((predicted), factor(users$nextDayHeadache))

  
  # nun wenden wir das Gesamt-Modell auf die einzelnen User an
  nb_overall_results <-  data.frame(matrix(ncol=13,nrow=0))
  for(user in usersByDay){
    if(dim(user)[1] > 7 & ('TRUE' %in% user$nextDayHeadache == TRUE)){
      predicted <- predict(overallModel, user[,2:ncol(user)]) 
      user$nextDayHeadache <- factor(user$nextDayHeadache)
      levels(user$nextDayHeadache) <- c(FALSE, TRUE)
      conf <- confusionMatrix(predicted, user$nextDayHeadache)
      print(as.logical(predicted))
      print(as.logical(user$nextDayHeadache))
      print(conf$overall)
      if(conf$overall[2]<1){ # Kappa von 1 ist ein Artefakt, den wir nicht wollen
        nb_overall_results <- rbind(nb_overall_results, c(conf$table,dim(user)[1],table(user$nextDayHeadache)['TRUE'], conf$overall))
      }
      }
  }

  names(nb_overall_results) <- c("FF", "FT", "TF", "TT", "nDays", "nTrue", "accuracy" , "kappa", "accu_lower", "accu_upper", "accu_null", "accuP", "Pmnemar")
  plot(nb_overall_results$kappa ~ nb_overall_results$nDays, col = rainbow(30)[10 * nb_overall_results$accuracy], pch=16, main="Naive Bayes (Modell über alle User)", xlab="Anzahl Tage", ylab="Kappa (in Farbe: Genauigkeit)", ylim=c(-0.5, 1), xlim=c(5,30))
  abline(lm(nb_overall_results$kappa ~ nb_overall_results$nDays), col="grey")
  plot(nb_results$kappa ~ nb_results$nDays, main="Naive Bayes (Modell für jeden User einzeln)", col = rainbow(30)[10 * nb_results$accuracy], pch=16, xlab="Anzahl Tage", ylab="Kappa (in Farbe: Genauigkeit)", ylim=c(-0.5, 1), xlim=c(5,30))  
  abline(lm(nb_results$kappa ~ nb_results$nDays), col="grey")

  
    
  