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
  
  # durch user iterieren, aber wir machen ML nur wenn über 15 Datensätze vorhanden sind
  models <- data.frame()
  confs <- data.frame()
  
  for(user in usersByDay){
    user$nextDayHeadache <- factor(user$nextDayHeadache)
    user$prevDayHeadache <- factor(user$prevDayHeadache)
    if(dim(user)[1] > 15 & ('TRUE' %in% user$nextDayHeadache)){
      print("")
      print("")
      print("--------------------")
      print(user$nextDayHeadache)
      model <- naiveBayes(nextDayHeadache ~ ., user[,2:ncol(user)], laplace=3)
      models <- rbind(models, model)
      predicted <- predict(model, user[,2:ncol(user)]) 
      print(predicted)
      # schauen wie gut das ist
      conf <- confusionMatrix((predicted), factor(user$nextDayHeadache))
      confs <- rbind(confs, conf)
      print(conf$table)
      print(conf$overall)
    }
  }
  