library(e1071) # für naive bayes
library(caret) # für die confusion matrix
library(randomForest) # für random forest
source('./thesis-helper.R')

## This function runs naive bayes for a list of users, with training a own model for each user.
##
## Parameters: 
##   - userList: a list of users, with their observations prepared as a data.frame
##   - threshold: the minimum number of days a user needs for prediction to be made (default: 10)
##      users with less days are skipped
##   - log: determines if the results are printed on the console (default: TRUE)
## 
## Return value:
##   - a dataframe with the observations with the results of each users 
##
## Author: hessg1@bfh.ch  Date: 2018-05-29
##
nb_byUser <- function(userList, threshold = 10, log = TRUE){
  userList <- usersByDay
  user <- usersByDay[[20]]
  nb_results <-  data.frame(matrix(ncol=13,nrow=0))
  # iterate through users
  for(user in userList){
    # we skip users with to little data
    if(dim(user)[1] > threshold & ('TRUE' %in% user$nextDayHeadache == TRUE)){
      user$nextDayHeadache <- factor(user$nextDayHeadache)
      user$prevDayHeadache <- factor(user$prevDayHeadache)
      levels(user$eating) <- c('irregular', 'regular', NA)
      
      model <- naiveBayes(nextDayHeadache ~ ., user[,2:ncol(user)], laplace=3)
      predicted <- predict(model, user[,2:ncol(user)]) 
      conf <- confusionMatrix((predicted), factor(user$nextDayHeadache))
      nb_results <- rbind(nb_results,  c(conf$table,dim(user)[1],table(user$nextDayHeadache)['TRUE'], conf$overall))
      if(log == TRUE){
        print("")
        print(as.logical(predicted))
        print(as.logical(user$nextDayHeadache))
        print(conf$overall)
      }
    }
  }
  names(nb_results) <- c("FF", "FT", "TF", "TT", "nDays","nTrue", "accuracy" , "kappa", "accu_lower", "accu_upper", "accu_null", "accuP", "Pmnemar")
  return(nb_results)
}

## This function runs naive bayes for a list of users, with training a shared model over all users
##
## Parameters: 
##   - trainUserSet: the list of users from which the model should be trained
##   - testUserSet: the list of users on which the model should be tested
##   - log: determines if the results are printed on the console (default: TRUE)
## 
## Return value:
##   - a dataframe with the observations with the results of each users 
##
## Author: hessg1@bfh.ch  Date: 2018-05-29
##
nb_byCohort <- function(trainUserSet, testUserSet, log = TRUE){
  # we have to shift the days from each user by a own factor, so we avoid overlapping 
  # (10 years space for each user should do the trick)
  for(i in 1:length(trainUserSet)){
    userFactor <- 10 * i * 365
    trainUserSet[[i]]$day <- as.Date(trainUserSet[[i]]$day) - userFactor
  }
  
  # create a single data.frame from all users
  userdf <- rbind.fill(trainUserSet)
  # and split it into days
  trainUserSet <- splitIntoDays(userdf)
  
  # factorize
  trainUserSet$nextDayHeadache <- factor(trainUserSet$nextDayHeadache)
  trainUserSet$prevDayHeadache <- factor(trainUserSet$prevDayHeadache)
  # day column doesn't explain anything, thus is erased
  trainUserSet <- trainUserSet[,2:ncol(trainUserSet)]
  overallModel <-  naiveBayes(nextDayHeadache ~ ., trainUserSet, laplace=0)
  
  
  # now lets apply the model to the users
  nb_overall_results <-  data.frame(matrix(ncol=13,nrow=0))
  str(trainUserSet)
  for(user in testUserSet){
    user$nextDayHeadache <- factor(user$nextDayHeadache)
    if(nlevels(user$nextDayHeadache) > 1){
      user$prevDayHeadache <- factor(user$prevDayHeadache)
      levels(user$eating) <- c('irregular', 'regular', NA)
      
      predicted <- predict(overallModel, user[,2:ncol(user)]) 
      
      conf <- confusionMatrix(predicted, user$nextDayHeadache)
      if(log == TRUE){
        print(as.logical(predicted))
        print(as.logical(user$nextDayHeadache))
        print(conf$overall)
      }
      if(conf$overall[2]<1){ # kappa of 1.0 is an artefact we don't want
        nb_overall_results <- rbind(nb_overall_results, c(conf$table,dim(user)[1],table(user$nextDayHeadache)['TRUE'], conf$overall))
      }
    }
  }
  names(nb_overall_results) <- c("FF", "FT", "TF", "TT", "nDays", "nTrue", "accuracy" , "kappa", "accu_lower", "accu_upper", "accu_null", "accuP", "Pmnemar")
  return(nb_overall_results)
}


## This function runs naive bayes for a list of users, with training a shared model over all users
##
## Parameters: 
##   - trainUserSet: the list of users from which the model should be trained
##   - testUserSet: the list of users on which the model should be tested
##   - log: determines if the results are printed on the console (default: TRUE)
## 
## Return value:
##   - a dataframe with the observations with the results of each users 
##
## Author: hessg1@bfh.ch  Date: 2018-05-30
##
rf_byCohort <- function(trainUserSet, testUserSet, log = TRUE){
  # we have to shift the days from each user by a own factor, so we avoid overlapping 
  # (10 years space for each user should do the trick)
  for(i in 1:length(trainUserSet)){
    userFactor <- 10 * i * 365
    trainUserSet[[i]]$day <- as.Date(trainUserSet[[i]]$day) - userFactor
  }
  
  # create a single data.frame from all users
  userdf <- rbind.fill(trainUserSet)
  # and split it into days
  trainUserSet <- splitIntoDays(userdf)
  
  # factorize
  trainUserSet$nextDayHeadache <- factor(trainUserSet$nextDayHeadache)
  trainUserSet$prevDayHeadache <- factor(trainUserSet$prevDayHeadache)
  # day column doesn't explain anything, thus is erased
  trainUserSet <- trainUserSet[,2:ncol(trainUserSet)]
  
  rf_model <- randomForest(nextDayHeadache ~ ., data=trainUserSet, na.action = na.omit, ntree=1500, mtry=4, importance=T) # may take some seconds
  if(log == TRUE){
    plot(rf_model)
  }

  
  # now lets apply the model to the users
  rf_overall_results <-  data.frame(matrix(ncol=13,nrow=0))
  for(user in testUserSet){
    user$nextDayHeadache <- factor(user$nextDayHeadache)
    user$prevDayHeadache <- factor(user$prevDayHeadache)
    levels(user$eating) <- levels(trainUserSet$eating)
    levels(user$nextDayHeadache) <- levels(trainUserSet$nextDayHeadache)
    levels(user$prevDayHeadache) <- levels(trainUserSet$prevDayHeadache)
    if(dim(user)[1] > 5 & ('TRUE' %in% user$nextDayHeadache == TRUE)){
      
      predicted <- predict(rf_model, user[,2:ncol(user)]) 
      
      conf <- confusionMatrix(predicted, user$nextDayHeadache)
      if(log == TRUE){
        print(as.logical(predicted))
        print(as.logical(user$nextDayHeadache))
        print(conf$overall)
      }
      if(!is.nan(conf$overall[2]) & conf$overall[2]<1){ # kappa as NaN or 1.0 is an artefact, we don't want this
        rf_overall_results <- rbind(rf_overall_results, c(conf$table,dim(user)[1],table(user$nextDayHeadache)['TRUE'], conf$overall))
      }
    }
  }
  names(rf_overall_results) <- c("FF", "FT", "TF", "TT", "nDays", "nTrue", "accuracy" , "kappa", "accu_lower", "accu_upper", "accu_null", "accuP", "Pmnemar")
  return(rf_overall_results)
}
