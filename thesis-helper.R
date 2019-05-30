library (plyr)

## This function divides a set of observations into observations of a single day, 
## and indicates if it was a headache-day. Prepares the data for machine-learning (naive-bayes).
##
## Parameters: 
##   - observations: a list of observations, preferably for a single user
## 
## Return value:
##   - a dataframe with the observations, with one-hot encoding for the conditions and complaints
##     enriched by the information if the last and the following day was a headache day and if the sleep
##     duration was above or below average (within a margin of 1 hour)
##
## Author: hessg1@bfh.ch  Date: 2018-05-29
##
splitIntoDays <- function(observations){
  alldays <- split(observations, observations$day)
  days <- sapply(alldays, function(x){extractDays(x)}, simplify = F, USE.NAMES = T)
  days <- lapply(days, function(x){
    append(x, list(
      nextDayHeadache = otherDayHeadache(x$day, 1, days),
      prevDayHeadache = otherDayHeadache(x$day, -1, days)
    ))})
  days <- ldply (days, data.frame)
  sleepAvg <- mean(na.omit(days$sleepDur))
  days['lessSleep'] <- (days$sleepDur - sleepAvg) < -0.5
  days['moreSleep'] <- (days$sleepDur - sleepAvg) > 0.5
  return((days[,-c(1)]))
}


## extracts values for out a list of entries from a single day
##
## Parameters: 
##   - obs: a list of entries of a day. for properly working, 
##          all entries must be from the same day
## 
## Return value:
##   - a list of, mostly one-hot encoded, values for the given day
##
## Author: hessg1@bfh.ch  Date: 2018-05-26
##
extractDays <- function(obs){
  if(nlevels(factor(obs$day)) > 1) stop('not all entries from the same day')
  day <- obs$day[1]
  
  # onehot-encoding for symptoms
  headacheDay <- 'headache'   %in% obs$type
  attack      <- '216299002'  %in% obs$findingSCT
  vomit       <- '422400008'  %in% obs$findingSCT
  flicker     <- '73905001'   %in% obs$findingSCT
  speachImp   <- '29164008'   %in% obs$findingSCT
  phonophobia <- '313387002'  %in% obs$findingSCT
  hyperosmia  <- '45846002'   %in% obs$findingSCT
  yawning     <- '248626009'  %in% obs$findingSCT
  dysesthesia <- '279079003'  %in% obs$findingSCT
  photophobia <- '409668002'  %in% obs$findingSCT
  redEye      <- '267093002'  %in% obs$findingSCT
  eyeDisc     <- '267092007'  %in% obs$findingSCT
  nasalDisc   <- '267101005'  %in% obs$findingSCT
  nasalObst   <- '267100006'  %in% obs$findingSCT
  nausea      <- '162057007'  %in% obs$findingSCT
  menstruation<- '276319003'  %in% obs$findingSCT
  stress      <- '73595000'   %in% obs$findingSCT
  diffReading <- '309253009'  %in% obs$findingSCT
  calm        <- '102894008'  %in% obs$findingSCT
  
  # sleep values
  sleeps <- obs[which(obs$findingSCT == '248254009'),]
  if(dim(sleeps)[1] > 0){
    sleepInt <- mean(sleeps$intensity)
    sleepDur <- sum(as.numeric(sleeps$duration)) / (60 * 60)
  }
  else{
    sleepInt <- NA
    sleepDur <- NA
  }
  rm(sleeps)
  
  # eatinghabits
  eatRegular    <- '289141003'  %in% obs$findingSCT
  eatIrregular <- '225526009'  %in% obs$findingSCT
  eating <- NA
  if(eatIrregular & !eatRegular) eating = 'irregular'
  if(eatRegular & !eatIrregular) eating = 'regular'
  eating <- factor(eating)
  levels(eating) <- c('irregular', 'regular', NA)
  return(list(day=day, headache=headacheDay, attack=attack, vomitted=vomit, flickerLight=flicker, speachImp=speachImp,
              phonophobia=phonophobia, hyperosmia=hyperosmia, yawning=yawning, dysesthesia=dysesthesia, photophobia=photophobia,
              redEye=redEye, eyeDisc=eyeDisc, nasalDisc=nasalDisc, nasalObst=nasalObst, nausea=nausea, menstruation=menstruation,
              stress=stress, diffReading=diffReading, calm=calm, sleepInt=sleepInt, sleepDur=sleepDur, eating=eating))
}

## Checks if a related day is a headache day
##
## Parameters: 
##   - day: the reference day (as as.Date())
##   - diff: the difference of the day in question,
##           e.g. +1 for the day after the reference day
##           or -1 for the day before
##   - list: a list of day lists as returned from extractDays
## 
## Return value:
##   - TRUE if the day in question was a headache day
##     FALSE if the day in question was not a headache day
##
## Author: hessg1@bfh.ch  Date: 2018-05-26
##
otherDayHeadache <- function(day, diff, daysList){
  day <- as.Date(day)
  posOtherDay <- lapply(daysList, function(x){x$day == day + diff}) # finds the list position of the day in question
  otherDay <- daysList[which(posOtherDay == TRUE)] # gets the day from the list
  if(length(otherDay) > 0) {
    return (otherDay[[1]]$headache) # returns headache value of the day (TRUE / FALSE)
  }else{
    return (NA)
  }
}


