## This function divides a set of observations into observations of a single day, 
## and indicates if it was a headache-day. Prepares the data for machine-learning (naive-bayes).
##
## Parameters: 
##   - observations: TODO
## 
## Return value:
##   - TODO
##
## Author: hessg1@bfh.ch  Date: 2018-05-26
##
splitIntoDays <- function(observations){
  
  
  alldays <- split(observations, observations$day)
  obs <- alldays[[7]] 
  test <- extractDays(entry)
  test <- sapply(alldays, function(x){extractDays(x)}, simplify = FALSE, USE.NAMES = TRUE)
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
  day <- obs$day
  
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
  eating <- 'unknown'
  if(eatIrregular & !eatRegular) eating = 'irregular'
  if(eatRegular & !eatIrregular) eating = 'regular'

  return(list(day=day, headache=headacheDay, attack=attack, vomitted=vomit, flickerLight=flicker, speachImp=speachImp,
              phonophobia=phonophobia, hyperosmia=hyperosmia, yawning=yawning, dysesthesia=dysesthesia, photophobia=photophobia,
              redEye=redEye, eyeDisc=eyeDisc, nasalDisc=nasalDisc, nasalObst=nasalObst, nausea=nausea, menstruation=menstruation,
              stress=stress, diffReading=diffReading, calm=calm, sleepInt=sleepInt, sleepDur=sleepDur, eating=eating))
}

