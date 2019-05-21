## This function further formats a dataset as retrieved from extractData(), as it factorizes
## columns that need to be factorized, converts the time to a processable format and adds following
## (calculated) rows: duration (the duration of the occurence), uid (a factorized user id), day (the day
## of the occurence, without time).
##
## Parameters: 
##   - dataset*: a data set like retrieved from extractData()
## 
## Return value:
##   - the modified dataset, with added columns
##
## Author: hessg1@bfh.ch  Date: 2018-05-21
##

prepareData <- function(dataset){
  # remove invalid observations
  dataset <- dataset[which(dataset$wrongDate != TRUE),]
  
  # factorize
  dataset$findingText <- factor(dataset$findingText)
  dataset$bodysiteText <- factor(dataset$bodysiteText) 

  # format time
  dataset$timestamp <- as.POSIXct(dataset$timestamp, format="%Y-%m-%dT%H:%M:%S")
  dataset$endTime <- as.POSIXct(dataset$endTime, format="%Y-%m-%dT%H:%M:%S")
  dataset$startTime <- as.POSIXct(dataset$startTime, format="%Y-%m-%dT%H:%M:%S")
  
  # additional rows
  dataset$day <- as.Date(dataset$startTime)
  dataset$uid <- factor(dataset$name)
  levels(dataset$uid) <- as.character(1:nlevels(dataset$uid))
  dataset$duration <- dataset$endTime - dataset$startTime
  
  return(dataset)
}


## This function extracts the load data from a dataframe when quering MedicationStatmentes
##
## Parameters:
##  - input*: The result of a midata query, as returned from queryMidata()
## 
## Return value: 
##  - a dataframe with one observation per row in following order:
##    ID, Patient Identifier, Taking Time, Timestamp (of entry creation), medication name,
##    taken quantity, and if it helped or not
##
## Author: hessg1@bfh.ch  Date: 2019-05-12
##
extractMedication <- function(input){
  resources <- input$entry$resource
  numberOfEntries <- dim(resources)[1]
  datatable <- data.frame(ID=vector(), name=vector(), takingTime=vector(), timestamp=vector(), medication=vector(), quantity=vector(), helped=vector()) 
  
  for(i in 1:numberOfEntries){
    # metadata
    ID <- resources$id[i]
    name <- resources$subject$display[i]
    takingTime <- resources$effectiveDateTime[i]
    timestamp <- resources$meta$lastUpdated[i] 
    
    # "load" data
    medication <- resources$medicationCodeableConcept$coding[[i]]$display
    if(is.null(medication)){
      medication <- NA
    }
    quantity <- resources$dosage[[i]]$doseQuantity
    if(is.null(quantity)){
      quantity <- NA
    } else {
      quantity <- as.numeric(quantity)
    }
    
    helped <-   resources$dosage[[i]]$text
    if(is.null(helped)){
      helped <- NA
    }
    datatable[i,] <- c(ID, name, takingTime, timestamp, medication, quantity, helped)
  }
  
  datatable$name <- factor(datatable$name)
  datatable$helped <- factor(datatable$helped)
  
  return(datatable)
}

## This function extracts the following data from a dataframe as returned when querying 
## observations from midata with RonFHIR; and works with the old data set (from paper-
## based headache diary) as well as the version persisted by the heMigrania app.
##
## Parameters:
##  - input*: The result of a midata query, as returned from queryMidata()
## 
## Return value: 
##  - a dataframe with one observation per row in following order:
##    TODO TODO TODO
##
## Author: hessg1@bfh.ch  Date: 2019-05-20
##
extractObservation <- function(input){

  resources <- input$entry$resource
  numberOfObservations <- dim(resources)[1]
  datatable <- data.frame(ID=vector(), name=vector(), type=vector(), startTime=vector(), endTime=vector(), timestamp=vector(), bodysiteSCT=vector(), bodysiteText=vector(), findingText=vector(), findingSCT=vector(), intensity=vector(), app=vector(), wrongDate=vector()) 
  for(i in 1:numberOfObservations){
    
    # metadata
    ID <- resources$id[i]
    name <- resources$subject$display[i]
    startTime <- resources$effectivePeriod$start[i]
    endTime <- resources$effectivePeriod$end[i]
    timestamp <- resources$meta$lastUpdated[i] 
    app <- resources$meta$extension[[i]]$extension[[1]]$valueCoding$display[1]
    
    # "load" data
    bodysiteSCT <- resources$bodySite$coding[[i]]$code
    bodysiteText <- resources$bodySite$coding[[i]]$display
    findingText <- resources$valueCodeableConcept$coding[[i]]$display
    findingSCT <- resources$valueCodeableConcept$coding[[i]]$code
    
    # the following values can also be NULL and must then be NA
    if(is.null(bodysiteSCT)){
      bodysiteSCT <- NA
    }
    if(is.null(bodysiteText)){
      bodysiteText <- NA
    }
    if(is.null(findingSCT)){
      if(resources$code$coding[[i]]$code == "162306000"){
        findingSCT <- "74964007h"
      }else {
        findingSCT <- resources$component[[i]]$code$coding[[1]]$code # Schlaf
      }
      #if(is.null(findingSCT)){
       # findingSCT <- NA
      #}
    }
    if(is.null(findingText)){
      if(resources$code$coding[[i]]$code == "162306000"){
        findingText <- "other Headache"
      }else{
        findingText <- resources$component[[i]]$code$coding[[1]]$display # Schlaf
      }
      if(is.null(findingText)){
        findingText <- NA
      }
    }
    
    findingIntensity <- resources$extension[[i]]$valueDecimal #old way
    if(is.null(findingIntensity)){
      findingIntensity <- resources$component[[i]]$valueQuantity$value # try the new way
      if(is.null(findingIntensity)){
        findingIntensity <- resources$component[[i]]$valueQuantity$value[1] # sleep 
        if(is.null(findingIntensity)){
          findingIntensity <- 15 # NA # when it's still NULL, it should be NA
        }
      }
    }
    
 
    
    
    
    type <- NA
    headaches <- c("162307009", "162309007", "162308004", "74964007h")
    complaints <- c("422400008","279079003", "16932000", "73905001", "29164008", "162307009", "162309007", "162308004", "313387002", "45846002", "248626009", "409668002", "409668002", "267093002", "267092007", "267101005", "267100006", "162057007")
    conditions <- c("276319003", "73595000", "309253009", "102894008", "106126000")
    dayentries <- c("225526009", "289141003", "702970004", "248254009")
    
    
    if(findingSCT == "216299002"){
      type <- "attack"
    }
    if(findingSCT %in% headaches){
      type <- "headache"
    }
    if(findingSCT %in% complaints){
      type <- "complaint"
    }
    if(findingSCT %in% conditions){
      type <- "condition"
    }
    if(findingSCT %in% dayentries){
      type <- "dayentry"
    }
    if(grepl("G43", findingSCT)){
      type <- "diagnosis"
    }
    if(findingSCT == "74964007"){
      type <- "variousOthers"
    }
    
    if(is.na(startTime)){
      day <- as.POSIXlt(resources$effectiveDateTime[i], format="%Y-%m-%dT%H:%M:%S")
      day$hour <- 0
      day$minute <- 0
      day$second <- 0
      startTime <- as.character.POSIXt(day, format="%Y-%m-%dT%H:%M:%S")
      day$hour <- 23
      day$minute <- 59
      day$second <- 59
      endTime <- as.character.POSIXt(day, format="%Y-%m-%dT%H:%M:%S")
      
    }
    
    wrongDate <- startTime[1] > endTime[1]
    
    entry <- c(ID[1], name[1], type, startTime[1], endTime[1], timestamp[1], bodysiteSCT[1], bodysiteText[1], findingText[1], findingSCT[1],findingIntensity[1],app[1], wrongDate[1])
   #print(entry)
   #print(length(entry))
    
    datatable[i,] <- entry
    
  }
  
  datatable$findingText <- factor(datatable$findingText)
  datatable$bodysiteText <- factor(datatable$bodysiteText) 
  datatable$intensity <- as.numeric(datatable$intensity)
  datatable$name <- factor(datatable$name)
  datatable$type <- factor(datatable$type)
  datatable$app <- factor(datatable$app)
  
  #clear the console (disable this for debugging)
  #cat("\014")
  
  return(datatable)
}




## This function sets up a connection to MIDATA. OAuth authorization
## is required during the process.
## 
## Parameters: 
##  - url = the correct MIDATA URL (e.g. "https://test.midata.coop" for testing, default)
##  - forceLogin = TRUE (default), for forcing reauthentification of user
##                 FALSE, for using (possibly) cached oAuth token
## Return value:
##   - A RonFHIR client that can be used for querying
##
##    Author: hessg1@bfh.ch  Date: 2018-11
##    with very helpful input from Dick Chavez, I4MI
##
setupMidata <- function(url = "http://test.midata.coop", forceLogin = TRUE){
  library(RonFHIR)
  
  # if 
  if(forceLogin && file.exists(".httr-oauth")) {
    file.remove(".httr-oauth")
  }
  
  #Setting up the fhirClient
  client <- fhirClient$new(paste(url, "/fhir", sep=""))
  client_id <- "anakoda-R" # was "migrEnTest"
  client_secret <- "migren" # was "migrend"
  app_name <- "anakoda-R" # was migrenTest
  scopes <- ""
  options(httr_oauth_cache=F)
  
  # authorization stuff to retrieve a token
  app <- httr::oauth_app(appname = app_name, client_id, client_secret)
  oauth_endpoint <- httr::oauth_endpoint(authorize = paste(url, "/authservice?aud=", "http://localhost:1410", sep=""), access = client$tokenUrl)
  
  token <- httr::oauth2.0_token(endpoint = oauth_endpoint, app = app, scope = scopes)
  
  # set the token to the client
  client$setToken(token$credentials$access_token)
  rm(token)
  
  #clear the console (disable this for debugging)
  cat("\014")
  return(client)
}

## This function gets all observations of one user from midata. OAuth authorization
## is required during the process.
## 
## Parameters: 
##  - client*: A RonFHIR client, set up with setupMidata()
##
## Return value: 
##  - A nested dataframe, recommended to extract data with extractData() 
##
##    Author: hessg1@bfh.ch  Date: 2018-12-03
##
queryMidata <- function(client){
  library(RonFHIR)
  return(client$search("Observation", "code:in=418138009")) #"code:in=418138009"))
}
