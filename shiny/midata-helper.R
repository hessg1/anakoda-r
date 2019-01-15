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
## Author: hessg1@bfh.ch  Date: 2018-12-09
##

prepareData <- function(dataset){
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


## This function extracts the following data from a dataframe as returned when querying 
## observations from midata with RonFHIR; and works with the old data set (from paper-
## based headache diary) as well as the version persisted by the heMigrania app.
##
## Parameters:
##  - input*: The result of a midata query, as returned from queryMidata()
## 
## Return value: 
##  - a dataframe with one observation per row in following order:
##    ID, Patient Name, StartTime, EndTime, Timestamp, Bodysite (as SCT), Bodysite (Plain text), 
##    Finding (as plain text), Finding (as SCT), Intensity of the Finding
##
## Author: hessg1@bfh.ch  Date: 2019-01-14
##
extractDataFinal <- function(input){
  resources <- input$entry$resource
  numberOfObservations <- dim(resources)[1]
  datatable <- data.frame(ID=vector(), name=vector(), startTime=vector(), endTime=vector(), timestamp=vector(), bodysiteSCT=vector(), bodysiteText=vector(), findingText=vector(), findingSCT=vector(), intensity=vector() ) 
  for(i in 1:numberOfObservations){
    
    # metadata
    ID <- resources$id[i]
    name <- resources$subject$display[i]
    startTime <- resources$effectivePeriod$start[i]
    endTime <- resources$effectivePeriod$end[i]
    timestamp <- resources$meta$lastUpdated[i] 
    
    # "load" data
    bodysiteSCT <- resources$bodySite$coding[[i]]$code
    bodysiteText <- resources$bodySite$coding[[i]]$display
    findingText <- resources$valueCodeableConcept$coding[[i]]$display
    findingSCT <- resources$valueCodeableConcept$coding[[i]]$code
    
    # we the following values can also be NULL and must then be NA
    if(is.null(bodysiteSCT)){
      bodysiteSCT <- NA
    }
    if(is.null(bodysiteText)){
      bodysiteText <- NA
    }
    if(is.null(findingSCT)){
      findingSCT <- NA
    }
    if(is.null(findingText)){
      findingText <- NA
    }
    
    
    
    findingIntensity <- resources$extension[[i]]$valueDecimal #old way
    if(is.null(findingIntensity)){
      findingIntensity <- resources$component[[i]]$valueQuantity$value # try the new way
      if(is.null(findingIntensity)){
        findingIntensity <- NA # when it's still NULL, it should be NA
        #if(findingSCT == "276319003"| findingSCT == "73595000" | findingSCT == "309253009" | findingSCT == "102894008" | findingSCT == "106126000"){
        #  findingIntensity <- 10 # non quantified values are 10 when present (and not persisted when absent)
        #}
      }
    }
    

    
    datatable[i,] <- c(ID, name, startTime, endTime, timestamp, bodysiteSCT, bodysiteText, findingText, findingSCT,findingIntensity)
    
  }
  
  datatable$intensity <- as.numeric(datatable$intensity)
  
  #clear the console (disable this for debugging)
  cat("\014")
  
  return(datatable)
}


## This function extracts the following data from a dataframe as returned when querying 
## observations from midata with RonFHIR
##
## Parameters:
##  - input*: The result of a midata query, as returned from queryMidata()
## 
## Return value: 
##  - a dataframe with one observation per row in following order:
##    ID, Patient Name, StartTime, EndTime, Timestamp, Bodysite (as SCT), Bodysite (Plain text), 
##    Finding (as plain text), Finding (as SCT), Intensity of the Finding
##
## Author: hessg1@bfh.ch  Date: 2018-12-07
##
extractData <- function(input){
  resources <- input$entry$resource
  numberOfObservations <- dim(resources)[1]
  datatable <- data.frame(ID=vector(), name=vector(), startTime=vector(), endTime=vector(), timestamp=vector(), bodysiteSCT=vector(), bodysiteText=vector(), findingText=vector(), findingSCT=vector(), intensity=vector() ) 
  for(i in 1:numberOfObservations){
    
    # metadata
    ID <- resources$id[i]
    name <- resources$subject$display[i]
    startTime <- resources$effectivePeriod$start[i]
    endTime <- resources$effectivePeriod$end[i]
    timestamp <- resources$meta$lastUpdated[i] 
    
    # "load" data
    bodysiteSCT <- resources$bodySite$coding[[i]]$code
    bodysiteText <- resources$bodySite$coding[[i]]$display
    # we have to handle the case when bodysite has no value
    if(is.null(bodysiteSCT)){
      bodysiteSCT <- NA
    }
    if(is.null(bodysiteText)){
      bodysiteText <- NA
    }
    
    findingText <- resources$valueCodeableConcept$coding[[i]]$display
    findingSCT <- resources$valueCodeableConcept$coding[[i]]$code
    findingIntensity <- resources$extension[[i]]$valueDecimal
    if(is.null(findingIntensity)){
      findingIntensity <- NA
    }
    datatable[i,] <- c(ID, name, startTime, endTime, timestamp, bodysiteSCT, bodysiteText, findingText, findingSCT,findingIntensity)
    
  }
  
  datatable$intensity <- as.numeric(datatable$intensity)
  
  #clear the console (disable this for debugging)
  cat("\014")
  
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
  client_id <- "migrEnTest"
  client_secret <- "migrend"
  app_name <- "migrEnTest"
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
