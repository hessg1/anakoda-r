# migrEnDV: migraine data viewer
## the project
This repository is from a project at BFH TI / I4MI.
Within a "living case 2" project, we are working on connecting R directly to MIDATA, using RonFHIR; and displaying the data in a webapp with RShiny.

## how to install and run
- clone the repository with `git clone https://github.com/hessg1/migrEnDV` in your systems console
- if you have R and RStudio installed, you probably need to install the following packages
  - R on FHIR - type `install.packages("RonFHIR")` in your R console
  - markdown - - type `install.packages("markdown")` in your R console
- for starting the RShiny Webapp, just open either `shiny/server.R` or `shiny/ui.R` in RStudio and hit the "Run App" Button.
- That's all. A window for loggin into midata should open, here you can log in as patient or as researcher.
  - for logging in with another user, change `client <- setupMidata(forceLogin = FALSE)` to `client <- setupMidata(forceLogin = TRUE)` in server.R

## Issues
- oAuth with MIDATA does not work with RShiny on shinyapps.io. At the time, only runnig locally is supported.