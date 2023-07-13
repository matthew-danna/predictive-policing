# packages
#install.packages('googlesheets4')
library(googlesheets4)
library(tidyverse)

# Bus Stops
gs4_deauth()
bus.stops <- read_sheet('https://docs.google.com/spreadsheets/d/1OLjEjJJiyjuu5arluv2oiorM4TqzqIk7PQJ9g4Ichvo/edit#gid=0')

# Calls for Service
id.calls <- "1gRoL7lZlwm7sreA5F9WbPoH5su4n4iGS"
calls.full <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.calls))

# Crime
id.crime <- ""
crime.full <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.crime))