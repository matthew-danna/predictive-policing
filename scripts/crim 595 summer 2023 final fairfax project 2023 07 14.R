### Step 0
install.packages('googlesheets4')
install.packages('tidyverse')
install.packages('leaflet')
install.packages('sf')
install.packages('tigris')
library(googlesheets4)
gs4_deauth()
library(tidyverse)
library(leaflet)
library(sf)
library(tigris)

### Step 1
default.crs = sf::st_crs(4326)
fairfax.roads <- roads("VA", "Fairfax city")
fairfax.county <- county_subdivisions("VA", "Fairfax city")

ggplot() +
  geom_sf(data = fairfax.county) +
  geom_sf(data = fairfax.roads) +
  theme_void()

### Step 2
# Bus Stops
bus.stops <- read_sheet('https://docs.google.com/spreadsheets/d/1OLjEjJJiyjuu5arluv2oiorM4TqzqIk7PQJ9g4Ichvo')
pts.bus <- st_as_sf(bus.stops, coords = c("LONGITUDE", "LATITUDE"), crs=default.crs)

# City Development Projects
city.dev <- read_sheet('https://docs.google.com/spreadsheets/d/1bvJCJSDmoayfMljJs9ajSb4PN4waE5MKDoz9vewyax0')
pts.city <- st_as_sf(city.dev, coords = c("Lon", "Lat"), crs=default.crs)

### Step 3
# Calls
id.calls <- "1gRoL7lZlwm7sreA5F9WbPoH5su4n4iGS"
calls.full <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.calls))

calls.full$date <- as.Date(calls.full$date)
calls.full$dow <- weekdays(calls.full$date)
calls.full$day <- day(calls.full$date)
calls.full$month <- substr(calls.full$date, 6, 7)
calls <- subset(calls.full, !is.na(calls.full$lat))

calls <- subset(calls, calls$date > '2020-03-01')

pts.calls <- st_as_sf(calls, coords = c("lon", "lat"), crs=default.crs)

# Crime
id.crime <- "1Hdhg5dvPsb28gvkroUU99AKPcGu8rVCp"
crime.full <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.crime))
crime.full$date.report <- as.Date(crime.full$date.report)
crime.full$dow <- weekdays(crime.full$date.report)
crime.full$day <- day(crime.full$date.report)
crime.full$month <- substr(crime.full$date.report, 6, 7)
crime <- subset(crime.full, !is.na(crime.full$lat))
crime <- crime %>% filter(date.report > '2020-03-01')
pts.crime <- st_as_sf(crime, coords = c("lon", "lat"), crs=default.crs)

### Step 4
facility.mean <- as.numeric(mean(st_length(st_nearest_points(FACILITY FILE HERE, ACTIVITY FILE HERE))))
facility.sd <- as.numeric(sd(st_length(st_nearest_points(FACILITY FILE HERE, ACTIVITY FILE HERE))))

facility.buffer <- st_buffer(FACILITY POINT FILE HERE, NUMBER)

leaflet(FACILITIES) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, 
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = facility.buffer)

facility.activity <- st_join(pts.calls OR pts.crime, left = FALSE, facility.buffer["Bus"])

### Step 5
summary1 <- facility.activity %>%
  group_by(crime.description) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

summary2 <- facility.activity %>%
  group_by(Bus, crime.description) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))


ggplot() +
  geom_sf(data = fairfax.county) +
  geom_sf(data = fairfax.roads) +
  geom_sf(data = pts.calls) +
  theme_void()





