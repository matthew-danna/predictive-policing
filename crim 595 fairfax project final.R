### Packages
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

### Get Fairfax geo data, and set projection to the World Geodetic System 1984 (WGS84)
default.crs = sf::st_crs(4326)
fairfax.roads <- roads("VA", "Fairfax city")
fairfax.county <- county_subdivisions("VA", "Fairfax city")

ggplot() +
  geom_sf(data = fairfax.county) +
  geom_sf(data = fairfax.roads) +
  theme_void()

### Choose between Bus Stops or City Development projects
# Bus Stops
# get the data
bus.stops <- read_sheet('https://docs.google.com/spreadsheets/d/1OLjEjJJiyjuu5arluv2oiorM4TqzqIk7PQJ9g4Ichvo')
# convert to a map object
pts.bus <- st_as_sf(bus.stops, coords = c("LONGITUDE", "LATITUDE"), crs=default.crs)

# City Development
city.dev <- read_sheet('https://docs.google.com/spreadsheets/d/1bvJCJSDmoayfMljJs9ajSb4PN4waE5MKDoz9vewyax0')
# convert to a map object
pts.city <- st_as_sf(city.dev, coords = c("Lon", "Lat"), crs=default.crs)

### Choose between Calls for Service or Crime
# Calls for Service
# get the data
id.calls <- "1gRoL7lZlwm7sreA5F9WbPoH5su4n4iGS"
calls.full <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.calls))
# format and clean the data
calls.full$date <- as.Date(calls.full$date)
calls.full$dow <- weekdays(calls.full$date)
calls.full$day <- day(calls.full$date)
calls.full$month <- substr(calls.full$date, 6, 7)
calls <- subset(calls.full, !is.na(calls.full$lat))
# filter since COVID began
calls <- subset(calls, calls$date > 'yyyy-mm-dd')
# convert to a map object
pts.calls <- st_as_sf(calls, coords = c("lon", "lat"), crs=default.crs)

# Crime
# get the data
id.crime <- "1Hdhg5dvPsb28gvkroUU99AKPcGu8rVCp"
crime.full <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.crime))
# format and clean the data
crime.full$date.report <- as.Date(crime.full$date.report)
crime.full$dow <- weekdays(crime.full$date.report)
crime.full$day <- day(crime.full$date.report)
crime.full$month <- substr(crime.full$date.report, 6, 7)
crime <- subset(crime.full, !is.na(crime.full$lat))
# filter since COVID began
crime <- subset(crime, crime$date > '2020-03-01')
# (optional) filter for specific crime types
crime <- subset(crime, crime$crime.description == 'xxx')
# convert to a map object
pts.crime <- st_as_sf(crime, coords = c("lon", "lat"), crs=default.crs)


### Calculate stats and buffers for bus or developments
facility.mean <- as.numeric(mean(st_length(st_nearest_points(pts.bus, pts.calls))))
facility.sd <- as.numeric(sd(st_length(st_nearest_points(pts.bus, pts.calls))))
facility.buffer <- st_buffer(pts.bus, 25)
# test your buffers with a quick map
leaflet(bus.stops) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, 
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = facility.buffer)

### Join the calls or crime to the buffers
facility.activity <- st_join(pts.calls, left = FALSE, bus.buffer["Bus"])


# Analyze the activity types
# summary tables!
summary1 <- facility.activity %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

summary2 <- facility.activity %>%
  group_by(Bus) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

summary3 <- facility.activity %>%
  group_by(Bus, type) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

# Analyze the hotspots
# Analyze the hour, day of week, month of year, and year

ggplot() +
  geom_sf(data = fairfax.roads) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color = "red"), data = bus.stops) + 
  theme_void()

ggplot() +
  geom_sf(data = fairfax.roads) +
  geom_sf(data = bus.buffer) +
  theme_void()


