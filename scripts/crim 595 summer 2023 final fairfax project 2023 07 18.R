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
fairfax.city <- county_subdivisions("VA", "Fairfax city")

ggplot() +
  geom_sf(data = fairfax.city) +
  geom_sf(data = fairfax.roads) +
  theme_void()

### Step 2
# Bus Stops
bus.stops <- read_sheet('https://docs.google.com/spreadsheets/d/1OLjEjJJiyjuu5arluv2oiorM4TqzqIk7PQJ9g4Ichvo')

# alternative way to get the bus stop data:
id.bus <- "1UDSghDGA5u5z9VaWnqfIy1_CvpYyTXJz"
bus.stops <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.bus))

pts.bus <- st_as_sf(bus.stops, coords = c("LONGITUDE", "LATITUDE"), crs=default.crs)

# City Development Projects
city.dev <- read_sheet('https://docs.google.com/spreadsheets/d/1bvJCJSDmoayfMljJs9ajSb4PN4waE5MKDoz9vewyax0')
city.dev$Lat <- as.numeric(city.dev$Lat)
city.dev$Lon <- as.numeric(city.dev$Lon)
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

# subset 'since COVID' if analyzing bus stops
calls <- subset(calls, calls$date > 'yyyy-mm-dd')
# OPTIONAL - subsetting for specific call types
calls <- subset(calls, calls$type == 'MENTAL' | calls$type == 'SUSPICIOUS' |
                  calls$type == 'DOMESTIC' | calls$type == 'ASSAULT' |
                  calls$type == 'SEXUAL ASSAULT/RAPE')

calls <- subset(calls, !is.na(calls$type))

pts.calls <- st_as_sf(calls, coords = c("lon", "lat"), crs=default.crs)

# Crime
id.crime <- "1Hdhg5dvPsb28gvkroUU99AKPcGu8rVCp"
crime.full <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.crime))
crime.full$date.report <- as.Date(crime.full$date.report)
crime.full$dow <- weekdays(crime.full$date.report)
crime.full$day <- day(crime.full$date.report)
crime.full$month <- substr(crime.full$date.report, 6, 7)
crime <- subset(crime.full, !is.na(crime.full$lat))

# subset 'since COVID' if analyzing bus stops
crime <- crime %>% filter(date.report > 'yyyy-mm-dd')

# OPTIONAL - subsetting for specific crime types
crime <- crime %>% filter(crime.description == 'ROBBERY')

crime.list <- c("ROBBERY", "SIMPLE ASSAULT", "DRUNKENNESS", "ARSON", "POCKET PICKING")
crime.nolist <- c("ALL OTHER OFFENSES", "PEEPING TOM")
`%notin%` <- Negate(`%in%`)

crime <- crime %>% filter(crime.description %in% crime.list)
crime <- crime %>% filter(crime.description %notin% crime.nolist)

pts.crime <- st_as_sf(crime, coords = c("lon", "lat"), crs=default.crs)

### Step 4
# Choose one of these 4
facility.mean <- as.numeric(mean(st_length(st_nearest_points(pts.bus, pts.crime))))
facility.mean <- as.numeric(mean(st_length(st_nearest_points(pts.bus, pts.calls))))
facility.mean <- as.numeric(mean(st_length(st_nearest_points(pts.city, pts.crime))))
facility.mean <- as.numeric(mean(st_length(st_nearest_points(pts.city, pts.calls))))

# Chose one of these 4
facility.sd <- as.numeric(sd(st_length(st_nearest_points(pts.city, pts.calls))))
facility.sd <- as.numeric(sd(st_length(st_nearest_points(pts.city, pts.crime))))
facility.sd <- as.numeric(sd(st_length(st_nearest_points(pts.bus, pts.calls))))
facility.sd <- as.numeric(sd(st_length(st_nearest_points(pts.bus, pts.crime))))

# UPDATE THE NUMBER TO A BUFFER THAT WORKS FOR YOUR DATA!
facility.buffer <- st_buffer(pts.bus, 230)

leaflet(bus.stops) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, 
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = facility.buffer)

# Chose one of these 4
facility.activity <- st_join(pts.crime, left = FALSE, facility.buffer["Bus"])
facility.activity <- st_join(pts.calls, left = FALSE, facility.buffer["Bus"])
facility.activity <- st_join(pts.calls, left = FALSE, facility.buffer["Property"])
facility.activity <- st_join(pts.crime, left = FALSE, facility.buffer["Property"])

##### NEW - join the original table to the new table
# Only run one of these
facility.activity <- facility.activity %>% left_join(crime, by = 'ID')
facility.activity <- facility.activity %>% left_join(calls, by = 'ID')
#### now only keep the columns you need
# for crime:
facility.activity <- facility.activity[c(1:23,43)]
names(facility.activity) <- c("number", "date1", "time1", "type", "date2", "time2", "statute", "crime", 
                              "crime.description", "statute.description", "date.report", "time.report",
                              "hour1", "hour2", "hour.report", "year", "ID", "dow", "day", "month", "name", 
                              "lat", "lon", "geometry")
# for calls:
facility.activity <- facility.activity[c(1:13,23)]
names(facility.activity) <- c("date", "time", "number", "type", "hour", "year", "ID", 
                              "dow", "day", "month", "name", "lat", "lon", "geometry")

### Step 5
# for crime:
summary1 <- facility.activity %>%
  group_by(crime.description) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

# for calls:
summary1 <- facility.activity %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

# for crime:
summary2 <- facility.activity %>%
  group_by(name, crime.description) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

# for calls:
summary2 <- facility.activity %>%
  group_by(name, type) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

##### Spatial
# Cluster map example:
leaflet(facility.activity) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addMarkers(lng = ~lon, lat = ~lat, 
             popup = paste(
               "Call Type: ", facility.activity$type, "<br>",
               "Date:", facility.activity$date), 
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = fairfax.city, fillColor = 'white') %>%
  addPolygons(data = facility.buffer)

# Hotspot map examples:
ggplot() + 
  geom_sf(data = fairfax.city, color = "white") +
  geom_sf(data = fairfax.roads, inherit.aes = FALSE, color = "grey", size = .3, alpha = .5) + 
  geom_sf(data = facility.buffer) +
  geom_point(aes(x = lon, y = lat, color = "red"), data = facility.activity, alpha = 0.01, size = 1.5) +  
  theme_void() + 
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "Fairfax, VA", subtitle = "Calls since COVID near bus stops")

ggplot() + 
  geom_sf(data = fairfax.city, color = "white") +
  geom_sf(data = fairfax.roads, inherit.aes = FALSE, color = "grey", size = .3, alpha = .5) + 
  geom_sf(data = facility.buffer) +
  geom_point(aes(x = lon, y = lat, color = "red"), data = facility.activity, alpha = 0.01, size = 1.5) +  
  theme_void() + 
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "Fairfax, VA", subtitle = "Calls since COVID near bus stops") +
  facet_wrap(~ type, nrow = 3)

# Temporal examples
# overall
activity.day.time <- facility.activity %>%
  group_by(dow, hour) %>%
  summarise(count = n())
activity.day.time <- subset(activity.day.time, !is.na(activity.day.time$hour))

ggplot(activity.day.time, aes(hour, dow, fill = count)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = count), color = "black", size = 2.5) +
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position = "none") +
  coord_fixed()

# By activity type:
activity.type.day.time <- facility.activity %>%
  group_by(type, dow, hour) %>%
  summarise(count = n())
activity.type.day.time <- subset(activity.type.day.time, !is.na(activity.type.day.time$hour))

ggplot(activity.type.day.time, aes(hour, dow, fill = count)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = count), color = "black", size = 2.5) +
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position = "none") +
  coord_fixed() +
  facet_wrap(~ type, nrow = 10)


