# Step 0
#install.packages('tidyverse')
#install.packages('leaflet')
#install.packages('sf')
#install.packages('tigris')
library(tidyverse)
library(leaflet)
library(sf)
library(tigris)

# Step 1
# Calls for Service
id.calls <- "1gRoL7lZlwm7sreA5F9WbPoH5su4n4iGS"
calls.temp <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.calls))
                       
# Geographic data
fairfax.roads <- roads("VA", "Fairfax city")
fairfax.city <- county_subdivisions("VA", "Fairfax city")

# Step 2
calls.temp$date <- as.Date(calls.temp$date)
calls.temp$dow <- weekdays(calls.temp$date)
calls.temp$month <- substr(calls.temp$date, 6, 7)
calls.full <- subset(calls.temp, !is.na(calls.temp$lat))

# Step 3
calls.year <- calls.full %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

calls.type <- calls.full %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

calls.type <- calls.type[order(-calls.type$count),] # sort in descending order by count
diffs <- diff(calls.type$PCT)
diffs <- c(0, diffs)
calls.type$diff <- diffs

# Step 4 - JUST CHOOSE ONE
# for a single call type:
calls <- subset(calls.full, calls.full$type == 'MISC')
# for multiple call types:
calls <- subset(calls.full, calls.full$type == 'SHOOTING' | calls.full$type == 'STABBING' | calls.full$type == 'ARSON')

# Step 5
## points, roads, outline, and titles
ggplot() +
  geom_sf(data = fairfax.city) +
  geom_point(aes(x=lon, y=lat, color = "red"), data = calls) + 
  geom_sf(data = fairfax.roads) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "Fairfax, VA", subtitle = "Calls on the streets with the city outline")

# transparent points - UPDATE THE ALPHA AND SIZE!
ggplot() +
  geom_point(aes(x=lon, y=lat), data = calls, alpha = 0.10, size = 1.0) +
  theme(legend.position = "bottom")

# densities and contours
ggplot() + 
  geom_sf(data = fairfax.city) +
  geom_sf(data = fairfax.roads) +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = 0.01), 
                 size = 0.001, bins = 5, data = calls, geom = "polygon") + 
  geom_density2d(data = calls, aes(x = lon, y = lat), size = 0.15) +
  theme_bw()

ggplot() + 
  geom_sf(data = fairfax.city) +
  geom_sf(data = fairfax.roads) +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = 0.01), 
                 size = 0.001, bins = 5, data = calls, geom = "polygon") + 
  theme_classic() + 
  theme(legend.position = "none") +
  facet_wrap(~ year, nrow = 3)

# transparent points per year, with streets, titles, and a clean background:
ggplot() + 
  geom_sf(data = fairfax.roads, inherit.aes = FALSE, color = "grey", size = .3, alpha = .5) + 
  geom_point(aes(x = lon, y = lat, color = "red"), data = calls, alpha = 0.1, size = 1.5) +  
  theme_void() + 
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "Fairfax, VA", subtitle = "Calls and streets") +
  facet_wrap(~ year, nrow = 3) +
  theme(legend.position = "none")

# interactive clusters - city boundary and more pop-up text
leaflet(calls) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addMarkers(lng = ~lon, lat = ~lat, 
             popup = paste(
               "Call Type: ", calls$type, "<br>",
               "Date:", calls$date), 
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = fairfax.city)




