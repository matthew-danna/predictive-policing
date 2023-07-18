# Step 0 
install.packages('ragg')
install.packages('lubridate')
install.packages('tidyverse')
library(tidyverse)
library(lubridate)
library(ragg)

# Step 1
id.calls <- "1gRoL7lZlwm7sreA5F9WbPoH5su4n4iGS"
calls.temp <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.calls))

# Step 2
calls.temp$date <- as.Date(calls.temp$date)
calls.temp$dow <- weekdays(calls.temp$date)
calls.temp$day <- day(calls.temp$date)
calls.temp$month <- substr(calls.temp$date, 6, 7)

# Step 3
calls.type <- calls.temp %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

###### DON'T USE THESE CALL TYPES - CHOOSE YOUR OWN!
calls <- subset(calls.temp, calls.temp$type == 'MENTAL' |
                  calls.temp$type == 'NOISE COMPLAINT' |
                  calls.temp$type == 'SUSPICIOUS')

# Step 4
calls.day <- calls %>%
  group_by(date) %>%
  summarise(count = n())

first <- min(calls.day$date)
last <- max(calls.day$date)

calls.day <- calls.day %>% 
  complete(date = seq(ymd(first), 
                      ymd(last), 
                      "day")) %>%
  mutate(dow = wday(date, label = T, week_start = 1), 
         month.name = month(date, label = T, abbr = F),
         month.num = substr(date, 6,7),
         week = isoweek(date),
         day = day(date),
         year = year(date))

calls.day$count[is.na(calls.day$count)] = "0"
calls.day$count <- as.numeric(calls.day$count)

high <- as.numeric(max(calls.day$count))
low <- as.numeric(min(calls.day$count))
mean <- mean(calls.day$count)
sd <- sd(calls.day$count)

calls.day <- mutate(calls.day, 
                    week = case_when(month.name == "December" & week == 1 ~ 53,
                                     month.name == "January" & week %in% 52:53 ~ 0,
                                     TRUE ~ week),
                    category = cut(count, c(low-1, (mean-sd*3), (mean-sd*2), (mean-sd), mean, (mean+sd), (mean+sd*2), (mean+sd*3), high+1))) 

calls.summary <- calls.day %>%
  group_by(category) %>%
  summarise(count = n())

# Histograms
ggplot(calls.day, aes(x= count)) + geom_histogram(fill = "black")

ggplot(calls.day, aes(x= count)) + geom_histogram(fill = "black") +
  theme_classic() +
  theme(legend.position = "none")

ggplot(calls.day, aes(x= count)) + geom_histogram(fill = "black") +
  geom_vline(data = calls.day, aes(xintercept = mean(count), color = "red"), linetype = "dashed") +
  theme_classic() +
  theme(legend.position = "none")

ggplot(calls.day, aes(x= count, colour = dow)) + geom_histogram(fill = "white") +
  theme_classic() +
  theme(legend.position = "top")

ggplot(calls.day, aes(x= count, colour = dow)) + 
  geom_histogram(fill = "white", position = "dodge") +
  theme_classic() +
  theme(legend.position = "top")

ggplot(calls.day, aes(x= count, colour = dow)) + geom_histogram(fill = "grey") +
  geom_vline(data = calls.day, aes(xintercept = mean(count)), linetype = "dashed") +
  theme_classic() +
  theme(legend.position = "top") +
  facet_wrap(~ month.num, nrow = 4)

ggplot(calls.day, aes(x= count)) + geom_histogram(fill = "grey") +
  geom_vline(data = calls.day, aes(xintercept = mean(count)), linetype = "dashed") +
  theme_classic() +
  theme(legend.position = "top") +
  facet_wrap(~ month.num, nrow = 4)

# Step 5
calls.day.time <- calls %>%
  group_by(dow, hour) %>%
  summarise(count = n())
calls.day.time <- subset(calls.day.time, !is.na(calls.day.time$hour))

ggplot(calls.day.time, aes(hour, dow, fill = count)) +
  geom_tile()

ggplot(calls.day.time, aes(hour, dow, fill = count)) +
  geom_tile() + coord_fixed()

ggplot(calls.day.time, aes(hour, dow, fill = count)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + 
  coord_fixed()

ggplot(calls.day.time, aes(hour, dow, fill = count)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = count), color = "white", size = 1.5) +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_fixed()

ggplot(calls.day.time, aes(hour, dow, fill = count)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = count), color = "black", size = 2.5) +
  scale_fill_gradient(low = "blue", high = "red") +
  guides(fill = guide_colourbar(title = "Call Count")) +
  coord_fixed()

ggplot(calls.day.time, aes(hour, dow, fill = count)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = count), color = "black", size = 1.5) +
  scale_fill_gradient(low = "white", high = "red") +
  guides(fill = guide_colourbar(title = "Call Count", label = FALSE, ticks = FALSE)) +
  coord_fixed()

ggplot(calls.day.time, aes(hour, dow, fill = count)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = count), color = "black", size = 2.5) +
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position = "none") +
  coord_fixed()

calls.year.day.time <- calls %>%
  group_by(year, dow, hour) %>%
  summarise(count = n())
calls.year.day.time <- subset(calls.year.day.time, !is.na(calls.year.day.time$hour))

ggplot(calls.year.day.time, aes(hour, dow, fill = count)) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position = "none") +
  coord_fixed() +
  facet_wrap(~ year, nrow = 6)

graph <- ggplot(calls.day.time, aes(hour, dow, fill = count)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = count), color = "black", size = 2.5) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_classic() +
  theme(legend.position = "none") +
  coord_fixed()

graph2 <- ggplot(calls.day.time, aes(hour, dow, fill = count)) +
  geom_tile() + 
  geom_text(aes(label = count), color = "black", size = 2.5) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_classic() +
  coord_fixed()

graph3 <- ggplot(calls.day.time, aes(hour, dow, fill = count)) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red") +
  guides(fill = guide_colourbar(title = "Call Activity")) +
  theme_classic() +
  coord_fixed()

graph + 
  labs(x = "Hour of Day", y = "Day of Week", title = "Temporal Topology", subtitle = "By: Me", caption = "Source: ...") +
  theme_bw() +
  theme(axis.text = element_text(angle = 90))

graph + 
  labs(x = "Hour of Day", y = "Day of Week", title = "Temporal Topology", subtitle = "By: Me", caption = "Source: ...") +
  scale_y_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
  scale_x_continuous(breaks = c(0:23)) +
  theme_classic()

graph2 + 
  labs(x = "Hour of Day", y = "Day of Week", title = "Temporal Topology", subtitle = "By: Me") +
  scale_y_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
  scale_x_continuous(breaks = c(0:23))

graph3 + 
  labs(x = "Hour of Day", y = "Day of Week", title = "Temporal Topology") +
  scale_y_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
  scale_x_continuous(breaks = c(0:23))

#### Day of month, month of year
calls.year.month.day <- calls %>%
  group_by(year, month, day) %>%
  summarise(count = n())
calls.year.month.day <- subset(calls.year.month.day, !is.na(calls.year.month.day$day))


