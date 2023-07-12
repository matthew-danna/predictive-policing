#1
library(tidyverse)
library(zoo)
library(aTSA)
library(tseries)
library(forecast)

dc.data2023 <- read.csv("https://opendata.arcgis.com/datasets/89561a4f02ba46cca3c42333425d1b87_5.csv", stringsAsFactors = FALSE) 
dc.data2022 <- read.csv("https://opendata.arcgis.com/datasets/f9cc541fc8c04106a05a1a4f1e7e813c_4.csv", stringsAsFactors = FALSE)
dc.data2021 <- read.csv("https://opendata.arcgis.com/datasets/619c5bd17ca2411db0689bb0a211783c_3.csv", stringsAsFactors = FALSE)
dc.data2020 <- read.csv("https://opendata.arcgis.com/datasets/f516e0dd7b614b088ad781b0c4002331_2.csv", stringsAsFactors = FALSE)
dc.data2019 <- read.csv("https://opendata.arcgis.com/datasets/f08294e5286141c293e9202fcd3e8b57_1.csv", stringsAsFactors = FALSE)
dc.data2018 <- read.csv("https://opendata.arcgis.com/datasets/38ba41dd74354563bce28a359b59324e_0.csv", stringsAsFactors = FALSE)
dc.data2017 <- read.csv("https://opendata.arcgis.com/datasets/6af5cb8dc38e4bcbac8168b27ee104aa_38.csv", stringsAsFactors = FALSE)
dc.data2016 <- read.csv("https://opendata.arcgis.com/datasets/bda20763840448b58f8383bae800a843_26.csv", stringsAsFactors = FALSE)
dc.data2015 <- read.csv("https://opendata.arcgis.com/datasets/35034fcb3b36499c84c94c069ab1a966_27.csv", stringsAsFactors = FALSE)
dc.data2014 <- read.csv("https://opendata.arcgis.com/datasets/6eaf3e9713de44d3aa103622d51053b5_9.csv", stringsAsFactors = FALSE)
dc.data2013 <- read.csv("https://opendata.arcgis.com/datasets/5fa2e43557f7484d89aac9e1e76158c9_10.csv", stringsAsFactors = FALSE)
dc.data2012 <- read.csv("https://opendata.arcgis.com/datasets/010ac88c55b1409bb67c9270c8fc18b5_11.csv", stringsAsFactors = FALSE)
dc.data2011 <- read.csv("https://opendata.arcgis.com/datasets/9d5485ffae914c5f97047a7dd86e115b_35.csv", stringsAsFactors = FALSE)
dc.data2010 <- read.csv("https://opendata.arcgis.com/datasets/fdacfbdda7654e06a161352247d3a2f0_34.csv", stringsAsFactors = FALSE)
dc.data2009 <- read.csv("https://opendata.arcgis.com/datasets/73cd2f2858714cd1a7e2859f8e6e4de4_33.csv", stringsAsFactors = FALSE)
dc.data2008 <- read.csv("https://opendata.arcgis.com/datasets/180d56a1551c4e76ac2175e63dc0dce9_32.csv", stringsAsFactors = FALSE)

dc.data.temp <- rbind(dc.data2008, dc.data2009, dc.data2010, dc.data2011, dc.data2012, dc.data2013, dc.data2014, dc.data2015, 
                      dc.data2016, dc.data2017, dc.data2018, dc.data2019, dc.data2020, dc.data2021, dc.data2022, dc.data2023)

dc.data <- separate(dc.data.temp, REPORT_DAT, into = c("date", "time"), sep = " ")
dc.data$date <- as.Date(dc.data$date, format = "%Y/%m/%d") # format the date as a date
#### DONT USE THIS CRIME TYPE - CHOOSE YOUR ORIGINAL ONE

data <- subset(dc.data, dc.data$OFFENSE == 'ARSON')

crime.day <- data %>%  
  group_by(date) %>% 
  summarise(count = n())

first.date <- min(crime.day$date)
last.date <- max(crime.day$date)

crime.day <- crime.day %>% 
  complete(date = seq(ymd(first.date), 
                      ymd(last.date), 
                      "day"))

crime.day$count[is.na(crime.day$count)] = "0"
crime.day$count <- as.numeric(crime.day$count)

#2 - REPLACE THE XX-XX WITH DATE OF FIRST FORECAST
data.cleaned.original <- zoo(crime.day$count, seq(from = as.Date(min(crime.day$date)), to = as.Date('2023-xx-xx'), by = 1))
summary(data.cleaned.original)
stationary.data.original <- diff(data.cleaned.original)
arima.function.original <- auto.arima(stationary.data.original)
arima.function.original

#3 - REPLACE THE XX-XX WITH DATE OF FIRST FORECAST
dates <- as.numeric(max(crime.day$date) - as.Date('2023-xx-xx'))
dates

#4
forecast.update <- forecast(arima.function.original, h=dates)
additional.update <- round(sum(forecast.update$upper[,2]),0)
additional.update

#5 - REPLACE THE XX-XX WITH DATE OF FIRST FORECAST
new.crime <- subset(crime.day, crime.day$date > '2023-xx-xx')
new.crime.sum <- sum(new.crime$count)
new.crime.sum
additional.update - new.crime.sum

print(new.crime, n=nrow(new.crime))



