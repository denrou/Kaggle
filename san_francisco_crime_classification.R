library(dplyr)
library(rpart)
library(rpart.plot)
library(leaflet)
library(rattle)
library(randomForest)
library(doMC)
registerDoMC(5)
library(ggvis)
library(ggplot2)

Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "en_US.UTF-8")
weekdays.name <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

PdDistrictAdress <- data.frame(PdDistrict = c("CENTRAL", "SOUTHERN", "BAYVIEW", "MISSION", "NORTHERN", "PARK", "RICHMOND", "INGLESIDE", "TARAVAL", "TENDERLOIN"), Latitude = c(37.7998119, 37.7723802, 37.7297322, 37.762849, 37.7800522, 37.7681771, 37.7800026, 37.7249352, 37.7437308, 37.7836739), Longitude = c(-122.3992794, -122.3894121, -122.3979806, -122.422005, -122.4329453, -122.4550214, -122.4647522, -122.4432539, -122.4811864, -122.4128991))

sort.levels <- function(v){
  names(sort(table(v), decreasing = TRUE))
}

# Import data
if(file.exists("train")){
  load(file = "train")
} else{
  train <- read.csv("train.csv")
  save(train, file = "train")
}
train$Dates <- as.POSIXct(train$Dates)
train$NumericalDates <- as.numeric(strftime(train$Dates, format = "%s"))
train$Year <- as.numeric(strftime(train$Dates, format = "%Y"))
train$Month <- factor(strftime(train$Dates, format = "%B"), levels = month.name)
train$Weekday <- factor(weekdays(train$Dates), levels = weekdays.name)
train$Category <- factor(as.character(train$Category), levels = sort.levels(train$Category))
train$Descript <- factor(as.character(train$Descript), levels = sort.levels(train$Descript))
train$PdDistrict <- factor(as.character(train$PdDistrict), levels = sort.levels(train$PdDistrict))
train$Resolution <- factor(as.character(train$Resolution), levels = sort.levels(train$Resolution))
train$Address <- factor(as.character(train$Address), levels = sort.levels(train$Address))
train$Longitude <- train$X
train$Latitude <- train$Y
train$X <- NULL
train$Y <- NULL

# Clean data
train[train$Latitude > 50, c("Latitude", "Longitude")] <- c(NA, NA)

# Some graphics

# Statistics
hist(train$Dates, breaks = 20, freq = TRUE, xlab = "Year")
plot(train$Month, xlab = "")
plot(train$Weekday, xlab = "")
plot(train$Category)
plot(train$Descript)
plot(train$Resolution)

# Location of police department
leaflet(PdDistrictAdress) %>% 
  addTiles() %>% 
  addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~PdDistrict)
