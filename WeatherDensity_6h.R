require(DAAG)
require(MASS)
library(maptools)
library(plyr)
library(ggplot2)
library(car)
library(MASS)
library(sp)
source("twitterUtil.r")

# weather prediction data

# Import weather data
weather.2014 <- read.table(
  "http://www.wunderground.com/history/airport/KORD/2014/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2014&req_city=NA&req_state=NA&req_statename=NA&format=1",
  header=TRUE, sep=",")



# Change variable name (CST -> date)
colnames(weather.2014)[1] <- "date"
weather.2014$date<-as.Date(weather.2014$date)

# Keep only necessary variables
weather.2014<- weather.2014[, !(colnames(weather.2014) %in% c("Max.VisibilityMiles","Mean.VisibilityMiles",
                                                              "Min.VisibilityMiles","Max.Gust.SpeedMPH","WindDirDegrees.br..."))]

# Convert class of event to character to reduce categories of events
weather.2014$Events <- as.character(weather.2014$Events)

weather.2014$Events[weather.2014$Events == ""] <- "sunny"

weather.2014$Events[weather.2014$Events == "Snow"|
                      weather.2014$Events == "Fog-Snow"|
                      weather.2014$Events == "Rain-Snow"|
                      weather.2014$Events == "Fog-Rain-Snow"] <- "snow"


weather.2014$Events[weather.2014$Events == "Rain"|
                      weather.2014$Events == "Fog-Rain"|
                      weather.2014$Events == "Rain-Thunderstorm"|
                      weather.2014$Events == "Fog-Rain-Thunderstorm"|
                      weather.2014$Events == "Thunderstorm"] <- "rain"

weather.2014$Events[weather.2014$Events == "Fog"] <- "fog"

# Convert class of events as factor to build model
weather.2014$Events <- as.factor(weather.2014$Events)

# Create column with day of month
weather.2014$mday <- as.numeric(format(weather.2014$date,'%d'))

# Convert Precipitation into numeric
weather.2014$PrecipitationIn <- as.numeric(levels(weather.2014$PrecipitationIn))[weather.2014$PrecipitationIn]
class(weather.2014$PrecipitationIn)

# Fill up the NA with median of each events
weather.2014$PrecipitationIn[which(weather.2014$Events == "snow" & is.na(weather.2014$PrecipitationIn))] <- 
  median(weather.2014$PrecipitationIn[which(weather.2014$Events == "snow")], na.rm = TRUE)

weather.2014$PrecipitationIn[which(weather.2014$Events == "sunny" & is.na(weather.2014$PrecipitationIn))] <- 0

weather.2014$PrecipitationIn[which(weather.2014$Events == "rain" & is.na(weather.2014$PrecipitationIn))] <-
  median(weather.2014$PrecipitationIn[which(weather.2014$Events == "rain")], na.rm = TRUE)

weather.2014$PrecipitationIn[which(weather.2014$Events == "fog" & is.na(weather.2014$PrecipitationIn))] <- 0

# Check the all the classes of variables
str(weather.2014)

#save("weather.2014", file = "Capstone/weather.2014.Rdata")

#load("Capstone/weather.2014.Rdata")

#load("Capstone/training_density_revised.Rdata")

###merge two data sets into one by "mday"
weather.2014$month <- month(weather.2014$date)
weather.jan <- weather.2014[which(weather.2014$month == 1),]

weather.density.data <- merge(theft.training.density, weather.2014[which(weather.2014$date<"2014-02-01"),], by="mday", all.x=TRUE)

#save(weather.density.data, file = "Capstone/weather_density.Rdata")
load("weather_density.Rdata")
weather.density.data$sixhr_n <- (weather.density.data$mday-1)*4 + weather.density.data$sixhr  

# assign min\max\mean tempreture to different period
weather.density.data$TemperatureF <- NA

for (i in 1:nrow(weather.density.data)) {
  if (weather.density.data$sixhr[i] == 1){
    weather.density.data$TemperatureF[i] <- weather.density.data$Min.TemperatureF[i]
  } else if (weather.density.data$sixhr[i] == 3) {
    weather.density.data$TemperatureF[i] <- weather.density.data$Max.TemperatureF[i] 
  } else {
    weather.density.data$TemperatureF[i] <- weather.density.data$Mean.TemperatureF[i] 
  }
}
# View(weather.density.data[15000:16000,])
# save(weather.density.data, file ="Capstone/weather_density_data_td")
# load("Capstone/weather_density_data_r")


# assign min\max\mean dew point to different period
for (i in 1:nrow(weather.density.data)) {
  if (weather.density.data$sixhr[i] == 1){
    weather.density.data$DewPointF[i] <- weather.density.data$Min.DewpointF[i]
  } else if (weather.density.data$sixhr[i] == 3) {
    weather.density.data$DewPointF[i] <- weather.density.data$Max.Dew.PointF[i] 
  } else {
    weather.density.data$DewPointF[i] <- weather.density.data$MeanDew.PointF[i] 
  }
}

#save(weather.density.data, file = "Capstone/weather_density_td.data")

# drop extra columes
drop <- c("Min.TemperatureF", "Max.TemperatureF", "Mean.TemperatureF", "Min.DewpointF", "Max.Dew.PointF", "MeanDew.PointF",
          "Max.Humidity")
weather.density.data <- weather.density.data[,!names(weather.density.data) %in% drop]
names(weather.density.data)
!names(weather.density.data) %in% drop
