library(maptools)
library(plyr)
library(ggplot2)
library(car)
library(MASS)
library(sp)

source(CrimePredictionUtil)

#import crime data
theft.2014.jan.to.feb = sample.crime("2014_THEFT.csv", -1, 1, 2)
str(theft.2014.jan.to.feb)
theft.2013.dec = sample.crime("2013_THEFT.csv", -1, 12, 12)
str(theft.2013.dec)
theft.combined <- rbind(theft.2013.dec,theft.2014.jan.to.feb)
summary(theft.combined)

theft.2014.jan = theft.2014.jan.to.feb[theft.2014.jan.to.feb$month == 1,]
theft.2014.jan$sixhr <- cut(theft.2014.jan$timestamp, "6 hours")
str(theft.2014.jan$sixhr)
# read chicago boundary
city.boundary = read.shapefile("City_20Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")
# set prediction resolution
prediction.resolution.meters = 200

##### train model on responses from Jan 25-31th data, using predictors from Dec 24 - Jan 24. #####

# get negative observations within chicago
non.crime.points = cbind(0, get.grid.points(city.boundary, prediction.resolution.meters, TRUE))
head(non.crime.points)
names(non.crime.points)[1] = "response"

# get positive observations from february within chicago
training.crime.points = cbind(1, theft.feb[,c("x","y")])
names(training.crime.points)[1] = "response"

# combine positive and negative points
training.data = rbind(non.crime.points, training.crime.points)

# calculate crime density for each training observation based on january records
theft.density = run.spatial.kde(theft.jan[,c("x","y")], training.data[,c("x","y")], 1000)
