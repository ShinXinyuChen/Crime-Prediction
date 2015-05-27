require(DAAG)
require(MASS)
library(maptools)
library(plyr)
library(ggplot2)
library(car)
library(MASS)
library(sp)
library(ROSE)
library(lubridate)

source("CrimePredictionUtil.r")

# read chicago boundary
city.boundary = read.shapefile("City_20Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")

# read chicago boundary
city.boundary = read.shapefile("City_20Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")

# set up grid (neighborhood) for concatenating tweets 
bb <- bbox(city.boundary) # bbox of city boundary
cs <- c(1000, 1000)  # cell size 1000m *1000m
cc <- bb[, 1] + (cs/2)  # cell offset
cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
grid <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd) # create a grib topology

# convert grid topology to spatial data-frame 
sp_grid <- SpatialGridDataFrame(grid,
                                data=data.frame(id=1:prod(cd)),
                                proj4string=CRS(proj4string(city.boundary)))

# convert xy coordinate of weather-density data as spatial points

weather.density.xy <- weather.density.data[,c("x","y")]
coordinates(weather.density.xy) <- ~ x+y
proj4string(weather.density.xy) <- proj4string(city.boundary)

# assign weather-density to each grid (neighbourhood)
weather.density.grid <- over(weather.density.xy, sp_grid)
names(weather.density.grid) <- "grid_id"
Jan.weather.density.wgrid <- cbind(weather.density.data, weather.density.grid)

# merge weather and density with polarity and trend index
Jan.combined.wpt <- merge(Jan.weather.density.wgrid, Jan.2014.pol.trend.6hour, by = c("sixhr_n", "grid_id"), all.x = TRUE)
str(Jan.combined.wpt)
summary(Jan.combined.wpt)
# prepare training and test data
Jan.train <- Jan.combined.wpt[which(Jan.combined.wpt$mday>=2 & Jan.combined.wpt$mday<=24),]
sum(Jan.train$response==1)
sum(Jan.train$response==0)

summary(Jan.train)
str(Jan.train)

Jan.train$wday <- wday(Jan.train$date)

Jan.train.balanced <- ovun.sample(response~ . -sixhr_n -grid_id -mday -sixhr -x -y -month -date, data=Jan.train,
                                  p=0.5,
                                  seed=3, method="under", na.action = na.omit)$data
summary(Jan.train.balanced)


#Jan.train.balanced$wday <- wday(Jan.train.balanced$date)

glm.fit.1 = glm(response ~  . + cos(2*pi/7*(wday)) + sin(2*pi/7*(wday)) - sixhr_n - grid_id - mday - sixhr - x - y - month - date -
                  Max.Humidity - Min.Humidity - Max.Sea.Level.PressureIn - Min.Sea.Level.PressureIn - wday, family = binomial(), 
                data = Jan.train.balanced)
summary(glm.fit.1)
vif(glm.fit.1)
dim(Jan.train.balanced)
stepAIC(glm.fit.1)
names(Jan.train.balanced)
Jan.train.br <- Jan.train.balanced[,c("response","crime.density","Mean.Humidity","Mean.Sea.Level.PressureIn","Max.Wind.SpeedMPH","Mean.Wind.SpeedMPH","PrecipitationIn","CloudCover","Events","TemperatureF","DewPointF","polarity","trend_3")]
Jan.train.br$response <- as.factor(Jan.train.br$response)
library(randomForest)
rf.res <- tuneRF(Jan.train.br[,-1], Jan.train.br[,1], stepFactor=2, trace = T, plot = T)
rf1 <- randomForest(response ~., data = Jan.train.br, ntree = 150, mtry = 2) 
plot(rf1)

glm.fit.2 <- glm(formula = response ~ crime.density + Mean.Humidity + Mean.Sea.Level.PressureIn 
                 + Max.Wind.SpeedMPH + Events + TemperatureF + 
                   polarity + trend_3, family = binomial(), data = Jan.train)
summary(glm.fit.2)


glm.fit.3 <- glm(formula = response ~ crime.density + polarity +trend_3,  family = binomial(),
                 data = Jan.train.balanced)
summary(glm.fit.3)

glm.fit.4 <- glm(formula = response ~ crime.density + Mean.Humidity + Mean.Sea.Level.PressureIn + 
                   Mean.Wind.SpeedMPH + CloudCover + Events + TemperatureF + 
                   trend_3, family = binomial(), data = Jan.train.balanced)
summary(glm.fit.4)

vif(glm.fit.4)

glm.fit.5 <- glm(formula = response ~ crime.density, 
                 data = Jan.train.balanced)
summary(glm.fit.5)

glm.fit.6 <- glm(formula = response ~ crime.density + Mean.Humidity + Mean.Sea.Level.PressureIn + 
                   Mean.Wind.SpeedMPH + CloudCover + Events + TemperatureF + 
                   trend_3 + sin(2 * pi/7 * (wday)), family = binomial(), data = Jan.train.balanced)

summary(glm.fit.6)
vif(glm.fit.6)



#### lasso regression ####
Jan.train.num <- Jan.train[,c(8,10,11,12,13,14,15,16,17,18,22,23,24)]
normalize<-function(x){return((x-min(x))/(max(x)-min(x)))}
Jan.train.norm <- as.data.frame(lapply(Jan.train.num,normalize))
summary(Jan.train.norm)
Jan.train.norm1 <- cbind(Jan.train$response, Jan.train$Events, Jan.train$wday, Jan.train.norm)
summary(Jan.train.norm1)

Jan.train.balanced.norm <- ovun.sample(response~ ., data=Jan.train.norm1,
                                  p=0.5,
                                  seed=3, method="under", na.action = na.omit)$data

Jan.train.balanced.norm$response <- factor(Jan.train.balanced.norm$response)
library(glmnet)
names(Jan.train.balanced)
glmnet.fit <- glmnet(data.matrix(Jan.train.balanced[,-1], Jan.train.balanced$response, family = "binomial")
summary(glmnet.fit)
plot(glmnet.fit, xvar = "dev", label = TRUE)
plot(glmnet.fi, xvar = "lambda", label = TRUE)
print(glmnet.fit)

coef(glmnet.fit, s = 0.01) 

cvfit = cv.glmnet(data.matrix(Jan.train.balanced[,c(8,10,11,12,13,14,15,16,17,18,19,20,22,23,24,25,26)]), Jan.train.balanced$response, family = "binomial", type.measure = "class")
lamda.1se
plot(cvfit)
coef(cvfit, s = "lambda.min")

#############################
#### Predition #############
#############################
Jan.test <- Jan.combined.wpt[which(Jan.combined.wpt$mday >= 25),]
summary(Jan.test)
Jan.theft <- Jan.test[which(Jan.test$response == 1  & Jan.test$mday >= 25 & Jan.test$sixhr_n <= 123),]

str(Jan.theft)

Jan.test.1 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==25 & Jan.test$sixhr_n == 97),]
summary(Jan.test.1)
Jan.test.2 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==25 & Jan.test$sixhr_n == 98),]
summary(Jan.test.2)
Jan.test.3 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==25 & Jan.test$sixhr_n == 99),]
Jan.test.4 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==25 & Jan.test$sixhr_n == 100),]

Jan.test.5 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==26 & Jan.test$sixhr_n == 101),]
Jan.test.6 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==26 & Jan.test$sixhr_n == 102),]
Jan.test.7 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==26 & Jan.test$sixhr_n == 103),]
Jan.test.8 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==26 & Jan.test$sixhr_n == 104),]

Jan.test.9 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==27 & Jan.test$sixhr_n == 105),]
Jan.test.10 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==27 & Jan.test$sixhr_n == 106),]
Jan.test.11 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==27 & Jan.test$sixhr_n == 107),]
Jan.test.12 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==27 & Jan.test$sixhr_n == 108),]

Jan.test.13 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==28 & Jan.test$sixhr_n == 109),]
Jan.test.14 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==28 & Jan.test$sixhr_n == 110),]
Jan.test.15 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==28 & Jan.test$sixhr_n == 111),]
Jan.test.16 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==28 & Jan.test$sixhr_n == 112),]

Jan.test.17 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==29 & Jan.test$sixhr_n == 113),]
Jan.test.18 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==29 & Jan.test$sixhr_n == 114),]
Jan.test.19 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==29 & Jan.test$sixhr_n == 115),]
Jan.test.20 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==29 & Jan.test$sixhr_n == 116),]

Jan.test.21 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==30 & Jan.test$sixhr_n == 117),]
Jan.test.22 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==30 & Jan.test$sixhr_n == 118),]
Jan.test.23 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==30 & Jan.test$sixhr_n == 119),]
Jan.test.24 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==30 & Jan.test$sixhr_n == 120),]

Jan.test.25 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==31 & Jan.test$sixhr_n == 121),]
Jan.test.26 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==31 & Jan.test$sixhr_n == 122),]
Jan.test.27 <- Jan.test[which(Jan.test$response == 0 & Jan.test$mday==31 & Jan.test$sixhr_n == 123),]
summary(Jan.test.11)


# run prediction
#theft.threats = predict(glm.fit.3, Jan.test[which(Jan.test$sixhr_n <= 123),], type="response")

theft.threats.1 = predict(glm.fit.3, Jan.test.1, type="response")
#summary(theft.threats.4)
theft.threats.2 = predict(glm.fit.3, Jan.test.2, type="response")
theft.threats.3 = predict(glm.fit.3, Jan.test.3, type="response")
theft.threats.4 = predict(glm.fit.3, Jan.test.4, type="response")

theft.threats.5 = predict(glm.fit.3, Jan.test.5, type="response")
theft.threats.6 = predict(glm.fit.3, Jan.test.6, type="response")
theft.threats.7 = predict(glm.fit.3, Jan.test.7, type="response")
theft.threats.8 = predict(glm.fit.3, Jan.test.8, type="response")

theft.threats.9 = predict(glm.fit.3, Jan.test.9, type="response")
theft.threats.10 = predict(glm.fit.3, Jan.test.10, type="response")
theft.threats.11 = predict(glm.fit.3, Jan.test.11, type="response")
theft.threats.12 = predict(glm.fit.3, Jan.test.12, type="response")

theft.threats.13 = predict(glm.fit.3, Jan.test.13, type="response")
theft.threats.14 = predict(glm.fit.3, Jan.test.14, type="response")
theft.threats.15 = predict(glm.fit.3, Jan.test.15, type="response")
theft.threats.16 = predict(glm.fit.3, Jan.test.16, type="response")

theft.threats.17 = predict(glm.fit.3, Jan.test.17, type="response")
theft.threats.18 = predict(glm.fit.3, Jan.test.18, type="response")
theft.threats.19 = predict(glm.fit.3, Jan.test.19, type="response")
theft.threats.20 = predict(glm.fit.3, Jan.test.20, type="response")

theft.threats.21 = predict(glm.fit.3, Jan.test.21, type="response")
theft.threats.22 = predict(glm.fit.3, Jan.test.22, type="response")
theft.threats.23 = predict(glm.fit.3, Jan.test.23, type="response")
theft.threats.24 = predict(glm.fit.3, Jan.test.24, type="response")

theft.threats.25 = predict(glm.fit.3, Jan.test.25, type="response")
theft.threats.26 = predict(glm.fit.3, Jan.test.26, type="response")
theft.threats.27 = predict(glm.fit.3, Jan.test.27, type="response")

theft.threat <- theft.threats.1+theft.threats.2 +theft.threats.3 +theft.threats.4 +
  theft.threats.5+theft.threats.6 +theft.threats.7 +theft.threats.8 +
  theft.threats.9+theft.threats.10 +theft.threats.11 + theft.threats.12 +
  theft.threats.13+theft.threats.14 +theft.threats.15 + theft.threats.16 +
  theft.threats.17+theft.threats.18 +theft.threats.19 + theft.threats.20 +
  theft.threats.21+theft.threats.22 +theft.threats.23 + theft.threats.24 +
  theft.threats.25+theft.threats.26 +theft.threats.27

summary(theft.threat)
# build prediction dataframe
theft.prediction = data.frame(Jan.test[1:14910,]$x, Jan.test[1:14910,]$y, theft.threat)
names(theft.prediction) = c("x", "y", "threat")
summary(theft.prediction)

# evaluate prediction on march crime records
plot.surveillance.curve(theft.prediction, Jan.theft[,c("x","y")], 200, boundary = city.boundary)

###### Benchmark ###########
summary(theft.threat.1)
# run prediction
theft.threats.1.1 = predict(glm.fit.5, Jan.test.1, type="response")
theft.threats.2.1 = predict(glm.fit.5, Jan.test.2, type="response")
theft.threats.3.1 = predict(glm.fit.5, Jan.test.3, type="response")
theft.threats.4.1 = predict(glm.fit.5, Jan.test.4, type="response")

theft.threats.5.1 = predict(glm.fit.5, Jan.test.5, type="response")
theft.threats.6.1 = predict(glm.fit.5, Jan.test.6, type="response")
theft.threats.7.1 = predict(glm.fit.5, Jan.test.7, type="response")
theft.threats.8.1 = predict(glm.fit.5, Jan.test.8, type="response")

theft.threats.9.1 = predict(glm.fit.5, Jan.test.9, type="response")
theft.threats.10.1 = predict(glm.fit.5, Jan.test.10, type="response")
theft.threats.11.1 = predict(glm.fit.5, Jan.test.11, type="response")
theft.threats.12.1 = predict(glm.fit.5, Jan.test.12, type="response")

theft.threats.13.1 = predict(glm.fit.5, Jan.test.13, type="response")
theft.threats.14.1 = predict(glm.fit.5, Jan.test.14, type="response")
theft.threats.15.1 = predict(glm.fit.5, Jan.test.15, type="response")
theft.threats.16.1 = predict(glm.fit.5, Jan.test.16, type="response")

theft.threats.17.1 = predict(glm.fit.5, Jan.test.17, type="response")
theft.threats.18.1 = predict(glm.fit.5, Jan.test.18, type="response")
theft.threats.19.1 = predict(glm.fit.5, Jan.test.19, type="response")
theft.threats.20.1 = predict(glm.fit.5, Jan.test.20, type="response")

theft.threats.21.1 = predict(glm.fit.5, Jan.test.21, type="response")
theft.threats.22.1 = predict(glm.fit.5, Jan.test.22, type="response")
theft.threats.23.1 = predict(glm.fit.5, Jan.test.23, type="response")
theft.threats.24.1 = predict(glm.fit.5, Jan.test.24, type="response")

theft.threats.25.1 = predict(glm.fit.5, Jan.test.25, type="response")
theft.threats.26.1 = predict(glm.fit.5, Jan.test.26, type="response")
theft.threats.27.1 = predict(glm.fit.5, Jan.test.27, type="response")

theft.threat.t1 <- theft.threats.1.1 + theft.threats.2.1 +theft.threats.3.1 +theft.threats.4.1 +
  theft.threats.5.1+theft.threats.6.1 +theft.threats.7.1 +theft.threats.8.1 +
  theft.threats.9.1+theft.threats.10.1 +theft.threats.11.1 + theft.threats.12.1 +
  theft.threats.13.1+theft.threats.14.1 +theft.threats.15.1 + theft.threats.16.1 +
  theft.threats.17.1+theft.threats.18.1 +theft.threats.19.1 + theft.threats.20.1 +
  theft.threats.21.1+theft.threats.22.1 +theft.threats.23.1 + theft.threats.24.1 +
  theft.threats.25.1+theft.threats.26.1 +theft.threats.27.1


# build prediction dataframe
theft.prediction = data.frame(Jan.test[1:14910,]$x, Jan.test[1:14910,]$y, theft.threat.t1)
names(theft.prediction) = c("x", "y", "threat")
summary(theft.prediction)

# evaluate prediction on march crime records
#plot.surveillance.curve(theft.prediction, Jan.theft.xyz, 200, boundary = city.boundary, add = TRUE)

plot.surveillance.curve(theft.prediction, Jan.theft, 200, boundary = city.boundary, add = TRUE)
plot.surveillance.curve(theft.prediction,
                        Jan.test[which(Jan.test$response == 1 & Jan.test$sixhr_n %in% 98:104),],200, boundary = city.boundary)
###### Predict rf ################
# run prediction
#theft.threats = predict(glm.fit.3, Jan.test[which(Jan.test$sixhr_n <= 123),], type="response")

theft.threats.1 = predict(rf1, Jan.test.1, type="prob")
#summary(theft.threats.4)
theft.threats.2 = predict(rf1, Jan.test.2, type="prob")
theft.threats.3 = predict(rf1, Jan.test.3, type="prob")
theft.threats.4 = predict(rf1, Jan.test.4, type="prob")

theft.threats.5 = predict(rf1, Jan.test.5, type="prob")
theft.threats.6 = predict(rf1, Jan.test.6, type="prob")
theft.threats.7 = predict(rf1, Jan.test.7, type="prob")
theft.threats.8 = predict(rf1, Jan.test.8, type="prob")

theft.threats.9 = predict(rf1, Jan.test.9, type="prob")
theft.threats.10 = predict(rf1, Jan.test.10, type="prob")
theft.threats.11 = predict(rf1, Jan.test.11, type="prob")
theft.threats.12 = predict(rf1, Jan.test.12, type="prob")

theft.threats.13 = predict(rf1, Jan.test.13, type="prob")
theft.threats.14 = predict(rf1, Jan.test.14, type="prob")
theft.threats.15 = predict(rf1, Jan.test.15, type="prob")
theft.threats.16 = predict(rf1, Jan.test.16, type="prob")

theft.threats.17 = predict(rf1, Jan.test.17, type="prob")
theft.threats.18 = predict(rf1, Jan.test.18, type="prob")
theft.threats.19 = predict(rf1, Jan.test.19, type="prob")
theft.threats.20 = predict(rf1, Jan.test.20, type="prob")

theft.threats.21 = predict(rf1, Jan.test.21, type="prob")
theft.threats.22 = predict(rf1, Jan.test.22, type="prob")
theft.threats.23 = predict(rf1, Jan.test.23, type="prob")
theft.threats.24 = predict(rf1, Jan.test.24, type="prob")

theft.threats.25 = predict(rf1, Jan.test.25, type="prob")
theft.threats.26 = predict(rf1, Jan.test.26, type="prob")
theft.threats.27 = predict(rf1, Jan.test.27, type="prob")

theft.threats.rf <- theft.threats.1 + theft.threats.2 +theft.threats.3 +theft.threats.4 +
  theft.threats.5+theft.threats.6 +theft.threats.7 +theft.threats.8 #+
  theft.threats.9+theft.threats.10 +theft.threats.11 + theft.threats.12 +
  theft.threats.13+theft.threats.14 +theft.threats.15 + theft.threats.16 +
  theft.threats.17+theft.threats.18 +theft.threats.19 + theft.threats.20 +
  theft.threats.21+theft.threats.22 +theft.threats.23 + theft.threats.24 +
  theft.threats.25+theft.threats.26 +theft.threats.27


summary(theft.threats.rf)

# build prediction dataframe
theft.prediction = data.frame(Jan.test[1:14910,]$x, Jan.test[1:14910,]$y, theft.threats.rf[,2])
names(theft.prediction) = c("x", "y", "threat")
summary(theft.prediction)


# evaluate prediction on march crime records
plot.surveillance.curve(theft.prediction,
                        Jan.test[which(Jan.test$response == 1 & Jan.test$sixhr_n %in% 98:104),],200, boundary = city.boundary)
Jan.test[which(Jan.test$response == 1 & Jan.test$sixhr_n == 98),]

plot.surveillance.curve(theft.prediction, Jan.theft, 200, boundary = city.boundary, add = TRUE)
# evaluate prediction on march crime records
plot.surveillance.curve(theft.prediction, Jan.theft.xyz, 200, boundary = city.boundary, add = TRUE)

##### Benchmark ################
# build prediction dataframe
theft.prediction = data.frame(Jan.test[1:14910,]$x, Jan.test[1:14910,]$y, theft.threats.1.1)
names(theft.prediction) = c("x", "y", "threat")
summary(theft.prediction)

# evaluate prediction on march crime records
plot.surveillance.curve(theft.prediction, Jan.test[which(Jan.test$response == 1 & Jan.test$mday==25 & Jan.test$sixhr_n == 98),],
                        200, boundary = city.boundary, add = TRUE)


theft.threats.27 = predict(rf1, Jan.test.27, type="prob")
