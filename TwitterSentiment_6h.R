library(maptools)
library(plyr)
library(ggplot2)
library(car)
library(MASS)
library(sp)
source("twitterUtil.r")

####################################
## Twitter data prerprocessing #####
####################################

# build the link to my PostgreSQL database
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host = 'localhost', port='5432', 
                 dbname = 'postgis_tweet',user = 'postgres', 
                 password='cxy5180112')

# draw query from PostgresSQL database
# get tweets from "2014-01-01 00:00:00" to "2014-01-31 11:59:59"
Jan.2014 <- tweet.qry("2014-01-01 00:00:00", "2014-01-31 11:59:59")

# read chicago boundary
city.boundary = read.shapefile("City_20Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")

# set up grid (neighborhood) for concatenating tweets 
bb <- bbox(city.boundary) # bbox of city boundary
cs <- c(1000, 1000)  # cell size 1000m *1000m
cc <- bb[, 1] + (cs/2)  # cell offset
cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
grid <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd) # create a grib topology

# conver grid topology to spatial data frame
sp_grid <- SpatialGridDataFrame(grid,
                                data=data.frame(id=1:prod(cd)),
                                proj4string=CRS(proj4string(city.boundary)))
#str(sp_grid)
#summary(sp_grid)
#plot(city.boundary, xlim=c(332777, 367345), ylim=c(552875, 594870))
#plot(sp_grid, add =TRUE)

# convert xy coordinate of tweets as spatial points 
Jan.2014.xy <- Jan.2014[8:9]
coordinates(Jan.2014.xy) <- ~ st_x+st_y
proj4string(Jan.2014.xy) <- proj4string(city.boundary)
points(Jan.2014.xy, pch =".")

# assign tweets to each grid (neighbourhood)
Jan.tweet.grid <- over(Jan.2014.xy, sp_grid)
names(Jan.tweet.grid) <- "grid_id"
Jan.2014.grid <- cbind(Jan.2014, Jan.tweet.grid)

# convert date type
Jan.2014.grid$dates <- as.POSIXct(Jan.2014.grid$created_at)

# splite time period into every 6-hour
dates.combine.tweets.crime <- c(Jan.2014.grid$dates,
                                theft.2014.jan.to.feb[which(theft.2014.jan.to.feb$month==1),]$timestamp)
factor.combine <- cut(dates.combine.tweets.crime, "6 hours")
length(factor.combine)
Jan.2014.grid$sixhr <- factor.combine[1:length(Jan.2014.grid$dates)]
# levels(Jan.2014.grid$sixhr)[1:10]
Jan.2014.grid$sixhr_n <- as.numeric(Jan.2014.grid$sixhr)
Jan.2014.grid$dates <- NULL

#names(Jan.test.grid$created_at)
#class(as.POSIXlt(Jan.test.grid$created_at[1:5]))

# concatenate tweets with same grid_id and 6-hour period together
Jan.2014.paste <- ddply(Jan.2014.grid, c("sixhr_n", "grid_id"), summarise,
                        text_p=paste(text, collapse=""))

# clean-up twitter data using twitter.clean function 
Jan.2014.paste.c <- twitter.clean(Jan.2014.paste, Jan.2014.paste$text_p)
row.to.keep <- !is.na(Jan.2014.paste.c$grid_id)
Jan.2014.paste.c <- Jan.2014.paste.c[row.to.keep,]

#summary(Jan.2014.paste.c)
#save("Jan.2014.paste.c", file = "Capstone/Jan_2014_paste_c_6hr.Rdata")
#load("Capstone/Jan_2014_paste_c_6hr.Rdata")

###################################
### calculate snetiment score #####
###################################

Jan.2014.pol.6h <- NULL
#options(warn=0, error = recover)

# load polarity file created by Lexicon.R
load("POLKEY.RData")

# calculate sentiment score 
system.time(
  Jan.2014.pol.6h<- polarity(Jan.2014.paste.c$text1, grouping.var = NULL, polarity.frame = POLKEY, 
                          constrain = TRUE, negators = qdapDictionaries::negation.words,
                          amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, 
                          question.weight = 0, amplifier.weight = .3, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
)

#    user  system elapsed 
# 5777.63    1.81 5841.16
# save("Jan.2014.pol.6h",file = "Jan_2014_pol_1000m_6h.Rdata")
# load("Jan_2014_pol_1000m_6h.Rdata")

# str(Jan.2014.pol.6h) 
# Jan.2013.paste.c$text1[1]

# Jan.2014.paste.c$pol <- Jan.2014.pol$all$polarity 
# test <- ddply(Jan.2014.paste.c, c("sixhr"), summarise,
#               difference = diff(pol,2))
# 
# Jan.2014.combined <- cbind(Jan.2014.paste.c[63:33498,], test[,2])
# test2 <- Jan.2014.combined[which(Jan.2014.combined$grid_id == 587),]
# head(test2)
# plot(test2[,6]/10)

# calculate mean from raw score
Jan.2014.pol.6h$mean <- mean(Jan.2014.pol.6h$all$polarity)

# center the data by subtracting $sum from $mean
Jan.2014.pol.6h$all$centered <- Jan.2014.pol.6h$all$polarity - Jan.2014.pol.6h$mean

# plot sentiment score without centering
qplot(Jan.2014.pol.6h$all$polarity, main = "Sentiment Histogram", xlab = "Score", ylab = "Frequency", binwidth = 0.015)

# plot centered sentiment score
qplot(pol.bb$all$centered, main = "Centered Sentiment Histogram", xlab = "Score", ylab = "Frequency", binwidth = 0.075)

# insert day of month and grid_id into large polarity 
Jan.2014.pol.6h$all$sixhr_n <- Jan.2014.paste.c$sixhr_n + 1 #shift a 6-hour period
Jan.2014.pol.6h$all$grid_id <- Jan.2014.paste.c$grid_id
# summary(Jan.2014.pol.6h$all$grid_id)

# create data.frame which contains 6-hour period, polarity and grid_id
Jan.2014.pol.6h.data <- data.frame()
Jan.2014.pol.6h.data <- data.frame(Jan.2014.pol.6h$all$sixhr, Jan.2014.pol.6h$all$grid_id, Jan.2014.pol.6h$all$polarity)
names(Jan.2014.pol.6h.data) <- c("sixhr_n", "grid_id", "polarity")

# inset missing row from ddply

vals <- expand.grid(sixhr_n = 2:123,grid_id = 1:max(Jan.2014.pol.6h.data$grid_id, na.rm = TRUE))
head(vals)
summary(vals)
Jan.2014.pol.6h.data.m <- merge(vals, Jan.2014.pol.6h.data, all.x =TRUE)
#summary(Jan.2014.pol.6h.data.m)
# impute 0 to those missing polarity
Jan.2014.pol.6h.data.m[is.na(Jan.2014.pol.6h.data.m$polarity),"polarity"] <- 0

# calculate trend index for all grid area
Jan.2014.pol.trend.6hour <- data.frame()
system.time(
  for (i in 1:max(Jan.2014.pol.6h.data$grid_id, na.rm = TRUE)){
    sub <- subset(Jan.2014.pol.6h.data.m, grid_id == i)
    sub$trend_3 <- trend.idx(sub$polarity,3,0.1)
    Jan.2014.pol.trend.6hour <- rbind(Jan.2014.pol.trend.6hour,sub)
  }
)

str(Jan.2014.pol.trend.6hour)
summary(Jan.2014.pol.trend.6hour)
#save(Jan.2014.pol.trend.6hour, file = "Capstone/allsub_6hr.Rdata")
#View(Jan.2014.pol.trend.6hour)

# visualize sentiment score and its trend in neighbourhood 587 (downtown

sub.587 <- subset(Jan.2014.pol.6h$all, grid_id == 587)[,c(3,7)]

# bb.scatter.587

bb.scatter.587 <- ggplot(sub.587, aes(x = sub.587$sixhr_n, y = sub.587$polarity))
bb.scatter.587 <- bb.scatter.587 + geom_point() + geom_line() + ylim(-1, 1)
bb.scatter.587 <- bb.scatter.587 + xlab("Period") + ylab("Sentiment") + ggtitle("Neighborhood 587")
bb.scatter.587

# calsulate trend index
sub.587$trend_2 <- trend.idx(sub.587$polarity,2,0.1)
sub.587$trend_3 <- trend.idx(sub.587$polarity,3,0.1)

# plot trend index for each 12-hour
t_2.scatter.587 <- ggplot(sub.587, aes(x = sub.587$mday, y = sub.587$trend_2))
t_2.scatter.587 <- t_2.scatter.587 + geom_point() + geom_line()
t_2.scatter.587 <- t_2.scatter.587 + xlab("Date") + ylab("trend_2") + ggtitle("587")
t_2.scatter.587

# plot trend index for each 18-hour
t_3.scatter.587 <- ggplot(sub.587, aes(x = sub.587$sixhr_n, y = sub.587$trend_3))
t_3.scatter.587 <- t_3.scatter.587 + geom_point() + geom_line()
t_3.scatter.587 <- t_3.scatter.587 + xlab("Period") + ylab("Trend_3") + ggtitle("Neighborhood 587")
t_3.scatter.587

# use multiplot function to plot both trend index
multiplot(bb.scatter.587, t_3.scatter.587)
