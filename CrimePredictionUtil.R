require(RPostgreSQL)
library(qdap)
library(rgdal)
library(maptools)
library(lubridate)
library(quantmod)

#' querying tweets within chicago city boundary from certain time period. Reture numeric xy coorninates
#' @param from time start from YYYY-MM-DD hh:mm:ss
#' @parm to time end at YYYY-MM-DD hh:mm:ss
tweet.qry <- function(from, to){
  out <- dbGetQuery(con, sprintf("SELECT created_at, id, place_id, reply_to_status_id, reply_to_user_id, text, user_id, ST_X(ST_Transform(coordinates, 26971)), ST_Y(ST_Transform(coordinates, 26971))  
                               FROM tweets_with_gps_coordinates
                               WHERE created_at BETWEEN '%s' AND '%s'
                               AND coordinates && ST_MakeEnvelope(-88, 41.6, -87.5, 42.5, 4326);",from, to))
  return(out)
}

#' Reads an ESRI shapefile.
#' @param path Filesystem path (relative or absolute) to shapefile.
#' @param type Type of shapefile:  "poly" or "points"
#' @param sourceProj Source projection of shapefile in proj4 format (e.g., "+init=epsg:3435").
#' @param targetProj Target projection of shapefile in proj4 format (e.g., "+init=epsg:26971").
#' @return A \code{SpatialPolygonDataFrame} projected as desired.
read.shapefile = function(path, type, sourceProj, targetProj)
{
  if(type == "poly")
    shapefile = readShapePoly(path)
  else if(type == "points")
    shapefile = readShapePoints(path)
  else if(type == "lines")
    shapefile = readShapeLines(path)
  else
    message(paste("Unrecognized shapefile type:", type))
  
  # reproject shapefile
  proj4string(shapefile) = sourceProj
  shapefile = spTransform(shapefile, CRS(targetProj))
  
  return (shapefile)
}

#' Gets grid of points covering the bounding box of a shapefile.
#' @param shapefile Shapefile to analyze.
#' @param resolution.meters Resolution of grid in meters.
#' @param filter Whether or not to filter out grid points residing outside the given shapefile.
#' @return An n-by-2 data frame of evenly spaced points points.
get.grid.edge = function(shapefile, resolution.meters, filter=TRUE)
{
  shapefile.range = get.shapefile.range(shapefile)
  x.values = seq(shapefile.range[1]-resolution.meters/2, shapefile.range[2]+resolution.meters/2, resolution.meters)
  y.values = seq(shapefile.range[3]-resolution.meters/2, shapefile.range[4]+resolution.meters/2, resolution.meters)
  grid.edge = expand.grid(x.values, y.values)
  names(grid.edge) = c("x", "y")
  
  if(filter)
  {
    grid.edge = filter.points.to.boundary(grid.edge, proj4string(shapefile), shapefile)
  }
  
  return (grid.edge)
}

#' Filters out points that are not within a boundary.
#' @param points Matrix (n-by-2) of points in the projection given by \code{pointsProj4String}.
#' @param pointsProj4String Projection string for points.
#' @param boundary Shapefile boundary to use.
#' @return Filtered version of \code{points}.
filter.points.to.boundary = function(points, pointsProj4String, boundary)
{
  sp.points = SpatialPoints(points, proj4string=CRS(pointsProj4String))
  points.in.boundary = !is.na(over(sp.points, boundary)$OBJECTID)
  points = points[points.in.boundary,]
  return (points)
}
# re-formate the dataframe up by change projection, removing NA, adding hour, day of week, month, day of month, day of year
# reformat.twitter.doc <- function(doc, targetProj, ){ 
# out.2013Jan.xy.m = project(data.matrix(doc[8:9]), proj="+init=epsg:26971")
# 
# out.2013Jan.m <- cbind(out.2013Jan, out.2013Jan.xy.m)
# names(out.2013Jan.m)[10] = "x"
# names(out.2013Jan.m)[11] = "y"
# out.2013Jan.m$xgrid<-cut(out.2013Jan.m$x,breaks= unique(kde.est.points$x),include.lowest=T)
# out.2013Jan.m$ygrid<-cut(out.2013Jan.m$y,breaks= unique(kde.est.points$y),include.lowest=T)
# out.2013Jan.m$IDNgrid<-factor(out.2013Jan.m$IDgrid)
# levels(out.2013Jan.m$IDNgrid)<-seq_along(levels(out.2013Jan.m$IDNgrid))
# out.2013Jan.r <- na.omit(out.2013Jan.m)
# }



#' clean up twitter post by replacing emotican, removing punctuation, numbers, and swtching to lower cases
#' @param doc documents which contains twitter posts
twitter.clean <- function(doc, text.col){
  doc$text1 <- NA
  doc$text1 <- mgsub(as.character(emoticon[[2]]), as.character(emoticon[[1]]), text.col)
  #emoticon[[1]]
  # clear punctuation 
  doc$text1<- gsub('[[:punct:]]', ' ', doc$text1)
  # take control charactor out, need or not? 
  #out.2013Mar13pm.dt$text1<- gsub('[[:cntrl:]]', ' ', out.2013Mar13pm.dt$text1)
  doc$text1<- gsub('\\d+', ' ', doc$text1)
  doc$text1<- tolower(doc$text1)
  return(doc)
}

#' Gets a crime sample from a Socrata CSV file.
#' @param crime.csv.path Path to Socrata CSV file.
#' @param sample.size Size of sample. If -1 or larger than the number of actual records, all records will be returned.
#' @param start.month Starting month of sample (1-12).
#' @param end.month Ending month of sample (1-12).
#' @return Data frame containing crime sample with columns for \code{x}-location, \code{y}-location, \code{timestamp}, crime \code{hour}, crime \code{day.of.week}, and crime \code{month}.
sample.crime = function(crime.csv.path, sample.size, start.month = 1, end.month = 12)
{
  # read crime data and take sample
  crimes = read.csv(crime.csv.path, header=TRUE)
  
  # reproject lon/lat points to meters
  crimes.locations.lonlat = cbind(crimes$Longitude, crimes$Latitude)
  crimes.locations.meters = project(crimes.locations.lonlat, proj="+init=epsg:26971")
  
  # convert strings to dates
  crimes.dates = as.data.frame(strptime(crimes[,"Date"],"%m/%d/%Y %I:%M:%S %p"))
  
  # reassemble more friendly crimes.sample matrix
  crimes = as.data.frame(cbind(crimes.locations.meters[,1],  # x value
                               crimes.locations.meters[,2],  # y value
                               crimes.dates,                 # crime date/time
                               hour(crimes.dates[[1]]),      # crime hour
                               wday(crimes.dates[[1]]),      # crime day of week
                               month(crimes.dates[[1]]),      # crime month
                               mday(crimes.dates[[1]])))     # crime day of month
  
  # set column names for convenience
  names(crimes) = c("x","y","timestamp","hour","day.of.week","month", "mday")
  
  # remove NA values
  crimes = crimes[!(is.na(crimes$x) | is.na(crimes$y) | is.na(crimes$timestamp)),]
  
  # filter by month
  crimes = crimes[crimes$month >= start.month & crimes$month <= end.month,]
  
  # filter by size
  if(sample.size == -1 | sample.size > nrow(crimes))
    sample.size = nrow(crimes)
  
  crimes.sample.rows = sample(nrow(crimes), size=sample.size)
  crimes.sample = crimes[crimes.sample.rows,]
  
  return (crimes.sample)
}

#' filter non-crime points with crime points nearby
#' @param grid.points  Grid of points covering the bounding box of a shapefile.
#' @param filter.square.size Set up filter size for n*n squre around grid points
#' @return filter.polys Spacial Polygon DataFrame for squres surrounding grid points
filter = function(grid.points, filter.square.size){
  square <-  as.data.frame(c(grid.points, grid.points + c(0, filter.square.size),
                             grid.points + filter.square.size, grid.points + c(filter.square.size, 0), grid.points))
  head(square)
  square = as.matrix(square)
  
  square
  ID <- paste0('sq', seq_len(nrow(square)))
  
  # Create SP
  polys <- SpatialPolygons(mapply(function(poly, id) {
    xy <- matrix(poly, ncol=2, byrow=TRUE)
    Polygons(list(Polygon(xy)), ID=id)
  }, split(square, row(square)), ID))
  
  # Create SPDF
  filter.polys <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
  return(filter.polys)
}

#' Calculate trend index for polarity
#' @param polarity the polarity score dataset or matrix
#' @param n.days evaluate previews n days' trend
#' @target target increase or decrease
#' @return x trend index 
trend.idx <- function(polarity, n.days = 5, target) {
  r <- matrix(NA, ncol = n.days, nrow = length(polarity))
  for(x in 1:n.days) {r[,x] <- Delt(polarity+1, k= x)}
  x <- apply(r, 1, function(x) {sum(x[x > target | x < target])})
  x
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Plots a surveillance curve for a threat prediction and set of actual crime locations.
#' @param threat.prediction Dataframe with columns \code{x}, \code{y}, and \code{threat}, which give the x-y location of a prediction and its real-valued threat prediction.
#' @param eval.crime.points Dataframe with columns \code{x} and \code{y}, which give the location of actual crimes that occurred during the evaluation period.
#' @param prediction.resolution.meters Resolution of prediction points in meters.
#' @param boundary Spatial boundary of evaluation area (shapefile).
#' @param add Whether or not to add the plot to the current plot.
plot.surveillance.curve = function(threat.prediction, eval.crime.points, prediction.resolution.meters, boundary, add=FALSE)
{
  surv.plot.points = get.surveillance.plot.points(threat.prediction, eval.crime.points, prediction.resolution.meters, boundary)
  
  if(add)
  {
    lines(surv.plot.points, lty=2, col="blue")
  }
  else
  {
    plot(surv.plot.points, type="l", xlab="% Area Surveilled", ylab = "% Crime Captured")
    
    # show random guess
    abline(0,1,lty="dashed")
  }   
  
  # add AUC 
  x = surv.plot.points[,1]
  y = surv.plot.points[,2]
  idx = order(x)
  auc = round(sum(diff(x[idx])*rollmean(y[idx],2)),digits=2)
  
  auc.x.location = 0.6
  auc.y.location = 0.4
  if(add)
  {
    auc.y.location = 0.3
  }
  
  text(auc.x.location, auc.y.location, paste("AUC=",auc,sep=""))
}

#' Gets the plot points for a surveillance curve for a threat prediction and set of actual crime locations.
#' @param threat.prediction Dataframe with columns \code{x}, \code{y}, and \code{threat}, which give the x-y location of a prediction and its real-valued threat prediction.
#' @param eval.crime.points Dataframe with columns \code{x} and \code{y}, which give the location of actual crimes that occurred during the evaluation period.
#' @param prediction.resolution.meters Resolution of prediction points in meters.
#' @param boundary Spatial boundary of evaluation area (shapefile).
#' @return Plot points for surveillance curve.
get.surveillance.plot.points = function (threat.prediction, eval.crime.points, prediction.resolution.meters, boundary)
{
  # filter out prediction points that are outside of the boundary
  sp.points = SpatialPoints(threat.prediction[,c("x", "y")], proj4string=boundary@proj4string)
  points.in.boundary = !is.na(over(sp.points, boundary)$OBJECTID)
  threat.prediction = threat.prediction[points.in.boundary,]
  
  # filter out eval points that are outside of the boundary
  sp.points = SpatialPoints(eval.crime.points[,c("x", "y")], proj4string=boundary@proj4string)
  points.in.boundary = !is.na(over(sp.points, boundary)$OBJECTID)
  eval.crime.points = eval.crime.points[points.in.boundary,]
  
  # sort prediction rows by threat level
  threat.prediction = threat.prediction[order(threat.prediction$threat, decreasing=TRUE),]
  
  # augment prediction with columns for IDs, prediction squares, and captured crime counts
  threat.prediction = as.data.frame(cbind(seq(1,nrow(threat.prediction)),
                                          threat.prediction$threat,
                                          threat.prediction$x - prediction.resolution.meters / 2,
                                          threat.prediction$y - prediction.resolution.meters / 2,
                                          threat.prediction$x + prediction.resolution.meters / 2,
                                          threat.prediction$y + prediction.resolution.meters / 2,
                                          rep(0,nrow(threat.prediction))))
  
  names(threat.prediction) = c("id", "threat", "llx","lly","urx","ury", "captured.crimes")
  
  # get the square for each actual crime point
  eval.crime.points.squares = apply(eval.crime.points, 1, square.index.of.crime, threat.prediction)
  
  # get the number of actual crimes that fell into each square
  for(square in eval.crime.points.squares)
  {
    if(!is.na(square))
    {
      threat.prediction$captured.crimes[square] = threat.prediction$captured.crimes[square] + 1
    }
  }  
  
  threat.prediction$captured.crimes = cumsum(threat.prediction$captured.crimes)
  
  plot.x.points = threat.prediction$id / nrow(threat.prediction)
  plot.y.points = threat.prediction$captured.crimes / nrow(eval.crime.points)
  
  return (cbind(plot.x.points, plot.y.points))
}
