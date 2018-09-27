# set up ------------------------------------------------------------------

# Some additional functions are needed:

library(RCurl)

source('https://www.dropbox.com/s/t4bxf2olztv8alx/packages_and_setup.R?dl=1')

library(raster) # Package for working with raster files
library(adehabitatHR)  
library(maptools)
library(maps)
library(rgdal)
library(tidyr)

# Get point locations:

locs <- 
  read.csv(
    "https://www.dropbox.com/s/uxrbfxwi8l7k9lx/RSHomeRangeData.csv?dl=1", 
    header = T, 
    na.strings=c("NA", "NULL", "", "."))

# Take a quick look at how many location estimates we have for each of three birds:  

table(locs$bird)

# Extract the coordinates from the dataframe:  

xyt <-
  subset(
    locs,
    select = c(X,Y))

# Extract the birds IDs from the dataframe:  

id <-
  subset(
    locs, select = bird)

# Create the SpatialPointsDataFrame:   
  
locs1 <- 
  id

coordinates(locs1) <- 
  xyt

# Assign coordinate reference system

proj4string(locs1) <-
  CRS("+proj=utm +zone=20 +datum=WGS84")

# Confirm the class of the object and check summary information:

class(locs1) 

summary(locs1)

# Let's plot the point data:

plot(
  locs1, 
  col=as.data.frame(locs1)[,1]) #specifies unique color for each bird

# Create new dataframe for reprojected data

locs1Map <-
  locs1

locs1Map <-
  spTransform(
    locs1,
    CRS("+proj=longlat +datum=WGS84"))

# Map of Puerto Rico:

pR <- 
  map(
    "world",
    "Puerto Rico",
    fill=T)

IDs <- 
  sapply(strsplit(pR$names, ":"), function(x) x[1])

pRP <- 
  map2SpatialPolygons(
    pR,
    IDs = IDs,
    proj4string = CRS("+proj=longlat +datum=WGS84"))

# Plot polygon of Puerto Rico and locations together:

plot(pRP)

points(locs1Map, col = as.data.frame(locs1)[, 1])#points overlays the location estimates

# minimum convex polygon -------------------------------------------------

# Run the 95% minimum convex polygon analysis to create all three home range as a SpatialPolygonDataFrame:  

cp <-
  mcp(locs1[, 1], percent = 95)

# Graph the polygons and the points together:  

plot(
  cp, 
  border=as.data.frame(cp)[,1])

plot(
  locs1, 
  col=as.data.frame(locs1)[,1],
  add=T)

# The area of the three polygons will be listed in ha.  

cp

# The polygons can be written to a shapefile in the source directory so that in can be read into a GIS or GoogleEarth.

writeOGR(
  cp,
  dsn="cp.shp",
  layer="cp",
  driver="ESRI Shapefile")

# Or can be exported as a dataframe.

write.csv(
  as.data.frame(cp),
  "MCP.csv")

# kernel density estimation -----------------------------------------------

kud <- 
  kernelUD(locs1[, 1], h = "LSCV")

kud

image(kud)

# It is important to check that the cross-validation criterion converges towards a solution in the specified interval, if not the estimate should not be used. This can be visualized in the plots below.  

plotLSCV(kud)

# To calculate the home range size we can first convert the kernel density function into a SpatialPolygonsDataFrame:

homerange <- 
  getverticeshr(kud)

class(homerange)

# We can visualize the polygon:

plot(
  homerange,
  col=1:3) 

# Calculate kde for home range and core areas:

kde.areas <-
  kernel.area(
    kud, 
    percent=c(50,95))

kde.areas

# Plot isopleths:

vud <-
  getvolumeUD(kud) 

# Visualize 2nd bird:

image(vud[[2]])

xyzv <-
  as.image.SpatialGridDataFrame(vud[[2]])

contour(
  xyzv,
  add=TRUE)

# Calculate kde using href:

kud1 <- 
  kernelUD(
    locs1[,1],
    h="href")

# We will convert the 95 KDE as vectors  

homerange1 <- 
  getverticeshr(
    kud1,
    percent = 95)

# Plot the vector and the points  

plot(
  homerange1,
  border=1:3, 
  lwd=6) 

plot(
  locs1,
  col=as.data.frame(locs1)[,1],
  add=T)

# Again we can export the shapefiles for the 95% KDE "href" version of the home ranges:

writeOGR(
  homerange1, 
  dsn="95KdeHref.shp",
  layer="95kde",
  driver="ESRI Shapefile")

# As well as the 50 KDE, the "core area":  
  
# Convert to polygon/vector

core <- 
  getverticeshr(
    kud1,
    percent = 50) 

# Plot the vector and the points

plot(
  core,
  border=1:3,
  lwd=4, 
  lty = "dashed",
  add = T) 

# And export the shapefiles for the 50% KDE href version.

writeOGR(
  homerange1,
  dsn="50KdeHref.shp",
  layer="50kde",
  driver="ESRI Shapefile")


# exercise 1 --------------------------------------------------------------

BelizeTrackingDataSMSC <- 
  read.csv(
    "https://www.dropbox.com/s/iofzreo4nlwh3n4/BelizeTrackingDataSMSC.csv?dl=1",
    header = T,
    na.strings=c("NA", "NULL", "", "."))

# remote sensing ----------------------------------------------------------

# Load raster:

treeCover <- 
  raster("p019r048_TC_2015.tif")

# Let's plot the raster:  

plot(
  treeCover,
  col = rev(terrain.colors(200)))

#dummy raster to get rid of over 100

treeCoverNa <- 
  treeCover 

#make >100 NA

treeCoverNa[treeCoverNa>100]<-NA 

# plot:

plot(
  treeCoverNa,
  col = rev(terrain.colors(100)))

# add polygons:

homeRange95 <-
  shapefile("95KdeHrefWOTH.shp")

# Take a look at the polygon

summary(homeRange95)

# Let's plot the homeranges on the tree cover raster

plot(
  treeCoverNa,
  col = rev(terrain.colors(100)))

plot(
  homeRange95,
  border=1:2, 
  lwd=6,
  add=T)

# Will use extent function *2 in order to crop the raster slightly larger
# than the extent of the two polygons:

treeCoverCrop <-
  crop(treeCoverNa,
       extent(homeRange95)*2)\

# Let's plot again.

plot(
  treeCoverCrop, 
  col = rev(terrain.colors(100)))

plot(
  homeRange95, 
  border=1:2, 
  lwd=6, 
  add=T)

# Now that the raster is smaller we can explore some summary data from the raster file.   

hist(treeCoverCrop)

cellStats(treeCoverCrop, 'mean')

cellStats(treeCoverCrop, 'min')

cellStats(treeCoverCrop, 'max')

##Extracting values to polygon

meanTreeCover<-
  raster::extract(
    treeCoverCrop,
    homeRange95, 
    fun=mean, 
    na.rm = TRUE,
    sp=T) #mean value of all pixels

# Let's view the mean tree cover:

meanTreeCover






