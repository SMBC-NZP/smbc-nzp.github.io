# set up ------------------------------------------------------------------

# Libraries:

library(RCurl)
library(tidyverse)
library(sp)
library(rgdal)
library(maptools)
library(nleqslv)
library(ellipse)

# Set options:

select <- 
  dplyr::select

# Where is my working directory?

getwd()

# Some extra functions:

source('https://raw.githubusercontent.com/cran/sigloc/master/R/sigloc.R')

source('https://www.dropbox.com/s/t4bxf2olztv8alx/packages_and_setup.R?dl=1')

# load and format telemetry data ------------------------------------------

# Load telemetry data:

telemetryData <-
  read.csv('https://www.dropbox.com/s/rzi1ghq0bg24coh/exampleTelemetry.csv?dl=1')

telemetryData

# Make telemetry data spatial:

telemetryData_sp <-
  telemetryData %>%
  select(x, y) %>%
  SpatialPointsDataFrame(
    data = telemetryData,
    proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')
  )

telemetryData_sp

# Convert to Google Earth KML file and view:

kmlPoints(
  telemetryData_sp,
  kmlfile = 'telemetryData.kml')

# Perhaps a little more parsimoniously?

telemetryData %>%
  select(x, y) %>%
  SpatialPointsDataFrame(
    data = telemetryData,
    proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')
  ) %>%
  kmlPoints(
    kmlfile = 'telemetryData.kml'
  )

# Custom icons:

telemetryData %>%
  select(x, y) %>%
  SpatialPointsDataFrame(
    data = telemetryData,
    proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')
  ) %>%
  kmlPoints(
    kmlfile = 'telemetryData.kml',
    kmlname = 'Example telemetry data',
    icon = 'http://maps.google.com/mapfiles/ms/micons/sunny.png'
  )


# signal-location frames: -------------------------------------------------

observations <- 
  telemetryData %>%
  make_sigFrame

observations

# Compare class of objects:

class(telemetryData)

class(observations)

# Compare structure of objects:

str(telemetryData)

str(observations)

# Compare object summary information:

summary(telemetryData)

summary(observations)

# Plotting:

plot(observations)

plot(observations, bearings = TRUE)

# Change bearing lines to a spatial object:

make_bearingLines(telemetryData)

# Bearing lines as a google earth file:

make_bearingLines(telemetryData) %>%
  kmlLines(
    kmlfile = 'bearingLines.kml',
    col = 'white',
    lwd = 2)


# cross points ------------------------------------------------------------

crossPoints <- 
  findintersects(observations)

crossPoints

plot(observations, bearings = TRUE)

plot(crossPoints, add = TRUE)

# Let's add these intersects to our Google Earth map:

crossPoints %>%
  get_intersects_sp %>%
  kmlPoints(
    kmlfile = 'crossPoints.kml',
    kmlname = 'Cross points'
  )

# MLE estimate of transmitter location:

loc <- 
  locate(observations)

loc

# Plot all of the above:

plot(observations, bearings = TRUE)

plot(crossPoints, add = TRUE)

plot(loc, add = TRUE)

# Send to google earth:

loc_df <-
  data.frame(loc)

loc_df %>%
  select(X, Y) %>%
  SpatialPointsDataFrame(
    data = loc_df,
    proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')
  ) %>%
  kmlPoints(
    kmlfile = 'pointEstimate.kml',
    kmlname = 'Point estimate',
    name = "Point estimate",
    icon = 'http://maps.google.com/mapfiles/kml/paddle/grn-stars.png'
  )

# error ellipse -----------------------------------------------------------

loc %>% 
  make_ellipsePolygon %>%
  kmlPolygon('errorEllipse.kml',
             col = 'red',
             lwd = 2,
             border = 'blue') 


