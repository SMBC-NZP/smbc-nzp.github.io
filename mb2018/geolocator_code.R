# Set up ------------------------------------------------------------------

# Clean your global environment:

rm(list = ls())

# Packages you will be using for this module:

library(GeoLight)
library(maps)
library(raster)
library(ks)
library(RColorBrewer)
library(RCurl)
library(tidyverse)

# Some additional functions are needed:

source('https://www.dropbox.com/s/t4bxf2olztv8alx/packages_and_setup.R?dl=1')

# Load data:

oven <-
  read_lig('https://www.dropbox.com/s/9qr0wl7zb2jdzwt/oven.lig?dl=1') 

woth <-
  read_lightBug('https://www.dropbox.com/s/e69h0og3mwdwafv/woth.txt?dl=1') 

# Explore the data:

head(oven)

head(woth)

str(oven)

str(woth)

# determine transitions ---------------------------------------------------

# Calculate transitions:

oven_transitions <-
  twilightCalc(
    datetime = oven[,2],
    light= oven[,4],
    LightThreshold=3,    # Here is where you set the threshold level
    ask=FALSE)           # Here you can go through every twilight

# sun elevation angle -----------------------------------------------------

# Subset to breeding season:

oven_breeding <-
  oven_transitions %>%
  filter(tFirst < '2012-07-31')

# Get sun elevation angle:

SunElev <-
  getElevation(
    tFirst = oven_breeding$tFirst,
    tSecond = oven_breeding$tSecond,
    type = oven_breeding$type,
    known.coord = c(-71.45,43.945),
    plot=TRUE)

SunElev

# Location estimates based on sun elevation angle:

ovenLocations <-
  coord(
    tFirst= oven_transitions$tFirst,
    tSecond= oven_transitions$tSecond,
    type=oven_transitions$type, 
    degElevation = SunElev)

head(ovenLocations)

# Oven locations as a data frame:

ovenLocations <-
  coord(
    tFirst= oven_transitions$tFirst,
    tSecond= oven_transitions$tSecond,
    type=oven_transitions$type, 
    degElevation = SunElev) %>%
  as.data.frame %>%
  bind_cols(
    oven_transitions
  )

head(ovenLocations)


# plot location data ------------------------------------------------------

ovenLocations %>%
  dplyr::select(lon, lat) %>%
  plot(
    pch = "*", 
    col = "red",
    xlab = "Longitude",
    ylab = "Latitude")

maps::map(
  'world',
  add = TRUE)


# delete me ---------------------------------------------------------------

ovenLocations_sub <-
  ovenLocations %>%
  filter(
    between(lon, -140, -67),
    between(lat, 6, 68))

ovenLocations_sub <-
  ovenLocations %>%
  filter(
    between(lon, -180, 0),
    between(lat, 0, 90))

# kernel density estimates ------------------------------------------------

# Filter to breeding and non-breeding periods:

breeding_oven <-
  ovenLocations_sub %>%
  filter(tFirst <= '2011-07-31')

NB_oven <-
  ovenLocations_sub %>%
  filter(
    tFirst >= '2011-11-01',
    tFirst <= '2012-03-03'
  )

# Calculate bandwidth for KDE:

Bwidth <- 
  Hlscv(breeding_oven[,1:2])

NBwidth <-
  Hlscv(NB_oven[,1:2])

# Generate KDE and convert to a raster file:

Breeding_KDE <- 
  breeding_oven[,1:2] %>%
  kde(H = Bwidth) %>%
  raster

NonBreeding_KDE <- 
  NB_oven[,1:2] %>%
  kde(H = NBwidth) %>%
  raster

# plotting kde ------------------------------------------------------------

# Set KDE values of 0 to NA (for better viewing):

Breeding_KDE[values(Breeding_KDE) == 0] <- 
  NA

NonBreeding_KDE[values(NonBreeding_KDE) == 0] <- 
  NA

# Set breaks to define colors:

Breed.breaks <-
  seq(
    from = 0,
    to = maxValue(Breeding_KDE),
    (maxValue(Breeding_KDE) / 100))

NB.breaks <-
  seq(
    from = 0,
    to = maxValue(NonBreeding_KDE),
    (maxValue(NonBreeding_KDE) / 100))

# Set a color palette:

colorPal <-
  colorRampPalette(
    brewer.pal(9,"Blues"))(100)

# Use the extent values of the two kde rasters to make a new plot extent:

extent(Breeding_KDE)

extent(NonBreeding_KDE)

# Delete me:

plotExtent <-
  extent(-78, -63, 9, 47)

# Plot the results:

plot(
  Breeding_KDE,
  axes=FALSE,
  breaks=Breed.breaks,
  col=colorPal,
  legend=FALSE,
  add = TRUE)

plot(
  NonBreeding_KDE,
  axes=FALSE,
  breaks=NB.breaks,
  col=colorPal,
  legend=FALSE,
  add=TRUE)

plot(test, add = TRUE)

maps::map(
  'world',
  add=TRUE)




