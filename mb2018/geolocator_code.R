# Set up ------------------------------------------------------------------

# Clean your global environment:

rm(list = ls())

# Some additional functions are needed:

library(RCurl)

source('https://www.dropbox.com/s/t4bxf2olztv8alx/packages_and_setup.R?dl=1')


# Packages you will be using for this module:

library(GeoLight)
library(maps)
library(raster)
library(ks)
library(RColorBrewer)
library(tidyverse)

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


# exercise 1 --------------------------------------------------------------

oven_sub <-
  oven %>%   #
  filter %>% #

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

# For better viewing, we can set values with a KDE of close to zero as 
# below:

Breeding_KDE[values(Breeding_KDE) < 0.01] <- 
  NA

NonBreeding_KDE[values(NonBreeding_KDE) < 0.01] <- 
  NA

# Save as KML files:

KML(
  Breeding_KDE,
  'breeding_kde.kml',
  overwrite = TRUE)

KML(
  NonBreeding_KDE,
  'nonBreeding_kde.kml',
  overwrite = TRUE)


