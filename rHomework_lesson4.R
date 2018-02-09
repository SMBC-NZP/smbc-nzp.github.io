#=========================================================================================*
# ---- Set up ----
#=========================================================================================*

# Load RCurl library:

library(RCurl)

# Load a source script:

script <-
  getURL(
    "https://raw.githubusercontent.com/bsevansunc/workshop_languageOfR/master/sourceCode.R"
  )

# Evaluate then remove the source script:

eval(parse(text = script))

rm(script)

#-----------------------------------------------------------------------------------------*
# ---- Question 1 ----
#-----------------------------------------------------------------------------------------*

# 1.1 Complete the for loop below to calculate the population of the Bahamas from
# 1995 to 2013. 

# For loop to calculate the population of the Bahamas across years:

yrs <- unique(population[population$country == 'Bahamas',]$year)

populationVector <- vector('numeric', length = length(yrs))

for(i in seq_along(yrs)){
  whoCountrySubset <- whoPopulation[whoPopulation$country == 'Bahamas',]
  whoYrSubset <- whoCountrySubset[whoCountrySubset$year == yrs[i],]
  populationVector[i] <- whoYrSubset$population
}

populationVector

# 1.2 Use the population vector you created above to create a two column data
# frame where each record (row) contains the year (column 1) and population 
# (column 2).

data_frame(year = yrs, population = populationVector)

#-----------------------------------------------------------------------------------------*
# ---- Question 2 ----
#-----------------------------------------------------------------------------------------*

# 2.1 The states1975 dataset contains the fields (columns) region, division, state
# name, area(square miles), and population of each state in 1975. Complete the for
# loop below to calculate the population density of each region (population per 
# square mile). Save your results in a two column data frame with the column names 
# region and populationDensity.

states1975

str(states1975)

regions <- unique(states1975$region)

densityVector <- vector('numeric', length = length(regions))

for(i in seq_along(outVector)){
  regionSubset <- states1975[states1975$region == regions[i],]
  totalArea <- sum(regionSubset$area)
  totalPopulation <- sum(regionSubset$population)
  densityVector[i] <- totalPopulation/totalArea
}

# 2.2 Use the densityVector you created above to create a two column data
# frame where each record (row) contains the region (column 1) and population 
# density for that region (column 2).

data_frame(region = regions, populationDensity = densityVector)

#-----------------------------------------------------------------------------------------*
# ---- Question 3 ----
#-----------------------------------------------------------------------------------------*

# You are given two datasets that describe characters of the Star Wars movies. One
# dataset, measurements, provides character names, heights, and body mass 
# measurements. The other dataset, origins, describes characters by homeworld and
# species. Please take a moment to explore these datasets.

measurements <- starwars %>%
  select(name, height, mass) %>%
  filter(!is.na(mass))

origins <- starwars %>%
  select(name, homeworld, species) %>%
  filter(name %in% measurements$name)

measurements

origins

# 3.1 Complete the function below to calculate the average mass of a given 
# species in the Star Wars universe. Use the function to calculate the average
# mass of Droid characters.

sppMass <- function(spp){
  namesSubset <- origins[origins$species == spp, ]$name
  measuresSubset <- measurements[measurements$name %in% namesSubset,]
  mean(measuresSubset$mass)
}

sppMass('Droid')

# 3.2 Use the function you created above to write a for loop that will calculate
# the average mass of each species in the Star Wars universe. Return your
# results as a data frame with the columns "species" and "meanMass".

speciesVector <- unique(origins$species)

massVector <- vector('numeric', length = length(speciesVector))

for(i in seq_along(speciesVector)){
  massVector[i] <- sppMass(speciesVector[i])
}

data_frame(species = speciesVector, meanMass = massVector)


