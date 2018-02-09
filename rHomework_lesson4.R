#=================================================================================*
# ---- Set up ----
#=================================================================================*

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

#---------------------------------------------------------------------------------*
# ---- Question 1 ----
#---------------------------------------------------------------------------------*

# 1.1 Complete the for loop below to calculate the population of the Bahamas from
# 1995 and 2013. 

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

#---------------------------------------------------------------------------------*
# ---- Question 2 ----
#---------------------------------------------------------------------------------*

# 2.1 The states1975 dataset contains the fields (columns) region, division, state
# name, area(square miles), and population of each state in 1975. Complete the for
# loop below to calculate the population density of each region (population per 
# square mile). Save your results in a two column data frame with the column names 
# region and populationDensity.

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
# density (column 2).

data_frame(region = regions, populationDensity = densityVector)

#---------------------------------------------------------------------------------*
# ---- Question 3 ----
#---------------------------------------------------------------------------------*



