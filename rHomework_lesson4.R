
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

# 2.1 The states1975 dataset contains the region, division, state name, area,
# and population of each state in the year 1975. Use these data and a for loop
# to calculate the population density of each region. Save your results in a two
# column data frame with the column names region and populationDensity.

regions <- unique(states1975$region)

outVector <- vector('numeric', length = length(regions))

for(i in seq_along(outVector)){
  statesSubset <- states1975[states1975$region == regions[i],]
  outVector[i] <- regions
}

#---------------------------------------------------------------------------------*
# ---- Question 3 ----
#---------------------------------------------------------------------------------*



