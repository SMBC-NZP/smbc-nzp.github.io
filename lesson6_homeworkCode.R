#=================================================================================*
# ---- setup ----
#=================================================================================*

# Load libraries:

library(RCurl)
library(lubridate)

# Load a source script:

script <-
  getURL(
    "https://raw.githubusercontent.com/bsevansunc/workshop_languageOfR/master/sourceCode.R"
  )

# Evaluate then remove the source script:

eval(parse(text = script))

rm(script)

#=================================================================================*
# 1. The population of the Bahamas ----
#=================================================================================*

whoPopulation

# Calculate the population of the Bahamas from 1995-2013. Return your results as 
# a two-column data frame with the columns year and population.


whoPopulation %>%
  filter(country == 'Bahamas') %>%
  select(year, population)
  
#=================================================================================*
# 2. The population density of US regions ----
#=================================================================================*

states1975

# Use group_by and summarize to calculate the population density of each region.
# Return your results as a two-column data frame with the columns region and 
# popDensity.    


states1975 %>%
  group_by(region) %>%
  summarize(popDensity = sum(population)/sum(area))

#=================================================================================*
# 3. Counting birds at site "apple" ----
#=================================================================================*

birdCounts

birdHabits

# Calculate the total number of each diet class observed at site "apple" in  2009.
# Return your results as a two-column data frame with the columns diet and count.

birdCounts %>%
  filter(year(date) == 2009, site == 'apple') %>%
  left_join(birdHabits, by = 'species') %>%
  group_by(diet) %>%
  summarize(count = sum(count))


#=================================================================================*
# 4. Star Wars ----
#=================================================================================*

measurements

origins

# Calculate the average mass and number of characters associated with each  
# species of the Star Wars universe. Return your results as a three-column data
# frame with the columns: species, nCharacters, and meanMass.

measurements %>%
  left_join(origins, by = 'name') %>%
  group_by(species) %>%
  summarize(nSpecies = length(unique(name)),
            meanMass = mean(mass))

#=================================================================================*
# 5. Iris petal shape ----
#=================================================================================*

irisTbl

# You have developed a derived variable, "petalShape" to describe Iris flowers. 
# This variable is defined as the ratio of petalLength to petalWidth 
# (petalLength/petalWidth). Calculate the average (mean) shape of petals in the
# iris dataset. Return your results as a two-column data frame with the columns 
# species and petalShape_mean.

irisTbl %>% 
  transmute(species, petalShape = sepalLength/sepalWidth) %>%
  group_by(species) %>% 
  summarize(petalShape_mean = mean(petalShape))


