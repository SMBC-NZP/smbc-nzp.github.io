#=================================================================================*
# ---- setup ----
#=================================================================================*

# Load libraries:

library(RCurl)
library(lubridate)

# Load a source script:

script <-
  getURL(
    "https://raw.githubusercontent.com/bsevansunc/workshop_languageOfR/master/sourceCode_lesson6.R"
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



#=================================================================================*
# 2. The population density of US regions ----
#=================================================================================*

states1975

# Use group_by and summarize to calculate the population density of each region.
# Return your results as a two-column data frame with the columns region and 
# popDensity.   



#=================================================================================*
# 3. Counting birds at site "apple" ----
#=================================================================================*

birdCounts

birdHabits

# Calculate the total number of each diet class observed at site "apple" in  2009.
# Return your results as a two-column data frame with the columns diet and count.



#=================================================================================*
# 4. Star Wars ----
#=================================================================================*

measurements

origins

# Calculate the average mass and number of characters associated with each  
# species of the Star Wars universe. Return your results as a three-column data
# frame with the columns: species, nCharacters, and meanMass.



#=================================================================================*
# 5. Iris petal shape ----
#=================================================================================*

irisTbl

# You have developed a derived variable, "petalShape" to describe Iris flowers. 
# This variable is defined as the ratio of petalLength to petalWidth 
# (petalLength/petalWidth). Calculate the average (mean) shape of petals in the
# iris dataset. Return your results as a two-column data frame with the columns 
# species and petalShape_mean.


