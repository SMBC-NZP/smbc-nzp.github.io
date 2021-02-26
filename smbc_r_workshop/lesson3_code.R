#=================================================================================*
# ---- set up ----
#=================================================================================*

# Load libraries

library(RCurl)

library(tidyverse)

# Note: If you have yet to install these library, please do so with: 
# install.packages('rCurl') ; install.packages('tidyverse')

# Provide the web addresses of the files (note: iris is preloaded example data):

url <- 'https://raw.githubusercontent.com/bsevansunc/workshop_languageOfR/master'

habitsURL <- getURL(
  paste(url, 'birdHabits.csv', sep = '/')
)

countsURL <- getURL(
  paste(url, 'birdCounts.csv', sep = '/')
)

# Read in the data:

birdHabits <- tbl_df(
  read.csv(text = habitsURL, stringsAsFactors = FALSE)
)

birdCounts <- tbl_df(
  read.csv(text = countsURL, stringsAsFactors = FALSE)
)

# Clean up iris for analysis:

irisTbl <- tbl_df(iris)

names(irisTbl) <-
  c('sepalLength',
    'sepalWidth',
    'petalLength',
    'petalWidth',
    'species')

#=================================================================================*
# ---- functions ----
#=================================================================================*
#---- First function ----
#---------------------------------------------------------------------------------*

addOneFun <- function(x){
  x+1
}

# Testing the function on a numeric value:

42+1

addOneFun(42)

# Testing the function on a vector of numeric values:

v <- c(1,1,2,3,5)

v + 1

addOneFun(v)

#---------------------------------------------------------------------------------*
#---- Query by species function ----
#---------------------------------------------------------------------------------*

# Explore birdCounts data:

str(birdCounts)

head(birdCounts)

# Matrix notation query:

birdCounts[birdCounts$species == 'grca', ]

# Query function:

speciesSubset <- function(spp){
  birdCounts[birdCounts$species == spp, ]
}

# Test function:

birdCounts[birdCounts$species == 'grca', ]

speciesSubset('grca')

#---------------------------------------------------------------------------------*
#---- Query by species function, generalized ----
#---------------------------------------------------------------------------------*

speciesSubset <- function(dfIn, spp){
  dfIn[dfIn$species == spp, ]
}

# Test function, birdCounts:

birdCounts[birdCounts$species == 'grca', ]

speciesSubset(birdCounts, 'grca')

# Test function, birdHabits:

birdHabits[birdHabits$species == 'grca', ]

speciesSubset(birdHabits, 'grca')

#---------------------------------------------------------------------------------*
#---- Query function, very generalized----
#---------------------------------------------------------------------------------*

# Subset to catbirds using $ and matrix notation:

birdHabits[birdHabits$species == 'grca', ]

birdHabits[birdHabits[,'species'] == 'grca',]

# Very generalized query:

query <- function(dfIn, variable, condition){
  dfIn[dfIn[,variable] == condition,]
}

# Test query:

birdHabits[birdHabits$species == 'grca', ]

birdHabits[birdHabits[,'species'] == 'grca',]

query(birdHabits, 'species', 'grca')

#---------------------------------------------------------------------------------*
#---- Query function, mean count ----
#---------------------------------------------------------------------------------*

meanSpeciesCounts <- function(spp){
  # Number of unique site values:
  nSites <- length(unique(birdCounts$site))
  # Subset birdCounts to the species of interest:
  birdCounts_sppSubset <- birdCounts[birdCounts$species == spp, ]
  # Calculate the total number of birds observed:
  nBirds <- sum(birdCounts_sppSubset$count)
  # Return mean number of birds per site:
  return(nBirds/nSites)
}

# What is the average number of observed catbirds?

meanSpeciesCounts('grca')

#---------------------------------------------------------------------------------*
#---- Query function, mean count, nested ----
#---------------------------------------------------------------------------------*

# Query by species function, generalized:

meanSpeciesCounts <- function(dfIn, spp){
  # Number of unique site values:
  nSites <- length(unique(dfIn$site))
  # Calculate the total number of birds observed:
  nBirds <- sum(speciesSubset(dfIn, spp)$count)
  # Return mean number of birds per site:
  return(nBirds/nSites)
}

# What is the average number of observed catbirds?

meanSpeciesCounts(birdCounts, 'grca')

#=================================================================================*
# ---- for loops ----
#=================================================================================*
#---- Looking at iris petal length again ----
#---------------------------------------------------------------------------------*

# Filter irisTbl to setosa:

irisTbl[irisTbl$species == 'setosa', ]

# Extract the petalLength field (column):

irisTbl[irisTbl$species == 'setosa', ]$petalLength

# Calculate the mean of petal lengths:

mean(irisTbl[irisTbl$species == 'setosa', ]$petalLength)

#---------------------------------------------------------------------------------*
#---- First for loop, exploration ----
#---------------------------------------------------------------------------------*

# Generate vector v:

v <- c(1,1,2,3,5)

v


# Explore vector v using indexing:

i <- 3

v[i]

v[3]

v[3] == v[i]

# Add 1 to the value of v at position three:

v[3] + 1

v[i] + 1

# Define a vector for output:

vNew <- vector('numeric', length = length(v))

str(vNew)

# Explore filling values of vNew by index:

i <- 3

v[i]

vNew[i] <- v[i] + 1

vNew[i]

v[i] + 1 == vNew[i]

# Explore sequence:

v

1:5

1:length(v)

seq_along(v)

#---------------------------------------------------------------------------------*
#---- First for loop, complete ----
#---------------------------------------------------------------------------------*

vNew <- numeric(length = length(v))

for(i in seq_along(v)){
  vNew[i] <- v[i] + 1
}

# Explore first for loop output:

vNew

v

vNew == v

#---------------------------------------------------------------------------------*
#---- Using for loops for subsetting, exploration ----
#---------------------------------------------------------------------------------*

# Mean petal lengths of Iris species without a for loop:

mean(irisTbl[irisTbl$species == 'setosa', ]$petalLength)

mean(irisTbl[irisTbl$species == 'versicolor', ]$petalLength)

mean(irisTbl[irisTbl$species == 'virginica', ]$petalLength)

# Make a vector of species to loop across:

irisSpecies <- levels(irisTbl$species)

irisSpecies

# For loop output statement:

petalLengths <- vector('numeric',length = length(irisSpecies))

petalLengths

# Exploring the iris data, subsetting by species:

i <- 3

irisSpecies[i]

irisTbl[irisTbl$species == irisSpecies[i], ]

# Split:

iris_sppSubset <- irisTbl[irisTbl$species == irisSpecies[i], ]

# Calculate mean petal length of each subset:

mean(iris_sppSubset$petalLength)

#---------------------------------------------------------------------------------*
# ---- Subsetting for loop, complete ----
#---------------------------------------------------------------------------------*

# Make a vector of species to loop across:

irisSpecies <- levels(irisTbl$species)

# For loop output statement:

petalLengths <- vector('numeric',length = length(irisSpecies))

# For loop:

for(i in seq_along(irisSpecies)){
  iris_sppSubset <- irisTbl[irisTbl$species == irisSpecies[i], ]
  petalLengths[i] <- mean(iris_sppSubset$petalLength)
}

# Make a tibble data frame of the for loop output:

petalLengthFrame <- data_frame(species = irisSpecies, count = petalLengths)

petalLengthFrame

#---------------------------------------------------------------------------------*
#---- Subsetting with for loops across data objects, exploration ----
#---------------------------------------------------------------------------------*

# Explore the bird count data:

head(birdCounts)

str(birdCounts)

# Explore the bird trait data:

head(birdHabits)

str(birdHabits)

# Extract vector of omnivorous species:

omnivores <- birdHabits[birdHabits$diet == 'omnivore',]$species

# Generate a vector of unique sites:

sites <- unique(birdCounts$site)

# Site at position i:

i <- 3

sites[i]

# Subset data:

birdCounts_siteSubset <- birdCounts[birdCounts$site == sites[i],]

birdCounts_siteSubset

# Just a vector of omnivore counts:

countVector <- birdCounts_siteSubset[birdCounts_siteSubset$species %in% omnivores, ]$count

# Get total number of omnivores at the site:

nOmnivores <- sum(countVector)

#---------------------------------------------------------------------------------*
#---- Subsetting with for loops across data objects, complete ----
#---------------------------------------------------------------------------------*

# Extract vector of omnivorous species:

omnivores <- birdHabits[birdHabits$diet == 'omnivore',]$species

# Generate a vector of unique sites:

sites <- unique(birdCounts$site)

outVector <- vector('numeric', length = length(unique(sites)))

for(i in seq_along(sites)){
  birdCounts_siteSubset <- birdCounts[birdCounts$site == sites[i],]
  countVector <- birdCounts_siteSubset[birdCounts_siteSubset$species %in% omnivores, ]$count
  outVector[i] <- sum(countVector)
}

# Combine:

data_frame(site = sites, nOmnivores = outVector)

#---------------------------------------------------------------------------------*
#---- Subsetting with for loops across data objects, alternate ----
#---------------------------------------------------------------------------------*

# Extract vector of omnivorous species:

omnivores <- birdHabits[birdHabits$diet == 'omnivore',]$species

# Generate a vector of unique sites:

sites <- unique(birdCounts$site)

outList <- vector('list', length = length(unique(sites)))

for(i in seq_along(sites)){
  birdCounts_siteSubset <- birdCounts[birdCounts$site == sites[i],]
  countVector <- birdCounts_siteSubset[birdCounts_siteSubset$species %in% omnivores, ]$count
  outList[[i]] <- data_frame(site = sites[i], nOmnivores = sum(countVector))
}

# Combine:

bind_rows(outList)

#---------------------------------------------------------------------------------*
#---- For loops for simulation, exploration ----
#---------------------------------------------------------------------------------*

# For loop output statement:


n <- vector('numeric', length = 5)

n

# Setting the seed value:

n[1] <- 10

n

# Exploring the construction of the for loop body:

i <- 2

n[i]

n[i-1]

n[i] <- 2*n[i-1]

n

#---------------------------------------------------------------------------------*
#---- For loops for simulation, complete ----
#---------------------------------------------------------------------------------*

# Output:

n <- vector('numeric', length = 5)

# Seed:

n[1] <- 10

# For loop:

for(i in 2:5){
  n[i] = n*v[i-1]
}



