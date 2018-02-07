# R worksheet script, lesson 4

#=================================================================================*
# ---- set-up ----
#=================================================================================*

# Load RCurl library (allows you to read online data and code to be read):

library(RCurl)

# Note: If you have not yet installed RCurl, please use the following to do so:
# install.packages(RCurl)

# Load a script that provides source code, automatically reading in data
# associated with this worksheet:

script <-
  getURL(
    "https://raw.githubusercontent.com/bsevansunc/workshop_languageOfR/master/sourceCode.R"
  )

# Evaluate the source code and then remove the file from your working environment:

eval(parse(text = script))

rm(script)

#=================================================================================*
# ---- why would you use for loops? ----
#=================================================================================*

# Filter irisTbl to setosa:

irisTbl[irisTbl$species == 'setosa', ]

# Extract the petalLength field (column):

irisTbl[irisTbl$species == 'setosa', ]$petalLength

# Calculate the mean of petal lengths:

mean(irisTbl[irisTbl$species == 'setosa', ]$petalLength)

#=================================================================================*
# ---- exercise one ----
#=================================================================================*

# Mean petal lengths, matrix notation:



# Mean petal lengths, function method:



#=================================================================================*
# ---- indexing review ----
#=================================================================================*

# Explore vector v:

v

class(v)

str(v)

length(v)

# Explore vector v using indexing:

i <- 3

v[i]

v[3]

v[3] == v[i]

# Add 1 to the value of v at position three:

i <- 3

v[3] + 1

v[i] + 1

#=================================================================================*
# ---- for loops, simple example ----
#=================================================================================*

# Define a vector for output:

vNew <- vector('numeric', length = length(v))

str(vNew)

# Explore filling values of vNew by index:

i <- 3

v[i]

vNew[i] <- v[i] + 1

vNew[i]

v[i] + 1 == vNew[i]

# For loop sequence:

v

1:5

1:length(v)

seq_along(v)

# Example for loop sequence statements:

# for(i in 1:length(v))

# for(i in seq_along(v))

# For loop body:

i <- 3

vNew[i] <- v[i] + 1

#---------------------------------------------------------------------------------*
# ---- for loop, putting it all together (simple) ----
#---------------------------------------------------------------------------------*

# For loop output:

vNew <- vector('numeric',length = length(v))

# For loop sequence:

for(i in seq_along(v)){
  # For loop body:
  vNew[i] <- v[i] + 1
}

# Explore first for loop output:

vNew

vNew == v + 1

#=================================================================================*
# ---- exercise two ----
#=================================================================================*

# 2.1 Convert to a function with arguments m, b, and x



# 2.2 Generate a sequential vector of values containing all integers from 1-10. 
# Assign the name x to the vector object.



# 2.3 Use a for loop and the function above to calculate values of y where: 
# m = 0.5, b = 1.0, and x refers to the vector x above (Note: A for loop is not
# really required here)



#=================================================================================*
# ---- subsetting with for loops (split, apply, combine) ----
#=================================================================================*

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

# Calculate mean petal length of each subset (apply):

mean(iris_sppSubset$petalLength)

#---------------------------------------------------------------------------------*
# ---- for loop, putting it all together (subsetting) ----
#---------------------------------------------------------------------------------*

# Make a vector of species to loop across:

irisSpecies <- levels(irisTbl$species)

# For loop output statement:

petalLengths <- vector('numeric',length = length(irisSpecies))

# For loop:

for(i in seq_along(irisSpecies)){
  # Split:
  iris_sppSubset <- irisTbl[irisTbl$species == irisSpecies[i], ]
  # Apply:
  petalLengths[i] <- mean(iris_sppSubset$petalLength)
}

# Make a tibble data frame of the for loop output (combine):

petalLengthFrame <-
  data_frame(species = irisSpecies, count = petalLengths)

petalLengthFrame

#=================================================================================*
# ---- exercise three ----
#=================================================================================*

birdHabits

# Use a for loop and the birdHabits data frame to calculate the number species in
# each diet guild.

#=================================================================================*
# ---- for loops across data objects ----
#=================================================================================*

# Explore the bird count data:

head(birdCounts)

str(birdCounts)

# Explore the bird trait data:

head(birdHabits)

str(birdHabits)

#---------------------------------------------------------------------------------*
# ---- for loops across data objects: example, apples and omnivores ----
#---------------------------------------------------------------------------------*

# Extract vector of omnivorous species:

omnivores <- birdHabits[birdHabits$diet == 'omnivore',]$species

# Subset the counts to omnivores:

birdCounts[birdCounts$species %in% omnivores, ]$count

# Calculate the sum of counts:

sum(birdCounts[birdCounts$species %in% omnivores, ]$count)

# Subset the omnivore counts to site apple:

birdCounts[birdCounts$species %in% omnivores &
             birdCounts$site == 'apple', ]

# Extract the count column:

birdCounts[birdCounts$species %in% omnivores &
             birdCounts$site == 'apple', ]$count

# Calculate the sum:

sum(birdCounts[birdCounts$species %in% omnivores &
                 birdCounts$site == 'apple', ]$count)

#=================================================================================*
# ---- exercise 4 ----
#=================================================================================*

# Using the birdHabits and birdCounts data frames, modify the function below such
# that it will calculate the number of species of a given guild at a selected 
# site.

richnessSiteGuild <- function(site, guild){
  guildSpp <- birdHabits[birdHabits$foraging # COMPLETE
  countSppSubset <- birdCounts[birdCounts$ # COMPLETE
  countSppSiteSubset <- countSppSubset[# COMPLETE
  nSpp <- # COMPLETE
  return(nSpp)
}

richnessSiteGuild('apple', 'ground')

#=================================================================================*
# ---- for loops across data objects (continued) ----
#=================================================================================*

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

countVector <-
  birdCounts_siteSubset[birdCounts_siteSubset$species %in%
                          omnivores,]$count

# Get total number of omnivores at the site:

nOmnivores <- sum(countVector)

#---------------------------------------------------------------------------------*
# ---- for loops across data objects: complete for loop, method 1 ----
#---------------------------------------------------------------------------------*

sites <- unique(birdCounts$site)

outVector <- vector('numeric', length = length(sites))

for(i in seq_along(sites)){
  birdCounts_siteSubset <- birdCounts[birdCounts$site == sites[i],]
  countVector <-
    birdCounts_siteSubset[birdCounts_siteSubset$species %in%
                            omnivores, ]$count
  outVector[i] <- sum(countVector)
}

# Combine:

data_frame(site = sites, nOmnivores = outVector)

#---------------------------------------------------------------------------------*
# ---- for loops across data objects: complete for loop, method 2 ----
#---------------------------------------------------------------------------------*

sites <- unique(birdCounts$site)

outList <- vector('list', length = length(sites))

for(i in seq_along(sites)){
  birdCounts_siteSubset <- birdCounts[birdCounts$site == sites[i],]
  countVector <-
    birdCounts_siteSubset[birdCounts_siteSubset$species %in%
                            omnivores,]$count
  outList[[i]] <- data_frame(
    site = sites[i],
    nOmnivores = sum(countVector))
}

# Combine:

bind_rows(outList)

#=================================================================================*
# ---- exercise 5 ----
#=================================================================================*

# Using the richnessSiteGuild function you created in Exercies Four and the 
# birdHabits and birdCounts data frames, modify the for loop code below to count
# the number of species that are ground foragers at each site.

sites <- unique(# COMPLETE 
  
outList <- vector('list', length = # COMPLETE 
                      
for(i in # COMPLETE 
  outList[[i]] <- data_frame(site = sites[i],
  # COMPLETE 
  }

bind_rows(# COMPLETE 
  
#=================================================================================*
# ---- simulation with for loops ----
#=================================================================================*

# For loop output:


n <- vector('numeric', length = 5)

n

# Set the seed value:

n[1] <- 10

n

# For loop sequence:

# for(i in 2:length(n))

# Exploring the construction of the for loop body:

i <- 2

n[i]

n[i-1]

n[i] <- 2*n[i-1]

n

#---------------------------------------------------------------------------------*
# ---- simulation with for loops, complete for loop ----
#---------------------------------------------------------------------------------*

# Output:

n <- vector('numeric', length = 5)

# Seed:

n[1] <- 10

# For loop:

for(i in 2:5){
  n[i] = n*v[i-1]
}

#=================================================================================*
# ---- exercise 6 ----
#=================================================================================*

# One of my favorite <i>for loops</i> was created by Leonardo Bonacci (Fibonacci). 
# He created the first known population model, from which the famous Fibonacci 
# number series was created. He described a population (N) of rabbits at time t 
# as the sum of the population at the previous time step plus the time step before
# that:

# 6.1 Create an output vector of 20 numeric values.



# 6.2 Seed the vector with the first two values, 0 and 1.



# 6.3 Use the formula above and your seed vector to generate the first 20 numbers
# of the Fibonacci number sequence.



# END #
