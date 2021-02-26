# R Lesson 2: Querying and Logic

##Review: logical values

# The "is equal to" logical operator:

3 == 3

3 == 4

3 == 2 + 1

3 == 3 + 1

(3 == 3) + (3 == 2 + 1)

##Using logic with objects

# Generate and evaluate whether values in vector v are equal to 3:

v <- c(1,1,2,3,5,8)

v

v == 3

# Generate and evaluate whether values in matrix m are equal to 3:

m <- matrix(v, ncol = 2)

m

m == 3

# Generate and evaluate whether values in data frame df are equal to 3:

df <- as.data.frame(m)

df

df == 3

##Logical operators

##Logic: Comparing values
# Explore logical operators with vector v:

v

v != 3

!(v == 3)

v < 3

v <= 3

v > 3

v >= 3

##Logic: Comparing sets

# Test whether values in v match values 1 or 3:

v %in% c(1,3)

# Test whether values in v DO NOT match values 1 or 3:

!(v %in% c(1,3))

# Test whether values in v match values 1 OR 3:

v == 1 | v ==3

# Test whether values in v match values 1 AND 3:

v == 1 & v == 3

# Test whether values in v are less than 5 and not equal to 2:

v < 5 & v != 2

##Indexing & logic: Query vectors

# Use indexing to subset a vector:

v[3]

v[3:4]

v[c(1,3)]

##Indexing & logic: Vectors

# Use logic to subset a vector:

v

v > 2

v[v > 2]

# At which indices does our logical statement evaluate to TRUE?

v

v > 2

which(v > 2)

v[4:6]

v[v > 2]

##Indexing & logic: Matrices

# Index matrix m by row (x) and column (y) position [x,y]:

m[,]

m[1,1]

m[2,2]

m[2, ]

m[ ,2]

m[1:2,2]

# Logical test of whether the first column of m is greater than 1:

m[,1] > 1

# At which indices does our logical statement evaluate to TRUE?

which(m[,1] > 1)

# Query m by index and logical statement:

m[3, ]

m[m[,1] > 1, ]

# Querying matrix by column 1 less than 2 and column 2 less than 5

which(m[,1] < 2 &  m[,2] < 5)

m[m[,1] < 2 &  m[,2] < 5 , ]

##Indexing & logic: Data frames

# Index dataframe df by row (x) and column (y) position [x,y]:

df[,]

df[1,1]

df[2,2]

df[2, ]

df[ ,2]

df[1:2,2]

# Attributes of object df:

attributes(df)

# Reset names of object df:

names(df) <- c('hello', 'world')

df

attributes(df)

# Data frame indexing by position ...

df[,1]

df[,2]

# ... is equivalent to:

df$hello

df$world

# Querying data frames using "matrix notation" ...

df[,1] > 1

which(df[,1] > 1)

df[df[,1] > 1,]

# ... is equivalent to using column names:

df$hello > 1

which(df$hello > 1)

df[df$hello > 1,]

##Indexing & logic: Strings

library(tidyverse)

fruit

# Query the first five fruits by index:

fruit[1:5]

# Which position in the fruit vector is apple?

fruit == 'apple'

which(fruit == 'apple')

# Subsetting to fruits named "apple" by position ...

fruit[1]

fruit[which(fruit == 'apple')]

# ... is equivalent to subsetting by condition:

fruit[fruit == 'apple']

# Subset "hello world" to just "hello":

str_sub('hello world', start = 1, end = 5)

# Subset to just "world":

str_sub('hello world', start = 7, end = 12)

# You actually don't have to specify the end point here:

str_sub('hello world', start = 7)

# You can also count from the end:

str_sub('hello world', start = -5)

# str_sub can be used to examine the first letter of each value:

str_sub(fruit, start = 1, end = 1)

str_sub(fruit, start = 1, end = 1) == 'a'

which(str_sub(fruit, start = 1, end = 1) == 'a')

# Subsetting to fruits with 7 characters by index or condition:

fruit[1:3]

fruit[which(str_sub(fruit, start = 1, end = 1) == 'a')]

fruit[str_sub(fruit, start = 1, end = 1) == 'a']

# str_count can be used to determine the number of letters
# or symbols in a string:

fruit

str_count(fruit)

# Which fruits have exactly 5 letters?

str_count(fruit) == 5

which(str_count(fruit) == 5)

# Subsetting to fruits with 5 characters by index ...

fruit[c(1,34,36,44,49,53,58)]

# ... is equivalent to ...

fruit[which(str_count(fruit) == 5)]

# ... and is also equivalent to subsetting by condition:

fruit[str_count(fruit) == 5]

# Detecting a pattern within a string:

str_detect('hello world', pattern = 'hello')

str_detect('hello world', pattern = 'ello')

str_detect('hello world', pattern = 'lo wo')

str_detect('hello world', pattern = 'world')

str_detect('hello world', pattern = "foo")

str_detect('hello world', pattern = 'helloworld')

# Which fruits contain the pattern "apple"?

str_detect(fruit, 'apple')

which(str_detect(fruit, 'apple'))

# Subsetting to fruits with the pattern "apple" by index ...

fruit[c(1,62)]

fruit[which(str_detect(fruit, 'apple'))]

# ... and is also equivalent to subsetting by condition:

fruit[str_detect(fruit, 'apple')]

##Summarizing and queries
# Make a dummy data frame:

dummyData <-  data.frame(
  gen = c('a', 'b', 'a', 'a', 'b'),
  n = c(3,5,8,13, 21)
)

dummyData

# Basic summary statistics of the dummy data frame:

summary(dummyData)

min(dummyData$n)

max(dummyData$n)

# Subset to maximum and minimum values, returning separate results:

dummyData[dummyData$n == min(dummyData$n),]

dummyData[dummyData$n == max(dummyData$n),]

# Subset to maximum and minimum n values, returning both
# results in one frame:

dummyData[dummyData$n == min(dummyData$n)|
            dummyData$n == max(dummyData$n),]
# Subset to group "a":

dummyData[dummyData$gen == 'a',]

# As a data frame, we can pull out the second column using indexing ...

dummyData[dummyData$gen == 'a',2]

# ... or by name (my preference):

dummyData[dummyData$gen == 'a',]$n


# We can then calculate the minimum value in group "a" as:

min(dummyData[dummyData$gen == 'a',]$n)

##Practice with real data

# Load WHO's TB dataset:

population

# Subset population data to the United States:

population[population$country == 'United States', ]

# What are the country names?

unique(population$country)

# Which of these countries contain the pattern "United"?

str_detect(unique(population$country), 'United')

unique(population$country)[str_detect(unique(population$country),
                                      'United')]

# They call the United States "United States of America", so 
# the query could be written as:

population[population$country == 'United States of America',]

# To determine the size of a population in 2004:

population[population$year == 2004,]

# Therefore, the data frame reduced to the population 
# of the United States in 2004:

population[population$country == 'United States of America' &
             population$year == 2004,]

# To return just the population information:

population[population$country == 'United States of America' & 
             population$year == 2004,]$population

# Subset the population data frame to records with a 
# population of greater than 300 million:

population[population$population > 300000000,]

# Repeat the above, but subset the records to the United States:

population[population$country == 'United States of America' &
             population$population > 300000000,]

# To return just the years:

population[population$country == 'United States of America' &
             population$population > 300000000, ]$year



