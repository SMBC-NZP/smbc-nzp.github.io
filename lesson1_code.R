#------------------------------------------------------------------*
# ---- description ----
#------------------------------------------------------------------*
# title: R Workshop 2018, Introduction to R for data science

# date: 2017-01-17

# author: Brian Evans, Smithsonian Migratory Bird Center

# purpose: This code accompanies the introductory presentation for
#  the R workshop. The presentation is available at the following
#  link:

#------------------------------------------------------------------*
# ---- Types of values: Numeric ----
#------------------------------------------------------------------*

# Create a vector of numeric values:

numericV <- c(3, 2, 1, 1)

numericV

# What type of object is this?

class(numericV)

str(numericV)

summary(numericV)

# Create a vector of numeric integer values:

numericInteger <- 1:5

numericInteger

# What type of object is this?

class(numericInteger)

str(numericInteger)

summary(numericInteger)

#------------------------------------------------------------------*
# ---- Types of values: Character ----
#------------------------------------------------------------------*

# Create a vector of character values:

exampleCharacter <- c('three', 'two', 'one', 'one')

exampleCharacter

# What type of object is this?

class(exampleCharacter)

str(exampleCharacter)

summary(exampleCharacter)

#------------------------------------------------------------------*
# ---- Types of values: Factor ----
#------------------------------------------------------------------*

# Create a vector of factor values:

exampleFactor <- factor(c('three', 'two', 'one', 'one'))

exampleFactor

# What type of object is this?

class(exampleFactor)

str(exampleFactor)

summary(exampleFactor)

# Set factor levels:

factor(c('three', 'two', 'one', 'one'))

factor(
  c('three', 'two', 'one', 'one'),
  levels = c('one', 'two', 'three')
)

# Set factor levels and labels:

factor(c('three', 'two', 'one', 'one'))

factor(
  c('three', 'two', 'one', 'one'),
  levels = c('one', 'two', 'three'),
  labels = c('One', 'Two', 'Three')
)

#------------------------------------------------------------------*
# ---- Types of values: Logical ----
#------------------------------------------------------------------*

# Observe the behavior of logical values:

FALSE

TRUE

as.numeric(FALSE)

as.numeric(TRUE)

FALSE + TRUE

FALSE + TRUE + TRUE

# The "is equal to" logical operator:

3 == 3

3 == 4

3 == 2 + 1

3 == 3 + 1

(3 == 3) + (3 == 2 + 1)

#------------------------------------------------------------------*
# ---- Objects: vectors ----
#------------------------------------------------------------------*

# A vector of numeric values:

numericVector <- c(1, 1, 2, 3)

numericVector

summary(numericVector)

# All values in a vector must be of the same class:

numericVector

messyVector <- c(1, 'one', 2, 3)

messyVector

# Use indexing to subset a vector:

numericVector

numericVector[3]

numericVector[3:4]

numericVector[c(1,3)]

# Attributes of the vector:

class(numericVector)

length(numericVector)

str(numericVector)

# Adding attributes to a vector:

numericVector

names(numericVector)

names(numericVector) <- c('orange', 'pear', 'apple', 'apple')

# Note that the names attribute is also a vector:

names(numericVector)

# You can index values in a vector by name:

numericVector[2]

numericVector['pear']

numericVector[2] == numericVector['pear']

numericVector[c('orange', 'pear')]

#------------------------------------------------------------------*
# ---- Objects: matrices ----
#------------------------------------------------------------------*

# Generate matrix:

m <- matrix(c(1, 1, 2, 3), ncol = 2)

m

# Compare matrices built row-wise and column-wise:

matrix(c(1, 1, 2, 3), ncol = 2, byrow = TRUE)

matrix(c(1, 1, 2, 3), ncol = 2, byrow = FALSE)

# Matrix built with multiple types:

messyMatrix <- matrix(c(1, 'one', 2, 3), ncol = 2)

messyMatrix

# Index by row (x) and column (y) position [x,y]:

m[1,1]

m[2,2]

m[1:2,2]

# View matrix attributes:

class(m)

length(m)

dim(m)

str(m)

summary(m)

# Naming rows and columns:

colnames(m) <- c('a', 'b')

rownames(m) <- c('c', 'd')

attributes(m)

#------------------------------------------------------------------*
# ---- Objects: lists ----
#------------------------------------------------------------------*

#------------------------------------------------------------------*
# ---- Objects: data frames ----
#------------------------------------------------------------------*

# Generate a data frame:

df <- data.frame(a = c(1, 1), b =  c(2, 3))

df

# View data frame attributes:

class(df)

length(df)

dim(df)

str(df)

summary(df)

# Change the names of the data frame:

names(df)

names(df) <- c('gen1', 'gen2')

df

# Generate a tibble data frame:

dplyr::data_frame(a = c(1, 'one'), b =  c(2, 3))






