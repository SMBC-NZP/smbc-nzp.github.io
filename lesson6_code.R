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

library(lubridate)

options(knitr.table.format = "html")

#=================================================================================*
# ---- nested functions (review) ----
#=================================================================================*

#---------------------------------------------------------------------------------*
# --- Compare simple example of nested and non-nested forms ----
#---------------------------------------------------------------------------------*

# Non-nested form:

v <- c(1, 1, 2)

mean(v)

# Nested form:

mean(c(1,1,2))

# Check equivalence:

mean(v) == mean(c(1,1,2))

#---------------------------------------------------------------------------------*
# --- Nested and non-nested forms with custom functions ----
#---------------------------------------------------------------------------------*

# Generate an object, z, containing the numbers 1 through 5:

z <- 1:5

# Some very silly functions for illustration:

multiplyByTwo <- function(x){
  x*2
}

addOne <- function(x){
  x+1
}

# Non-nested, new object for each step:

z1 <- multiplyByTwo(z)

addOne(z1)

# Non-nested, overwrite object for each step:

z <- 1:5

z <- multiplyByTwo(z)

addOne(z)

# Nested:

addOne(multiplyByTwo(1:5))

#=================================================================================*
# ---- the pipe (review) ----
#=================================================================================*

# Non-nested, new object for each step:

z <- 1:5

z1 <- multiplyByTwo(z)

addOne(z1)

# Nested:

addOne(multiplyByTwo(1:5))

# Piped version:

1:5 %>%
  multiplyByTwo %>%
  addOne

#=================================================================================*
# ---- joining data ----
#=================================================================================*

# Tables to join:

birdCounts

birdHabits

# Join tables:

left_join(x = birdCounts, y = birdHabits, by = 'species')

# Now you! Join birdHabits to birdCounts using a pipe:



#=================================================================================*
# ---- subsetting data: select (review) ----
#=================================================================================*

# Subset columns using indexing:

birdHabits[, c('species', 'foraging')]

birdHabits[,1:2]

# Subset columns using select:

select(birdHabits, species, foraging)

# Now you! Select columns using a pipe:



#=================================================================================*
# ---- subsetting data: slice ----
#=================================================================================*

birdHabits

# Subset rows using indexing:

birdHabits[1:4,]

# Subset rows using slice:

slice(birdHabits, 1:4)

# Now you! Slice rows using a pipe:



#=================================================================================*
# ---- subsetting data: distinct ----
#=================================================================================*

select(birdCounts, site, date)

# Subset birdCounts to distinct records of site and date:

distinct(select(birdCounts, site, date))

# Now you! Subset to unique site and date records using a pipe:



#=================================================================================*
# ---- subsetting data: filter ----
#=================================================================================*

# Subset rows by condition in base R:

birdHabits[birdHabits$diet == 'omnivore',]

# Subset rows by condition using filter:

filter(birdHabits, diet == 'omnivore')

# Now you! Filter rows using a pipe:



#=================================================================================*
# --- Exercise One ----
#=================================================================================*

# Subset birdCounts to ground foraging birds:

birdCounts %>%
  left_join(birdHabits # COMPLETE
  filter(foraging # COMPLETE
  select(s # COMPLETE  

#=================================================================================*
# ---- adding and modifying columns: mutate ----
#=================================================================================*

#---------------------------------------------------------------------------------*
# --- Modify an existing column ----
#---------------------------------------------------------------------------------*

# Mutate species column in base R:

birdCounts

birdCounts$species <- toupper(species)

birdCount$species <- tolower(species)

# Mutate species column using mutate:

mutate(birdCounts, species  = toupper(species))

# Now you! Mutate using a pipe:



#---------------------------------------------------------------------------------*
# --- Add a new column ----
#---------------------------------------------------------------------------------*

# Add a year column in base R:

birdCounts$year <- year(birdCounts$date)

birdCounts

birdCounts <- birdCounts[,-5]

# Mutate year column using mutate:

mutate(birdCounts, year  = year(date))

# Now you! Mutate using a pipe:



#---------------------------------------------------------------------------------*
# ---- transmute ----
#---------------------------------------------------------------------------------*

# Add a year column, subset to site and year in base R:

newFrame <- data_frame(
  site = birdCounts$site,
  year = year(birdCounts$date)
)

# Transmute to add a year column, subset to site and year:

transmute(birdCounts,
          site,
          year = year(date))

# Now you! Transmute using a pipe:



#---------------------------------------------------------------------------------*
# ---- select and rename ----
#---------------------------------------------------------------------------------*

# Rename the species column using select:

select(birdCounts, site, date, spp = species, count)

# Rename the species column using rename:

rename(birdCounts, spp = species)

# Now you! Rename the species column to spp using rename and a pipe:



#=================================================================================*
# --- Exercise 2 ----
#=================================================================================*

# Subset birdCounts to point counts from 2009:

birdCounts %>%
  mutate(year # COMPLETE
  filter(year # COMPLETE

#=================================================================================*
# ---- grouping data ----
#=================================================================================*

# Group birdCounts by site:

group_by(birdCounts, site)

# Now you! Group birdCounts by site using a pipe:

# Species richness by site (across years):

birdCounts %>%
  group_by(site) %>%
  mutate(nSpecies = length(unique(species)))

# Group birdCounts by site and year:

group_by(mutate(birdCounts, year = year(date)), site, year)

# Now you! Group birdCounts by site and year using a pipe:



#=================================================================================*
# ---- exercise three ----
#=================================================================================*

# Calculate the species richness for each site and year:

birdCounts %>%
  mutate(year = # COMPLETE
  group_by(site, # COMPLETE
  mutate(nSpecies = # COMPLETE
  select(site, year, # COMPLETE
  # COMPLETE

#=================================================================================*
# ---- summarizing data ----
#=================================================================================*

# Note: In base R this would require a for loop!

# Species richness by site, across years:

summarize(group_by(birdCounts, site),
          nSpecies = length(unique(species)))

# Now you! Calculate species richness by site using a pipe:



#=================================================================================*
# ---- exercise four ----
#=================================================================================*

# Calculate the species richness for each site and year:

birdCounts %>%
  mutate(year # COMPLETE
  group_by(site, # COMPLETE 
  summarize(nSpecies = # COMPLETE
