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

library(knitr) ; library(kableExtra)

options(knitr.table.format = "html")

#=================================================================================*
# ---- nested functions (review) ----
#=================================================================================*

# Some very silly functions for illustration:

multiplyByTwo <- function(x){
  x*2
}

addOne <- function(x){
  x+1
}

# Non-nested, new object for each step:

z <- 1:5

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

z %>%
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

# Join tables using a pipe operator:

birdCounts %>%
  left_join(y = birdHabits, by = 'species')

#=================================================================================*
# ---- subsetting data: select (review) ----
#=================================================================================*

# Subset columns using indexing:

birdHabits[, c('species', 'foraging')]

birdHabits[,1:2]

# Subset columns using select:

select(birdHabits, species, foraging)

# Subset columns using select (piped):

birdHabits %>%
  select(species, foraging)

#=================================================================================*
# ---- subsetting data: filter ----
#=================================================================================*

# Subset rows by condition using indexing:

birdHabits[birdHabits$diet == 'omnivore',]

# Subset rows by condition using filter:

filter(birdHabits, diet == 'omnivore')

# Subset rows by condition using filter (piped):

birdHabits %>%
  filter(diet == 'omnivore')

#---------------------------------------------------------------------------------*
# --- Example process, subset counts to ground foraging birds ----
#---------------------------------------------------------------------------------*

# In base:

omnivores <- birdHabits[birdHabits$diet == 'omnivore',]$species

birdCounts[birdCounts$species %in% omnivores,]

# In tidyverse (with filter, nested):

filter(birdCounts, species %in% omnivores)

# In tidyverse (with filter, piped):

birdCounts %>%
  filter(species %in% omnivores)

# In tidyverse (with join, nested):

select(filter(
  left_join(birdCounts, birdHabits, by = 'species'),
  diet == 'omnivore'
), site:count)

# In tidyverse (with join, piped):

birdCounts %>%
  left_join(birdHabits, by = 'species') %>%
  filter(diet == 'omnivore') %>%
  select(site:count)

#=================================================================================*
# ---- adding and modifying columns: mutate ----
#=================================================================================*

# Add a year column:

mutate(birdCounts, year = year(date))

# Add a year column (piped):

birdCounts %>%
  mutate(year = year(date))

#---------------------------------------------------------------------------------*
# --- Example process, mutate then subset counts to a given year ----
#---------------------------------------------------------------------------------*

# Using indexing:

birdCounts[year(birdCounts$date) == 2009,]

# Filter without mutation:

filter(birdCounts, year(date) == 2009)

# Filter without mutation (piped):

birdCounts %>%
  filter(year(date) == 2009)

# Filter, using mutated year:

filter(mutate(birdCounts, year = year(date)), year == 2009)

# Filter, using mutated year (piped):

birdCounts %>%
  mutate(year = year(date)) %>%
  filter(year == 2009)

#=================================================================================*
# ---- adding and modifying columns: transmute ----
#=================================================================================*

# Add a year column, subset to site and year:

transmute(birdCounts,
          site = site,
          year = year(date))

# Add a year column, subset to site and year (piped):

birdCounts %>%
  transmute(site = site,
            year = year(date))

# Rename a column using select:

select(birdCounts, site, date, spp = species, count)

# Rename a column using select (piped):

birdCounts %>%
  select(site, date, spp = species, count)

# Rename a column using rename:

rename(birdCounts, spp = species)

# Rename a column using rename (piped):

birdCounts %>%
  rename(spp = species)

#=================================================================================*
# ---- grouping data ----
#=================================================================================*

# birdCounts grouped by site:

group_by(birdCounts, site)

# birdCounts grouped by site (piped):

birdCounts %>%
  group_by(site)

# birdCounts grouped by site and year:

group_by(mutate(birdCounts, year = year(date)), site, year)

# birdCounts grouped by site and year (piped):

birdCounts %>%
  mutate(year = year(date)) %>%
  group_by(site, year)

#=================================================================================*
# ---- using grouped data with mutate ----
#=================================================================================*

# Note: In base R this would require a for loop!

# Mutate works on groups:

# Species richness by site, across years:

birdCounts %>%
  group_by(site) %>%
  mutate(nSpecies = length(unique(species)))

birdCounts %>%
  group_by(site) %>%
  mutate(nSpecies = length(unique(species))) %>%
  ungroup %>%
  select(site, nSpecies) %>%
  distinct

# Species richness by site and year:

birdCounts %>%
  mutate(year = year(date)) %>%
  group_by(site, year) %>%
  mutate(nSpecies = length(unique(species))) %>%
  ungroup %>%
  select(site, year, nSpecies) %>%
  distinct

#=================================================================================*
# ---- summarizing data ----
#=================================================================================*

# Note: In base R this would require a for loop!

# Species richness by site, across years:

birdCounts %>%
  group_by(site) %>%
  summarize(nSpecies = length(unique(species)))

# Species richness by site and year:

birdCounts %>%
  mutate(year = year(date)) %>%
  group_by(site, year) %>%
  summarize(nSpecies = length(unique(species)))