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
# ---- functions ----
#=================================================================================*

# Add region to a plot (based on siteId field):

addRegion <- function(x){
  xState <- str_sub(x, start = -3, end = -2)
  case_when(
    xState == 'GA' ~ 'Atlanta',
    xState == 'CO' ~ 'Colorado',
    xState == 'FL' ~ 'Gainesville',
    xState == 'NC' ~ 'Raleigh',
    xState == 'PA' ~ 'Pittsburgh',
    xState == 'MA' ~ 'Springfield',
    xState %in% c('MD','DC', 'VA') ~ 'Washington, DC',
    TRUE ~ 'noData'
  )
}

generateObservationID <- function(){
  n1 <- sample(100:999,1)
  l1 <- sample(letters,1)
  n2 <- sample(1000:9999, 1)
  l2 <- sample(letters, 1)
  paste0(l1, n1, '-', n2, l2)
}

#=================================================================================*
# ---- get data ----
#=================================================================================*

caps <- read.csv('captureTable.csv') %>%
  as_tibble

names(caps) <- str_replace_all(names(caps), 'Capture', '')

bandMeasures <- caps %>%
  select(siteID, spp, bandNumber, enc, date, mass, wing, tl, age, sex) %>%
  filter(mass != '99999', wing != '99999', tl != '99999', age != 'HY') %>%
  mutate_at(c('mass', 'wing', 'tl'), as.numeric) %>%
  na.omit %>%
  group_by(spp) %>%
  mutate(n = n()) %>%
  ungroup %>%
  filter(n > 100) %>%
  select(-n) %>%
  filter(age != 'U', sex != 'U') %>%
  mutate(region = addRegion(siteID)) %>%
  select(region, spp:sex) %>%
  mutate(date = as.Date(date)) %>%
  arrange(region, date)

#=================================================================================*
# ---- remove outliers ----
#=================================================================================*

outlierBottom <- function(x){
  median(x) - 2.5*mad(x)
}

outlierTop <- function(x){
  median(x) + 2.5*mad(x)
}

bandMeasuresClean <- bandMeasures %>%
  group_by(spp, sex) %>%
  mutate(
    massB = outlierBottom(mass),
    massT = outlierTop(mass),
    wingB = outlierBottom(wing),
    wingT = outlierTop(wing),
    tlB = outlierBottom(tl),
    tlT = outlierTop(tl)
  ) %>%
  ungroup %>%
  filter(mass > massB & mass < massT,
         wing > wingB & wing < wingT,
         tl > tlB & tl < tlT) %>%
  select(region:sex)

#=================================================================================*
# ---- return cleaned data ----
#=================================================================================*

birdMeasures <- bandMeasuresClean %>%
  filter(age != 'noData', sex != 'noData') %>%
  filter(spp %in% c('GRCA', 'CACH', 'AMRO', 'BCCH', 'NOCA')) %>%
  rowwise()%>%
  mutate(observationID = generateObservationID()) %>%
  select(id =  observationID, region:sex)

write_csv(birdMeasures, 'bandingRecord.csv')
