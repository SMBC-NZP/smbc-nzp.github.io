# Prep nn data

#setwd('C:/Users/Guest user/gits/smbc-nzp.github.io/mb2018')

library(tidyverse)
library(lubridate)

captures <-
  read_csv('data/dbBackup/backup_captures_downloaded_2018-09-12.csv')

partRs <-
  read_csv('data/dbBackup/backup_partRs_downloaded_2018-09-12.csv')

techRs <-
  read_csv('data/dbBackup/backup_techRs_downloaded_2018-09-12.csv')

sites <-
  read_csv('data/dbBackup/backup_sites_downloaded_2018-09-12.csv')

visits <-
  read_csv('data/dbBackup/backup_visits_downloaded_2018-09-12.csv')

# Add impervious to sites:

sites <-
  read.csv('data/impervious.csv') %>%
  filter(
    siteID %in% sites$siteID,
    distance == 500,
    !is.na(imp)) %>%
  select(siteID, region, imp)

# Filter to sites with six visits from 2012 to 2017:

goodSites <-
  visits %>%
  transmute(site = siteID, year = year(date)) %>%
  filter(year >= 2012 & year < 2018) %>%
  distinct %>%
  group_by(
    site
  ) %>%
  summarize(n = n()) %>%
  filter(n == 6) %>%
  .$site


captureData <-
  captures %>%
  filter(
    year(date) >= 2012 & year(date) < 2018,
    sex != 'U',
    age != 'HY',
    !(year(date) >= 2016 & enc == 'B'),
    siteID %in% goodSites,
    spp %in% c('GRCA', 'NOCA', 'SOSP')) %>%
  select(siteID, date, time, enc, spp, bandNumber:cpBp)

goodSites_subset <-
  goodSites[goodSites %in% captureData$siteID]

siteData <-
  sites %>%
  filter(siteID %in% goodSites_subset) %>%
  select(siteID, region, imp)

resightData <-
  bind_rows(
    partRs %>%
      filter(
        siteID %in% goodSites_subset,
        bandNumber %in% captureData$bandNumber) %>%
      mutate(type = 'P') %>%
      select(siteID, date, bandNumber, type),
    techRs %>%
      filter(
        siteID %in% goodSites_subset,
        bandNumber %in% captureData$bandNumber) %>%
      mutate(type = 'T') %>%
      select(siteID, date, bandNumber, type)
  ) %>%
  filter(year(date) <2018 & year(date) > 2011)


goodCh <-
  bind_rows(
  captureData %>%
    select(bandNumber, date),
  resightData %>%
    select(bandNumber, date)
) %>%
  mutate(date = year(date), enc = 1) %>%
  distinct %>%
  spread(key = date, value = enc, fill = 0) %>%
  unite(ch, -bandNumber, sep = '') %>%
  filter(!ch %in% c('000001', '000010','000011')) 


# final data --------------------------------------------------------------

captureData_final <-
  captureData %>%
  filter(bandNumber %in% goodCh$bandNumber)

siteData_final <-
  siteData %>%
  filter(siteID %in% captureData_final$siteID) %>%
  group_by(region) %>%
  mutate(
    siteCode = 1:n(),
    siteCode = paste(region, siteCode,sep = '_'))

resightData_final <-
  resightData %>%
  select(-siteID) %>%
  filter(bandNumber %in% captureData_final$bandNumber) %>%
  arrange(date, bandNumber)

write_csv(
  captureData_final %>%
    left_join(siteData_final, by = 'siteID') %>%
    select(siteCode, date:cpBp) %>%
    rename(siteID = siteCode) %>%
    arrange(date, siteID),
  'bandingRecords.csv')

write_csv(siteData_final %>% 
            mutate(siteID = siteCode) %>%
            select(-siteCode), 'sites.csv')

write_csv(resightData_final, 'resights.csv')


# exploring final data and making capture histories -----------------------

# Make a band Number by year frame:

encounters_byYear <-
  bind_rows(
    captureData_final %>%
      select(bandNumber, date),
    resightData_final %>%
      select(bandNumber, date)
  ) %>%
  transmute(
    bandNumber,
    year = year(date)) %>%
  distinct

# How many times were birds re-encountered?

encounters_byYear %>%
  group_by(bandNumber) %>%
  summarize(nEncounters = n()) %>%
  group_by(nEncounters) %>%
  summarize(n = n())

# Make an encounter history:

encounterHistory <-
  encounters_byYear %>%
  mutate(enc = 1) %>%
  spread(key = year, value = enc, fill = 0) %>%
  unite(ch, -bandNumber, sep = '')

# Summary of encounter histories:

encounterHistory %>%
  group_by(ch) %>%
  summarize(n = n()) %>% View

# Summary of the number of times birds were encountered:

encounters_byYear %>%
  group_by(bandNumber) %>%
  summarize(nEncounters = n()) %>%
  group_by(nEncounters) %>%
  summarize(n = n())

# Separating species:

bandNumber_noca <-
  captureData_final %>%
  filter(spp == 'NOCA') %>%
  .$bandNumber

bandNumber_grca <-
  captureData_final %>%
  filter(spp == 'GRCA') %>%
  .$bandNumber


bandNumber_grca <-
  captureData_final %>%
  filter(spp == 'GRCA') %>%
  .$bandNumber

encounters_byYear %>%
  filter(bandNumber %in% bandNumber_grca) %>%
  group_by(bandNumber) %>%
  summarize(nEncounters = n()) %>%
  group_by(nEncounters) %>%
  summarize(n = n())



