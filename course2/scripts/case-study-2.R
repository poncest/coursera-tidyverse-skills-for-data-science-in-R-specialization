
# Case Study #2: Firearms ----

## As a reminder, we’re interested in the following question: At the state-level, what is the relationship between firearm legislation strength and annual rate of fatal police shootings?

## Census Data
census <- read_csv('https://raw.githubusercontent.com/opencasestudies/ocs-police-shootings-firearm-legislation/master/data/sc-est2017-alldata6.csv',                    n_max = 236900)

## Counted Data 2015
counted15 <- read_csv("https://raw.githubusercontent.com/opencasestudies/ocs-police-shootings-firearm-legislation/master/data/the-counted-2015.csv")

## Suicide Data
suicide_all <- read_csv("https://raw.githubusercontent.com/opencasestudies/ocs-police-shootings-firearm-legislation/master/data/suicide_all.csv")

# read in firearm suicide data
suicide_firearm <- read_csv("https://raw.githubusercontent.com/opencasestudies/ocs-police-shootings-firearm-legislation/master/data/suicide_firearm.csv")

## Brady Data
library(readxl)
library(httr)

# specify URL to file
url = "https://github.com/opencasestudies/ocs-police-shootings-firearm-legislation/blob/master/data/Brady-State-Scorecard-2015.xlsx?raw=true"

# Use httr's GET() and read_excel() to read in file
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))

## read brady data
brady <- read_excel(tf, sheet = 1)


## Crime Data
# specify URL to file
url = "https://github.com/opencasestudies/ocs-police-shootings-firearm-legislation/blob/master/data/table_5_crime_in_the_united_states_by_state_2015.xls?raw=true"

# Use httr's GET() and read_excel() to read in file
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))

crime <- read_excel(tf, sheet = 1, skip = 3)

## Land Area Data
# specify URL to file
url = "https://github.com/opencasestudies/ocs-police-shootings-firearm-legislation/blob/master/data/LND01.xls?raw=true"

# Use httr's GET() and read_excel() to read in file
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))

land <- read_excel(tf, sheet = 1)

## Unemployment Data
# This data was available online from the Bureau of Labor Statistics (BLS), now it is available on the archive, but there is no easy download of the table. It is also difficult to simply copy and paste; it doesn’t hold it’s table format. Thus we will want to use web scraping to most easily and accurately obtain this information using the rvest package.

# As a reminder, to view the HTML of a webpage, right-click and select “View page source.”

library(rvest)

# specify URL to where we'll be web scraping
url <- read_html("https://web.archive.org/web/20210205040250/https://www.bls.gov/lau/lastrk15.htm")

# scrape specific table desired
out <- html_nodes(url, "table") %>%
  .[2] %>%
  html_table(fill = TRUE) 

# store as a tibble
unemployment <- as_tibble(out[[1]]) 

# saving all the raw file  
library(here)
here() # "C:/Users/poncest/OneDrive - Bristol Myers Squibb/Desktop/Coursera/coursera-tidyverse-skills-for-data-science-in-R-specialization"

library(here)
save(census, counted15, suicide_all, suicide_firearm, brady, crime, land, unemployment , file = here::here("course2", "data", "raw_data", "case_study_2.rda"))
