
# Case Study #1: Health Expenditures ----

# The data for this case study are available in CSVs hosted on GitHub. CSVs from URLs can be read directly using read_csv() from readr (a core tidyverse package).

# data link: https://github.com/opencasestudies/ocs-healthexpenditure/tree/master/data/KFF

# As a reminder, we’re ultimately interested in answering the following questions with these data:
  
# - Is there a relationship between healthcare coverage and healthcare spending in the United States?
  
# - How does the spending distribution change across geographic regions in the United States?
  
# - Does the relationship between healthcare coverage and healthcare spending in the United States change from 2013 to 2014?

library(tidyverse)

# Examining the fist 10 lines
read_lines(file = 'https://raw.githubusercontent.com/opencasestudies/ocs-healthexpenditure/master/data/KFF/healthcare-coverage.csv', n_max = 10)

# Coverage data
coverage <- read_csv('https://raw.githubusercontent.com/opencasestudies/ocs-healthexpenditure/master/data/KFF/healthcare-coverage.csv', 
                     skip = 2)

head(coverage, n=15)
tail(coverage, n=15)

# We need to remove all unnecessary 'notes' entries

coverage <- read_csv('https://raw.githubusercontent.com/opencasestudies/ocs-healthexpenditure/master/data/KFF/healthcare-coverage.csv', 
                     skip = 2,
                     n_max  = which(coverage$Location == "Notes")-1)

tail(coverage) # Looks much better now!

glimpse(coverage)

# Spending data
## parsing error
spending <- read_csv('https://raw.githubusercontent.com/opencasestudies/ocs-healthexpenditure/master/data/KFF/healthcare-spending.csv', 
                     skip = 2)

# We need to remove all unnecessary 'notes' entries

spending <- read_csv('https://raw.githubusercontent.com/opencasestudies/ocs-healthexpenditure/master/data/KFF/healthcare-spending.csv', 
                     skip = 2, 
                     n_max  = which(spending$Location == "Notes")-1)

tail(spending, n=10)

# saving the raw files into the raw data folder

library(here)
here() # "C:/Users/poncest/OneDrive - Bristol Myers Squibb/Desktop/Coursera/coursera-tidyverse-skills-for-data-science-in-R-specialization"

save(coverage, spending, file = here::here("course2", "data", "raw_data", "case_study_1.rda"))

