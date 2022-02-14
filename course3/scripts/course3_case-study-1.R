
# Case Study #1: Health Expenditures ----

library(pacman)
p_load(tidyverse, janitor)

load('course3/data/raw_data/case_study_1.rda') 


# States Data
# To work with these data, we’ll also want to be able to switch between full state names and two letter abbreviations. There’s data in R available to you for just this purpose!

library(datasets)
data(state)
state.name

# Washington DC
state.abb <-  c(state.abb, 'DC')
state.region <- as.factor(c(as.character(state.region), 'South'))
state.name <- c(state.name, 'Distric of Columbia')
state_data <- tibble(Location = state.name,
                     abb = state.abb,
                     region = state.region)

## coverage dataset ----
names(coverage)

# While a lot of information in here will be helpful, it’s not in a tidy format. This is because, each variable is not in a separate column. For example, each column includes year, the type of coverage and the amount spent by state. We’ll want to use each piece of information separately downstream as we start to visualize and analyze these data. So, let’s work to get these pieces of information separated out now.


# pivor longer - tidy
coverage <- coverage %>% 
  mutate(across(starts_with('20'), as.integer)) %>% 
  pivot_longer(-Location,
               names_to = 'year_type',
               values_to = 'tot_coverage')
  
# Unfortunately, the year_type column still contains two pieces of information. 
coverage <- coverage %>% 
  separate(year_type, sep = '__',
           into = c('year', 'type'),
           convert = TRUE
           )

# Let’s go one step further and add in the state-level abbreviations and region for each row. 
coverage <- coverage %>% 
  left_join(state_data, by = 'Location')


## spending dataset ----
names(spending)

# pivot longer
spending <- spending %>% 
  pivot_longer(-Location,
               names_to = 'year',
               values_to = 'tot_spending')

# separate year and name columns
spending <- spending %>% 
  separate(year, sep="__", 
           into = c("year", "name"), 
           convert = TRUE) %>% 
  select(-name)


## join the data ----

# we only want information from years that are found in both the coverage and the spending datasets
hc <- inner_join(coverage, spending,
                 by = c('Location', 'year'))

# filter to only include state level
hc <- hc %>% 
  filter(Location != "United States")

# Another problem is that inside our hc dataset, there are multiple types of healthcare coverage.
table(hc$type)

# The “Total” type is not really a formal type of healthcare coverage.
pop <- hc %>% 
  filter(type == "Total") %>% 
  select(Location, year, tot_coverage)


# ad population level information
hc <- hc %>% 
  filter(type != "Total") %>% 
  left_join(pop, by = c("Location", "year")) %>% 
  rename(tot_coverage = tot_coverage.x, 
         tot_pop = tot_coverage.y)

table(hc$type)

# Lets calculate the proportion of people who are coverage in each state
hc <- hc %>% 
  mutate(prop_coverage = tot_coverage/tot_pop) 

# The tot_spending column is reported in millions (1e6). Therefore, to calculate spending_capita we will need to adjust for this scaling factor to report it on the original scale (just dollars) and then divide by tot_pop. We can again use mutate() to accomplish this:
hc <- hc %>% 
  mutate(spending_capita = (tot_spending*1e6) / tot_pop)

## file save
save(coverage, spending, file = here::here("course3", "data", "tidy_data", "case_study_1_tidy.rda"))


