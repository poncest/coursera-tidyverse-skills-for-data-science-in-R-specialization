
# Case Study #2: Firearms----

library(pacman)
p_load(tidyverse, janitor)

load('course3/data/raw_data/case_study_2.rda') 

names(census)
glimpse(census)

# what is a RACE of 1? What does that mean?
# https://github.com/opencasestudies/ocs-police-shootings-firearm-legislation/blob/master/data/sc-est2017-alldata6.pdf

## census ----
# summarize by ethnicity - 2015 data 
census_stats <- census %>% 
  filter(ORIGIN == 0, SEX == 0) %>%        # - = total
  group_by(NAME) %>% 
  summarise(
    white = sum(POPESTIMATE2015[RACE == 1])/sum(POPESTIMATE2015)*100,
    black = sum(POPESTIMATE2015[RACE == 2])/sum(POPESTIMATE2015)*100
    )

# add hispanic information
census_stats$hispanic <- census %>%
  filter(SEX == 0) %>% 
  group_by(NAME) %>%
  summarise(
    x = sum(POPESTIMATE2015[ORIGIN == 2]) / sum(POPESTIMATE2015[ORIGIN == 0])*100) %>%
  pull(x)

# add male information
census_stats$male <- census %>%
  filter(ORIGIN == 0) %>%
  group_by(NAME) %>%
  summarise(
    x = sum(POPESTIMATE2015[SEX == 1])/sum(POPESTIMATE2015[SEX == 0])*100) %>%
  pull(x)

# add total population information
census_stats$total_pop <- census %>%
  filter(ORIGIN == 0, SEX == 0 ) %>%
  group_by(NAME) %>%
  summarise(
    total = sum(POPESTIMATE2015)) %>%
  pull(total)

# lowercase state name for consistency
census_stats$NAME <- tolower(census_stats$NAME)


## age ----
# get state-level age information - 2015 data 
age_stats <- census %>%
  filter(ORIGIN == 0, SEX == 0) %>%
  group_by(NAME, AGE) %>%
  summarize(sum_ages = sum(POPESTIMATE2015))

# pivot wider
age_stats <- age_stats %>%
  pivot_wider(names_from = "NAME",
              values_from = "sum_ages")

# calculate cumsum age for each state
age_stats %>%
  select(-AGE) %>%
  map(cumsum) %>%
  map(function(x) x/x[nrow(age_stats)]) %>%
  as_tibble()

# We can see that we create a list of vectors for each state.
# calculate median age for each state
age_stats <- age_stats %>%
  select(-AGE) %>%
  map_df(cumsum) %>%
  map_df(function(x) x/x[nrow(age_stats)]) %>%
  mutate(AGE = age_stats$AGE) %>%
  select(AGE, everything())

as_tibble(age_stats)


## violent crimes ----
names(crime)

# Because of the messy names here (we’ll clean them up in a bit), we’ll see the column index to select columns instead of the complicated names. Also, we print a specified row of violent crime to observe the X__1 group we are looking for – Rate per 100,000 inhabitants (per the study.)

violentcrime <- crime %>% 
  select(c(1,3,5))

# we’re ultimately interested in Rate per 100,000 inhabitants,
violentcrime <- violentcrime %>% 
  fill(State) %>%  # Fill in missing values with previous/next value
  filter(.[[2]] == "Rate per 100,000 inhabitants") %>%
  rename( violent_crime = `Violent\ncrime1`) %>%
  select(-`...3`)

# If we look closely at our data, we’ll notice that some of our state names have numbers at the end of them.
unique(violentcrime$State)

# lower case and remove numbers from State column
violentcrime <- violentcrime %>%
  mutate(State = tolower(gsub('[0-9]+', '', State)))

# join with census data
firearms <- left_join(census_stats, violentcrime, 
                      by = c("NAME" = "State"))


## brady scores ----
names(brady)

# This dataset includes a lot of information, but we’re interested in the brady scores for each state. These are stored in the row where the first column is equal to “TOTAL STATE POINTS,” 
brady <- brady %>%
  rename(Law = `States can receive a maximum of 100 points`) %>% 
  filter(Law == "TOTAL STATE POINTS") %>%
  select((ncol(brady) - 49):ncol(brady)) %>%   #?
  pivot_longer(everything(), 
               names_to = "state",
               values_to = "brady_scores") %>%
  mutate_at("brady_scores", as.numeric)

# Only problem now is that we have the two letter state code, rather than the full state name we’ve been joining on so far here. We can, however, use the state datasets we used in the first case study here!

brady <- brady %>% 
  left_join(rename(state_data, state = abb), 
            by = "state") %>%
  select(Location, brady_scores) %>%
  rename(state = Location) %>%
  mutate(state = tolower(state))

# join with firearm data
firearms <- left_join(firearms, brady, 
                      by = c("NAME" = "state"))


## counted fatal shootings ----
names(counted15)

counted15 <- counted15 %>%
  mutate(state = tolower(state.name[match(state, state.abb)]))

# summarize this at the state level by ethnicity, gender, and armed status

# get overall stats
counted_stats <- counted15 %>%
  group_by(state) %>%
  filter(classification == "Gunshot") %>%
  tally() %>%
  rename("gunshot_tally" = "n")

# get summary for subset of population
gunshot_filtered <- counted15 %>%
  group_by(state) %>%
  filter(
    classification == "Gunshot",
    raceethnicity != "white", 
    armed != "No", 
    gender == "Male") %>%
  tally() %>% 
  rename("gunshot_filtered" = "n")

# join data together
counted_stats <- left_join(
  counted_stats, gunshot_filtered, by = "state") %>%
  mutate(
    total_pop = census_stats$total_pop[match(state, census_stats$NAME)],
    gunshot_rate = (gunshot_tally/total_pop)*1000000/2) %>% 
  select(-total_pop)

# join with firearm data
firearms <- left_join(firearms, counted_stats, 
                      by = c("NAME" = "state"))


## unemployment data ----
names(unemployment)

# Let’s first rename the columns to clean things up.
unemployment <- unemployment %>% 
  rename("state" = "State", 
         "unemployment_rate" = "2015rate", 
         "unemployment_rank" = "Rank") %>%
  mutate(state = tolower(state)) %>%
  arrange(state)

# join with firearm data
firearms <- left_join(firearms, unemployment, 
                      by = c("NAME" = "state"))


glimpse(firearms)

# convert type for unemployment columns
firearms <- firearms %>%
  mutate_at("unemployment_rate", as.numeric) %>%
  mutate_at("unemployment_rank", as.integer)


## population 2015
totalPop <- census %>%
  filter(ORIGIN == 0, SEX == 0 ) %>%  # 0 = total
  group_by(NAME) %>%
  summarize(total = sum(POPESTIMATE2015)) %>%
  mutate(NAME = tolower(NAME))

# Then, we select LND110210D by looking at the land table and comparing values on other sites (such as the census or Wikipedia) to find the correct column. This column corresponds to land area in square miles. We’ll convert all state names to lower case for easy merging with our growing data frame in a few steps.

landSqMi <- land %>%
  select(Areaname, land_area = LND110210D) %>% 
  mutate(Areaname = tolower(Areaname))

# We can then calculate density and remove the total and land_area columns to only keep state name and density for each state:
popdensity <- left_join(totalPop, landSqMi, by=c("NAME" = "Areaname")) %>% 
  distinct() %>%
  mutate(density = total/land_area) %>%
  select(-c(total, land_area))

# join with firearm data
firearms <- left_join(firearms, popdensity, 
                      by = "NAME")


## firearm ownership ----
# we calculate firearm ownership as a percent of firearm suicides to all suicides.
ownership_df <- as_tibble(list("NAME" = tolower(suicide_all$State), 
                               "ownership" = suicide_firearm$Deaths/suicide_all$Deaths*100))


# join with firearm data
firearms <- left_join(firearms, ownership_df, 
                      by = "NAME")


## file save ----
save(coverage, spending, file = here::here("course3", "data", "tidy_data", "case_study_2_tidy.rda"))


