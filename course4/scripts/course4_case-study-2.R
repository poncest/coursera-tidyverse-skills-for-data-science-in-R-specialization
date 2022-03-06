
# Case Study #2: Firearms ----

library(pacman)
p_load(tidyverse, skimr, ggrepel)


## Load the data -----
load('course4/data/tidy_data/case_study_2_tidy.rda') 


## Exploratory Data Analysis (EDA) ----

# get summary of the data
skim(firearms)


# violent crimes
ggplot(firearms, 
       aes(x = total_pop,
           y = violent_crime)) + 
  geom_point() +
  labs(x = "Total Population",
       y = "Violent Crime") + 
  theme_classic() + 
  geom_text_repel(aes(label = NAME))


# violent crimes
ggplot(firearms, 
       aes(x = unemployment_rate,
           y = violent_crime)) + 
  geom_point() +
  labs(x = "Unemployment Rate",
       y = "Violent Crime") + 
  theme_classic()


# violent crimes
ggplot(firearms, 
       aes(x = gunshot_rate,
           y = ownership)) + 
  geom_point() +
  labs(x = "Annualized Rate of Fatal \n Police Shootings per 1,000,000",
       y = "Gun Ownership \n (firearm suicides:all suicides)") + 
  theme_classic()


## Q: Relationship between Fatal Police Shootings and Legislation? ----

# visualize legistlation and shootings
ggplot(firearms, aes(x = brady_scores, 
                     y = gunshot_rate)) + 
  geom_point() +
  labs(x = "Firearm Legislative Strength Score",
       y = "Annualized Rate of Fatal \n Police Shootings per 1,000,000") + 
  theme_classic()


# label points with state name
ggplot(firearms, aes(x = brady_scores, 
                     y = gunshot_rate)) + 
  geom_point() +
  labs(x = "Firearm Legislative Strength Score",
       y = "Annualized Rate of Fatal \n Police Shootings per 1,000,000") + 
  theme_classic() +
  geom_text(aes(label = NAME), 
            nudge_x = 7)


# repel points with state name 
ggplot(firearms, aes(x = brady_scores, 
                     y = gunshot_rate)) + 
  geom_point() +
  labs(x = "Firearm Legislative Strength Score",
       y = "Annualized Rate of Fatal \n Police Shootings per 1,000,000") + 
  theme_classic() +
  geom_text_repel(aes(label = NAME))



## Save ----
ggsave("course4/figures/exploratory/Fatal_police_shootings_and_firearm_legislative_strength.pdf")
      

