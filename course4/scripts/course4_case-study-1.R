
# Case Study #1: Health Expenditures ----

library(pacman)
p_load(tidyverse, skimr, janitor)


## Load the data -----
load('course4/data/tidy_data/case_study_1_tidy.rda') 


## Exploratory Data Analysis (EDA) ----

# get summary of the data
skim(hc)

# group by year
hc %>% 
  group_by(year) %>%
  skim()

## Q1: Relationship between coverage and spending? ----

hc %>%
  filter(type == "Employer", 
         year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage)) +
  geom_point() + 
  labs(x = "spending per capita",
       y = "coverage proportion") + 
  theme_minimal()

# generate scatterplot with fit line
hc %>%
  filter(type == "Employer", 
         year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage)) + 
  geom_point() + 
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") +
## `geom_smooth()` using formula 'y ~ x'
  theme_minimal()

# add state abbreviation labels
hc %>%
  filter(type == "Employer", 
         year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage)) + 
  geom_point() + 
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150) + 
## `geom_smooth()` using formula 'y ~ x'
  theme_minimal()


# color by region
hc %>%
  filter(type == "Employer", 
         year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150, 
            show.legend = FALSE) +
## `geom_smooth()` using formula 'y ~ x'
  theme_minimal()


# color by region
hc %>%
  filter(type == "Employer") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  facet_wrap(~year) +
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150, 
            show.legend = FALSE) +
  theme_minimal()


# visualize 2013 data by type
hc %>%
  filter(year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  facet_wrap(~type) +
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150, 
            show.legend = FALSE) +
  theme_minimal()


# visualize 2014 data by type
hc %>%
  filter(year == "2014") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  facet_wrap(~type) +
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150, 
            show.legend = FALSE) +
  theme_minimal()



## final plots

hc %>%
  filter(type == "Employer") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  facet_wrap(~year) +
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb),  
            nudge_x = 150, 
            show.legend = FALSE)

ggsave("course4/figures/exploratory/2013and2014_spending_and_coverage.pdf")



hc %>%
  filter(year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  facet_wrap(~type) +
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150, 
            show.legend = FALSE)

ggsave("course4/figures/exploratory/2013_coverage_type.pdf")


hc %>%
  filter(year == "2014") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  facet_wrap(~type) +
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150, 
            show.legend = FALSE)

ggsave("course4/figures/exploratory/2014_coverage_type.pdf")
       

## Q2: Spending Across Geographic Regions? ----

# generate boxplot
hc %>% 
  ggplot(aes(x = region, 
             y = spending_capita)) + 
  geom_boxplot() +
  labs(y = "spending per capita") + 
  theme_minimal()


# add data points to boxplot
hc %>% 
  filter(type == "Employer") %>%
  ggplot(aes(x = region, 
             y = spending_capita)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.2) +
  labs(y = "spending per capita") +  
  theme_minimal()



## Q3: Coverage and Spending Change Over Time? ----

# color by region
hc %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  facet_grid(year~type) +  
  theme_minimal()




