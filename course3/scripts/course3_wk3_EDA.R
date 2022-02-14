
# Course 3
# Week 3 - General Principles of EDA

library(pacman)
p_load(tidyverse, skimr, janitor)

# load the data
df<- read_csv('course3/data/raw_data/athlete_events.csv',
                col_types = cols(Year = col_character())) %>% 
    clean_names()
  
#  examing the data
skim(df)

# histogram
ggplot(df, aes(x = age)) +
    geom_histogram(color = 'black', fill = 'white')

# box plot
ggplot(df, aes(x = sex,
               y = age)) + 
    geom_boxplot()

# distribution - gender
share_woman <- df %>% 
    group_by(year, sex) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n / sum(n)) %>% 
    filter(sex =='F')

ggplot(share_woman, aes(x = year, y = freq)) + 
    geom_point() +
    geom_line() +
    ylab('Share of Women')


# are taller athletes more likely to win a medal? 

df <- df %>% 
    mutate(has.medal = medal %in% c('Gold', 'Silver', 'Bronze'))

table(df$has.medal)

ggplot(df, aes(x = has.medal,
               y = height)) +
    geom_boxplot() +
    scale_x_discrete(limits = c('TRUE', 'FALSE'))+
    labs(title="Are taller athletes more likely to win a medal?",
         y = 'Height',
         x = 'Has Medal')


    
