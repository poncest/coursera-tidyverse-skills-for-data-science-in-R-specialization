
# Wrangling Data in the Tidyverse Course Project
# Author: Steven Ponce
# Date: 2022-02-13

library(pacman)
p_load(tidyverse, janitor, skimr, lubridate, tidytext)


# load the data and lets change the date format
complaints <- read_csv('course3/project/data_complaints.csv') %>% 
  clean_names() %>% 
  mutate(date_received = mdy(date_received),
         date_sent_to_company = mdy(date_sent_to_company)) 


# examine the data
names(complaints)
glimpse(complaints)
skim(complaints)


## Q1. How many student loan complaints did the US state with second most complaints have? ----

# Consider the column titled `Date received` and filter rows for all Student loan complaints received after January 1, 2018.  Count the number of student loan complaints received by each US state and rank in a decreasing order with the state with the most complaints at the top and the states with the least complaints at the bottom. 

q1 <- complaints %>% 
  filter(product == 'Student loan' & date_received >= '2018-01-01') %>% 
  select(state, product) %>% 
  group_by(state, product) %>% 
  summarize(N=n()) %>% 
  arrange(desc(N))

table(q1[2,3])

## [OUT] NY 1374 
 
 
## Q2. Consider all complaints that were submitted by fax. what is the average amount of time between the complaint being received by CFPB (`Date received`) and the CFPB sending the complaint to the company (`Date sent to company`) ? ----

# Round to the nearest tenths digit (1 dp). 

rm(q1)

q2 <- complaints %>% 
  filter(submitted_via == 'Fax') %>% 
  
  select(product, submitted_via, date_received, date_sent_to_company) %>%  
  mutate(time_diff = date_sent_to_company - date_received) %>% 
  arrange(product)

q2 %>% 
  pull(time_diff) %>% 
  mean()
  
## [OUT] Time difference of 1.723363 days = 1.7 days   
    
  
## Q3. Consider all Credit card or prepaid carde complaints.  How many times does the string "student" appear in the string of text in the `Consumer complaint narrative` column? ----

rm(q2)

q3 <- complaints %>% 
  filter(product == 'Credit card or prepaid card') %>% 
  select(state, product, consumer_complaint_narrative)

student_df <- q3 %>% 
  select(consumer_complaint_narrative) %>% 
  drop_na() 

student_count <- str_extract_all(student_df, 'student')  %>% 
  table()  %>% 
  print()

## [OUT] `student` appears 222 times  


## Q4. Consider only Vehicle loan or lease complaints. ----

# The `Issue` column contains a set of plain text categorical variables that the consumer identified in the complaint. For example, one type of issue is "Trouble during payment process" for Mortgage complaints.       

# For each `Issue`, calculate what is the average length of characters in the string text in the `Consumer complaint narrative` column. 

# Which `Issue` has the longest complaint length on average? 

rm(q3, student_df, student_count)

q4 <- complaints %>% 
  filter(product == 'Vehicle loan or lease') %>% 
  group_by(issue) %>% 
  select(state, product, issue, consumer_complaint_narrative) %>% 
  drop_na() %>% 
  arrange(issue)
  
character_df <- q4 %>% 
  mutate(char_legnth =  str_length(consumer_complaint_narrative)) %>% 
  group_by(issue) %>% 
  summarise(mean_char_lenght = mean(char_legnth)) %>% 
  arrange(desc(mean_char_lenght))

## [OUT] 	`Getting a loan or lease` has the longest complaint length on average


## Q5. Consider only Mortgage or lease complaints. Consider the two columns `Product` and `Consumer complaint narrative`. ----

# For the string text in the `Consumer complaint narrative` column, drop any rows with `NA` in the column and remove any strings such as "XX", "XXX", "XXXX" in the complaint.

# Transform this data frame with two columns into a data frame with three columns (`Product`, `id_number`, and `word`)  where the text in the `Consumer complaint narrative` column gets converted into a format where each row corresponds to one word of the complaint (`word`) while also keeping track of which complaint we are working with (`id_number)`. 
                      
# Remove any stop words.  

# What are the top three most frequently used words for this type of complaint?  

rm(q4, character_df)

q5 <- complaints %>% 
  filter(product == 'Mortgage') %>% 
  select(complaint_id, product, consumer_complaint_narrative) %>% 
  drop_na() %>% 
  # remove any strings such as "XX", "XXX", "XXXX" in the complaint
  filter(!str_detect(consumer_complaint_narrative, '[X]+'))
 
# includes all words
all_text_df <- q5 %>% 
  tibble(text = consumer_complaint_narrative, title = product) %>% 
  unnest_tokens(word, text) %>% 
  count(product, consumer_complaint_narrative, word, sort = TRUE) %>%   # remove complaint_id, consumer_complaint_narrative,
  arrange(desc(n)) 

# without stop words
no_stop_text_df <- all_text_df %>%
  bind_tf_idf(word, consumer_complaint_narrative, n) %>% 
  group_by(consumer_complaint_narrative, product) %>% 
  arrange(desc(tf_idf))

## [OUT] 	payment, payments, loan.

## Something is not quite right.  I still need it to sort by n and manually search for the non-stop words.

