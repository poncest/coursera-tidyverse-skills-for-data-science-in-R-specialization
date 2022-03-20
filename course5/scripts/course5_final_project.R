# Peer-graded Assignment: Modeling Data in the Tidyverse Course Project

# Background
# In this course, we have learned about modeling data in the Tidyverse in R. This project will give you the opportunity to practice those skills in greater depth. 

# Here, we will continue to use consumer complaints data from the Consumer Complaint Database (CFPB) that was used in the Wrangling Data in the Tidyverse Course Project. The CFPB is an independent agency of the United States government that promotes transparency and protects consumers by providing information needed to make decisions when choosing financial institutions including banking institutions, lenders, mortgage services, credit unions, securities firms, foreclosure services, and debt collectors. One of the purposes of the agency is to receive and process complaints and questions about consumer financial products and services. 

# When a complaint is submitted by a consumer, the CFPB has to determine which category the complaint falls in (e.g. "Mortgage", "Student loan", etc). In this project, your goal will be to use the skills you have learned about in this course to build a classification algorithm to classify consumer complaints into one of four categories: "Credit card or prepaid card", "Mortgage", "Student loan", or "Vehicle loan or lease". 

# Data 

# Training data -- complaints from CFPB (data_complaints_train.csv) 
# Testing data -- complaints from CFPB (data_complaints_test.csv) 


## Libraries and Data Import ----
# libraries
library(pacman)
p_load(tidyverse, tidymodel, tidytext, skimr, tm, here, janitor)


# data
train <- read_csv(here('course5', 'data', 'tidy_data', 'data_complaints_train.csv')) %>% 
  clean_names()

test <- read_csv(here('course5', 'data', 'tidy_data', 'data_complaints_test.csv')) %>% 
  clean_names()


## Explore the data ----
glimpse(train)
glimpse(test)

skim(train)
skim(test)

unique(train$product)
unique(train$submitted_via)

# count / pct by product
train %>% 
  group_by(product) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count / sum(count) * 100 ) %>% 
  ungroup() %>% 
  arrange(desc(count))

## Tidydata ----

train_clean_tbl <- train %>% 
  # select specific cols
  select(product, consumer_complaint_narrative) %>% 
  
  # remove any strings such as "XX", "XXX", "XXXX" in the complaint
  filter(!str_detect(consumer_complaint_narrative, '[X]+')) %>% 
  
  # lower case 
  mutate(
    product = str_to_lower(product),
    consumer_complaint_narrative = str_to_lower(consumer_complaint_narrative)
  )  %>% 
  
  # rename column
  rename(complaint = consumer_complaint_narrative) %>% 
  
  # remove digits and punctuation
  mutate(
    complaint = removePunctuation(complaint),
    complaint = stripWhitespace(complaint),
    complaint = gsub('[0-9]+', "", complaint),
    # complaint = complaint %>% str_replace(pattern = '[0-9]+', replacement = ""),
    complaint = complaint %>% str_trim()
  ) %>% 
  
  mutate(product = as.factor(product))


## Text Pre-Processing ----
# Corpus - collection of text documents.
complaints_corpus <- Corpus(VectorSource(train_clean_tbl$complaint))


## Document-Term Matrix ----
complaints_dtm <- DocumentTermMatrix(complaints_corpus, 
                                     control = list(
                                       stripWhitespace = TRUE,
                                       stopwords = TRUE,
                                       stemming = TRUE,
                                       removeNumbers = TRUE,
                                       removePunctuation = TRUE,
                                       wordLengths = c(4, Inf)))

inspect(complaints_dtm)




complaints_dtm <- removeSparseTerms(complaints_dtm, 0.95)
inspect(complaints_dtm)


## Model ----

dtm_mat <- 


  
  




