
## Case Study #1: Predicting Annual Air Pollution

library(pacman)
p_load(tidyverse, skimr, corrplot, tidymodels)


## data import ----
pm <- read_csv('course5/data/tidy_data/pm25_data.csv')

## examine the data ----
glimpse(pm)
skim(pm)


# convert `id, fips, zcta` to factors
pm <-pm %>%
  mutate(across(c(id, fips, zcta), as.factor)) 

pm %>% 
  select(id, fips, zcta) %>% 
  glimpse()
  
# Let’s take a look to see which states are included using the distinct() function of the dplyr package:

pm %>% 
  distinct(state)

## Evaluate correlation ----

# Pearson correlation coefficients
PM_cor <- cor(pm %>% dplyr::select_if(is.numeric))

# correlation
corrplot::corrplot(PM_cor, tl.cex = 0.5)


## splitting the data ----
set.seed(1234)
pm_split <- rsample::initial_split(data = pm, prop = 2/3)
pm_split

# extract the testing and training data
train_pm <-rsample::training(pm_split)
test_pm <-rsample::testing(pm_split)


## making a recipe ----
simple_rec <- train_pm %>%
  recipes::recipe(value ~ .)

simple_rec

simple_rec <- train_pm %>%
  recipes::recipe(value ~ .) %>%
  recipes::update_role(id, new_role = "id variable")

simple_rec

# We want to dummy encode our categorical variables so that they are numeric as we plan to use a linear regression for our model.
simple_rec %>%
  step_dummy(state, county, city, zcta, one_hot = TRUE)

# We can remove the fips variable from the predictors using update_role() to make sure that the role is no longer "predictor".
simple_rec %>%
  update_role("fips", new_role = "county id")

simple_rec %>%
  step_corr(all_predictors(), - CMAQ, - aod)


# Remember: it is important to add the steps to the recipe in an order that makes sense just like with a cooking recipe
simple_rec


## Running Preprocessing ----
prepped_rec <- prep(simple_rec, verbose = TRUE, retain = TRUE)
names(prepped_rec)


preproc_train <- bake(prepped_rec, new_data = NULL)
glimpse(preproc_train)

# original data
glimpse(pm)


# Extract preprocessed testing data using bake()
baked_test_pm <- recipes::bake(prepped_rec, new_data = test_pm)

glimpse(baked_test_pm)


traincities <- train_pm %>% distinct(city)
testcities <- test_pm %>% distinct(city)

pm %>%
  mutate(city = case_when(city == "Not in a city" ~ "Not in a city",
                          city != "Not in a city" ~ "In a city"))

# alternative
pm %<>%
  mutate(city = case_when(city == "Not in a city" ~ "Not in a city",
                          city != "Not in a city" ~ "In a city"))

set.seed(1234) # same seed as before
pm_split <-rsample::initial_split(data = pm, prop = 2/3)
pm_split

train_pm <-rsample::training(pm_split)
test_pm <-rsample::testing(pm_split)

# new recipe
novel_rec <-train_pm %>%
  recipe() %>%
  update_role(everything(), new_role = "predictor") %>%
  update_role(value, new_role = "outcome") %>%
  update_role(id, new_role = "id variable") %>%
  update_role("fips", new_role = "county id") %>%
  step_dummy(state, county, city, zcta, one_hot = TRUE) %>%
  step_corr(all_numeric()) %>%
  step_nzv(all_numeric()) 

# check the preprocessed data again to see if we still have NA values.
prepped_rec <- prep(novel_rec, verbose = TRUE, retain = TRUE)

preproc_train <- bake(prepped_rec, new_data = NULL)
glimpse(preproc_train)

baked_test_pm <- recipes::bake(prepped_rec, new_data = test_pm)

glimpse(baked_test_pm)


## Specifying the Model ----
PM_model <- parsnip::linear_reg() # PM was used in the name for particulate matter
PM_model

# We would like to use the ordinary least squares method to fit our linear regression.
lm_PM_model <- 
  PM_model  %>%
  parsnip::set_engine("lm")

lm_PM_model

# Here, we aim to predict the air pollution. You can do this with the set_mode() function of the parsnip package, by using either set_mode("classification") or set_mode("regression").
lm_PM_model <- 
  PM_model  %>%
  parsnip::set_engine("lm") %>%
  set_mode("regression")

lm_PM_model

# If you recall novel_rec is the recipe we previously created with the recipes package and lm_PM_model was created when we specified our model with the parsnip package. Here, we combine everything together into a workflow.
PM_wflow <-workflows::workflow() %>%
  workflows::add_recipe(novel_rec) %>%
  workflows::add_model(lm_PM_model)

PM_wflow

# Next, we “prepare the recipe” (or estimate the parameters) and fit the model to our training data all at once. Printing the output, we can see the coefficients of the model.
PM_wflow_fit <- parsnip::fit(PM_wflow, data = train_pm)

PM_wflow_fit

## Assessing the Model Fit ----












