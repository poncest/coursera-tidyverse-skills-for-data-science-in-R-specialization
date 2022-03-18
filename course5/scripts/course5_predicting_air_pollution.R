
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

# After we fit our model, we can use the broom package to look at the output from the fitted model in an easy/tidy way.
wflowoutput <- PM_wflow_fit %>% 
  extract_fit_parsnip() %>% 
  broom::tidy() 

wflowoutput

# Let’s take a look at the top 10 contributing variables:
# install.packages('vip')
library(vip)

PM_wflow_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 10)


## Model Performance: Getting Predicted Values ----

#  let’s pull out our predicted outcome values from the models we fit (using different approaches).
wf_fit <- PM_wflow_fit %>% 
  extract_fit_parsnip()

wf_fitted_values <- wf_fit$fit$fitted.values
head(wf_fitted_values)


# Alternatively, we can get the fitted values using the augment() function of the broom package using the output from workflows:
wf_fitted_values <- 
  broom::augment(wf_fit$fit, data = preproc_train) %>% 
  select(value, .fitted:.std.resid)

head(wf_fitted_values)

# Finally, we can also use the predict() function. Note that because we use the actual workflow here, we can (and actually need to) use the raw data instead of the preprocessed data.
values_pred_train <- 
  predict(PM_wflow_fit, train_pm) %>% 
  bind_cols(train_pm %>% select(value, fips, county, id)) 

values_pred_train


## Visualizing Model Performance ----

# Now, we can compare the predicted outcome values (or fitted values)  to the actual outcome values that we observed:

wf_fitted_values %>% 
  ggplot(aes(x =  value, y = .fitted)) + 
  geom_point() + 
  xlab("actual outcome values") + 
  ylab("predicted outcome values")


## Quantifying Model Performance ----

# One way to calculate these metrics within the tidymodels framework is to use the yardstick package using the metrics() function.

yardstick::metrics(wf_fitted_values, 
                   truth = value, estimate = .fitted)


# Alternatively if you only wanted one metric you could use the mae(), rsq(), or rmse() functions, respectively.
yardstick::rmse(wf_fitted_values, 
                truth = value, estimate = .fitted)


## Assessing Model Performance on v -folds Using tune ----

# Again, because these are created at random, we need to use the base set.seed() function in order to obtain the same results each time. We will create 10 folds.
set.seed(1234)

vfold_pm <- rsample::vfold_cv(data = train_pm, v = 10)
vfold_pm

pull(vfold_pm, splits)

# We can fit the model to our cross validation folds using the fit_resamples() function of the tune package, by specifying our workflow object and the cross validation fold object we just created.

set.seed(122)
resample_fit <- tune::fit_resamples(PM_wflow, vfold_pm)

# now take a look at various performance metrics based on the fit of our cross validation “resamples”.
resample_fit

collect_metrics(resample_fit)


## Random Forest ---- 

# Now, we are going to predict our outcome variable (air pollution) using a decision tree method called random forest.
RF_rec <- recipe(train_pm) %>%
  update_role(everything(), new_role = "predictor")%>%
  update_role(value, new_role = "outcome")%>%
  update_role(id, new_role = "id variable") %>%
  update_role("fips", new_role = "county id") %>%
  step_novel("state") %>%
  step_string2factor("state", "county", "city") %>%
  step_rm("county") %>%
  step_rm("city") %>% ####
  step_rm("zcta") %>%
  step_corr(all_numeric())%>%
  step_nzv(all_numeric())

# Now that we have our recipe (RF_rec), let’s specify the model with rand_forest() from parsnip.
library(randomForest)

PMtree_model <- 
  parsnip::rand_forest(mtry = 10, min_n = 4)
PMtree_model

# Next, we set the engine and mode:
RF_PM_model <- 
  PMtree_model %>%
  set_engine("randomForest") %>%
  set_mode("regression")

RF_PM_model 


# Then, we put this all together into a workflow
RF_wflow <- workflows::workflow() %>%
  workflows::add_recipe(RF_rec) %>%
  workflows::add_model(RF_PM_model)
RF_wflow

# Finally, we fit the data to the model:
RF_wflow_fit <- parsnip::fit(RF_wflow, data = train_pm)
RF_wflow_fit

# Now, we will look at variable importance:
RF_wflow_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 10)

# Now let’s take a look at model performance by fitting the data using cross validation:
set.seed(456)
resample_RF_fit <- tune::fit_resamples(RF_wflow, vfold_pm)
collect_metrics(resample_RF_fit)

# OK, so our first model had a mean rmse value of 2.13. It looks like the random forest model had a much lower rmse value of 1.68.

# If we tuned our random forest model based on the number of trees or the value for mtry (which is “The number of predictors that will be randomly sampled at each split when creating the tree models”), we might get a model with even better performance.

# However, our cross validated mean rmse value of 1.68 is quite good because our range of true outcome values is much larger: (3.496, 22.259).


## Model Tuning ----
RF_PM_model <- 
  parsnip::rand_forest(mtry = 10, min_n = 4) %>%
  set_engine("randomForest") %>%
  set_mode("regression")

RF_PM_model

# we can use the tune() function of the tune package like so: mtry = tune(). This indicates that these hyperparameters are to be tuned.
tune_RF_model <- rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine("randomForest") %>%
  set_mode("regression")

tune_RF_model

# Again we will add this to a workflow
RF_tune_wflow <- workflows::workflow() %>%
  workflows::add_recipe(RF_rec) %>%
  workflows::add_model(tune_RF_model)
RF_tune_wflow

# You can see how many cores you have access to on your system using the 
# detectCores() function in the parallel package.
library(parallel)
parallel::detectCores()

# The registerDoParallel() function will use the number for cores specified using the cores= argument, or it will assign it automatically to one-half of the number of cores detected by the parallel package.
doParallel::registerDoParallel(cores=2)
set.seed(123)

tune_RF_results <- tune_grid(object = RF_tune_wflow, resamples = vfold_pm, grid = 20)
tune_RF_results

# Now we can use the collect_metrics() function again to take a look at what happened with our cross validation tests. We can see the different values chosen for mtry and min_n and the mean rmse and rsq values across the cross validation samples.

tune_RF_results%>%
  collect_metrics() %>%
  head()

# We can now use the show_best() function as it was truly intended, to see what values for min_n and mtry resulted in the best performance.
show_best(tune_RF_results, metric = "rmse", n =1)

# There we have it… looks like an mtry of 17 and min_n of 4 had the best rmse value 

## Final model performance evaluation -----

# So, first we need to specify these values in a workflow. We can use the select_best() function of the tune package to grab the values that were determined to be best for mtry and min_n.So, first we need to specify these values in a workflow. We can use the select_best() function of the tune package to grab the values that were determined to be best for mtry and min_n.

tuned_RF_values<- select_best(tune_RF_results, "rmse")
tuned_RF_values

# Now we can finalize the model/workflow that we we used for tuning with these values.
RF_tuned_wflow <-RF_tune_wflow %>%
  tune::finalize_workflow(tuned_RF_values)

# the results will show the performance using the testing data.
overallfit <-tune::last_fit(RF_tuned_wflow, pm_split)
# or
overallfit <-RF_wflow %>%
  tune::last_fit(pm_split)

# To see the performance on the test data we can use the collect_metrics() function like we did before.
collect_metrics(overallfit)

# ow if you wanted to take a look at the predicted values for the test set (the 292 rows with predictions out of the 876 original monitor values) you can use the collect_predictions() function of the tune package:
test_predictions <-collect_predictions(overallfit)
head(test_predictions)

# Now, we can compare the predicted outcome values (or fitted values) to the actual outcome values that we observed:
test_predictions %>% 
  ggplot(aes(x =  value, y = .pred)) + 
  geom_point() + 
  xlab("actual outcome values") + 
  ylab("predicted outcome values") +
  geom_smooth(method = "lm")


sessionInfo()








































































































