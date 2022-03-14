
## Course5: Modeling Data in the Tidyverse ----

## WEEK 3 ----

library(tidyverse)
library(tidymodels)

trees %>%
    ggplot() + 
    geom_point(aes(Height, Girth))

## run the regression
fit <- lm(Girth ~ Height , data = trees)
fit


par(mfrow = c(2, 2))
plot(fit)


## take a look at the output
summary(fit)


library(broom)
tidy(fit)

## WEEK 4 -----

## take a look at scatterplot
ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

## model the data without confounder
fit <- lm(mpg ~ wt, data = mtcars)
tidy(fit)

## look at the difference in relationship 
## between Engine types
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  facet_wrap(~vs)

## include engine (vs) as a confounder
fit <- lm(mpg ~ wt + vs, data = mtcars)
tidy(fit)


## WEEK 5 ----

## generate the dataset
set.seed(34) 
soda_ounces <- rnorm(100, mean = 12, sd = 0.04)
head(soda_ounces)

## check for normality
ggplot(as.data.frame(soda_ounces))+
  geom_histogram(aes(soda_ounces), bins = 10)

## carry out t-test
t.test(soda_ounces, mu = 12)

# from linear regression
regression_output <-  lm(soda_ounces ~ 1)

# calculate confidence interval
confint(regression_output)


## WEEK 6 -----
soda_ounces <-as_tibble(soda_ounces)
soda_ounces

library(infer)
set.seed(342)

CI <-soda_ounces %>%
  specify(response = value) %>%
  hypothesize(null = "point", mu = 12) %>%
  generate(rep = 1000, type = "bootstrap") %>%
  calculate(stat = "mean") %>% 
  get_confidence_interval()

CI

set.seed(342)

bootstrap_means <-soda_ounces %>%
  specify(response = value) %>%
  hypothesize(null = "point", mu = 12) %>%
  generate(rep = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

bootstrap_means %>%
  visualize()

## WEEK 8

set.seed(1234)
split_iris <-initial_split(iris, prop = 2/3) 
split_iris

training_iris <-training(split_iris)
head(training_iris)

testing_iris <-testing(split_iris)
head(testing_iris)

first_recipe <- training_iris %>%
  recipe(Sepal.Length ~ Sepal.Width + Species)
first_recipe


first_recipe <- recipe(training_iris) %>%
  recipes::update_role(Sepal.Length, new_role = "outcome")  %>%
  recipes::update_role(Sepal.Width, new_role = "predictor") %>%
  recipes::update_role(Species, new_role = "predictor")
first_recipe


summary(first_recipe)

first_recipe <- first_recipe %>%
  step_dummy(Species, one_hot = TRUE)

first_recipe

prepped_rec <- prep(first_recipe, verbose = TRUE, retain = TRUE )
prepped_rec 
names(prepped_rec)

prepped_rec$var_info

preproc_train <-recipes::bake(prepped_rec, new_data = NULL)
glimpse(preproc_train)

baked_test_pm <- recipes::bake(prepped_rec, new_data = testing_iris)  
glimpse(baked_test_pm)

Lin_reg_model <- parsnip::linear_reg()
Lin_reg_model

Lin_reg_model <- 
  Lin_reg_model  %>%
  parsnip::set_engine("lm")

Lin_reg_model


Lin_reg_model <- 
  Lin_reg_model %>%
  parsnip::set_engine("lm") %>%
  parsnip::set_mode("regression")

Lin_reg_model

iris_reg_wflow <-workflows::workflow() %>%
  workflows::add_recipe(first_recipe) %>%
  workflows::add_model(Lin_reg_model)
iris_reg_wflow

iris_reg_wflow_fit <- parsnip::fit(iris_reg_wflow, data = training_iris)
iris_reg_wflow_fit

wf_fit <- iris_reg_wflow_fit %>% 
  pull_workflow_fit()

head(wf_fit$fit$fitted.values)

predict(iris_reg_wflow_fit, new_data = training_iris)


wf_fitted_values <- 
  broom::augment(wf_fit$fit, data = preproc_train) %>% 
  select(Sepal.Length, .fitted:.std.resid)

head(wf_fitted_values)


yardstick::rmse(wf_fitted_values, 
                truth = Sepal.Length, 
                estimate = .fitted)
 

wf_fitted_values %>%
  ggplot(aes(x = Sepal.Length, y = .fitted)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs( x = "True Sepal Length", y = "Predicted Sepal Length")


overallfit <-iris_reg_wflow %>%
  tune::last_fit(split_iris)


collect_metrics(overallfit)

set.seed(1234)
initial_split(iris, strata = Species, prop = 2/3)

training_iris <-training(split_iris)
head(training_iris)

count(training_iris, Species)


testing_iris <-testing(split_iris)
head(testing_iris)

count(testing_iris, Species)


set.seed(1234)
vfold_iris <- rsample::vfold_cv(data = training_iris, v = 4)
vfold_iris

pull(vfold_iris, splits)

first_fold <-vfold_iris$splits[[1]]
head(as.data.frame(first_fold, data = "analysis")) 
head(as.data.frame(first_fold, data = "assessment"))


cat_recipe <- training_iris %>%
  recipe(Species ~ .)

library(rpart)
cat_model <- parsnip::decision_tree() %>%
  parsnip::set_mode("classification") %>%
  parsnip::set_engine("rpart")
cat_model


iris_cat_wflow <-workflows::workflow() %>%
  workflows::add_recipe(cat_recipe) %>%
  workflows::add_model(cat_model)
iris_cat_wflow


iris_cat_wflow_fit <- parsnip::fit(iris_cat_wflow, data = training_iris)
iris_cat_wflow_fit


wf_fit_cat <- iris_cat_wflow_fit %>% 
  pull_workflow_fit()


wf_fit_cat$fit$variable.importance

pred_species<-predict(iris_cat_wflow_fit, new_data = training_iris)

yardstick::accuracy(training_iris, 
                    truth = Species, estimate = pred_species$.pred_class)

count(training_iris, Species)

count(pred_species, .pred_class)

predicted_and_truth <-bind_cols(training_iris, 
                                predicted_species = pull(pred_species, .pred_class))

head(predicted_and_truth)

library(tune)
set.seed(122)
resample_fit <- tune::fit_resamples(iris_cat_wflow, vfold_iris)

resample_fit
collect_metrics(resample_fit)


set.seed(122)
library(tune)
cat_model_tune <- parsnip::decision_tree(min_n = tune()) %>%
  parsnip::set_mode("classification") %>%
  parsnip::set_engine("rpart") 
cat_model_tune


iris_cat_wflow_tune <-workflows::workflow() %>%
  workflows::add_recipe(cat_recipe) %>%
  workflows::add_model(cat_model_tune)

reasmple_fit <-tune::tune_grid(iris_cat_wflow_tune, resamples = vfold_iris, grid = 4)

tune::collect_metrics(resample_fit)
tune::show_best(resample_fit, metric = "accuracy")



























































