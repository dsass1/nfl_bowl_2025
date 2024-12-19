
library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)
library(doParallel)
library(here)

set.seed(1234)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
# num_cores <- parallel::detectCores(logical = TRUE)
num_cores <- 8
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)

# load data
load(here("recipes/recipe_base.rda"))
load(here("recipes/split_down.rda"))

# model -------------------------------------------------------------------

bt_model <- boost_tree(mode = "classification",
                        trees = tune(),
                        min_n = tune(),
                        mtry = tune(),
                       learn_rate = tune()) %>% 
  set_engine("xgboost", importance = "impurity")

# define parameters to update
bt_params <- extract_parameter_set_dials(bt_model) %>% 
  update(mtry = mtry(c(5, 25)),
         trees = trees(c(500, 2000)),
         learn_rate = learn_rate(c(-5, -.1)),
         min_n = min_n(c(2, 40)))

bt_grid <- grid_regular(bt_params, 
                        levels = c(5, 4,
                                   10, 5))

# Create workflow
bt_workflow <- workflow() |> 
  add_model(bt_model) |> 
  add_recipe(recipe_base)

tuned_bt_base <- bt_workflow |> 
  tune_grid(folds,
            grid = bt_grid,
            control = control_grid(save_workflow = TRUE))

save(tuned_bt_base, 
     file = here("results/tuned_bt_base.rda"))

stopCluster(cl)