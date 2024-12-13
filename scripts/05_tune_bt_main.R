
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
registerDoParallel(cores = 8)

# load data
load(here("recipes/recipe_main.rda"))
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
  update(mtry = mtry(c(1, 25)))

bt_grid <- grid_regular(bt_params, levels = 5)

# Create workflow
bt_workflow <- workflow() |> 
  add_model(bt_model) |> 
  add_recipe(recipe_main)

tuned_bt_main <- bt_workflow |> 
  tune_grid(folds,
            grid = bt_grid,
            control = control_grid(save_workflow = TRUE))

save(tuned_bt_main, 
     file = here("results/tuned_bt_main.rda"))
