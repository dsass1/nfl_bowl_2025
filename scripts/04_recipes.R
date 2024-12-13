
library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)
library(doParallel)
library(themis)

# handle common conflicts
tidymodels_prefer()

# load data
run_plays_final <- read_rds("data/run_plays_final.rds")

# no missing issues
naniar::miss_var_summary(run_plays_final)

# split the data
train <- run_plays_final |> 
  filter(week <= 6)

train |> 
  count(rush_loc_calc)

set.seed(622)
train_down <- train |> 
  group_by(rush_loc_calc) |> 
  sample_n(894)

train_down |> 
  count(rush_loc_calc)

folds <- vfold_cv(train_down, v = 4, repeats = 3, strata = rush_loc_calc)

test <- run_plays_final |> 
  filter(week > 6)

save(train_down, test, folds, 
     file = "recipes/split_down.rda")

#load("data/split_1.rda")

# recipe ------------------------------------------------------------------
recipe_main <- recipe(rush_loc_calc ~ .,
                       data = train_down) |> 
  step_rm(unique_id, game_id, play_id, week,
          rush_location_type,
          pff_run_concept_primary, pff_pass_coverage
  ) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors()) |> 
  step_corr(all_predictors()) |> 
  step_normalize(all_predictors())


check <- prep(recipe_main) |>  
  bake(new_data = NULL)

save(recipe_main, 
     file = "recipes/recipe_main.rda")
################################################################
################################################################
################################################################
# BASELINE NO COMPUTED VARIABLES
################################################################
################################################################
################################################################
recipe_base <- recipe(rush_loc_calc ~ quarter + down + yards_to_go +
                      possession_team + defensive_team + absolute_yardline_number +
                      offense_formation + receiver_alignment + play_clock_at_snap +
                      pff_run_pass_option + pff_man_zone + 
                      num_rb + num_wr + no_huddle,
                   data = train_down) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors()) |> 
  step_corr(all_predictors()) |> 
  step_normalize(all_predictors())


check <- prep(recipe_base) |>  
  bake(new_data = NULL)

save(recipe_base, 
     file = "recipes/recipe_base.rda")
