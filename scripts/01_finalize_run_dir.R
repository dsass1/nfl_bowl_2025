
library(tidyverse)
library(nflverse)
library(here)

# get function to compute left, middle, right location
source(here("scripts/00_functions.R"))

# load data
game_week <- read_csv(here("data/raw/games.csv")) |> 
  janitor::clean_names() |> 
  select(game_id, week)

# from 00_prep_run_data
run_plays <- read_rds(here("data/run_plays.rds")) |> 
  left_join(game_week)

# from 00_prep_run_data
tracking_runs <- read_rds(here("data/tracking_runs.rds"))
############################################################
# calculate run direction
run_list <- lapply(run_plays$unique_id, 
                       \(x) define_run(
                         tracking_data = tracking_runs,
                         unique_tag = x) )
# convert to data frame
run_dir_calc <- bind_rows(run_list)

write_rds(run_dir_calc, here("data/run_dir_calc.rds"))

###############################################################
# join with rest of data
run_plays_dir <- run_plays |> 
  left_join(run_dir_calc)

write_rds(run_plays_dir, here("data/run_plays_dir.rds") )
