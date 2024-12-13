# load libraries
library(tidyverse)
library(here)
library(aspace)

# get function to calculate qb and rb relationships
source(here("scripts/00_functions.R"))

# needed data
games <- read_csv(here("data/raw/games.csv")) |> 
  janitor::clean_names()

# from 00_prep_run_data
run_plays <- read_rds(here("data/run_plays.rds"))

# from 00_prep_run_data
tracking_runs <- read_rds(here("data/tracking_runs.rds"))

#####################################################
# offensive line gaps for predicting run direction?
#####################################################
# we will calculate potential gaps in 2 ways
# direction_o: based on offensive player orientation
# direction_tm: based on if there are two offense in a row with no defender between

plays_run <- run_plays |> 
  # add week
  left_join(games[,c("game_id", "week")]) |>
  relocate(week, .after = "game_id")

# calculate run gap
run_gap_list <- lapply(plays_run$unique_id, 
                     \(x) run_gap(
                       tracking_data = tracking_runs,
                       unique_tag = x) )

# convert to data frame
run_gap_calc <- bind_rows(run_gap_list)


write_rds(run_gap_calc, file = here("data/run_gap_calc.rds"))
