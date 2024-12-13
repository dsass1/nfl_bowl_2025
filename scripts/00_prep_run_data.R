# load packages
library(tidyverse)
library(here)

plays <- read_csv(here("data/raw/plays.csv")) |> 
  janitor::clean_names()

players <- read_csv(here("data/raw/players.csv")) |> 
  janitor::clean_names()

# credit: https://github.com/qntkhvn/tackle
tracking <- list.files(here("data/raw/")) |>
  str_subset("tracking_week_") |> 
  str_c("data/raw/", `...` = _) |> 
  map(read_csv) |> 
  list_rbind()

# 6183 run plays total
run_plays <- plays |> 
  # only plays that are a run
  filter(!is.na(rush_location_type)) |>
  # this is what we are predicting; uninterested in UNKNOWNS
  filter(rush_location_type != "UNKNOWN") |> 
  # we defined scrambles as intended pass plays
  filter(!str_detect(dropback_type, "SCRAMBLE") | 
           is.na(dropback_type)) |> 
  # unique_id for matching
  mutate(unique_id = paste0(game_id, "_", play_id), .before = 0)

# filter on only run plays
tracking_runs <- tracking |> 
  janitor::clean_names() |> 
  mutate(unique_id = paste0(game_id, "_", play_id)) |>
  filter(unique_id %in% run_plays$unique_id)  |> 
  left_join(players[,c("nfl_id", "position")])

# save out our main run plays datasets
write_rds(run_plays, here("data/run_plays.rds") )
write_rds(tracking_runs, here("data/tracking_runs.rds"), 
          compress = "gz")

###########################################################
###########################################################
# data to visualize run direction definition
visual_run <- tracking_runs |> 
  filter(game_id == 2022092509 & play_id == 2829)

# visual_run_2 <- tracking_runs |> 
#  filter(game_id == 2022103000 & play_id == 2274)

write_rds(visual_run, here("data/visual_run.rds") )
# write_rds(visual_run_2, "data/visual_run_2.rds")  

###########################################################
###########################################################
# data to visualize run gap definition
visual_gap <- tracking_runs |> 
    filter(game_id == 2022092509&
             play_id ==3135)
  
write_rds(visual_gap, here("data/visual_gap.rds"))
