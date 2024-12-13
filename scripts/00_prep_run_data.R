# load packages
library(tidyverse)
library(here)

plays <- read_csv(here("data/raw/plays.csv")) |> 
  janitor::clean_names()

players <- read_csv(here("data/raw/players.csv")) |> 
  janitor::clean_names()

tracking <- here("data/raw/") |> 
  list.files() |> 
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
#2022092509 2829
#2022103000 2274

visual_run_1 <- tracking_runs |> 
  filter(game_id == 2022092509 & play_id == 2829)
visual_run_2 <- tracking_runs |> 
  filter(game_id == 2022103000 & play_id == 2274)

write_rds(visual_run_1, "data/visual_run_1.rds")  
write_rds(visual_run_2, "data/visual_run_2.rds")  

###########################################################
###########################################################
# data to visualize run gap definition
set.seed(711)
rand_id <- tracking_runs |> 
  distinct(game_id) |> 
  pull(game_id) |> 
  sample(10)

rand_play <- tracking_runs |> 
  filter(game_id %in% rand_id) |> 
  group_by(game_id) |> 
  summarize(play_id = sample(play_id, 1))

for(i in 1:10){
  visual_gap <- tracking_runs |> 
    filter(game_id %in% rand_play$game_id[i] &
             play_id %in% rand_play$play_id[i])
  
  write_rds(visual_gap, paste0("data/visual_gap_",i,".rds"))
}


