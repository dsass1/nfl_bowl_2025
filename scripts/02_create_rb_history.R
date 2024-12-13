
library(tidyverse)
library(here)
library(janitor)

#conflicted::conflicts_prefer(base)

game_week <- read_csv(here("data/raw/games.csv")) |> 
  janitor::clean_names() |> 
  select(game_id, week)

player_play <- read_csv(here("data/raw/player_play.csv")) |> 
  janitor::clean_names()

players <- read_csv(here("data/raw/players.csv")) |> 
  janitor::clean_names() |> 
  rename("full_name" = "display_name")

# from 00_prep_run_data
run_plays <- read_rds(here("data/run_plays.rds"))

################################################################
# which players ran?
player_run <- player_play |> 
  filter(had_rush_attempt == 1) |> 
  select(game_id, play_id, nfl_id, 
         possession_team = team_abbr,
         rusher_motion_snap = in_motion_at_ball_snap, 
         rusher_shift_line = shift_since_lineset,
         rusher_motion_line = motion_since_lineset)

# add variables to help in calculations
run_plays <- run_plays |>
  mutate(
    #run_side = ifelse(str_detect(rush_location_type, "LEFT"), "left", "right"),
    run_loc = case_when(
      str_detect(rush_location_type, "INSIDE") ~ "middle",
      str_detect(rush_location_type, "LEFT") ~ "left",
      str_detect(rush_location_type, "RIGHT") ~ "right"
    )
  ) |> 
  select(game_id, play_id, possession_team,
         rush_location_type, run_loc)

##############################################################
# combine run plays with the player that ran and week
run_players_1 <- run_plays |> 
  left_join(player_run) |> 
  left_join(game_week)


##############################################################
# player weekly run tendencies
run_players_2 <- run_players_1 |> 
  group_by(game_id, week, nfl_id, possession_team) |> 
  summarize(num_runs = n(),
            # naming pct because will convert sum to pct later
            rb_pct_left = sum(run_loc == "left"),
            rb_pct_right = sum(run_loc == "right"),
            rb_pct_middle = sum(run_loc == "middle")
            ) |> 
  ungroup()

# cumulative history
run_players_3 <- run_players_2 |> 
  arrange(week) |>  
  group_by(nfl_id) |>  
  mutate(
    num_runs = cumsum(num_runs),
    rb_pct_left = cumsum(rb_pct_left),
    rb_pct_right = cumsum(rb_pct_right),
    rb_pct_middle = cumsum(rb_pct_middle)
         ) |> 
  ungroup() |> 
  # only use players who have had multiple runs
  filter(num_runs >= 3)

# cumulative history
run_players_4 <- run_players_3 |> 
  # convert to percent
  mutate(
    rb_pct_left = rb_pct_left/num_runs,
    rb_pct_right = rb_pct_right/num_runs,
    rb_pct_middle = rb_pct_middle/num_runs
    )

has_run <- run_players_4 |> 
  distinct(nfl_id) |> 
  pull()

##############################################################
# get all running backs involved in play
player_rb <- player_play |> 
  select(game_id, play_id, nfl_id,
         rusher_motion_snap = in_motion_at_ball_snap, 
         rusher_shift_line = shift_since_lineset,
         rusher_motion_line = motion_since_lineset) |> 
  left_join(players[,c("nfl_id", "position")]) |> 
  filter(position == "RB")

# arrange so each play is an observation
# and we can see multiple running backs
player_rb_2 <- player_rb |> 
  mutate(unique_id = paste0(game_id, "_", play_id)) |> 
  group_by(unique_id) |> 
  mutate(rb_num = cumsum(position == "RB")) |> 
  select(unique_id, game_id, play_id, nfl_id, rb_num) |> 
  pivot_wider(names_from = rb_num,
              values_from = nfl_id) |> 
  janitor::clean_names()

######################################################
# only need info for run plays
run_play_vec <- run_plays |>
  mutate(unique_id = paste0(game_id, "_", play_id)) |>
  pull(unique_id)

player_rb_3 <- player_rb_2 |> 
  filter(unique_id %in% run_play_vec)
######################################################

# now we need to merge in the running back tendencies
# and handle cases when there are multiple running backs on the play
play_info <- list()
for(i in 1:nrow(player_rb_3)){
#for(i in 1:10){
  d1 <- run_players_4 |> 
    filter(game_id == player_rb_3$game_id[i],
           nfl_id == player_rb_3$x1[i])
  d2 <- run_players_4 |> 
    filter(game_id == player_rb_3$game_id[i],
           nfl_id == player_rb_3$x2[i])
  d3 <- run_players_4 |> 
    filter(game_id == player_rb_3$game_id[i],
           nfl_id == player_rb_3$x3[i])

  # if multiple rb we will take the average
  # assuming all have equal chance of getting ball
  play_info[[i]] <- d1 |> 
    rbind(d2) |> 
    rbind(d3) |>
    group_by(game_id, week, possession_team) |> 
    summarise(across(c(rb_pct_left:rb_pct_middle), ~ mean(.x, na.rm = TRUE))) |> 
    mutate(play_id = player_rb_3$play_id[i])
  
  print(i)
}

play_info_rb <- bind_rows(play_info)

##############################################################
# were any running backs in motion on play?
rb_motion <- player_run |> 
  group_by(game_id, play_id) |> 
  summarize(
    rusher_motion_snap = sum(rusher_motion_snap, na.rm = TRUE),
    rusher_shift_line = sum(rusher_shift_line, na.rm = TRUE),
    rusher_motion_line = sum(rusher_motion_line, na.rm = TRUE)
  )


##############################################################
# save out rb history
rb_history <- play_info_rb  |> 
  left_join(rb_motion) |> 
  ungroup()

write_rds(rb_history, here("data/rb_history.rds"))
