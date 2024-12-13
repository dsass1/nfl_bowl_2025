
library(nflverse)
library(tidyverse)
library(here)

# import data
game_week <- read_csv(here("data/raw/games.csv")) |> 
  janitor::clean_names() |> 
  select(game_id, week)

player_play <- read_csv(here("data/raw/player_play.csv")) |> 
  janitor::clean_names()

# from 00_prep_run_data
run_plays_dir <- read_rds(here("data/run_plays_dir.rds"))

# nflverse: participation for defenders in box
participation <- load_participation(2022) |> 
  janitor::clean_names() |> 
  select(game_id = old_game_id,
         play_id, possession_team,
         defenders_in_box) |> 
  mutate(game_id = parse_number(game_id))

# distance and direction of qb/rb
# calculated in 01_create_qb_rb_calc.R
qb_rb_calc <- read_rds("data/qb_rb_calc.rds")

# gap analysis
# calculated in 01_create_run_gap_calc.R
run_gap_calc <- read_rds("data/run_gap_calc.rds")

# rb tendencies
# calculated in 01_create_rb_history.R
rb_history <- read_rds("data/rb_history.rds")

################################################################################
################################################################################

# ------------------------------------------------------------------------------
# relevant play data -----------------------------------------------------------
# ------------------------------------------------------------------------------

## given the play was a run we will predict run direction
## only want run plays
run_plays_1 <- run_plays_dir |> 
  mutate(
    no_huddle = ifelse(str_detect(play_description, "No Huddle"), 1, 0),
    yardline_side = ifelse(yardline_number == 50, "50", yardline_side)
    #run_side = ifelse(str_detect(rush_location_type, "LEFT"), "left", "right"),
    #run_loc = ifelse(str_detect(rush_location_type, "INSIDE"), "inside", "outside")
  ) |> 
  relocate(c(rush_loc_calc), .before = 0)

# ------------------------------------------------------------------------------
# only variables pre-snap -----------------------------------------------------------
# ------------------------------------------------------------------------------

run_plays_2 <- run_plays_1 |> 
  select(-c(play_description, yardline_side, yardline_number,
            game_clock, pre_snap_home_score, pre_snap_visitor_score,
            play_nullified_by_penalty, pre_snap_home_team_win_probability,
            pre_snap_visitor_team_win_probability, expected_points,
            pass_result, pass_length, target_x, target_y, play_action,
            dropback_type, dropback_distance, pass_location_type, 
            time_to_throw, time_in_tackle_box, time_to_sack, pass_tipped_at_line,
            unblocked_pressure, qb_spike, qb_kneel, qb_sneak,
            penalty_yards, pre_penalty_yards_gained, yards_gained,
            home_team_win_probability_added, visitor_team_win_probility_added,
            expected_points_added, is_dropback, pff_run_concept_secondary
            ) )

# ------------------------------------------------------------------------------
# add defender info -----------------------------------------------------------
# ------------------------------------------------------------------------------
run_plays_3 <- run_plays_2 |> 
  left_join(participation)

# ------------------------------------------------------------------------------
# add qb rb info -----------------------------------------------------------
# ------------------------------------------------------------------------------
run_plays_4 <- run_plays_3 |> 
  left_join(qb_rb_calc) |> 
  select(-team)

# ------------------------------------------------------------------------------
# add spatial gap info -----------------------------------------------------------
# ------------------------------------------------------------------------------
run_plays_5 <- run_plays_4 |> 
  left_join(run_gap_calc) |>  
  mutate(
    left_gap_o = ifelse(is.na(left_gap_o), 0, left_gap_o),
    right_gap_o = ifelse(is.na(right_gap_o), 0, right_gap_o),
    middle_gap_o = ifelse(is.na(middle_gap_o), 0, middle_gap_o),
    left_gap_tm = ifelse(is.na(left_gap_tm), 0, left_gap_tm),
    right_gap_tm = ifelse(is.na(right_gap_tm), 0, right_gap_tm),
    middle_gap_tm = ifelse(is.na(middle_gap_tm), 0, middle_gap_tm)
    )


# ------------------------------------------------------------------------------
# time series info -----------------------------------------------------------
# ------------------------------------------------------------------------------
# current game
run_plays_6 <- run_plays_5 |> 
  arrange(game_id, play_id) |> 
  group_by(game_id, possession_team) |> 
  mutate(pct_left_tmp = cumsum(rush_loc_calc == "left")/cumsum(rush_loc_calc %in% c("left", "right", "middle") ),
         # can't include current play
         game_pct_left = dplyr::lag(pct_left_tmp),
         # if NA it is the first play of the game set to 0.33
         game_pct_left = ifelse(is.na(game_pct_left), 0.33, game_pct_left),
         pct_right_tmp = cumsum(rush_loc_calc == "right")/cumsum(rush_loc_calc %in% c("left", "right", "middle") ),
         # can't include current play
         game_pct_right = dplyr::lag(pct_right_tmp),
         # if NA it is the first play of the game set to 0.33
         game_pct_right = ifelse(is.na(game_pct_right), 0.33, game_pct_right),
         pct_middle_tmp = cumsum(rush_loc_calc == "middle")/cumsum(rush_loc_calc %in% c("left", "right", "middle") ),
         # can't include current play
         game_pct_middle = dplyr::lag(pct_middle_tmp),
         # if NA it is the first play of the game set to 0.33
         game_pct_middle = ifelse(is.na(game_pct_middle), 0.33, game_pct_middle),
         .before = game_id) |> 
  # will handle multicollinearity later
  select(-c(pct_left_tmp, pct_right_tmp, pct_middle_tmp)) |> 
  ungroup()

# ------------------------------------------------------------------------------
# rb history -----------------------------------------------------------
# ------------------------------------------------------------------------------
run_plays_7 <- run_plays_6 |>
  left_join(rb_history)  

# not all plays are in rb_history creating missing weeks
run_plays_7 <- run_plays_7|> 
  rows_update(game_week, by = "game_id")

# not all plays have rb_history meaning rb did not have historical data.
# fill in random direction of 0.33
run_plays_7 <- run_plays_7 |> 
  mutate(rb_pct_left = ifelse(is.na(rb_pct_left), 0.33, rb_pct_left),
         rb_pct_right = ifelse(is.na(rb_pct_right), 0.33, rb_pct_right),
         rb_pct_middle = ifelse(is.na(rb_pct_middle), 0.33, rb_pct_middle),
         rusher_motion_snap = ifelse(is.na(rusher_motion_snap), 0, rusher_motion_snap),
         rusher_shift_line = ifelse(is.na(rusher_shift_line), 0, rusher_shift_line),
         rusher_motion_line = ifelse(is.na(rusher_motion_line), 0, rusher_motion_line)
         )

# ------------------------------------------------------------------------------
# is motion -----------------------------------------------------------
# ------------------------------------------------------------------------------
# movement <- player_play |> 
#   summarize(
#     motion_snap = sum(in_motion_at_ball_snap, na.rm = TRUE),
#     shift_line = sum(shift_since_lineset, na.rm = TRUE),
#     motion_line = sum(motion_since_lineset, na.rm = TRUE),
#     .by = c(game_id, play_id, team_abbr)
#   )

# do we need if RB is in motion and direction?

#run_plays_7 <- run_plays_6 |> 
#  left_join(movement, join_by(game_id, play_id, possession_team == team_abbr))




# ------------------------------------------------------------------------------
# finalize data -----------------------------------------------------------
# ------------------------------------------------------------------------------

run_plays_final <- run_plays_7 |> 
  # data typing and cleaning
  mutate(
    rush_location_type = str_to_lower(rush_location_type),
    # rush_location_type_mod = case_when(
    #   str_detect(rush_location_type, "inside") ~ "middle",
    #   str_detect(rush_location_type, "left") ~ "left",
    #   str_detect(rush_location_type, "right") ~ "right",
    #   TRUE ~ "check"),
    field_side = ifelse(field_side == "left", 1, 0),
    rb_side = ifelse(rb_side == "left", 1, 0),
    across(where(is_character), factor),
    # not sure why possession team is being weird
    possession_team = as_factor(possession_team)
  )

write_rds(run_plays_final, "data/run_plays_final.rds")

