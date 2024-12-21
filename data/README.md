## Large data files

Download the `tracking_week_1.csv` through `tracking_week_9.csv` 
files [here](https://www.kaggle.com/competitions/nfl-big-data-bowl-2025/data)
and place in the `data/raw/` subdirectory.

All datasets and results are then reproducible by running the files in the `scripts/` subdirectory in order.

The final dataset used for modeling is called `run_plays_final`.

## run_plays_final codebook

- `rush_loc_calc`: **response variable** calculated direction the runner ran based on offensive linemen (`left`, `right`, or `middle` factor)
- `unique_id`: game_id "_" play_id used for merging (text)
- `game_pct_left`: percentage of runs to the left for all plays in the current game leading up to the play being analyzed (numeric)
- `game_pct_right`: percentage of runs to the right for all plays in the current game leading up to the play being analyzed (numeric)
- `game_pct_middle`: percentage of runs to the middle for all plays in the current game leading up to the play being analyzed (numeric)
- `game_id`: Game identifier, unique (numeric)
- `play_id`: Play identifier, not unique across games (numeric)
- `quarter`: Game quarter (numeric)
- `down`: Down (numeric)
- `yards_to_go`: Distance needed for a first down (numeric)
- `possession_team`: Team abbr of team on offense with possession of ball (factor)
- `defensive_team`: Team abbr of team on defense (factor)
- `absolute_yardline_number`: Distance from end zone for possession team (numeric)
- `offensive_formation`: Formation used by possession team (factor)
- `receiver_alignment`: Enumerated as 0x0, 1x0, 1x1, 2x0, 2x1, 2x2, 3x0, 3x1, 3x2 (factor)
- `play_clock_at_snap`: What the play clock value was at time of snap (numeric)
- `rush_location_type`: Original direction the runner ran based on where the offensive linemen were during the play (Inside left, Inside right, Outside left, Outside right, Unknown, factor)
- `pff_run_concept_primary`: Not used. The primary run concept on the play (text)
- `pff_run_pass_option`: Not used. Whether or not the play was a run-pass option (numeric)
- `pff_pass_coverage`: the pass coverage concept employed by the defense on the play (factor)
- `pff_man_zone`: Whether the defense employed man or zone coverage on the play (factor)
- `week`: Week of game
- `no_huddle`: Whether or not the offense huddled on the play (numeric)
- `defenders_in_box`: Number of defenders in the box (numeric)
- `num_rb`: Number of running backs on the play (numeric)
- `num_wr`: Number of wide receivers on the play (numeric)
- `dist_rb_qb`: Euclidean distance between the running back and quarter back at line set (numeric)
- `dir_qb_adj`: Angle of Quarterback motion at line set, 0 degrees is end zone for offensive team (numeric)
- `dir_rb_adj`: Angle of running back motion at line set, 0 degrees is end zone for offensive team(numeric)
- `o_qb_adj`: Quarterback orientation at line set, 0 degrees is end zone for offensive team (numeric)
- `o_rb_adj`: Running back orientation at line set, 0 degrees is end zone for offensive team (numeric)
- `field_side`: Whether the football is positioned on the right or left side of the field (factor)
- `rb_side`: Whether the running back is on the left or right side of the quarterback (factor)
- `middle_gap_o`: Size of orientation gap between offensive linemen (numeric)
- `right_gap_o`: Size of orientation gap right of offensive linemen (numeric)
- `left_gap_o`: Size of orientation gap left of offensive linemen (numeric)
- `middle_gap_tm`: Size of sequential offensive player gap between offensive linemen (numeric)
- `right_gap_tm`: Size of sequential offensive player gap right of offensive linemen (numeric)
- `left_gap_tm`: Size of sequential offensive player gap left of offensive linemen (numeric)
- `rb_pct_left`: Historical percent of runs directed left by the running back on the play (numeric)
- `rb_pct_right`: Historical percent of runs directed right by the running back on the play (numeric)
- `rb_pct_middle`: Historical percent of runs directed middle by the running back on the play (numeric)
- `rusher_motion_snap`: Whether or not the running back was in motion at snap (1 or 0)
- `rusher_shift_line`: Whether or not the running back shifted since lineset (1 or 0)
- `rusher_motion_line`: Whether or not the running back was in motion since lineset (1 or 0)