# Reproducing results

Download the `tracking_week_1.csv` through `tracking_week_9.csv` 
files [here](https://www.kaggle.com/competitions/nfl-big-data-bowl-2025/data)
and place in the `data/raw/` subdirectory.

All datasets and results are then reproducible by running the `scripts/` in order.

The final dataset used for modeling is called `run_plays_final`.

## run_plays_final codebook

- `rush_loc_calc`
- `unique_id`
- `game_pct_left`
- `game_pct_right`
- `game_pct_middle`
- `game_id`
- `play_id`
- `quarter`
- `down`
- `yards_to_go`
- `possession_team`
- `defensive_team`
- `absolute_yardline_number`
- `offensive_formation`
- `receiver_alignment`
- `play_clock_at_snap`
- `rush_location_type`
- `pff_run_concept_primary`
- `pff_run_pass_option`
- `pff_pass_coverage`
- `pff_man_zone`
- `week`
- `no_huddle`
- `defenders_in_box`
- `num_rb`
- `num_wr`
- `dist_rb_qb`
- `dir_qb_adj`
- `dir_rb_adj`
- `o_qb_adj`
- `o_rb_adj`
- `field_side`
- `rb_side`
- `middle_gap_o`
- `right_gap_o`
- `left_gap_o`
- `middle_gap_tm`
- `right_gap_tm`
- `left_gap_tm`
- `rb_pct_left`
- `rb_pct_right`
- `rb_pct_middle`
- `rusher_motion_snap`
- `rusher_shift_line`
- `rusher_motion_line`