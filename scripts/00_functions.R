# Function: qb rb info --------------------------------------------------------------
# function used to calculate info in 02_create_qb_rb_calc.R
qb_rb_fun <- function(data){
  
  # get distinct plays
  distinct_plays <- data |> 
    select(game_id, play_id) |> 
    distinct()
  
  # only use pre-snap data
  loc_tmp <- data |>
    mutate(unique_id = paste0(game_id, "_", play_id)) |> 
    filter(position == "QB" | position == "RB") |> 
    filter(frame_type == "BEFORE_SNAP") 
  
  # get information at line set
  loc_line <- loc_tmp |> 
    filter(event == "line_set")
  
  # some plays do not have a line_set
  loc_noline <- loc_tmp |> 
    filter(! (unique_id %in% loc_line$unique_id) ) |> 
    group_by(unique_id, nfl_id) |> 
    slice_max(frame_id) |> 
    ungroup()
  
  loc_one <- rbind(loc_line, loc_noline)
  ######################################
  
  loc_football <- data |>
    filter(club == "football") |> 
    filter(frame_type == "BEFORE_SNAP") |> 
    group_by(game_id, play_id, nfl_id) |> 
    slice_max(frame_id) |> 
    #filter(frame_id == max(frame_id)) |> 
    ungroup() |> 
    select(game_id, play_id,
           x_ball = x,
           y_ball = y)
  
  wk_loc <- loc_one |> 
    left_join(loc_football)
  
  #####################################################
  #####################################################
  # error handling:
  # if there are 2 QB's assume one is acting as a RB
  mod_pos <- wk_loc |> 
    #filter(position == "QB") |> 
    mutate(ball_dist = sqrt((x -x_ball)^2 + (y-y_ball)^2)) |> 
    group_by(game_id, play_id, position) |> 
    mutate(position = case_when(
      position != "QB" ~ position,
      ball_dist == min(ball_dist) ~ position,
      TRUE ~ "RB")) |> 
    # select(-ball_dist) |> 
    ungroup()
  
  ##################################################
  ##################################################
  ##################################################
  
  # count of wide receivers
  num_wr <- data |>
    filter(position == "WR") |> 
    # only getting count so frame shouldn't matter
    filter(frame_type == "BEFORE_SNAP" |
             frame_type == "SNAP") |> 
    group_by(game_id, play_id, nfl_id) |> 
    filter(frame_id == max(frame_id)) |> 
    ungroup() |> 
    group_by(game_id, play_id) |> 
    dplyr::summarize(num_wr = sum(position == "WR"))
  
  # get RB dataset
  rb <- mod_pos |> 
    filter(position == "RB") |> 
    select(game_id, play_id, nfl_id,
           team = club,
           play_direction,
           x_rb = x,
           y_rb = y,
           dir_rb = dir,
           o_rb = o)
  
  # count of running backs
  num_rb <- mod_pos |> 
    filter(position == "RB") |> 
    group_by(game_id, play_id) |> 
    dplyr::summarize(num_rb = sum(position == "RB"))
  
  
  qb <- mod_pos |> 
    filter(position == "QB") |> 
    select(game_id, play_id, #nfl_id, 
           team = club,
           play_direction,
           x_qb = x,
           y_qb = y,
           dir_qb = dir,
           o_qb = o)
  
  qb_rb_tmp <- full_join(qb, rb, relationship = "many-to-many")
  
  #####################################################
  # error handling:
  # if there is no qb then assume it is a direct snap to RB
  qb_rb_check <- qb_rb_tmp |> 
    filter(is.na(y_qb))
  
  if(nrow(qb_rb_check) >0){
    tmp <- data |> 
      filter(game_id %in% qb_rb_check$game_id) |> 
      filter(play_id %in% qb_rb_check$play_id) |> 
      filter(frame_type == "BEFORE_SNAP") |> 
      group_by(game_id, play_id) |> 
      slice_max(frame_id) |>  
      filter(club == "football") |> 
      ungroup() |> 
      # let's assume the RB to receive the ball is lined
      # up behind the ball
      select(game_id, play_id, 
             y_qb = y)
    
    # update missing qb y pos with football y pos
    qb_rb_tmp2 <- rows_update(qb_rb_tmp, tmp,
                              by = "play_id")
    
    qb_rb_tmp3 <- qb_rb_tmp2 |> 
      # set remaining missing qb data equal to rb data
      mutate(x_qb = ifelse(is.na(x_qb), x_rb, x_qb),
             dir_qb = ifelse(is.na(dir_qb), dir_rb, dir_qb),
             o_qb = ifelse(is.na(o_qb), o_rb, o_qb))
    
    # y_qb is equal to football and close enough that
    # the only RB selected will be the closest to football and
    # distance will be almost 0
    
    # overwrite original with updated
    qb_rb_tmp <- qb_rb_tmp3
  }
  
  #####################################################
  #####################################################
  
  qb_rb <- qb_rb_tmp |> 
    left_join(num_rb) |> 
    left_join(num_wr) |> 
    mutate(num_rb = ifelse(is.na(num_rb), 0, num_rb),
           num_wr = ifelse(is.na(num_wr), 0, num_wr))
  
  wk_qb_rb_calc_tmp <- qb_rb |> 
    mutate(
      # euclidean distance between rb and qb
      dist_rb_qb = sqrt((x_qb-x_rb)^2 + (y_qb-y_rb)^2),
      # have 0 degrees be direction of endzone
      # 0 to - 180 = left; 0 to + 180 = right
      dir_qb_adj = case_when(
        play_direction == "right" & dir_qb <= 270 ~ dir_qb - 90,
        play_direction == "right" & dir_qb > 270 ~ dir_qb - 360- 90,
        play_direction == "left" & dir_qb <= 90 ~ dir_qb + 90,
        play_direction == "left" & dir_qb > 90 ~ dir_qb - 360 + 90
      ),
      o_qb_adj = case_when(
        play_direction == "right" & o_qb <= 270 ~ o_qb - 90,
        play_direction == "right" & o_qb > 270 ~ o_qb - 360- 90,
        play_direction == "left" & o_qb <= 90 ~ o_qb + 90,
        play_direction == "left" & o_qb > 90 ~ o_qb - 360 + 90
      ),
      dir_rb_adj = case_when(
        play_direction == "right" & dir_rb <= 270 ~ dir_rb - 90,
        play_direction == "right" & dir_rb > 270 ~ dir_rb - 360- 90,
        play_direction == "left" & dir_rb <= 90 ~ dir_rb + 90,
        play_direction == "left" & dir_rb > 90 ~ dir_rb - 360 + 90
      ),
      o_rb_adj = case_when(
        play_direction == "right" & o_rb <= 270 ~ o_rb - 90,
        play_direction == "right" & o_rb > 270 ~ o_rb - 360- 90,
        play_direction == "left" & o_rb <= 90 ~ o_rb + 90,
        play_direction == "left" & o_rb > 90 ~ o_rb - 360 + 90
      ),
      # in relation to end zone is the ball favoring the left or right side
      field_side = case_when(
             play_direction == "right" & y_qb > 53.3/2 ~ "left",
             play_direction == "right" & y_qb <= 53.3/2 ~ "right",
             play_direction == "left" & y_qb > 53.3/2 ~ "right",
             play_direction == "left" & y_qb <= 53.3/2 ~ "left",
             TRUE ~ "check"),
      # is the rb on the left or right side of qb
      rb_side = case_when(
             play_direction == "right" & y_qb > y_rb ~ "right",
             play_direction == "right" & y_qb <= y_rb ~ "left",
             play_direction == "left" & y_qb > y_rb ~ "left",
             play_direction == "left" & y_qb <= y_rb ~ "right",
             TRUE ~ "none")
    ) |> 
    select(game_id, play_id, team, num_rb, num_wr,
           dist_rb_qb, dir_qb_adj, dir_rb_adj, 
           o_qb_adj, o_rb_adj, field_side, rb_side)
  
  # if there are more than 1 RB,
  # only pick the closest RB
  wk_qb_rb_calc <- wk_qb_rb_calc_tmp |> 
    # if no RB on the play set distance to 27 
    # (half the field essentially out of bounds)
    mutate(dist_rb_qb = ifelse(is.na(dist_rb_qb),
                               27,
                               dist_rb_qb) ) |> 
    group_by(game_id, play_id) |> 
    filter(dist_rb_qb == min(dist_rb_qb))
  
  return(wk_qb_rb_calc)
}


# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# Function: run gap info --------------------------------------------------------------
# function used to calculate info in 02_create_run_gap.R
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
#unique_tag = "2022092501_308"
#tracking_data <- tracking_runs

run_gap <- function(tracking_data, unique_tag){
  
  # check progress
  print(paste0(unique_tag))
  game_tag <- as.numeric(unlist(str_split(unique_tag, "_"))[1])
  play_tag <- as.numeric(unlist(str_split(unique_tag, "_"))[2])
  
  # get all player locations directly before snap  
  play_tmp <- tracking_data |> 
    filter(
      unique_id == unique_tag,
           frame_type == "BEFORE_SNAP")
    
  # get information at line set so everyone is facing forward
  # not every play has a line_set
  if("line_set" %in% unique(play_tmp$event)){
    play1 <- play_tmp |> 
      # get information at line set so everyone is facing forward
      filter(event == "line_set")
  }else{
    play1 <- play_tmp |> 
      # no lineset grab last frame
      slice_max(frame_id)
  }
  
  # which team is on offense?
  offense <- play1 |> 
    filter(position == "QB" | position == "RB") |> 
    distinct(club) |> 
    pull(club) 
  # which team is on defense?
  defense <- play1 |> 
    distinct(club) |> 
    filter(! (club %in% c(offense, "football")) ) |> 
    pull(club)
  
  # offensive play direction
  play_dir <- play1 |> 
    filter(club == offense) |> 
    distinct(play_direction) |> 
    pull(play_direction)
  
  ##############################################################
  # information that will help determine location
  # where is the football
  football_x <- play1 |> 
    filter(display_name == "football") |> 
    pull(x)
  football_y <- play1 |> 
    filter(display_name == "football") |> 
    pull(y)
  # need center info because some plays had football in wrong location
  center <- play1 |> 
    filter(position == "C") #|> 
    #pull(x)
  
  # if there are 2 centers keep the data closest to opponent
  if(length(center$x) >= 1){
    if(play_dir == "left"){
        center_x = min(center$x)
    }else{
        center_x = max(center$x)
    }
    center_y <- center |> 
      filter(x == center_x) |> 
      pull(y)
  }
  
  # if there are no centers let it be the football location
  if(length(center$x) == 0){
    center_x = football_x
  }else if(abs(center_x - football_x) < 1 & 
     abs(center_y - football_y) < 1){
    # if football and center are "close"the football is in correct
    # location and we can use football
    center_x = football_x
  }
  
  ##############################################################
  
  endpts <- play1 |> 
    # only care about players at the line of scrimmage
    # there were a few plays where football was not at center
    # filter(abs(x-football_x) <= 2.5) |> 
    filter(abs(x-center_x) <= 2.5) |> 
    # QB will not be blocking
    filter(! (position %in% c("QB")) ) |> 
    # only interested in players
    filter(club != "football") |> 
    # defense is facing the opposite direction as offense
    mutate(play_direction = case_when(
      club == offense ~ play_direction,
      club == defense & play_direction == "left" ~ "right",
      club == defense & play_direction == "right" ~ "left"
    )) |> 
    # adjust direction and orientation
    mutate(
      dir_adj = case_when(
        # have 0 degrees be direction of opponent
        # negative 0 to - 180 = turning left; 
        # positive 0 to + 180 = turning right
        play_direction == "right" & dir <= 270 ~ -(dir - 90),
        play_direction == "right" & dir > 270 ~ -(dir - 360- 90),
        play_direction == "left" & dir <= 90 ~ dir + 90,
        play_direction == "left" & dir > 90 ~ dir - 360 + 90
        ),
      o_adj = case_when(
        # same as above
        play_direction == "right" & o <= 270 ~ -(o - 90),
        play_direction == "right" & o > 270 ~ -(o - 360- 90),
        play_direction == "left" & o <= 90 ~ o + 90,
        play_direction == "left" & o > 90 ~ o - 360 + 90
      )
    ) |> 
    # let's have everyone take 1 step forward ~ 1 yard
    mutate(
      # we use orientation as the direction they are headed
      x_end = case_when(
        # x is decreasing
        play_direction == "left" ~ x + abs(cos_d(o_adj))*
          sign(abs(o_adj)-90),
        # x is increasing
        play_direction == "right" ~ x - abs(cos_d(o_adj))*
          sign(abs(o_adj)-90)
      ),
      y_end = y + abs(sin_d(o_adj))*sign(o_adj)
    )
  
  #############################################################
  # define the sideline so that we have an endpoint to use
  sideline <- tibble(
    club = c(offense, offense),
    position = c("sideline", "sideline"),
    y_end = c(0, 53.3),
    o_adj = c(0, 0)
  )
  
  # get offensive line positions to classify inside/outside gap
  ol_bounds <- endpts |> 
    filter(position %in% c("C", "G", "T")) |> 
    dplyr::summarize(upper_bound = max(y_end),
              center = median(y_end),
              lower_bound = min(y_end))
  
  # determining if there is a gap based on offensive orientation
  # example: if player on the left is oriented left and player on
  # the right is oriented right, the assumption is they are attempting
  # to push the defense in opposite directions creating a hole
  gap1 <- endpts |> 
    filter(club == offense) |> 
    select(club, position, y_end, o_adj) |>
    rbind(sideline) |> 
    arrange(desc(y_end)) |> 
    # get neighbor information
    mutate(o_adj_prior = lag(o_adj),
           y_end_prior = lag(y_end)) |> 
    # is there a gap between the two players?
    mutate(gap = case_when(
      o_adj <= 0 & o_adj_prior >= 0 ~ "yes",
      TRUE ~ "no"
    ),
    gap_side = case_when(
      y_end < ol_bounds$upper_bound &
        y_end_prior > ol_bounds$lower_bound ~ "middle",
      play_dir == "left" & y_end >= ol_bounds$upper_bound ~ "right",
      play_dir == "left" & y_end <= ol_bounds$lower_bound ~ "left",
      play_dir == "right" & y_end >= ol_bounds$upper_bound ~ "left",
      play_dir == "right" & y_end <= ol_bounds$lower_bound ~ "right",
      TRUE ~ "check"
    ),
    gap_loc = paste0(gap_side, "_gap_o"),
    gap_size = y_end_prior - y_end
    ) |> 
    filter(gap == "yes") |> 
    select(gap_loc, gap_size)
  
  # this gap is determining if two offensive players are in a row
  # if no defense is positioned between two offense the assumption
  # is that there could be a potential gap
  gap2 <- endpts |> 
    select(club, position, y_end, o_adj) |>
    rbind(sideline) |> 
    # label as offense and defense
    mutate(tm = ifelse(club == offense, "O", "D")) |> 
    # line them up
    arrange(desc(y_end)) |> 
    # get neighbor player info
    mutate(tm_prior = lag(tm),
           y_end_prior = lag(y_end)) |> 
    mutate(gap = case_when(
      # gap if two offense in a row
      tm == "O" & tm_prior == "O" ~ "yes",
      TRUE ~ "no"
    ),
    gap_side = case_when(
      y_end < ol_bounds$upper_bound &
        y_end_prior > ol_bounds$lower_bound ~ "middle",
      play_dir == "left" & y_end >= ol_bounds$upper_bound ~ "right",
      play_dir == "left" & y_end <= ol_bounds$lower_bound ~ "left",
      play_dir == "right" & y_end >= ol_bounds$upper_bound ~ "left",
      play_dir == "right" & y_end <= ol_bounds$lower_bound ~ "right",
      TRUE ~ "check"
    ),
    gap_loc = paste0(gap_side, "_gap_tm"),
    gap_size = y_end_prior - y_end
    ) |> 
    filter(gap == "yes") |> 
    select(gap_loc, gap_size)
  
  # put the data together
  gap <- gap1 |> 
    rbind(gap2) |> 
    # only want one of each location type
    group_by(gap_loc) |> 
    slice_max(gap_size) |> 
    # convert to variables for merging
    pivot_wider(names_from = gap_loc,
                values_from = gap_size) |> 
    mutate(game_id = game_tag,
           play_id = play_tag)
  
  gap_frame <- tibble(
    game_id = game_tag,
    play_id = play_tag,
    middle_gap_o = as.numeric(NA),
    right_gap_o = as.numeric(NA),
    left_gap_o = as.numeric(NA),
    middle_gap_tm = as.numeric(NA),
    right_gap_tm = as.numeric(NA),
    left_gap_tm = as.numeric(NA)
  )
  
  gap_data <- gap_frame |> 
    rows_update(gap, by = "play_id")
  
  return(gap_data)
}


# Function: define run ---------------------------------------------
## classify as left, middle, right

# tracking_data <- tracking_runs
# unique_tag <- "2022091802_1034"
define_run <- function(tracking_data, unique_tag){
  # check progress
  print(paste0(unique_tag))
  game_tag <- as.numeric(unlist(str_split(unique_tag, "_"))[1])
  play_tag <- as.numeric(unlist(str_split(unique_tag, "_"))[2])
  
  # get play data
  track1 <- tracking_data |> 
    filter(game_id == game_tag, play_id == play_tag)
  
  # get football
  football <- track1 |> 
    filter(club == "football") |> 
    filter(frame_type == "AFTER_SNAP") |> 
    select(frame_id, event,
           x_ball = x, 
           y_ball = y)
  
  # where did the ball start for comparison later
  ball_initial <- track1 |> 
    filter(club == "football") |> 
    filter(frame_type == "SNAP")
  
  # which team is on offense?
  offense <- track1 |> 
    filter(position == "QB" | position == "RB") |> 
    distinct(club) |> 
    pull(club) 
  # which team is on defense?
  defense <- track1 |> 
    distinct(club) |> 
    filter(! (club %in% c(offense, "football")) ) |> 
    pull(club)
  
  # get data after handoff
  handoff_frame <- football |> 
    filter(event == "handoff") |> 
    pull(frame_id)
  # handle if there is no handoff frame
  if(length(handoff_frame) == 0){
    handoff_frame <- football |> 
      slice_min(frame_id) |> 
      pull(frame_id)
    # add a few frames so ball is not ahead of line
    # arbitrary and will not impact later calculations
    handoff_frame <- handoff_frame + 5
  }
  
  # is play direction left or right?
  play_dir <- track1 |> 
    distinct(play_direction) |> 
    pull(play_direction)
  
  ####################
  # get our center (needed for linemen check)
  center <- track1 |> 
    filter(frame_type == "SNAP") |> 
    filter(position %in% c("C")) |> 
    mutate(dist_ball = sqrt((x - ball_initial$x)^2 + (y-ball_initial$y)^2)) |> 
    slice_min(dist_ball)
  
  # if there is no "center" get player closest to ball
  if(nrow(center) == 0){
    center <- track1 |> 
      filter(frame_type == "SNAP") |> 
      filter(position %in% c("G", "T")) |> 
      mutate(dist_ball = sqrt((x - ball_initial$x)^2 + (y-ball_initial$y)^2)) |> 
      slice_min(dist_ball)
  }
  
  #check positions
  plot <- ggplot(track1[track1$frame_type == "SNAP",], aes(x = x, y = y)) +
    geom_text(aes(label = position)) +
    ggtitle(unique_tag)
  print(plot)
  
  # get our 5 linemen
  line_pos <- track1 |> 
    filter(frame_type == "SNAP") |> 
    filter(position %in% c("C", "G", "T")) |> 
    select(frame_id, club, play_direction, x, y, position, nfl_id) |> 
    mutate(dist_y = y - center$y) 
  
  # sometimes players do not match their "position"
  # perhaps due to injury or change of line up
  # check that 2 players are left and 2 are right of C
  # to check for mislabeled linemen
  if(sum(sign(line_pos$dist_y)) != 0){
    line_pos <- track1 |> 
      filter(frame_type == "SNAP") |> 
      # sometimes a mislabeled TE is in the line
      filter(position %in% c("T", "G", "C", "TE")) |>
      mutate(dist_ball = sqrt((x - ball_initial$x)^2 + (y-ball_initial$y)^2),
             dist_x = abs(x-center$x)) |> 
      slice_min(dist_ball, n = 5) |> 
      select(frame_id, club, play_direction, x, y, position, nfl_id)
  }
  
  print(line_pos$position)
  
  
  if(play_dir == "left"){
    # get left and right lineman
    player_left <- line_pos |> 
      slice_min(y) |> 
      pull(nfl_id)
    player_right <- line_pos |> 
      slice_max(y) |> 
      pull(nfl_id)
    
    # where do the lineman start at snap?
    out_left_initial <- track1 |> 
      filter(nfl_id == player_left) |> 
      filter(frame_type == "SNAP")
    out_right_initial <- track1 |> 
      filter(nfl_id == player_right) |> 
      filter(frame_type == "SNAP")
    
    # when does the ball pass the player on the x axis
    # then determine if it was to the right or left of player
    out_left_tmp <- track1 |> 
      filter(nfl_id == player_left) |> 
      filter(frame_id > handoff_frame) |> 
      select(frame_id, club, play_direction, event, x, y) |> 
      left_join(football, by = join_by(frame_id, event)) |> 
      mutate(pass_player = ifelse(x_ball >= x, "no", "yes"),
             pass_initial = ifelse(x_ball >= out_left_initial$x,
                                   "no", "yes"))
    
    ################################
    # if "yes" is the first frame there is an error with data
    # ball should not be ahead of linemen at snap
    which_player_no <- which(out_left_tmp$pass_player == "no")
    which_init_no <- which(out_left_tmp$pass_initial == "no")
    
    # if pass_player is not all yes and not all no
    if(length(which_player_no) != 0 & length(which_player_no) != nrow(out_left_tmp)){
      # keep first "no" to end of data
      out_left_tmp <- out_left_tmp[min(which_player_no):nrow(out_left_tmp),]
      # now if all no and initial_player is not all yes
    } else if(length(which_player_no) != nrow(out_left_tmp) & length(which_init_no) != 0){
      # keep first "no" to end of data
      out_left_tmp <- out_left_tmp[min(which_init_no):nrow(out_left_tmp),]
    }
    ################################
    
    # now determine if the rush was left or right of left lineman
    if(sum(str_detect(out_left_tmp$pass_player, "yes")) >= 1 ){
      out_left <- out_left_tmp |> 
        filter(pass_player == "yes") |> 
        slice_min(frame_id) |> 
        mutate(which_side = ifelse(y_ball < y, "left", "right"))
    } else if(sum(str_detect(out_left_tmp$pass_initial, "yes")) >= 1 ){
      # if never passes player let's use when ball passes
      # initial player position
      out_left <- out_left_tmp |> 
        filter(pass_initial == "yes") |> 
        slice_min(frame_id) |> 
        mutate(which_side = ifelse(y_ball < out_left_initial$y, "left", "right"))
      
      # if distance is marginal let's verify end position
      if(round(abs(out_left$y_ball - out_left_initial$y), 1) <= 0.5){
        out_left <- out_left_tmp |> 
          filter(event == "first_contact" | event == "touchdown" | 
                   event == "tackle" | event == "qb_sack" |
                   event == "fumble" | event == "out_of_bounds") |> 
          slice_max(frame_id) |> 
          mutate(which_side = ifelse(y_ball < out_left_initial$y, "left", "right"))
      }
    } else{
      # if never passes line of play
      # when the first_contact happens
      out_left <- out_left_tmp |> 
        filter(event == "first_contact" | event == "touchdown" | 
                 event == "tackle" | event == "qb_sack" |
                 event == "fumble" | event == "out_of_bounds") |> 
        slice_max(frame_id) |> 
        mutate(which_side = ifelse(y_ball < out_left_initial$y, "left", "right"))
    }
    
    # now do the same check for the right lineman
    out_right_tmp <- track1 |> 
      filter(nfl_id == player_right) |> 
      filter(frame_id > handoff_frame) |> 
      select(frame_id, club, play_direction, event, x, y) |> 
      left_join(football, by = join_by(frame_id, event)) |> 
      mutate(pass_player = ifelse(x_ball >= x, 
                                  "no", "yes"),
             pass_initial = ifelse(x_ball >= out_right_initial$x,
                                   "no", "yes"))
    
    ################################
    # if "yes" is the first frame there is an error with data
    # ball should not be ahead of linemen at snap
    which_player_no <- which(out_right_tmp$pass_player == "no")
    which_init_no <- which(out_right_tmp$pass_initial == "no")
    
    # if pass_player is not all yes and not all no
    if(length(which_player_no) != 0 & length(which_player_no) != nrow(out_right_tmp)){
      # keep first "no" to end of data
      out_right_tmp <- out_right_tmp[min(which_player_no):nrow(out_right_tmp),]
      # now if all no and initial_player is not all yes
    } else if(length(which_player_no) != nrow(out_right_tmp) & length(which_init_no) != 0){
      # keep first "no" to end of data
      out_right_tmp <- out_right_tmp[min(which_init_no):nrow(out_right_tmp),]
    }
    ################################
    
    # now did rush happen left or right of right lineman
    if(sum(str_detect(out_right_tmp$pass_player, "yes") ) >= 1 ){
      out_right <- out_right_tmp |> 
        filter(pass_player == "yes") |> 
        slice_min(frame_id) |> 
        mutate(which_side = ifelse(y_ball < y, "left", "right"))
    }else if(sum(str_detect(out_right_tmp$pass_initial, "yes") ) >= 1){
      # if never passes player let's use when ball passes
      # initial player position
      out_right <- out_right_tmp |> 
        filter(pass_initial == "yes") |> 
        slice_min(frame_id) |> 
        mutate(which_side = ifelse(y_ball < out_right_initial$y, "left", "right"))
      
      # if distance is marginal let's verify end position
      if(round(abs(out_right$y_ball - out_right_initial$y), 1) <= 0.5){
        out_right <- out_right_tmp |> 
          filter(event == "first_contact" | event == "touchdown" | 
                   event == "tackle" | event == "qb_sack" |
                   event == "fumble" | event == "out_of_bounds") |> 
          slice_max(frame_id) |> 
          mutate(which_side = ifelse(y_ball < out_right_initial$y, "left", "right"))
      }  
    }else{
      # if never passes line of play
      # when the first_contact happens
      out_right <- out_right_tmp |> 
        filter(event == "first_contact" | event == "touchdown" | 
                 event == "tackle" | event == "qb_sack" |
                 event == "fumble" | event == "out_of_bounds") |> 
        slice_max(frame_id) |> 
        mutate(which_side = ifelse(y_ball < out_right_initial$y, "left", "right"))
    }
  #################################################
  #################################################
  }else{ # play direction right
    # reverse for opposite direction
    player_left <- line_pos |> 
      slice_max(y) |> 
      pull(nfl_id)
    player_right <- line_pos |> 
      slice_min(y) |> 
      pull(nfl_id)
    
    # where do the lineman start?
    out_left_initial <- track1 |> 
      filter(nfl_id == player_left) |> 
      filter(frame_type == "SNAP")
    out_right_initial <- track1 |> 
      filter(nfl_id == player_right) |> 
      filter(frame_type == "SNAP")
    
    # when does the ball pass the player on the x axis
    # then determine if it was to the right or left of player
    out_left_tmp <- track1 |> 
      filter(nfl_id == player_left) |> 
      filter(frame_id > handoff_frame) |> 
      select(frame_id, club, play_direction, event, x, y) |> 
      left_join(football, by = join_by(frame_id, event)) |> 
      mutate(pass_player = ifelse(x_ball <= x, "no", "yes"),
             pass_initial = ifelse(x_ball <= out_left_initial$x,
                                   "no", "yes"))
    
    ################################
    # if "yes" is the first frame there is an error with data
    # ball should not be ahead of linemen at snap
    which_player_no <- which(out_left_tmp$pass_player == "no")
    which_init_no <- which(out_left_tmp$pass_initial == "no")
    
    # if pass_player is not all yes and not all no
    if(length(which_player_no) != 0 & length(which_player_no) != nrow(out_left_tmp)){
      # keep first "no" to end of data
      out_left_tmp <- out_left_tmp[min(which_player_no):nrow(out_left_tmp),]
      # now if all no and initial_player is not all yes
    } else if(length(which_player_no) != nrow(out_left_tmp) & length(which_init_no) != 0){
      # keep first "no" to end of data
      out_left_tmp <- out_left_tmp[min(which_init_no):nrow(out_left_tmp),]
    }
    ################################
    
    # now did rush happen left or right of that lineman
    if(sum(str_detect(out_left_tmp$pass_player, "yes")) >= 1){
      out_left <- out_left_tmp |> 
        filter(pass_player == "yes") |> 
        slice_min(frame_id) |> 
        mutate(which_side = ifelse(y_ball > y, "left", "right"))
    } else if(sum(str_detect(out_left_tmp$pass_initial, "yes")) >= 1 ){
      # if never passes player let's use when ball passes
      # initial player position
      out_left <- out_left_tmp |> 
        filter(pass_initial == "yes") |> 
        slice_min(frame_id) |> 
        mutate(which_side = ifelse(y_ball > out_left_initial$y, "left", "right"))
      
      # if distance is marginal let's verify end position
      if(round(abs(out_left$y_ball - out_left_initial$y), 1) <= 0.5){
        out_left <- out_left_tmp |> 
          filter(event == "first_contact" | event == "touchdown" | 
                   event == "tackle" | event == "qb_sack" |
                   event == "fumble" | event == "out_of_bounds") |> 
          slice_max(frame_id) |> 
          mutate(which_side = ifelse(y_ball > out_left_initial$y, "left", "right"))
      }  
      
    } else{
      # if never passes line of play
      # when main event happens
      out_left <- out_left_tmp |> 
        filter( 
          event == "first_contact" | event == "touchdown" | 
          event == "tackle" | event == "qb_sack" |
          event == "fumble" | event == "out_of_bounds") |> 
        slice_max(frame_id) |> 
        mutate(which_side = ifelse(y_ball > out_left_initial$y, "left", "right"))
    }
    
    # same check for right lineman
    out_right_tmp <- track1 |> 
      filter(nfl_id == player_right) |> 
      filter(frame_id > handoff_frame) |> 
      select(frame_id, club, play_direction, event, x, y) |> 
      left_join(football, by = join_by(frame_id, event)) |> 
      mutate(pass_player = ifelse(x_ball <= x, 
                                  "no", "yes"),
             pass_initial = ifelse(x_ball <= out_right_initial$x,
                                   "no", "yes"))
    
    ################################
    # if "yes" is the first frame there is an error with data
    # ball should not be ahead of linemen at snap
    which_player_no <- which(out_right_tmp$pass_player == "no")
    which_init_no <- which(out_right_tmp$pass_initial == "no")
    
    # if pass_player is not all yes and not all no
    if(length(which_player_no) != 0 & length(which_player_no) != nrow(out_right_tmp)){
      # keep first "no" to end of data
      out_right_tmp <- out_right_tmp[min(which_player_no):nrow(out_right_tmp),]
      # now if all no and initial_player is not all yes
    } else if(length(which_player_no) != nrow(out_right_tmp) & length(which_init_no) != 0){
      # keep first "no" to end of data
      out_right_tmp <- out_right_tmp[min(which_init_no):nrow(out_right_tmp),]
    }
    ################################
    
    # now did rush happen left or right of right lineman
    if(sum(str_detect(out_right_tmp$pass_player, "yes")) >= 1 ){
      out_right <- out_right_tmp |> 
        filter(pass_player == "yes") |> 
        slice_min(frame_id) |> 
        mutate(which_side = ifelse(y_ball > y, "left", "right"))
    } else if(sum(str_detect(out_right_tmp$pass_initial, "yes") ) >= 1){
      # if never passes player let's use when ball passes
      # initial player position
      out_right <- out_right_tmp |> 
        filter(pass_initial == "yes") |> 
        slice_min(frame_id) |> 
        mutate(which_side = ifelse(y_ball > out_right_initial$y, "left", "right"))
    
      # if distance is marginal let's verify end position
      if(round(abs(out_right$y_ball - out_right_initial$y), 1) <= 0.5){
        out_right <- out_right_tmp |> 
          filter(event == "first_contact" | event == "touchdown" | 
                   event == "tackle" | event == "qb_sack" |
                   event == "fumble" | event == "out_of_bounds") |> 
          slice_max(frame_id) |> 
          mutate(which_side = ifelse(y_ball > out_right_initial$y, "left", "right"))
      }  
      
    } else{
      # if never passes line of play
      # when the first_contact happens
      out_right <- out_right_tmp |> 
        filter(event == "first_contact" | event == "touchdown" | 
                 event == "tackle" | event == "qb_sack" |
                 event == "fumble" | event == "out_of_bounds") |> 
        slice_max(frame_id) |> 
        mutate(which_side = ifelse(y_ball > out_right_initial$y, "left", "right"))
    }
  }
  #######################################################################
  #######################################################################
  # organize output data
  run_calc <- tibble(
    game_id = game_tag,
    play_id = play_tag,
    rush_loc_calc = case_when(
      play_dir == "left" & 
        ball_initial$y > out_left$y_ball & 
        out_left$which_side == "left" ~ "left",
      play_dir == "left" & 
        ball_initial$y < out_right$y_ball & 
        out_right$which_side == "right" ~ "right",
      play_dir == "right" & 
        ball_initial$y < out_left$y_ball & 
        out_left$which_side == "left" ~ "left",
      play_dir == "right" & 
        ball_initial$y > out_right$y_ball & 
        out_right$which_side == "right" ~ "right",
      out_right$which_side == "left" & 
        out_left$which_side == "left" ~ "left",
      out_left$which_side == "right" & 
        out_right$which_side == "right" ~ "right",
      out_left$which_side == "right" & 
        out_right$which_side == "left" ~ "middle",
      TRUE ~ "middle"
    )
  )
  
  return(run_calc)
}


run_plays_dir <- read_rds(here("data/run_plays_dir.rds") )

check <- run_plays_dir |> 
  filter(rush_loc_calc == "check")
#2022091802_1034
game_tag = 2022091802
play_tag = 1034

tmp_data <- track1 |> 
  filter(frame_type == "AFTER_SNAP")
ggplot() +
  geom_point(data = tmp_data[tmp_data$nfl_id == 43305,],
             aes(x = x, y = y), color = "red") +
  geom_point(data = tmp_data[tmp_data$nfl_id == 53436,],
             aes(x = x, y = y), color = "blue") +
  geom_point(data = tmp_data[tmp_data$club == "football",],
             aes(x = x, y = y), color = "black") +
  ggtitle(unique_tag)
