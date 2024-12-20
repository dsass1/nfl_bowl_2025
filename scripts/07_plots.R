
library(tidyverse)
library(vip)
library(gganimate)
library(ggtext)
# needed for icons
library(showtext)
library(RColorBrewer)
library(here)
# trig in degrees
library(aspace)

# conflicted::conflict_prefer_all("dplyr")

# colorblind friendly palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load data
visual_run <- read_rds(here("data/visual_run.rds"))
visual_gap <- read_rds(here(paste0("data/visual_gap.rds"))) |> 
  filter(event == "line_set")

load(here("recipes/split_down.rda"))
run_plays_final <- read_rds(here("data/run_plays_final.rds") )

# split the data
train <- run_plays_final |> 
  filter(week <= 6)

###################################################
# VI results
load(here("results/bt_results_main.rda"))

vi_table <-  bt_results_main |> 
  vip::vi() |> 
  janitor::clean_names() |> 
  slice_max(importance, n = 10) |> 
  mutate(var_names = c("RB historical percent right",
                       "sequential offensive player gap left",
                       "RB historical percent left",
                       "RB orientation",
                       "sequential offensive player gap right",
                       "RB historical percent middle",
                       "orientation gap right",
                       "orientation gap left",
                       "sequential offensive player gap middle",
                       "absolute yardline")
  )

write_rds(vi_table, here("results/vi_table.rds"))

###################################################
# ANIMATION ON RUSH DIRECTION DEFINITION
###################################################
# Defining rush direction
#2022092509 2829
plays <- read_csv("data/raw/plays.csv")
plays |> 
  filter(gameId == 2022092509, playId == 2829) |> 
  pull(playDescription)

line_of_scrimmage <- visual_run |> 
  filter(club == "football", event == "line_set")

# only track frames after snap up until rusher passes
rusher <- visual_run |> 
  filter(club == "JAX") |> 
  filter(frame_type %in% c("SNAP", "AFTER_SNAP")) |> 
  filter(jersey_number == 1) |> 
  select(frame_id, x_run = x, y_run = y)

frame_pass <- visual_run |> 
  filter(club == "JAX") |> 
  filter(frame_type %in% c("SNAP", "AFTER_SNAP")) |> 
  filter(jersey_number %in% c(74, 75)) |> 
  left_join(rusher) |> 
  mutate(pass = ifelse(x > x_run, "yes", "no")) |>
  group_by(jersey_number) |> 
  filter(pass == "yes") |> 
  slice_min(frame_id) |> 
  ungroup() |> 
  slice_max(frame_id) |> 
  pull(frame_id)

##############################################
# tracking main players
run_1_offense <- visual_run |> 
  filter(club == "JAX") |> 
  filter(frame_type %in% c("AFTER_SNAP")) |> 
  filter(frame_id <= frame_pass) |> 
  mutate(left_lineman = ifelse(jersey_number == 74, TRUE, FALSE),
         right_lineman = ifelse(jersey_number == 75, TRUE, FALSE),
         outside_lineman = ifelse(position == "T", TRUE, FALSE),
         position = ifelse(!(jersey_number %in% c(1, 74, 75)), "O", position))

run_1_track <- run_1_offense |>
  filter(jersey_number %in% c(1, 74, 75)) |> 
  group_by(jersey_number) |> 
  mutate(xlag = dplyr::lag(x),
         ylag = dplyr::lag(y)) |> 
  ungroup()
  
run_1_football <- visual_run |> 
  filter(club == "football") |> 
  filter(frame_type %in% c("SNAP", "AFTER_SNAP"))

# get coordinates for field
frame_x <- run_1_track |> 
  dplyr::summarize(min = floor(min(run_1_track$x)/5)*5,
            max = ceiling(max(run_1_track$x)/5)*5)

## stationary initial offense position for points
run_1_lineset <- visual_run |> 
  filter(club == "JAX") |>  
  filter(event == "line_set") |> 
  select(-frame_id) |> 
  mutate(position = ifelse(!(jersey_number %in% c(1, 74, 75)), "O", position))

# labels for plot
label_info <- tibble(
  label = c("line of scrimmage", "outside\nlinemen", "ball carrier"),
  x = c(26.3, 27.4, 31.2),
  y = c(48.7, 32, 35)
)

label_arrows <- tibble(
  x = c(26.2, 27.25, 27.25, 31.2),
  xend = c(line_of_scrimmage$x+.1, 
           26.2+.2, 26.2+.2, 30.3+.2),
  y = c(48.5, 32, 32, 35),
  yend = c(48.5, 26.7, 32.8, 29.8+.3)
)


hash_lines <- tibble(
  x = c(seq(frame_x$min, frame_x$max, 1),
        seq(frame_x$min, frame_x$max, 1),
        seq(frame_x$min, frame_x$max, 1),
        seq(frame_x$min, frame_x$max, 1)), 
  xend = c(seq(frame_x$min, frame_x$max, 1), 
           seq(frame_x$min, frame_x$max, 1),
           seq(frame_x$min, frame_x$max, 1),
           seq(frame_x$min, frame_x$max, 1)),
  y = c(rep(19, frame_x$max - frame_x$min + 1), 
        rep(53.3 - 19, frame_x$max - frame_x$min + 1),
        rep(1, frame_x$max - frame_x$min + 1), 
        rep(53.3 - 1, frame_x$max - frame_x$min + 1)),
  yend = c(rep(20.5, frame_x$max - frame_x$min + 1), 
           rep(53.3 - 20.5, frame_x$max - frame_x$min + 1),
           rep(2.5, frame_x$max - frame_x$min + 1), 
           rep(53.3 - 2.5, frame_x$max - frame_x$min + 1))
)


anim1 <- ggplot() +
  # 5 yard marks
  geom_vline(xintercept = seq(frame_x$min, frame_x$max, 5), color = "#bebebe") +
  geom_segment(data = hash_lines, 
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "#bebebe") +
  # static players at line set
  geom_point(data = run_1_lineset, 
             aes(x, y, fill = position), 
             color = "black", 
             size = 3, shape = 21
             ) +
  # line of scrimmage
  geom_vline(xintercept = line_of_scrimmage$x,
             color = cbp1[7], linetype = "dashed",
             size = 1) +
  
  # labels
  geom_text(data = label_info, 
            aes(x = x, y = y, label = label),
            hjust = 0,
            size = 9,
            size.unit = "pt") +
  geom_segment(data = label_arrows, aes(x = x, y = y,
                                        xend = xend, yend = yend),
               arrow = arrow(length = unit(0.3,"cm"))) +
  # track players
  # geom_point(data = run_1_track, 
  #            aes(x, y, #group = x, 
  #                color = position), 
  #             size = 3) +
  geom_segment(data = run_1_track, 
             aes(x = xlag, y = ylag, 
                 xend = x, yend = y,
                 color = position),
             lineend = "round",
             size = 1.5) +
  transition_time(frame_id) +
  scale_color_manual(values = c("T" = cbp1[6], 
                                "RB" = cbp1[2],
                                "O" = cbp1[1]),
                     ) +
  scale_fill_manual(values = c("T" = cbp1[6], 
                                "RB" = cbp1[2],
                                "O" = cbp1[1]),
  ) +
  ease_aes("linear") +
  #xlim(c(frame_x$min, frame_x$max)) +
  #ylim(c(0, 53.3)) +
  coord_cartesian(xlim = c(frame_x$min, frame_x$max),
                  ylim = c(0, 53.3),
                  expand = FALSE) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.subtitle = element_text(size = 11, face = "italic", hjust = 0.5),
        plot.title = ggtext::element_markdown(hjust = 0.5, size = 12),
        text = element_text(family = "Chivo", color = "#26282A"),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  labs(title = "Ball carrier rushes **left** of outside linemen",
       caption = "Game ID: 2022092509\nPlay ID: 2829") +
  shadow_mark(
    size = 1.5)


run_dir_1 <- animate(
  anim1,
  width = 520,
  height = 320,
  duration = 6.3,
  fps = 10,
  end_pause = 20,
  res = 105
)

run_dir_1 

anim_save("images/01_run_definition.gif", run_dir_1)
###################################################
###################################################
###################################################
# GAP VARIABLE CALCULATION
# icon: user or person or street view

# download icons here: https://fontawesome.com/download
# put .otf files in working directory
# font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
#font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

# 2022092509_3135

# get player positions
line_of_scrimmage <- visual_gap |> 
  filter(club == "football") |> 
  pull(x)
  
tm_o <- visual_gap |> 
  filter(position == "QB") |> 
  distinct(club) |> 
  pull(club)
  
tm_d <- visual_gap |> 
  filter(club != "football" & club != tm_o) |> 
  distinct(club) |> 
  pull(club)

play_dir <- unique(visual_gap$play_direction)
  
line_set <- visual_gap |> 
  filter(club != "football") |> 
  # only players at line 
  filter(x < line_of_scrimmage + 2.5 & 
           x > line_of_scrimmage - 2.5) |> 
  filter(position != "QB") |> 
  mutate(label = "<span style='font-family: \"fa-solid\"'>&#xf007;</span>") |> 
  mutate(play_direction = case_when(
    club == tm_o ~ play_direction,
    club == tm_d & play_direction == "left" ~ "right",
    club == tm_d & play_direction == "right" ~ "left"
  )) |> 
  group_by(nfl_id) |> 
  mutate(angle = ifelse(play_dir == "left", 
                        o + 90,
                        o - 90)) |> 
  # adjust orientation
  mutate(
    o_adj = case_when(
      # same as above
      play_direction == "right" & o <= 270 ~ (o - 90),
      play_direction == "right" & o > 270 ~ (o - 360- 90),
      play_direction == "left" & o <= 90 ~ o + 90,
      play_direction == "left" & o > 90 ~ o - 360 + 90
    )
  ) |> 
  # let's have everyone take 1 step forward ~ 0.5 yards
  mutate(
    # we use orientation as the direction they are headed
    x_add = case_when(
      # x is decreasing
      play_direction == "left" ~ abs(cos_d(o_adj) )*
        sign(180-o),
      # x is increasing
      play_direction == "right" ~ abs(cos_d(o_adj))*
        sign(180-o)
    ),
    y_add = abs(sin_d(o_adj))*sign(o_adj),
    y_add = case_when(
      # x is decreasing
      play_direction == "left" ~ abs(sin_d(o_adj))*sign(o_adj),
      # x is increasing
      play_direction == "right" ~ abs(sin_d(o_adj))*sign(-o_adj)
    ),
    xend = x + x_add,
    yend = y + y_add
  ) |> 
  ungroup()

offense <- line_set |> 
  filter(club == tm_o, position != "QB") 
  
defense <- line_set |> 
  filter(club == tm_d)
  
# create table of gap line
adj_plot <- 10 # so not as wide for visual
sideline <- tibble(y = c(0+adj_plot, 53.3-adj_plot),
                   o_adj = c(0, 0) )
  
# create lines for left, middle, right
gap_bounds <- offense |> 
  filter(position == "T") |> 
  dplyr::summarize(sideline_left = sideline$y[1],
            lineman_left = min(y),
            lineman_right = max(y),
            sideline_right = sideline$y[2]
            )
  
gap_df <- tibble(
  name = colnames(gap_bounds),
  y = unlist(as.vector(gap_bounds))
)

gap_df$yend = lag(gap_df$y)
gap_df$label = c("NA","left", "middle", "right")
gap_df$ymid = (gap_df$yend + gap_df$y)/2

# create table of gap line
gap_define <- offense |> 
  select(y, o_adj) |> 
  rbind(sideline) |> 
  arrange(y) |> 
  mutate(y_lag = lag(y),
         o_lag = lag(o_adj),
         gap = case_when(
           o_adj >= 0 & o_lag <= 0 ~ "yes",
           TRUE ~ "no"
         )
  )
  
showtext_auto() 
showtext_opts(dpi = 250)

#showtext::showtext_opts(dpi = knitr::opts_chunk$get()$dpi)

run_gap_1 <- ggplot() +
  # outline for left/middle/right
  geom_rect(aes(xmin=gap_df$y[2:4], 
                xmax=gap_df$yend[2:4], 
                ymin= 1.25, 
                ymax= 1.5),
            fill = NA, 
            color = "black",
            linewidth = 0.1
            #linetype = "dashed"
            ) +
  # define gap
  geom_rect(
    data = gap_define,
    aes(xmin = y_lag, xmax = y,
        min = 0.5, ymax = 1.25,
        fill = gap, alpha = gap),
    color = "black",
    linewidth = 0.1
  ) +
  # left, middle, right text
  geom_text(data = gap_df, 
            aes(x = ymid, y = 1.25 + (1.5-1.25)/2, 
                label = label),
            size = 11, size.unit = "pt") +
  # degree
  annotate("text",
           x = c(40, 41.5, 42.8), 
           y = c(0.6, 0.75, 0.6), 
           label = c("-90°", "0°", "90°"),
           size = 9, size.unit = "pt") +
  geom_richtext(aes(x = 41.4, y = 0.6,
                    label = "<span style='font-family: \"fa-solid\"'>&#xf007;</span>",
                    angle = 0),
                # remove label and outline
                fill = NA, label.colour = NA,
                col = "black", #cbp1[6], 
                size = 6) +
  geom_point(aes(x = 41.4, y = 0.6),
             shape = 1, size = 17) +
  # user icons
  geom_richtext(data = offense,
                aes(x = y, y = 1,
                    label = label,
                    angle = -o_adj),
                # "<span style='font-family: \"fa-solid\"'>&#xf007;</span>"),
                # remove label and outline
                fill = NA, label.colour = NA,
                col = cbp1[6],
                size = 6) +
  # add numbers to players
  geom_label(data = gap_define[2:10,], 
            aes(x = y, y = 0.75, 
                label = seq(1, 9)),
            size = 11, size.unit = "pt") +
  scale_fill_manual(values = c("yes" = cbp1[5], 
                                "no" = cbp1[1])
  ) +
  scale_alpha_manual(values = c("yes" = 0.5, 
                               "no" = 0.25)
  ) +
  coord_cartesian(xlim = c(0+adj_plot, 53.3-adj_plot), 
                  ylim = c(0.5, 1.5), 
                  expand = FALSE) +
  theme_minimal() +
  #labs(title = "Gaps defined by offensive player orientation at line set",
  #     subtitle = "Players oriented away from each other form a gap"
  #     ) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.subtitle = element_text(size = 11, face = "italic", hjust = 0.5),
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
  
print(run_gap_1)

# use export it saves better
# ggsave("images/02_run_gap.png", 
#        run_gap_1,
#        width = 1116,
#        height = 327, 
#        units = "px",
#        dpi = 250
#        )

run_gap_table <- tibble(
  player = 1:9,
  orientation = arrange(offense, y)$o_adj
) |> 
  t() |> 
  as.data.frame()

rownames(run_gap_table) <- c("player", "orientation")

write_csv(run_gap_table, here("results/run_gap_table.csv"))


#################################
# sequential offense
# create table of gap line
adj_plot <- 10 # so not as wide for visual
sideline <- tibble(yend = c(0+adj_plot/2, 53.3-adj_plot),
                   club = c(tm_o, tm_o) )

# create lines for left, middle, right
gap_bounds <- offense |> 
  filter(position == "T") |> 
  dplyr::summarize(sideline_left = sideline$yend[1],
                   lineman_left = min(yend),
                   lineman_right = max(yend),
                   sideline_right = sideline$yend[2]
  )

gap_df <- tibble(
  name = colnames(gap_bounds),
  y = unlist(as.vector(gap_bounds))
)

gap_df$yend = lag(gap_df$y)
gap_df$label = c("NA","left", "middle", "right")
gap_df$ymid = (gap_df$yend + gap_df$y)/2

# create table of gap line
gap_define <- line_set |> 
  select(yend, club) |> 
  rbind(sideline) |> 
  arrange(yend) |> 
  mutate(club_lag = lag(club)) |> 
  filter(club == tm_o) |> 
  mutate(
    y_lag = lag(yend),
    gap = case_when(
           club == club_lag ~ "yes",
           TRUE ~ "no"
         )
  )

showtext_auto()
showtext_opts(dpi = 250)

ymin = -0.1
run_gap_2 <- ggplot() +
  # outline for left/middle/right
  geom_rect(aes(xmin=gap_df$y[2:4], 
                xmax=gap_df$yend[2:4], 
                ymin= .25, 
                ymax= .5),
            fill = NA, 
            color = "black",
            linewidth = 0.1
            #linetype = "dashed"
  ) +
  # define gap
  geom_rect(
    data = gap_define,
    aes(xmin = y_lag, xmax = yend,
        min = ymin, ymax = 0.25,
        fill = gap, alpha = gap),
    color = "black",
    linewidth = 0.1
  ) +
  # left, middle, right text
  geom_text(data = gap_df, 
            aes(x = ymid, y = .25 + (.5-.25)/2, 
                label = label),
            size = 11, size.unit = "pt") +
  # user icons offense start
  geom_richtext(data = offense,
                aes(x = y, y = 0.65,
                    label = label,
                    angle = -o_adj),
                # remove label and outline
                fill = NA, label.colour = NA,
                col = cbp1[6],
                size = 6,
                alpha = 0.5) +
  # label offense
  geom_segment(
    aes(x = 39, y = .65,
        xend = 37, yend = 0.65),
    arrow = arrow(length = unit(0.3,"cm")),
    col = cbp1[6], alpha = 0.5
  ) +
  annotate("text",
           x = 40, y = .65, label = "offense",
           hjust = 0,
           size = 11, size.unit = "pt"
  ) +
  # points where offense end
  geom_point(data = offense,
                aes(x = yend, y = .875
                    ),
                col = cbp1[6],
                size = 4
             ) +
  # user icons
  geom_richtext(data = defense,
                aes(x = y, y = 1.1,
                    label = label,
                    angle = -o_adj + 180),
                # "<span style='font-family: \"fa-solid\"'>&#xf007;</span>"),
                # remove label and outline
                fill = NA, label.colour = NA,
                col = cbp1[7],
                size = 6,
                alpha = 0.5) +
  # label defense
  geom_segment(
    aes(x = 39, y = 1.1,
        xend = 37, yend = 1.1),
    arrow = arrow(length = unit(0.3,"cm")),
    col = cbp1[7], alpha = 0.5
  ) +
  annotate("text",
           x = 40, y = 1.1, label = "defense",
           hjust = 0,
           size = 11, size.unit = "pt"
  ) +
  geom_point(data = defense,
                aes(x = yend, y = 0.875
                    ),
                col = cbp1[7],
                size = 4) +
  geom_segment(
    data = offense,
    aes(x = y, y = .65,
        xend = yend, yend = 0.875),
    arrow = arrow(length = unit(0.3,"cm")),
    col = cbp1[6], alpha = 0.5
  ) +
  geom_segment(
    data = defense,
    aes(x = y, y = 1.1,
        xend = yend, yend = 0.875),
    arrow = arrow(length = unit(0.3,"cm")),
    col = cbp1[7], alpha = 0.5
  ) +
  # a closer look at points
  geom_point(data = filter(line_set, club != "football"),
             aes(x = yend, y = 0.15,
                 col = club
             ),
             size = 4
  ) +
  # add numbers to players
  geom_label(data = gap_define[2:10,],
             aes(x = yend, y = 0.00,
                 label = seq(1, 9)),
             size = 11, size.unit = "pt") +
  geom_curve(aes(x = 13, xend = 13,
                   y = 0.85, yend = 0.15),
               arrow = arrow(length = unit(0.3,"cm")),
             curvature = .75) +
  scale_fill_manual(values = c("yes" = cbp1[5], 
                               "no" = cbp1[1]),
                    guide = "none"
  ) +
  scale_color_manual(values = c("LAC" = cbp1[6], 
                               "JAX" = cbp1[7]),
                     labels = c("defense",
                                "offense")
  ) +
  scale_alpha_manual(values = c("yes" = 0.5, 
                                "no" = 0.25),
                     guide = "none"
  ) +
  coord_cartesian(xlim = c(0+adj_plot/2, 53.3-adj_plot), 
                  ylim = c(ymin, 1.25), 
                  expand = FALSE) +
  theme_minimal() +
  labs(title = "Gaps defined by two offensive players in a row",
       subtitle = "Offense and defense positions are projected one yard forward onto the line of scrimmage"
  ) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        #legend.position = c(1, 1),
        #legend.justification = c(1,1),
        #legend.title = element_blank(),
        #legend.background = element_rect(linewidth = 0.1),
        plot.subtitle = element_text(size = 11, face = "italic", hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

print(run_gap_2)

# use export it saves better
# ggsave("images/03_run_gap.png", run_gap_2,
#        width = 1116, height=327,
#        units = "px", dpi = 250
#        )


### actual gap values
# left_gap_o = 15.35623
# left_gap_tm = 0
# middle_gap_o = 1.868662
# middle_gap_tm = 1.868662
# right_gap_o = 21.33704
# right_gap_tm = 21.33704
###################################################
###################################################
showtext_auto(FALSE)
# important variable 1: RB historical tendencies
rb_boxplot <- train |> 
  select(rush_loc_calc, 
         `RB historical left`= rb_pct_left, 
         `RB historical right` = rb_pct_right, 
         `RB historical middle` = rb_pct_middle) |> 
  pivot_longer(
    cols = -rush_loc_calc,
    names_to = "rb_loc",
    values_to = "loc_pct"
  ) |> 
  mutate(
    shade = case_when(
      rb_loc == "RB historical left"  & rush_loc_calc == "left" ~ "yes",
      rb_loc == "RB historical right"  & rush_loc_calc == "right" ~ "yes",
      rb_loc == "RB historical middle"  & rush_loc_calc == "middle" ~ "yes",
      TRUE ~ "no"
    )
  ) |>
  ggplot(aes(x = rush_loc_calc, y = loc_pct,
             fill = shade)) +
  geom_boxplot(alpha = 0.5) +
  facet_wrap(~rb_loc) +
  theme_bw() +
  scale_fill_manual(values = c("yes" = cbp1[6], 
                               "no" = "white"),
                    guide = NULL) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = NULL,
       x = "rush location",
       #title = "Distribution of RBs historical rush direction percent by the plays actual rush location") +
       title = "Relationship between a play's rush location and the RB's historical rush location tendencies") +
  theme(strip.background =element_rect(fill="#e6e6e6"),
        plot.title = element_text(hjust = 0.5, size = 12),
  )


print(rb_boxplot)

# ggsave("images/04_rb_boxplot.png", rb_boxplot,
#        width = 857, height=450,
#        units = "px" #, dpi = 250
# )

# important variable 3: rb orientation
train |> 
  ggplot(aes(x = rush_loc_calc, y = o_rb_adj)) +
  geom_boxplot()

# important variable 2: team gap
# normalize values
gap_boxplot <- train |> 
  # normalize variables
  mutate(left_gap_norm = (left_gap_tm - mean(left_gap_tm))/sd(left_gap_tm),
         right_gap_norm = (right_gap_tm - mean(right_gap_tm))/sd(right_gap_tm),
         middle_gap_norm = (middle_gap_tm - mean(middle_gap_tm))/sd(middle_gap_tm)) |> 
  select(rush_loc_calc, 
         `left gap size`= left_gap_norm, 
         `right gap size` = right_gap_norm, 
         `middle gap size` = middle_gap_norm) |> 
  pivot_longer(
    cols = -rush_loc_calc,
    names_to = "gap_loc",
    values_to = "gap_size"
  ) |> 
  mutate(
    shade = case_when(
      gap_loc == "left gap size"  & rush_loc_calc == "left" ~ "yes",
      gap_loc == "right gap size"  & rush_loc_calc == "right" ~ "yes",
      gap_loc == "middle gap size"  & rush_loc_calc == "middle" ~ "yes",
      TRUE ~ "no"
    )
  ) |>
  ggplot(aes(x = rush_loc_calc,
             y = gap_size,
             fill = shade)) +
  geom_boxplot(alpha = 0.5) +
  facet_wrap(~gap_loc) +
  theme_bw() +
  scale_fill_manual(values = c("yes" = cbp1[6], 
                               "no" = "white"),
                    guide = NULL) +
  labs(y = NULL,
       x = "rush location",
       title = "Normalized distribution of gap size by rush location") +
  ylim(-3,3) +
  theme(strip.background =element_rect(fill="#e6e6e6"),
        plot.title = element_text(hjust = 0.5, size = 12),
  )


# okay
#check <- train |> 
#  filter(middle_gap_tm > 5)

print(gap_boxplot)

# ggsave("images/05_gap_boxplot.png", gap_boxplot,
#        width = 857, height=450,
#        units = "px"
# )

# important variable: left gap o
train |> 
  select(rush_loc_calc, 
         `left gap size`= left_gap_o, 
         `right gap size` = right_gap_o, 
         `middle gap size` = middle_gap_o) |> 
  pivot_longer(
    cols = -rush_loc_calc,
    names_to = "gap_loc",
    values_to = "gap_size"
  ) |> 
  ggplot(aes(x = rush_loc_calc,
             y = gap_size)) +
  geom_boxplot() +
  facet_wrap(~gap_loc) +
  theme_minimal() +
  labs(y = "gap size",
       x = "rush location")

gap_boxplot

