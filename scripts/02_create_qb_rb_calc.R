# load libraries
library(tidyverse)
library(here)

# get function to calculate qb and rb relationships
source(here("scripts/00_functions.R"))

# load data
# from 00_prep_run_data
tracking_runs <- read_rds(here("data/tracking_runs.rds"))

#####################################################
#####################################################
# distance of rb from qb
# orientation and direction facing
# location on left or right side of field

# function from 00_functions.R
qb_rb_calc <- qb_rb_fun(tracking_runs)

write_rds(qb_rb_calc, file = here("data/qb_rb_calc.rds"))
