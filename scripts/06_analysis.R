
library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)
library(doParallel)

load("recipes/split_down.rda")

load("results/tuned_bt_base.rda")
load("results/tuned_bt_main.rda")

model_set <-
  as_workflow_set(
    "bt_base" = tuned_bt_base,
    "bt_main" = tuned_bt_main
  )


summary <- model_set |>
  collect_metrics() |>
  filter(.metric == "accuracy") |>
  #arrange(desc(mean)) |>
  group_by(wflow_id) |> 
  slice_max(mean) |> 
  select(wflow_id, mean) |> 
  arrange(desc(mean))

summary

autoplot(tuned_bt_main, metric = "accuracy")
autoplot(tuned_bt_base, metric = "accuracy")

show_best(tuned_bt_main, metric = "accuracy")
show_best(tuned_bt_base, metric = "accuracy")

select_best(tuned_bt_main, metric = "accuracy")
select_best(tuned_bt_base, metric = "accuracy")

# FINALIZE AND PREDICT ----------------------------------------------------
bt_workflow1 <- extract_workflow(tuned_bt_main) |>  
  finalize_workflow(select_best(tuned_bt_main, 
                                metric = "accuracy"))
bt_results_main <- fit(bt_workflow1, train_down)

bt_workflow2 <- extract_workflow(tuned_bt_base) |>  
  finalize_workflow(select_best(tuned_bt_base, 
                                metric = "accuracy"))
bt_results_base <- fit(bt_workflow2, train_down)

bt_results_main |> 
  vip::vip(num_features = 20)

vi_table <- bt_results_main |> 
  vip::vi() |> 
  janitor::clean_names()

vi_table <- vi_table[1:10,] |> 
  mutate(var_names = c("distance between QB and RB",
                       "sequential offensive player gap left",
                       "RB orientation",
                       "RB direction",
                       "QB direction",
                       "sequential offensive player gap middle",
                       "QB orientation",
                       "sequential offensive player gap right",
                       "orientation gap left",
                       "orientation gap right")
         )

## predict our test data
results <- test |> 
  select(rush_loc_calc) |> 
  bind_cols(predict(bt_results_main, new_data = test,
                    type = "class")) |>  
  rename(.pred_class_main = .pred_class) |> 
  bind_cols(predict(bt_results_base, new_data = test,
                    type = "class")) |> 
  rename(.pred_class_base = .pred_class) |> 
  mutate(.pred_class_naive = "middle",
         .pred_class_naive = factor(.pred_class_naive,
                                    levels = c("left","middle", "right")))

## conf mat
conf_mat(results, 
         truth = rush_loc_calc, 
         estimate = .pred_class_main)
conf_mat(results, 
         truth = rush_loc_calc, 
         estimate = .pred_class_base)

accuracy(results, 
         truth = rush_loc_calc, 
         estimate = .pred_class_main)
accuracy(results, 
         truth = rush_loc_calc, 
         estimate = .pred_class_base)
# naive guess of "middle"
accuracy(results, 
         truth = rush_loc_calc, 
         estimate = .pred_class_naive)
#.552


save(bt_results_main, file = here("results/bt_results_main.rda"))
write_rds(results, here("results/results.rds"))
write_rds(vi_table, here("results/vi_table.rds"))
