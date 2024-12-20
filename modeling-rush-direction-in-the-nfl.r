---
title: "Pre-Snap Advantage: Modeling Rush Direction in the NFL"
author: "Danielle Sass"
output:
  bookdown::html_document2:
    self_contained: true
    number_sections: false
---

```{r}
#| include: false

knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

library(tidyverse)
library(gt)
library(gtExtras)
library(yardstick)
library(here)

predictions <- read_rds("../input/results/predictions.rds")
run_gap_table <- read_csv("../input/results/run_gap_table.csv")
vi_table <- read_rds("../input/results/vi_table.rds")

accuracy_main <- accuracy(predictions, 
                          truth = rush_loc_calc,
                          estimate =.pred_class_main) |> pull(.estimate) |> round(3)

accuracy_base <- accuracy(predictions, 
                          truth = rush_loc_calc,
                          estimate =.pred_class_base) |> pull(.estimate) |> round(3)

accuracy_naive <- accuracy(predictions, 
                          truth = rush_loc_calc,
                          estimate =.pred_class_naive) |> pull(.estimate) |> round(3)

```

# Introduction

In football, an offensive play typically involves either a pass or a rush. For a defensive team, 
accurately predicting the type of play offers a significant strategic advantage. For instance, 
anticipating a rush allows the defense to position more players in the box, while expecting a pass 
might prompt defenders to drop back into coverage. Prior research has demonstrated that 
distinguishing between pass and rush plays can be achieved with 75-80% accuracy using a limited 
set of variables (Joash Fernandez et al., 2020). Building on this foundation, our study focuses 
on enhancing the defensive advantage by predicting the direction of a rush; categorized as left, 
middle, or right. More importantly, we aim to incorporate novel spatial tracking variables and 
identify those that serve as key indicators of rush direction, providing deeper insights into 
offensive tendencies. 

# Data preparation

To predict rush direction, we use tracking, play-by-play, player, and game data from the 2022 NFL 
season, provided by the NFL Big Data Bowl 2025 (Lopez et al., 2024), along with supplementary data 
from the `nflverse` package (Carl et al., 2023). Our analysis focuses exclusively on rush plays, 
totaling 6,183 observations. Scramble plays, while typically categorized as rushes, were excluded 
since they originate as pass plays and involve decisions made after the snap. This aligns with our 
emphasis on leveraging pre-snap information.

## Rush direction

Upon visualizing a sample of plays, we identified discrepancies between the tracking data and the 
provided rush location types. Given the lack of detailed documentation on how rush direction was 
determined and to enhance prediction accuracy, we recalculated rush direction manually using the 
tracking data.

At its simplest, rush direction is categorized as left, right, or middle, based on the ball 
carrier`s 
path relative to the offensive linemen. Specifically, a rush is classified as:

<ul>
  <li>`left`: if the ball carrier advances past the leftmost offensive lineman on the left,</li>
  <li>`right`: if they advance past the rightmost offensive lineman on the right,</li>
  <li>`middle`: if they advance between these two outside linemen.</li>
</ul>

- `left`: if the ball carrier advances past the leftmost offensive lineman on the left,
- `right`: if they advance past the rightmost offensive lineman on the right,
- `middle`: if they advance between these two outside linemen.

\@ref(fig:fig-run) illustrates a play where the ball carrier rushes to the left of the leftmost offensive lineman. However, the data provided misclassified this play as "inside-left," which corresponds to "middle" under this definition.

```{r}
#| label: "fig-run"
#| fig.cap: "This play shows a direct snap to T. Etienne. T. Etienne rushes left of the outside linemen."
#| out.width: 90%

knitr::include_graphics("https://github.com/dsass1/nfl_bowl_2025/blob/main/images/01_run_definition.gif?raw=true")

```


In more complex scenarios where the ball carrier does not advance past the offensive linemen or 
does not cross the line of scrimmage, we use the coordinates of the outside linemen at the line 
set to determine whether the rusher moved to the left, right, or middle relative to the linemen.

## Spatial tracking variables

Pre-snap data was used to build a predictive model. When rushing, a player looks for a gap or 
hole created by their teammates to run through. We aim to investigate whether pre-snap player 
orientation and positioning offers insights into potential gap formation, which could signal the 
rusher`s intended direction.

### Sequential offensive player gap 

Assuming the offensive and defensive players near the line of scrimmage are positioned one yard 
away from it, we project all players positions one yard forward in the direction of their 
orientation. This adjustment aligns all players along the line of scrimmage. 

A gap is defined as an open space between two offensive players positioned consecutively, or 
between an offensive player and the sideline. Gaps are categorized as left, right, or middle 
based on their position relative to the outermost offensive linemen. The size of a gap is 
calculated as the distance between the two offensive players forming it. 

\@ref(fig:fig-run-gap2) demonstrates a play with no left gaps, two middle gaps, and two right gaps. 
When multiple gaps exist within a region, the largest gap is selected. The sequential offensive 
player gap values for this play are as follows:
 
  - `left`: 0
  - `right`: 21.34
  - `middle`: 1.87

```{r}
#| label: "fig-run-gap2"
#| fig-cap: "Sequential offensive player gaps exist between players labeled 2 and 3, 4 and 5, 7 and 8, 9 and the sideline because no defenders are positioned between them."
#| out.width: "100%"

knitr::include_graphics("https://github.com/dsass1/nfl_bowl_2025/blob/main/images/03_run_gap_manual.png?raw=true")
```

### Orientation gap

Now considering only the offensive players near the line of scrimmage, a gap is defined as an open 
space between two offensive players oriented away from each other, or if their is no neighboring 
offensive player if the player is oriented away from the sideline. If players are oriented in 
opposite directions the intuition is that they are trying to push the defense in opposite directions. 
Again, gaps are categorized as left, right, or middle based on their position relative to the 
outermost offensive linemen and the size of the gap is the distance between the two offensive 
players forming it. \@ref(fig:fig-run-gap) shows a left gap between the sideline and player 1; a middle gap 
between players 4 and 5; and a right gap between player 9 and the sideline. 

```{r}
#| label: "fig-run-gap"
#| fig-cap: "Orientation gaps for the play are highlighted in yellow, either between two offensive players or between an offensive player and the sideline. A negative orientation indicates the degree to which a player is angled to the left, while a positive orientation indicates the degree to which they are angled to the right."
#| fig.show: "hold"


knitr::include_graphics("https://github.com/dsass1/nfl_bowl_2025/blob/main/images/02_run_gap_manual.png?raw=true")


rownames(run_gap_table) <- c("player", "orientation")

run_gap_table |>
  gt(rownames_to_stub = T) |>
  fmt_number(rows = "player",
             decimals = 0) |>
  fmt_number(rows = "orientation",
             decimals = 2) |>
  tab_header(
    title = "Gaps defined by offensive player orientation at line set",
    subtitle = "Players oriented away from each other form a gap"
  ) |> 
  tab_style(
    style = list(
      cell_fill(color = "#F0E442", alpha = 0.5),
      cell_text(weight = "bold")
      ),
    locations = cells_body(
      columns = c(2, 5, 6, 10)
    )
  ) |>
  tab_options(
    column_labels.hidden = TRUE,
    heading.title.font.size = 12,
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = 11
  )

```

### Quarterback and running back variables

Additional spatial variables include the running back`s (RB) 
and quarter back`s (QB) orientation, 
the distance between the RB and QB, whether the RB is positioned to the left or right of the QB, 
and whether the RB was in motion or shifted pre-snap. 

We also incorporate the RB`s historical tendencies. For all games prior to the current week, 
we calculate the percentage of runs directed left, right, or middle by the RB. If multiple 
RBs participated in the play, we use the average of their tendencies. Similarly, we consider 
game-specific tendencies by calculating the percentage of runs to the left, right, and middle 
for all plays in the current game leading up to the play being analyzed.


## Other predictor variables

In addition to the spatial variables we consider the following contextual factors: `quarter`, `down`, `yards to go`, `absolute yardline number`, `possession team`, `defensive team`, `offense formation`, `receiver alignment`, `play clock at snap`, `coverage`, `number of defenders in box`, `number or running backs`, `number of wide receivers`, and an indicator if the play was `no huddle`. For detailed descriptions of these variables, refer to the data codebook.

# Model

We employed a boosted tree model to predict a play\'s rush direction, using v-fold cross-validation 
with 4 folds and 3 repeats. The data was split into a training set (weeks 1 – 6) and a testing set 
(weeks 7 – 9). To address the severe class imbalance in rush direction 
(`left`: 996; `right`: 1,133; `middle`: 2,099), we downsampled the training set to form a balanced 
dataset and mitigate overfitting.

All variables discussed in [Spatial tracking variables] and [Other predictor variables] were 
included in the model. See the [Appendix] for details on tuning specifications and optimal 
parameters chosen. Predictions on the test dataset achieved an accuracy of `r accuracy_main*100`%, 
as shown by the 830 plays predicted correct out of 1,955 plays in \@ref(fig:fig-conf-mat).

```{r}
#| label: "fig-conf-mat"
#| fig-cap: "Confusion matrix of prediction results."
# Visualize confusion matrix
predictions |> 
  conf_mat(rush_loc_calc, .pred_class_main) |> 
  autoplot(type = "heatmap")
```


The low accuracy is unsurprising, as offenses are unlikely to telegraph their plays. While a 
naive strategy of always predicting "middle" could yield higher accuracy (`r accuracy_naive*100`%)
due to the class imbalance, such an approach would provide no insights into variable importance. 
The accuracy of `r accuracy_main*100`% is certainly better than that naive approach on a balanced 
dataset (33.3%).

To evaluate whether the novel spatial tracking variables contribute meaningful insights into 
rush direction, we compared our model`s performance to a baseline model. The baseline model, 
also a tuned boosted tree, was built using only the variables described in 
[Other predictor variables]. Its accuracy on the testing set was `r accuracy_base*100`%.

# Insights

Including spatial tracking variables improves prediction accuracy by 
`r accuracy_main*100 - accuracy_base*100`%. To better understand the impact of these variables, 
we examine their importance as determined by the boosted tree model.

```{r}
#| label: "tbl-vi"
#| tbl-cap: "Top 10 significant variables from the boosted tree model."

vi_table |> 
  select(variable = var_names, importance) |> 
  gt() |> 
  fmt_number(columns = "importance",
             decimals = 4)
```

\@ref(fig:fig-rb-boxplot) explores the relationship between the percentage of time the RB on the play 
rushed left, right, and middle in all prior games and the rush direction of the current play. 
From the "RB historical left" plot, we observe that plays with a left rush direction tend to 
feature RBs with a higher median percentage of historically rushing left.
Similar trends are evident in the "RB historical right" and "RB historical middle" plots, 
where RBs with higher historical percentages in those directions are more likely to rush in the 
corresponding direction during the current play.

This pattern aligns with intuition: in a football game, power rushers often run up the middle, 
while shifty rushers use their speed to exploit gaps on the outside. However, it is important 
to note that early-season games had little to no historical data due to being the first games 
of the season. Incorporating data from the prior season could potentially improve prediction 
accuracy.

```{r}
#| label: "fig-rb-boxplot"
#| fig-cap: "The boxplots show that a larger median gap size corresponds to the rush direction of the play."
#| out.width: "100%"

knitr::include_graphics("https://github.com/dsass1/nfl_bowl_2025/blob/main/images/04_rb_boxplot.png?raw=true")

```

Next we examine the relationship between the normalized size of sequential offensive player gaps 
and rush direction, as shown in \@ref(fig:fig-gap-boxplot). The "left gap size" plot illustrates the 
distribution of left gap sizes across all rush plays. Notably, plays where the rusher ran left 
tend to have a larger median gap size compared to plays with middle or right rush directions. 
Similarly, larger middle gap sizes are associated with rushes through the middle, while larger 
right gap sizes correspond to rushes to the right. These patterns indicate a weak but observable 
relationship between spatial gap size and rush direction.

```{r}
#| label: "fig-gap-boxplot"
#| fig-cap: "The boxplots show that a larger median gap size corresponds to the rush direction of the play."
#| out.width: "100%"

knitr::include_graphics("https://github.com/dsass1/nfl_bowl_2025/blob/main/images/05_gap_boxplot.png?raw=true")

```

# Discussion

We utilized a boosted tree model to predict whether the rusher on a play runs to the left, 
right, or middle of the offensive linemen. The inclusion of novel spatial tracking variables 
improved prediction accuracy over the baseline model by `r accuracy_main*100 - accuracy_base*100`%. 
We identified a very weak relationship between gap sizes and rush direction, we also found that a 
running back`s 
(RB`s) historical tendencies have a small, yet measurable, influence on the rush 
location.

Establishing a strong run game is critical for an offense as it helps open up the passing game, 
wear down the defense, and control the clock. Conversely, limiting an opponent`s run game makes 
defensive play calling and execution easier. Although the overall model accuracy of 
`r accuracy_main*100`% is modest, the findings suggest that defenses should prepare for rushes 
in any direction and that post-snap developments may provide more actionable insights into rush 
direction.

Future research could focus on using functional data to evaluate an RB`s effectiveness in rushing 
middle versus outside routes or on quantifying an offensive line`s impact through post-snap 
spatial gap sizes and their contributions to successful rushing plays.


# References

Carl S, Baldwin B, Sharpe L, Ho T, Edwards J (2023). "nflverse". [https://nflverse.nflverse.com/](https://nflverse.nflverse.com/).

Joash Fernandes C, et al., (2020), "Predicting Plays in the National Football League". Journal of Sports Analytics 6: (1), 35 – 43. DOI: 0.3233/JSA-190348.

Lopez M, Bliss T, Blake A, Mooney P, and Howard A, (2024), "NFL Big Data Bowl 2025". [https://kaggle.com/competitions/nfl-big-data-bowl-2025](https://kaggle.com/competitions/nfl-big-data-bowl-2025), Kaggle.



# Appendix

All code is available at [https://github.com/dsass1/nfl_bowl_2025](https://github.com/dsass1/nfl_bowl_2025).

## Boosted tree specifications

The boosted tree models were run using the "xgboost" engine. The following parameters were tuned to 
find the optimal model fit:

  - number of trees (500 to 2000 over a regular grid of 4)
  - minimum number of nodes (2 to 40 over a regular grid of 5)
  - number of variables to try (5 to 25 over a regular grid of 5)
  - learn rate (-5 to -0.1 on a log10 scale over a regular grid of 10)
  
The optimal parameters for the model results were 2,000 trees, 6 nodes minimum, 15 variables to try, 
and a learn rate of 0.00282. And the optimal parameters for the baseline model results were 1,500 
trees, 23 nodes minimum, 15 variables to try, and a learn rate of 0.00282.
