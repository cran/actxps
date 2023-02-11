## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages, message=FALSE--------------------------------------------------
library(actxps)
library(dplyr)

census_dat

## ----status-count-------------------------------------------------------------
(status_counts <- table(census_dat$status))

## ----naive--------------------------------------------------------------------
# incorrect
prop.table(status_counts)

## ----example------------------------------------------------------------------
exposed_data <- expose(census_dat, end_date = "2019-12-31",
                       target_status = "Surrender")

exposed_data

## ----term-rate----------------------------------------------------------------
sum(exposed_data$status == "Surrender") / sum(exposed_data$exposure)

## ----stats-1------------------------------------------------------------------
exp_stats(exposed_data)

## ----stats-grouped------------------------------------------------------------
library(dplyr)

exp_res <- exposed_data |>
  group_by(pol_yr, inc_guar) |>
  exp_stats()

exp_res

## ----stats-ae-----------------------------------------------------------------

expected_table <- c(seq(0.005, 0.03, length.out = 10), 0.2, 0.15, rep(0.05, 3))

# using 2 different expected termination rates
exposed_data <- exposed_data |>
  mutate(expected_1 = expected_table[pol_yr],
         expected_2 = ifelse(exposed_data$inc_guar, 0.015, 0.03))

exp_res <- exposed_data |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(expected = c("expected_1", "expected_2"))

exp_res


## ----plot, warning=FALSE, message=FALSE, dpi = 400----------------------------

library(ggplot2)

.colors <- c("#eb15e4", "#7515eb")
theme_set(theme_light())

exp_res |>
  autoplot() +
  scale_color_manual(values = .colors) +
  labs(title = "Observed Surrender Rates by Policy Year and Income Guarantee Presence")


## ----table, eval = FALSE------------------------------------------------------
#  autotable(exp_res)

## ----summary-1----------------------------------------------------------------
summary(exp_res)

## ----summary-2----------------------------------------------------------------
summary(exp_res, inc_guar)

## ----shiny, eval = FALSE------------------------------------------------------
#  exp_shiny(exposed_data)

