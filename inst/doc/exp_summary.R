## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages, message=FALSE--------------------------------------------------
library(actxps)
library(dplyr)

exposed_data <- expose(census_dat, end_date = "2019-12-31",
                       target_status = "Surrender")


## ----xp-basic-----------------------------------------------------------------
exp_stats(exposed_data)

## ----claim-check--------------------------------------------------------------
(amount <- sum(exposed_data$status == "Surrender"))

## ----expo-check---------------------------------------------------------------
(sum_expo <- sum(exposed_data$exposure))

## ----q-check------------------------------------------------------------------
amount / sum_expo

## ----grouped-1----------------------------------------------------------------

exposed_data |> 
  group_by(pol_yr) |> 
  exp_stats()


## ----grouped-2----------------------------------------------------------------

exposed_data |> 
  group_by(inc_guar, pol_yr) |> 
  exp_stats()


## ----targ-status--------------------------------------------------------------

exposed_data |> 
  mutate(exposure = ifelse(status == "Death", 1, status)) |> 
  group_by(pol_yr) |> 
  exp_stats(target_status = c("Surrender", "Death"))


## ----weight-res---------------------------------------------------------------

exposed_data |> 
  group_by(pol_yr) |> 
  exp_stats(wt = 'pol_val')


## ----act-exp------------------------------------------------------------------

expected_table <- c(seq(0.005, 0.03, length.out = 10), 0.2, 0.15, rep(0.05, 3))

# using 2 different expected termination assumption sets
exposed_data2 <- exposed_data |>
  mutate(expected_1 = expected_table[pol_yr],
         expected_2 = ifelse(exposed_data$inc_guar, 0.015, 0.03))

exp_res <- exposed_data2 |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(expected = c("expected_1", "expected_2"))
  

exp_res |> 
  select(pol_yr, inc_guar, q_obs, expected_1, expected_2, 
         ae_expected_1, ae_expected_2)


## ----act-exp-wt---------------------------------------------------------------

exposed_data2 |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(expected = c("expected_1", "expected_2"), 
            wt = "pol_val") |> 
  select(pol_yr, inc_guar, q_obs, expected_1, expected_2, 
         ae_expected_1, ae_expected_2)


## ----cred1--------------------------------------------------------------------
exposed_data2 |> 
  group_by(pol_yr, inc_guar) |>
  exp_stats(credibility = TRUE) |> 
  select(pol_yr, inc_guar, claims, q_obs, credibility)

## ----cred2--------------------------------------------------------------------
exposed_data2 |> 
  group_by(pol_yr, inc_guar) |>
  exp_stats(credibility = TRUE, cred_p = 0.98, cred_r = 0.03) |> 
  select(pol_yr, inc_guar, claims, q_obs, credibility)

## ----cred3--------------------------------------------------------------------
exposed_data2 |> 
  group_by(pol_yr, inc_guar) |>
  exp_stats(credibility = TRUE, expected = "expected_1") |> 
  select(pol_yr, inc_guar, claims, q_obs, credibility, adj_expected_1, 
         expected_1, ae_expected_1)

## ----summary1-----------------------------------------------------------------
summary(exp_res)

## ----summary2-----------------------------------------------------------------
summary(exp_res, pol_yr)

## ----summary3-----------------------------------------------------------------
summary(exp_res, inc_guar)

## ----col-names, eval=FALSE----------------------------------------------------
#  exposed_data |>
#    exp_stats(col_status = "curr_stat")

## ----not-exposed_df-----------------------------------------------------------

not_exposed_df <- data.frame(exposed_data)

exp_stats(not_exposed_df)


## ----not-exposed_df-2---------------------------------------------------------
exp_stats(not_exposed_df, target_status = "Surrender")

