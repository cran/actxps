## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages, message=FALSE--------------------------------------------------
library(actxps)
library(dplyr)

toy_census

## ----expose-1-----------------------------------------------------------------
exposed_data <- expose(toy_census, end_date = "2022-12-31")


## ----is-exposed---------------------------------------------------------------
is_exposed_df(exposed_data)

## ----expose-pol-1-------------------------------------------------------------
exposed_data |> filter(pol_num == 1)

## ----expose-pol-2-------------------------------------------------------------
exposed_data |> filter(pol_num == 2)

## ----expose-pol-3-------------------------------------------------------------
exposed_data |> filter(pol_num == 3)

## ----expose-start-------------------------------------------------------------
expose(toy_census, end_date = "2022-12-31", start_date = "2019-12-31")

## ----expose-targ--------------------------------------------------------------
exposed_data2 <- expose(toy_census, end_date = "2022-12-31", 
                        target_status = "Surrender")

## ----expose-targ-check--------------------------------------------------------
exposed_data2 |> 
  group_by(pol_num) |> 
  slice_max(pol_yr)

## ----expo-cal-----------------------------------------------------------------
exposed_cal <- toy_census |> 
  expose(end_date = "2022-12-31", cal_expo = TRUE, target_status = "Surrender")

exposed_cal |> filter(pol_num == 2)


## ----expo-mth-----------------------------------------------------------------
toy_census |> 
  expose(end_date = "2022-12-31", 
         cal_expo = TRUE,
         expo_length = "quarter", 
         target_status = "Surrender") |> 
  filter(pol_num == 2)

## ----expo-split---------------------------------------------------------------
split <- expose_split(exposed_cal)

split |> filter(pol_num == 2) |> 
  select(cal_yr, cal_yr_end, pol_yr, exposure_pol, exposure_cal)

## ----split-stats-unclear, eval = FALSE----------------------------------------
#  exp_stats(split)

## ----split-stats-unclear-cat, echo = FALSE------------------------------------

tryCatch(exp_stats(split),
         error = function(e) cat(e$message))

## ----split-stats-clear--------------------------------------------------------
exp_stats(split, col_exposure = "exposure_pol")

## ----split-qtr----------------------------------------------------------------
expose_cq(toy_census, "2022-12-31", target_status = "Surrender") |> 
  expose_split() |> 
  filter(pol_num == 2) |> 
  select(cal_qtr, cal_qtr_end, pol_yr, exposure_pol, exposure_cal)

## ----rec-expose---------------------------------------------------------------
library(recipes)

expo_rec <- recipe(status ~ ., toy_census) |>
  step_expose(end_date = "2022-12-31", target_status = "Surrender",
              options = list(expo_length = "month")) |>
  prep()

expo_rec

tidy(expo_rec, number = 1)

bake(expo_rec, new_data = NULL)


## ----col-names, eval=FALSE----------------------------------------------------
#  expose(toy_census, end_date = "2022-12-31",
#         target_status = "Surrender",
#         col_pol_num = "id")

## ----broadcast----------------------------------------------------------------
toy_census2 <- toy_census |> 
  mutate(plan_type = c("X", "Y", "Z"), 
         policy_value = c(100, 125, 90))

expose(toy_census2, end_date = "2022-12-31", 
       target_status = "Surrender")

## ----join-ex, eval=FALSE------------------------------------------------------
#  
#  # Illustrative example - assume `values` is a data frame containing the columns pol_num and pol_yr.
#  
#  exposed_data |>
#    left_join(values, by = c("pol_num", "pol_yr"))
#  

## ----combine-1----------------------------------------------------------------
exposed_data2 <- expose(toy_census, 
                        end_date = "2023-12-31", 
                        start_date = "1890-01-01",
                        target_status = "Surrender")

vctrs::vec_rbind(exposed_data, exposed_data2)

## ----combine-2----------------------------------------------------------------
dplyr::bind_rows(exposed_data, exposed_data2)

