## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

actxps:::set_actxps_plot_theme()

## ----packages-----------------------------------------------------------------
library(actxps)
library(lubridate)

## ----agg-exp-1----------------------------------------------------------------
agg_sim_exp_df <- agg_sim_dat |> 
  as_exp_df(col_exposure = "exposure_n", col_claims = "claims_n",
            conf_int = TRUE,
            start_date = 2005, end_date = 2019, target_status = "Surrender")

## ----agg-exp-2----------------------------------------------------------------
summary(agg_sim_exp_df, pol_yr)

## ----agg-exp-3----------------------------------------------------------------
summary(agg_sim_exp_df, inc_guar, product)

## ----pol-dur1-----------------------------------------------------------------
dates <- ymd("2022-12-31") + years(0:10)

# policy years
pol_yr(dates, "2022-05-10")

# policy quarters
pol_qtr(dates, "2022-05-10")

# policy months
pol_mth(dates, "2022-05-10")

# policy weeks
pol_wk(dates, "2022-05-10")



## ----pol-dur2-----------------------------------------------------------------
# days
pol_interval(dates, "2022-05-10", days(1))

# fortnights
pol_interval(dates, "2022-05-10", weeks(2))


## ----add-preds, fig.height=4, fig.width=5-------------------------------------
# create exposure records
exposed_data <- expose(census_dat, end_date = "2019-12-31",
                       target_status = "Surrender") |> 
  filter(pol_yr <= 10) |> 
  # add a response column for surrenders
  mutate(surrendered = status == "Surrender")

# create a simple logistic model
mod <- glm(surrendered ~ pol_yr, data = exposed_data, 
           family = "binomial", weights = exposure)

exp_res <- exposed_data |> 
  # attach predictions
  add_predictions(mod, type = "response", col_expected = "logistic") |> 
  # summarize results
  group_by(pol_yr) |> 
  exp_stats(expected = "logistic")
 
# create a plot
plot_termination_rates(exp_res)


## ----recipe, warning=FALSE----------------------------------------------------
library(recipes)

recipe(~ ., data = census_dat) |> 
  step_expose(end_date = "2019-12-31", target_status = "Surrender")


