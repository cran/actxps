## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

actxps:::set_actxps_plot_theme()

## ----packages-----------------------------------------------------------------
library(actxps)
library(dplyr)

exposed_data <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
exposed_data

## ----wd-head------------------------------------------------------------------
withdrawals

## ----add-trx------------------------------------------------------------------
exposed_trx <- add_transactions(exposed_data, withdrawals)
glimpse(exposed_trx)

## ----print-trx-types----------------------------------------------------------
exposed_trx

## ----trx-basic----------------------------------------------------------------
trx_stats(exposed_trx)

## ----grouped-1----------------------------------------------------------------
exposed_trx |> 
  group_by(inc_guar) |> 
  trx_stats()


## ----grouped-2----------------------------------------------------------------
exposed_trx |> 
  group_by(pol_yr, inc_guar) |> 
  trx_stats()


## ----pct-of-------------------------------------------------------------------
# attach account values data
exposed_trx_w_av <- exposed_trx |> 
  left_join(account_vals, by = c("pol_num", "pol_date_yr"))

trx_res <- exposed_trx_w_av |> 
  group_by(pol_yr, inc_guar) |> 
  trx_stats(percent_of = "av_anniv")

glimpse(trx_res)


## ----trx-conf1----------------------------------------------------------------
exposed_trx |> 
  group_by(pol_yr) |> 
  trx_stats(conf_int = TRUE) |> 
  select(pol_yr, trx_util, trx_util_lower, trx_util_upper)

## ----trx-conf2----------------------------------------------------------------
exposed_trx |> 
  group_by(pol_yr) |> 
  trx_stats(conf_int = TRUE, conf_level = 0.9) |> 
  select(pol_yr, trx_util, trx_util_lower, trx_util_upper)

## ----trx-conf3----------------------------------------------------------------
exposed_trx_w_av |> 
  group_by(pol_yr) |> 
  trx_stats(conf_int = TRUE, percent_of = "av_anniv") |> 
  select(pol_yr, starts_with("pct_of")) |> 
  glimpse()

## ----trx-plot, warning=FALSE, message=FALSE, fig.height=5.5, fig.width=7------
library(ggplot2)

trx_res |>
  # remove periods with zero transactions
  filter(trx_n > 0) |> 
  autoplot(y = pct_of_av_anniv_w_trx)

## ----trx-table, eval = FALSE--------------------------------------------------
#  trx_res |>
#    # remove periods with zero transactions
#    filter(trx_n > 0) |>
#    # first 10 rows showed for brevity
#    head(10) |>
#    autotable()

## ----select-trx-type----------------------------------------------------------
trx_stats(exposed_trx, trx_types = "Base")

## ----combine-trx--------------------------------------------------------------
trx_stats(exposed_trx, combine_trx = TRUE)

## ----partial-expo-ok----------------------------------------------------------
trx_stats(exposed_trx, full_exposures_only = FALSE)

## ----summary1-----------------------------------------------------------------
summary(trx_res)

## ----summary2-----------------------------------------------------------------
summary(trx_res, pol_yr)

## ----summary3-----------------------------------------------------------------
summary(trx_res, inc_guar)

## ----col-names-1, eval=FALSE--------------------------------------------------
#  exposed_data |>
#    add_transactions(withdrawals, col_trx_type = "transaction_code")

