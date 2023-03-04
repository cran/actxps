## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages-----------------------------------------------------------------
library(actxps)
library(lubridate)

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


