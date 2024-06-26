study_py <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")

expected_table <- c(seq(0.005, 0.03, length.out = 10), 0.2, 0.15, rep(0.05, 3))

set.seed(123)
study_py <- study_py |>
  mutate(expected_1 = expected_table[pol_yr],
         expected_2 = ifelse(inc_guar, 0.015, 0.03),
         weights = rnorm(nrow(study_py), 100, 50) |> abs())

exp_res <- study_py |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(expected = c("expected_1", "expected_2"),
            credibility = TRUE, conf_int = TRUE)

exp_res_weighted <- study_py |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(expected = c("expected_1", "expected_2"),
            credibility = TRUE, wt = "weights", conf_int = TRUE)


test_that("Partial credibility is between 0 and 1", {
  expect_lte(max(exp_res$credibility, exp_res$q_obs), 1)
  expect_gte(min(exp_res$credibility, exp_res$q_obs), 0)
})


test_that("Experience study summary method checks", {
  expect_identical(exp_res, summary(exp_res, pol_yr, inc_guar))
  expect_equal(exp_stats(study_py, expected = c("expected_1", "expected_2"),
                         credibility = TRUE, conf_int = TRUE),
               summary(exp_res))
  expect_equal(exp_stats(study_py, expected = c("expected_1", "expected_2"),
                         credibility = TRUE, wt = "weights",
                         conf_int = TRUE),
               summary(exp_res_weighted))
})


test_that("Confidence intervals work", {
  expect_true(all(exp_res$q_obs_lower < exp_res$q_obs))
  expect_true(all(exp_res$q_obs_upper > exp_res$q_obs))
  expect_true(all(exp_res_weighted$q_obs_lower < exp_res_weighted$q_obs))
  expect_true(all(exp_res_weighted$q_obs_upper > exp_res_weighted$q_obs))

  expect_true(all(exp_res$ae_expected_1_lower < exp_res$ae_expected_1))
  expect_true(all(exp_res$ae_expected_2_upper > exp_res$ae_expected_2))
  expect_true(all(exp_res_weighted$ae_expected_1_lower <
                    exp_res_weighted$ae_expected_1))
  expect_true(all(exp_res_weighted$ae_expected_2_upper >
                    exp_res_weighted$ae_expected_2))

  # verify that confidence intervals are tighter using lower confidence
  less_confident <- study_py |>
    group_by(pol_yr, inc_guar) |>
    exp_stats(expected = c("expected_1", "expected_2"),
              credibility = TRUE, conf_int = TRUE, conf_level = 0.5)
  expect_true(all(exp_res$q_obs_upper - exp_res$q_obs_lower >
                    less_confident$q_obs_upper - less_confident$q_obs_lower))

})
