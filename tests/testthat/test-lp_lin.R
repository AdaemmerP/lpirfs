context("check_input_lp_nl")

# Load data
  data_set_df <- interest_rules_var_data

# Create list for input
  specs <- list()

# Specify inputs
  specs$lags_lin       <- NaN
  specs$lags_criterion <- 'AIC'
  specs$max_lags       <- 2L
  specs$trend          <- 0L
  specs$shock_type     <- 1L
  specs$confint        <- 1.96
  specs$hor            <- 12L


test_that("Check whether trend is given", {
    specs$trend   <- NULL
    expect_error(lp_lin(data_set_df, specs),
                 'Please specify whether and which type of trend to include.', fixed = TRUE)
  })


test_that("Check whether shock_type is given", {
    specs$shock_type   <- NULL
    expect_error(lp_lin(data_set_df, specs),
                 'Please specify which type of shock to use.', fixed = TRUE)
  })


test_that("Check whether 'confint' is given", {
  specs$confint        <- NULL
  expect_error(lp_lin(data_set_df, specs),
               'Please specify a value for the width of the confidence bands.', fixed = TRUE)
})


test_that("Check whether number of horizons is given", {
  specs$hor          <- NULL
  expect_error(lp_lin(data_set_df, specs),
               'Please specify the number of horizons.', fixed = TRUE)
})


test_that("Check whether wrong lag length is given", {
  specs$lags_criterion <- 'AICCd'
  expect_error(lp_lin(data_set_df, specs),
               'Possible lag length criteria are AICc, AIC or BIC.', fixed = TRUE)
} )

test_that("Check whether lag criterion AND fixed number of lags is given", {
  specs$lags_lin <- 1
  expect_error(lp_lin(data_set_df, specs),
               'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.', fixed = TRUE)
} )

