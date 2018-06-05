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



test_that("Check whether data is a data.frame", {
    data_set_df   <- as.matrix(data_set_df)
    expect_error(lp_nl(data_set_df, specs),
                 'The data has to be a data.frame().', fixed = TRUE)
})

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
               'Possible lag length criteria are AICc, AIC or BIC or NaN if lag length is specified.', fixed = TRUE)
} )

test_that("Check whether lag criterion AND fixed number of lags are given", {
  specs$lags_lin <- 1
  expect_error(lp_lin(data_set_df, specs),
               'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.', fixed = TRUE)
} )


test_that("Check whether lag criterion AND maximum number of lags are given", {
  specs$max_lags <- NaN
  expect_error(lp_lin(data_set_df, specs),
               'Please provide a maximum number of lags for the lag length criterion.', fixed = TRUE)
} )



test_that("Check whether values for horizons are correct", {
  specs$hor <- -1
  expect_error(lp_lin(data_set_df, specs),
               'The number of horizons has to be an integer and > 0.', fixed = TRUE)
} )


test_that("Check whether lags are integers", {
  specs$lags_lin <- 1.5
  specs$lags_criterion <- NaN
  expect_error(lp_lin(data_set_df, specs),
               'The numbers of lags have to be an integer.', fixed = TRUE)
} )


test_that("Check whether trend is correctly specified", {
  specs$trend <- 12
  expect_error(lp_lin(data_set_df, specs),
               'For trend please enter 0 = no trend, 1 = trend, 2 = trend and quadratic trend.', fixed = TRUE)
} )



test_that("Check shock type is correctly specified", {
  specs$shock_type <- 12
  expect_error(lp_lin(data_set_df, specs),
               'The shock_type has to be 0 = standard deviation shock and 1 = unit shock.', fixed = TRUE)
} )

test_that("Check whether width of confidence bands is correctly specified", {
  specs$confint <- -1
  expect_error(lp_lin(data_set_df, specs),
               'The width of the confidence bands has to be >=0.', fixed = TRUE)
} )



