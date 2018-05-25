context("check_input_lp_nl")

# Load data
  data_set_df <- monetary_var_data

# Create list for input
  specs <- list()

# Fill list
  specs$lags_nl        <- NaN
  specs$lags_criterion <- 'AIC'
  specs$max_lags       <- 2
  specs$trend          <- 1
  specs$shock_type     <- 1

# Specifications for switching variable
  specs$switching      <- data_set_df$FF
  specs$hp_filter      <- 1
  specs$lambda         <- 129600
  specs$gamma          <- -3

# Horizons and cinfidence intervals
  specs$confint        <- 1.96
  specs$hor            <- 24

  data_set_df <- monetary_var_data

# Create list for input
  specs       <- list()

# Fill list
  specs$lags_nl        <- NaN
  specs$lags_criterion <- 'AIC'
  specs$max_lags       <- 2
  specs$trend          <- 1
  specs$shock_type     <- 1

# Specifications for switching variable
  specs$switching      <- data_set_df$FF
  specs$hp_filter      <- 1
  specs$lambda         <- 129600
  specs$gamma          <- -3

# Horizons and cinfidence intervals
  specs$confint        <- 1.96
  specs$hor            <- 24



test_that("Check whether trend is given", {
  specs$trend   <- NULL
  expect_error(lp_nl(data_set_df, specs),
              'Please specify whether and which type of trend to include.', fixed = TRUE)
})


test_that("Check whether shock_type is given", {
  specs$shock_type   <- NULL
  expect_error(lp_nl(data_set_df, specs),
              'Please specify which type of shock to use.', fixed = TRUE)
})


test_that("Check whether a switching variable is given", {
  specs$switching    <- NULL
  expect_error(lp_nl(data_set_df, specs),
              'Please specify a switching variable.', fixed = TRUE)
})


test_that("Check whether 'hp_filter' is given", {
  specs$hp_filter    <- NULL
  expect_error(lp_nl(data_set_df, specs),
              'Please specify whether to use the HP-filter for the switching variable.', fixed = TRUE)
})


test_that("Check whether lambda is given if hp_filter == 1", {
  specs$hp_filter    <- 1
  specs$lambda       <- NULL
  expect_error(lp_nl(data_set_df, specs),
              'Please specify lambda for the HP-filter.', fixed = TRUE)
})


test_that("Check whether 'gamma' is given", {
  specs$gamma        <- NULL
  expect_error(lp_nl(data_set_df, specs),
                                     'Please specify gamma for the transition function.', fixed = TRUE)
})


test_that("Check whether 'confint' is given", {
  specs$confint        <- NULL
  expect_error(lp_nl(data_set_df, specs),
               'Please specify a value for the width of the confidence bands.', fixed = TRUE)
})


test_that("Check whether number of horizons is given", {
  specs$hor          <- NULL
  expect_error(lp_nl(data_set_df, specs),
               'Please specify the number of horizons.', fixed = TRUE)
})


test_that("Check whether wrong lag length is given", {
  specs$lags_criterion <- 'AICCd'
  expect_error(lp_nl(data_set_df, specs),
               'Possible lag length criteria are AICc, AIC or BIC or NaN if lag length is specified.', fixed = TRUE)
} )



test_that("Check whether lag criterion AND fixed number of lags is given", {
  specs$lags_nl <- 1
  expect_error(lp_nl(data_set_df, specs),
                           'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.', fixed = TRUE)
} )

test_that("Check whether lag criterion AND maximum number of lags is given", {
  specs$max_lags <- NaN
  expect_error(lp_nl(data_set_df, specs),
               'Please provide a maximum number of lags for the lag length criterion.', fixed = TRUE)
} )




