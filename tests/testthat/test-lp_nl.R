context("check_input_lp_nl")

# Load data
  data_set_df <- monetary_var_data

# Create list for input
  specs <- list()

# Fill list
  specs$lags_lin       <- NaN
  specs$lags_nl        <- NaN
  specs$lags_criterion <- 'AIC'
  specs$max_lags       <- 2
  specs$trend          <- 1
  specs$shock_type     <- 1

# Specifications for switching variable
  specs$switching      <- data_set_df$FF
  specs$hp_filter      <- 1
  specs$lambda         <- 129600
  specs$gamma          <- 3

# Horizons and cinfidence intervals
  specs$confint        <- 1.96
  specs$hor            <- 24

  data_set_df <- monetary_var_data


test_that("Check whether data is a data.frame", {
  data_set_df   <- as.matrix(data_set_df)
    expect_error(lp_nl(data_set_df, specs),
                 'The data has to be a data.frame().', fixed = TRUE)
})


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


test_that("Check whether lag criterion AND fixed number of lags for non-linear are given", {
  specs$lags_nl <- 1
  expect_error(lp_nl(data_set_df, specs),
                           'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.', fixed = TRUE)
} )


test_that("Check whether lag criterion AND fixed number of lags for linear are given", {
  specs$lags_lin <- 1
  expect_error(lp_nl(data_set_df, specs),
               'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.', fixed = TRUE)
} )



test_that("Check whether lag criterion AND maximum number of lags are given", {
  specs$max_lags <- NaN
  expect_error(lp_nl(data_set_df, specs),
               'Please provide a maximum number of lags for the lag length criterion.', fixed = TRUE)
} )



test_that("Check whether values for horizons are correct", {
  specs$hor <- -1
  expect_error(lp_nl(data_set_df, specs),
               'The number of horizons has to be an integer and > 0.', fixed = TRUE)
} )


test_that("Check whether lags are integers", {
  specs$lags_lin         <- 1.4
  specs$lags_nl          <- -2
  specs$lags_criterion   <- NaN
  expect_error(lp_nl(data_set_df, specs),
               'The numbers of lags have to be a positive integer.', fixed = TRUE)
} )


test_that("Check whether trend is correctly specified", {
  specs$trend <- 12
  expect_error(lp_nl(data_set_df, specs),
               'For trend please put 0 = no trend, 1 = trend, 2 = trend and quadratic trend.', fixed = TRUE)
} )


test_that("Check shock type is correctly specified", {
  specs$shock_type <- 12
  expect_error(lp_nl(data_set_df, specs),
               'The shock_type has to be 0 = standard deviation shock or 1 = unit shock.', fixed = TRUE)
} )

test_that("Check whether width of confidence bands is correctly specified", {
  specs$confint <- -1
  expect_error(lp_nl(data_set_df, specs),
               'The width of the confidence bands has to be >=0.', fixed = TRUE)
} )


test_that("Check whether gamma is positive", {
  specs$gamma <- -1
  expect_error(lp_nl(data_set_df, specs),
               'Gamma has to be a positive number.', fixed = TRUE)
} )


test_that("Check whether hp_filter is 0 or 1", {
  specs$hp_filter <- - 2
  expect_error(lp_nl(data_set_df, specs),
               'Please set hp_filter = 0 (do not use HP-filter), or hp_filter = 1 (use HP-filter).', fixed = TRUE)
} )

