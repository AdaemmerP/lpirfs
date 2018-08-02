context("check_input_lp_nl")

# Load data
  data_set_df <- monetary_var_data

# Create list for input
#  specs <- list()

# Fill list
  lags_lin       <- NaN
  lags_nl        <- NaN
  lags_criterion <- 'AIC'
  max_lags       <- 2
  trend          <- 1
  shock_type     <- 1

# Specifications for switching variable
  switching      <- data_set_df$FF
  use_hp         <- 1
  lambda         <- 129600
  gamma          <- 3

# Horizons and cinfidence intervals
  confint        <- 1.96
  hor            <- 24

  data_set_df <- monetary_var_data


test_that("Check whether data is a data.frame", {
  data_set_df   <- as.matrix(data_set_df)
    expect_error(lp_nl(data_set_df,
                                    lags_lin   = lags_lin,
                                    lags_nl    = lags_nl,
                                    lags_criterion = lags_criterion,
                                    max_lags   = max_lags,
                                    trend      = trend,
                                    shock_type = shock_type,
                                    switching  = switching,
                                    use_hp     = use_hp,
                                    lambda     = lambda,
                                    gamma      = gamma,
                                    confint    = confint,
                                    hor        = hor),
                 'The data has to be a data.frame().', fixed = TRUE)
})


test_that("Check whether trend is given", {
  trend   <- NULL
  expect_error(lp_nl(data_set_df, lags_lin  = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                  max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                  switching = switching, use_hp = use_hp,
                                  lambda    = lambda,    gamma     = gamma,      confint     = confint, hor = hor),
              'Please specify whether and which type of trend to include.', fixed = TRUE)
})


test_that("Check whether shock_type is given", {
  shock_type   <- NULL
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
              'Please specify which type of shock to use.', fixed = TRUE)
})


test_that("Check whether a switching variable is given", {
  switching    <- NULL
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
              'Please provide a switching variable.', fixed = TRUE)
})


test_that("Check whether 'use_hp' is given", {
  use_hp    <- NULL
  expect_error(lp_nl(data_set_df, lags_lin  = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                  max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                  switching = switching, use_hp = use_hp,
                                  lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
              'Please specify whether to use the HP-filter for the switching variable.', fixed = TRUE)
})


test_that("Check whether lambda is given if use_hp == 1", {
  use_hp    <- 1
  lambda       <- NULL
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
              'Please specify lambda for the HP-filter.', fixed = TRUE)
})


test_that("Check whether 'gamma' is given", {
   gamma        <- NULL
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
                                     'Please specify gamma for the transition function.', fixed = TRUE)
})

test_that("Check whether 'confint' is given", {
  confint        <- NULL
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin,
                                  lags_nl    = lags_nl,
                                  lags_criterion = lags_criterion,
                                  max_lags    = max_lags,
                                  trend       = trend,
                                  shock_type  = shock_type,
                                  switching = switching,
                                  use_hp = use_hp,
                                  lambda    = lambda,
                                  gamma     = gamma,
                                  confint   = confint,
                                  hor       = hor),
               'Please specify a value for the width of the confidence bands.', fixed = TRUE)
})


test_that("Check whether number of horizons is given", {
  hor          <- NULL
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
               'Please specify the number of horizons.', fixed = TRUE)
})


test_that("Check whether wrong lag length is given", {
  lags_criterion <- 'AICCd'
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
               'Possible lag length criteria are AICc, AIC or BIC or NaN if lag length is specified.', fixed = TRUE)
} )


test_that("Check whether lag criterion AND fixed number of lags for non-linear are given", {
  lags_nl <- 1
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
                           'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.', fixed = TRUE)
} )


test_that("Check whether lag criterion AND fixed number of lags for linear are given", {
  lags_lin <- 1
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
               'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.', fixed = TRUE)
} )



test_that("Check whether lag criterion AND maximum number of lags are given", {
  max_lags <- NaN
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
               'Please provide a maximum number of lags for the lag length criterion.', fixed = TRUE)
} )


test_that("Check whether values for horizons are correct", {
  hor <- -1
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
               'The number of horizons has to be an integer and > 0.', fixed = TRUE)
} )


test_that("Check whether lags are integers", {
  lags_lin         <- 1.4
  lags_nl          <- -2
  lags_criterion   <- NaN
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
               'The number of lags have to be a positive integer.', fixed = TRUE)
} )


test_that("Check whether trend is correctly specified", {
  trend <- 12
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
               'For trend please set 0 = no trend, 1 = trend, 2 = trend and quadratic trend.', fixed = TRUE)
} )


test_that("Check shock type is correctly specified", {
  shock_type <- 12
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
               'The shock_type has to be 0 = standard deviation shock or 1 = unit shock.', fixed = TRUE)
} )

test_that("Check whether width of confidence bands is correctly specified", {
  confint <- -1
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
               'The width of the confidence bands has to be >=0.', fixed = TRUE)
} )


test_that("Check whether gamma is positive", {
  gamma <- -1
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
               'Gamma has to be a positive number.', fixed = TRUE)
} )


test_that("Check whether use_hp is 0 or 1", {
  use_hp <- - 2
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
               'Please set use_hp = 0 (do not use HP-filter), or use_hp = 1 (use HP-filter).', fixed = TRUE)
} )

test_that("Check whether maximum number of lags is positive", {
  max_lags <- - 2
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
               'The maximum number of lags has to be a positive integer.', fixed = TRUE)
} )


test_that("Check whether whether no lag length criterion is given but maximum number of lags.", {
  lags_lin       <- 3
  lags_nl        <- 2
  lags_criterion <- NaN
  max_lags       <- 3
  expect_error(lp_nl(data_set_df, lags_lin   = lags_lin, lags_nl    = lags_nl, lags_criterion = lags_criterion,
                                   max_lags  = max_lags, trend      = trend, shock_type       = shock_type,
                                   switching = switching, use_hp = use_hp,
                                   lambda    = lambda, gamma        = gamma, confint          = confint, hor = hor),
               'The maximum number of lags can only be used if a lag length criterion is given.', fixed = TRUE)
} )

