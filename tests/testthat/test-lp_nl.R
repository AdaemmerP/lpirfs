context("lp_nl")

# Load data
  endog_data <- monetary_var_data

# Create list for input
#  specs <- list()

# Fill list
  lags_endog_lin       <- NaN
  lags_endog_nl        <- NaN
  lags_criterion       <- 'AIC'
  max_lags             <- 2
  trend                <- 1
  shock_type           <- 1

# Specifications for switching variable
  switching      <- endog_data$FF
  use_hp         <- 1
  lambda         <- 129600
  gamma          <- 3

# Horizons and cinfidence intervals
  confint        <- 1.96
  hor            <- 24

test_that("Test whether data is a data.frame", {
  endog_data   <- as.matrix(endog_data)
  testthat::expect_error(lp_nl(endog_data,
                                    lags_endog_lin   = lags_endog_lin,
                                    lags_endog_nl    = lags_endog_nl,
                                    lags_criterion   = lags_criterion,
                                    max_lags         = max_lags,
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


test_that("Test whether trend is given", {
  trend   <- NULL
  testthat::expect_error(lp_nl(endog_data, lags_endog_lin   = lags_endog_lin,
                                 lags_endog_nl    = lags_endog_nl,
                                  lags_criterion  = lags_criterion,
                                  max_lags        = max_lags,
                                  trend           = trend,
                                  shock_type      = shock_type,
                                  switching       = switching,
                                  use_hp          = use_hp,
                                  lambda          = lambda,
                                  gamma           = gamma,
                                  confint         = confint,
                                  hor             = hor),
              'Please specify whether and which type of trend to include.', fixed = TRUE)
})


test_that("Test whether shock_type is given", {
  shock_type   <- NULL
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
              'Please specify which type of shock to use.', fixed = TRUE)
})


test_that("Test whether a switching variable is given", {
  switching    <- NULL
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
              'Please provide a switching variable.', fixed = TRUE)
})


test_that("Test whether 'use_hp' is given", {
  use_hp    <- NULL
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
              'Please specify whether to use the HP-filter for the switching variable.', fixed = TRUE)
})


test_that("Test whether lambda is given if use_hp == 1", {
  use_hp    <- T
  lambda    <- NULL

  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
              'Please specify lambda for the HP-filter.', fixed = TRUE)
})


test_that("Test whether 'gamma' is given", {
   gamma        <- NULL
   testthat::expect_error(lp_nl(endog_data,
                                lags_endog_lin   = lags_endog_lin,
                                lags_endog_nl    = lags_endog_nl,
                                lags_criterion   = lags_criterion,
                                max_lags         = max_lags,
                                trend            = trend,
                                shock_type       = shock_type,
                                switching        = switching,
                                use_hp           = use_hp,
                                lambda           = lambda,
                                gamma            = gamma,
                                confint          = confint,
                                hor = hor),
                                     'Please specify gamma for the transition function.', fixed = TRUE)
})

test_that("Test whether 'confint' is given", {
  confint        <- NULL
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'Please specify a value for the width of the confidence bands.', fixed = TRUE)
})


test_that("Test whether number of horizons is given", {
  hor          <- NULL
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'Please specify the number of horizons.', fixed = TRUE)
})


test_that("Test whether wrong lag length is given", {
  lags_criterion <- 'AICCd'
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'Possible lag length criteria are AICc, AIC or BIC or NaN if lag length is specified.', fixed = TRUE)
} )


test_that("Test whether lag criterion AND fixed number of lags for non-linear are given", {
  lags_endog_nl <- 1
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
                           'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.', fixed = TRUE)
} )


test_that("Test whether lag criterion AND fixed number of lags for linear are given", {
  lags_endog_lin <- 1
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.', fixed = TRUE)
} )



test_that("Test whether lag criterion AND maximum number of lags are given", {
  max_lags <- NaN
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'Please provide a maximum number of lags for the lag length criterion.', fixed = TRUE)
} )


test_that("Test whether values for horizons are correct", {
  hor <- -1
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'The number of horizons has to be an integer and > 0.', fixed = TRUE)
} )


test_that("Test whether lags are integers", {
  lags_endog_lin         <- 1.4
  lags_endog_nl          <- -2
  lags_criterion   <- NaN
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'The number of lags have to be a positive integer.', fixed = TRUE)
} )


test_that("Test whether trend is correctly specified", {
  trend <- 12
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'For trend please set 0 = no trend, 1 = trend, 2 = trend and quadratic trend.', fixed = TRUE)
} )


test_that("Test shock type is correctly specified", {
  shock_type <- 12
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'The shock_type has to be 0 = standard deviation shock or 1 = unit shock.', fixed = TRUE)
} )

test_that("Test whether width of confidence bands is correctly specified", {
  confint <- -1
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'The width of the confidence bands has to be >=0.', fixed = TRUE)
} )


test_that("Test whether gamma is positive", {
  gamma <- -1
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'Gamma has to be a positive number.', fixed = TRUE)
} )


test_that("Test whether use_hp is 0 or 1", {
  use_hp <- - 2
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'Please set use_hp = 0 (do not use HP-filter), or use_hp = 1 (use HP-filter).', fixed = TRUE)
} )

test_that("Test whether maximum number of lags is positive", {
  max_lags <- - 2
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'The maximum number of lags has to be a positive integer.', fixed = TRUE)
} )


test_that("Test whether whether no lag length criterion is given but maximum number of lags.", {
  lags_endog_lin       <- 3
  lags_endog_nl        <- 2
  lags_criterion <- NaN
  max_lags       <- 3
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin   = lags_endog_lin,
                               lags_endog_nl    = lags_endog_nl,
                               lags_criterion   = lags_criterion,
                               max_lags         = max_lags,
                               trend            = trend,
                               shock_type       = shock_type,
                               switching        = switching,
                               use_hp           = use_hp,
                               lambda           = lambda,
                               gamma            = gamma,
                               confint          = confint,
                               hor = hor),
               'The maximum number of lags can only be used if a lag length criterion is given.', fixed = TRUE)
} )

test_that("Test that model works when irfs are based on lag length criterion.", {
  testthat::expect_error(lp_nl(endog_data,
                     lags_endog_lin   = lags_endog_lin,
                     lags_endog_nl    = lags_endog_nl,
                     lags_criterion   = lags_criterion,
                     max_lags         = max_lags,
                     trend            = trend,
                     shock_type       = shock_type,
                     switching        = switching,
                     use_hp           = use_hp,
                     lambda           = lambda,
                     gamma            = gamma,
                     confint          = confint, hor = hor,
                     num_cores        = 1),
               NA)
} )

test_that("Test that model works wfor different newey west options.", {


  # Load (endogenous) data
  endog_data <- interest_rules_var_data

  # Choose data for switching variable (here Federal Funds Rate)
  # Important: The switching variable does not have to be used within the VAR!
  switching_data <-  endog_data$Infl

  # Estimate model a
  testthat::expect_error(lp_nl(endog_data,
                         lags_endog_lin  = 4,
                         lags_endog_nl   = 3,
                         trend           = 0,
                         shock_type      = 1,
                         confint         = 1.96,
                         hor             = 24,
                         switching       = switching_data,
                         use_nw           = T,
                         nw_prewhite      = F,
                         use_hp          = TRUE,
                         lambda          = 1600,
                         gamma           = 3,
                         num_cores        = 1),
                         NA)


  # Estimate model and save results
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin  = 4,
                               lags_endog_nl   = 3,
                               trend           = 0,
                               shock_type      = 1,
                               confint         = 1.96,
                               hor             = 24,
                               switching       = switching_data,
                               use_nw           = F,
                               nw_prewhite      = F,
                               use_hp          = TRUE,
                               lambda          = 1600,
                               gamma           = 3,
                               num_cores        = 1),
                         NA)

  # Estimate model and save results
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin  = 4,
                               lags_endog_nl   = 3,
                               trend           = 0,
                               shock_type      = 1,
                               confint         = 1.96,
                               hor             = 24,
                               switching       = switching_data,
                               use_nw           = T,
                               nw_prewhite      = T,
                               use_hp          = TRUE,
                               lambda          = 1600,
                               gamma           = 3,
                               num_cores        = 1),
                         NA)

  # Estimate model and save results
  testthat::expect_error(lp_nl(endog_data,
                               lags_endog_lin  = 4,
                               lags_endog_nl   = 3,
                               trend           = 0,
                               shock_type      = 1,
                               confint         = 1.96,
                               hor             = 24,
                               switching       = switching_data,
                               use_nw           = F,
                               nw_prewhite      = T,
                               use_hp          = TRUE,
                               lambda          = 1600,
                               gamma           = 3,
                               num_cores        = 1),
                         NA)







} )
