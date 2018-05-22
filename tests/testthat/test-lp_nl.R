context("check_input_lp_nl")

## Not run:
# Load packages
# library(dplyr)
# library(doSNOW)
# library(parallel)
# library(mFilter)
# library(Rcpp)
# library(lpirfs)


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


test_that("Check whether trend is given", {
  specs$trend   <- NULL
  dftest2       <- expect_error(lp_nl(data_set_df, specs),
                                'Please specify whether and which type of trend to include.', fixed = TRUE)
} )


test_that("Check whether shock_type is given", {
  specs$shock_type   <- NULL
  dftest2            <- expect_error(lp_nl(data_set_df, specs),
                                'Please specify which type of shock to use.', fixed = TRUE)
} )


test_that("Check whether lag criterion AND fixed number of lags is given", {
  specs$lags_nl <- 1
  dftest2       <-  expect_error(lp_nl(data_set_df = data_set_df, specs = specs),
                           'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.', fixed = TRUE)
} )



