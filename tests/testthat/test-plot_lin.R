context("plot_lin")

# Load data
  endog_data <- interest_rules_var_data

# Specify inputs
  lags_endog_lin <- 4
  lags_criterion <- NaN
  max_lags       <- NaN
  trend          <- 0L
  shock_type     <- 1L
  confint        <- 1.96
  hor            <- 12L



test_that("Test whether list length is consistent number of endogenous variables
          for lin model",{

  results_lin <- lp_lin(endog_data,
                           lags_endog_lin = lags_endog_lin,
                           lags_criterion = lags_criterion,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           max_lags       = max_lags,
                           trend          = trend,
                           shock_type     = shock_type,
                           confint        = confint,
                           hor            = hor,
                           num_cores      = 1)

  plots_lin   <- plot_lin(results_lin)
  num_plots   <- ncol(endog_data)*ncol(endog_data)


  expect_equal(length(plots_lin), num_plots)
})



# Load package data
  ag_data           <- ag_data
  sample_start      <- 7
  sample_end        <- dim(ag_data)[1]

# Endogenous data
  endog_data <- ag_data[sample_start:sample_end,3:5]

# 'Instrument' variable
  instrument <- as.data.frame(ag_data$Gov[sample_start:sample_end])


test_that("Test whether list length is consistent number of endogenous variables
          for lin_iv model", {

  results_lin_iv <- lp_lin_iv(endog_data,
                               shock          = instrument,
                               lags_endog_lin = 4,
                               exog_data      = NULL,
                               lags_exog      = NULL,
                               contemp_data   = NULL,
                               lags_criterion = NaN,
                               max_lags       = NaN,
                               trend          = 0,
                               confint        = 1.96,
                               hor            = 20,
                               num_cores      = 1)

  plots_lin_iv   <- plot_lin(results_lin_iv)
  num_plots_iv   <- 1*ncol(endog_data)

  expect_equal(length(plots_lin_iv), num_plots_iv)

})

