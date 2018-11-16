context("lp_nl_iv")

# Load package data
  ag_data           <- ag_data
  sample_start      <- 7
  sample_end        <- dim(ag_data)[1]

# Endogenous data
  endog_data <- ag_data[sample_start:sample_end,3:5]

# 'Instrument' variable
  instrument <- as.data.frame(ag_data$Gov[sample_start:sample_end])

# Choose the 7-quarter moving average growth rate as switching variable.
# and adjust it to have suffiently long recession periods.
  switching_variable <- ag_data$GDP_MA[sample_start:sample_end] - 0.8



  test_that("Test whether endogenous data is a data.frame", {
    endog_data    <- as.matrix(endog_data)

    testthat::expect_error(lp_nl_iv(endog_data,
                              lags_endog_nl           = 3,
                              shock             = instrument,
                              exog_data         = NULL,
                              lags_exog         = NULL,
                              contemp_data      = NULL,
                              lags_criterion    = NaN,
                              max_lags          = NaN,
                              trend             = 0,
                              confint           = 1.96,
                              hor               = 20,
                              switching         = switching_variable,
                              use_hp            = 0,
                              lambda            = NaN,
                              gamma             = 3,
                              num_cores         = 1),
    'The endogenous data has to be a data.frame.', fixed = TRUE)
  })


  test_that("Test whether trend is given", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = 3,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = NaN,
                          max_lags          = NaN,
                          trend             = NULL,
                          confint           = 1.96,
                          hor               = 20,
                          switching         = switching_variable,
                          use_hp            = 0,
                          lambda            = NaN,
                          gamma             = 3,
                          num_cores         = 1),
                 'Please specify whether and which type of trend to include.', fixed = TRUE)
  })



  test_that("Test whether switching variable is given", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = 3,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = NaN,
                          max_lags          = NaN,
                          trend             = 0,
                          confint           = 1.96,
                          hor               = 20,
                          switching         = NULL,
                          use_hp            = 0,
                          lambda            = NaN,
                          gamma             = 3,
                          num_cores         = 1),
                 'Please provide a switching variable.', fixed = TRUE)
  })


  test_that("Test whether use_hp is set", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = 3,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = NaN,
                          max_lags          = NaN,
                          trend             = 0,
                          confint           = 1.96,
                          hor               = 20,
                          switching         = switching_variable,
                          use_hp            = NULL,
                          lambda            = NaN,
                          gamma             = 3,
                          num_cores         = 1),
                 'Please specify whether to use the HP-filter for the switching variable.', fixed = TRUE)
  })


  test_that("Test whetherlambda is given", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = 3,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = NaN,
                          max_lags          = NaN,
                          trend             = 0,
                          confint           = 1.96,
                          hor               = 20,
                          switching         = switching_variable,
                          use_hp            = TRUE,
                          lambda            = NULL,
                          gamma             = 3,
                          num_cores         = 1),
                 'Please specify lambda for the HP-filter.', fixed = TRUE)
  })


  test_that("Test whether gamma is given", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = 3,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = NaN,
                          max_lags          = NaN,
                          trend             = 0,
                          confint           = 1.96,
                          hor               = 20,
                          switching         = switching_variable,
                          use_hp            = TRUE,
                          lambda            = 1600,
                          gamma             = NULL,
                          num_cores         = 1),
                 'Please specify gamma for the transition function.', fixed = TRUE)
  })


  test_that("Test whether width for confidence bands is given", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = 3,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = NaN,
                          max_lags          = NaN,
                          trend             = 0,
                          confint           = NULL,
                          hor               = 20,
                          switching         = switching_variable,
                          use_hp            = TRUE,
                          lambda            = 1600,
                          gamma             = 3,
                          num_cores         = 1),
                 'Please specify a value for the width of the confidence bands.', fixed = TRUE)
  })

  test_that("Test whether number of horizons is given", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = 3,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = NaN,
                          max_lags          = NaN,
                          trend             = 0,
                          confint           = 2,
                          hor               = NULL,
                          switching         = switching_variable,
                          use_hp            = TRUE,
                          lambda            = 1600,
                          gamma             = 3,
                          num_cores         = 1),
                 'Please specify the number of horizons.', fixed = TRUE)
  })

  test_that("Test whether lag  length criterion is correctly spelled", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = NaN,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = 'AIP',
                          max_lags          = 4,
                          trend             = 0,
                          confint           = 2,
                          hor               = 20,
                          switching         = switching_variable,
                          use_hp            = TRUE,
                          lambda            = 1600,
                          gamma             = 3,
                          num_cores         = 1),
            'Possible lag length criteria are AICc, AIC or BIC or NaN if lag length is specified.', fixed = TRUE)
  })


  test_that("Test whether lags criterion and fixed number of lags for nonlinear model is given", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = 3,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = 'AIC',
                          max_lags          = NULL,
                          trend             = 0,
                          confint           = 2,
                          hor               = 20,
                          switching         = switching_variable,
                          use_hp            = TRUE,
                          lambda            = 1600,
                          gamma             = 3,
                          num_cores         = 1),
           'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.', fixed = TRUE)
  })


  test_that("Test whether maximum number of lags is given", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = NaN,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = 'AIC',
                          max_lags          = NaN,
                          trend             = 0,
                          confint           = 2,
                          hor               = 20,
                          switching         = switching_variable,
                          use_hp            = TRUE,
                          lambda            = 1600,
                          gamma             = 3,
                          num_cores         = 1),
                 'Please provide a maximum number of lags for the lag length criterion.', fixed = TRUE)
  })

  test_that("Test whether number of horizons is positive", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = NaN,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = 'AIC',
                          max_lags          = 4,
                          trend             = 0,
                          confint           = 2,
                          hor               = -1,
                          switching         = switching_variable,
                          use_hp            = TRUE,
                          lambda            = 1600,
                          gamma             = 3,
                          num_cores         = 1),
                 'The number of horizons has to be an integer and > 0.', fixed = TRUE)
  })


  test_that("Test whether trend is correctly specified", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = NaN,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = 'AIC',
                          max_lags          = 4,
                          trend             = -1,
                          confint           = 2,
                          hor               = 12,
                          switching         = switching_variable,
                          use_hp            = TRUE,
                          lambda            = 1600,
                          gamma             = 3,
                          num_cores         = 1),
                 'For trend please set 0 = no trend, 1 = trend, 2 = trend and quadratic trend.', fixed = TRUE)
  })



  test_that("Test whether width of confidence bands is correctly specified", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = NaN,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = 'AIC',
                          max_lags          = 4,
                          trend             = 1,
                          confint           = -2,
                          hor               = 12,
                          switching         = switching_variable,
                          use_hp            = TRUE,
                          lambda            = 1600,
                          gamma             = 3,
                          num_cores         = 1),
                 'The width of the confidence bands has to be >=0.', fixed = TRUE)
  })


  test_that("Test whether gamma is negative number", {
    testthat::expect_error(lp_nl_iv(endog_data,
                          lags_endog_nl           = NaN,
                          shock             = instrument,
                          exog_data         = NULL,
                          lags_exog         = NULL,
                          contemp_data      = NULL,
                          lags_criterion    = 'AIC',
                          max_lags          = 4,
                          trend             = 1,
                          confint           = 2,
                          hor               = 12,
                          switching         = switching_variable,
                          use_hp            = TRUE,
                          lambda            = 1600,
                          gamma             = -3,
                          num_cores         = 1),
                 'Gamma has to be a positive number.', fixed = TRUE)
  })


  test_that("Test that data is correctly specified when lag length criterion is given", {
    testthat::expect_error(lp_nl_iv(endog_data,
                                    lags_endog_nl     = NaN,
                                    shock             = instrument,
                                    exog_data         = NULL,
                                    lags_exog         = NULL,
                                    contemp_data      = NULL,
                                    lags_criterion    = 'AIC',
                                    max_lags          = 4,
                                    trend             = 2,
                                    confint           = 2,
                                    hor               = 12,
                                    switching         = switching_variable,
                                    use_logistic      = FALSE,
                                    lag_switching     = TRUE,
                                    use_hp            = TRUE,
                                    lambda            = 1600,
                                    gamma             = 3,
                                    num_cores         = 1),
                           NA)
  })

# This example replicates results from the Supplementary Appendix
# by Ramey and Zubairy (2018), based on results from their provided
# Matlab code. The results evaluate findings from Auerbach and
# Gorodnichenko (2012) with local projections by Jordá (2005).
# The data and Matlab code is available on \url{https://www.journals.uchicago.edu/doi/10.1086/696277}{JoPE}


# Load and prepare data
# The sample length of RZ-2018 is 1948:III-2008:III
  ag_data           <- ag_data
  sample_start      <- 7
  sample_end        <- dim(ag_data)[1]
  endog_data        <- ag_data[sample_start:sample_end, 3:5]

# Choose instrument
  instrument        <- as.data.frame(ag_data$Gov_shock_mean[sample_start:sample_end])

# Choose exogenous data
  exog_data         <- as.data.frame(ag_data$GDP_MA[sample_start:sample_end])

# Choose exogenous data
  switching_variable <- as.data.frame(ag_data$GDP_MA[sample_start:sample_end]) - 0.8


# These results are taken from the available Matlab code by Ramey and Zubairy (2018)
# They coresspond to row 2 and 3 in table 'regg' from the Matlab output
  rz_s1_results <- c(1.00,	1.02,	0.78,	0.60,	0.37,	0.12,	0.04,	-0.09,	0.04,	-0.07,	-0.21,
                     -0.30,	-0.28,	-0.42,	-0.29,	-0.23,	0.02,	0.11,	0.13,	-0.01)

  rz_s2_results <- c(1.00, 1.24,	1.65,	2.01,	2.24,	2.12,	1.97,	1.73,	1.44,	1.48,	1.45,
                     1.27,	1.26,	1.19,	1.04,	1.10,	0.87,	0.86,	0.89,	0.97)



test_that("Compare results with RZ-2018", {
  # Estimate local projections
  results_nl_iv <- lp_nl_iv(endog_data,
                            lags_endog_nl     = 3,
                            shock             = instrument,
                            exog_data         = exog_data,
                            lags_exog         = 4,
                            contemp_data      = NULL,
                            lags_criterion    = NaN,
                            max_lags          = NaN,
                            trend             = 0,
                            confint           = 1.96,
                            hor               = 20,
                            switching         = switching_variable,
                            use_logistic      = TRUE,
                            lag_switching     = TRUE,
                            use_hp            = 0,
                            lambda            = NaN,
                            gamma             = 3,
                            num_cores         = 1)

  s1_results <- round(results_nl_iv$irf_s1_mean[1, ], 2)
  s2_results <- round(results_nl_iv$irf_s2_mean[1, ], 2)

  testthat::expect_equal(rz_s1_results, s1_results)
  testthat::expect_equal(rz_s2_results, s2_results)

})
