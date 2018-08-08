context("check_input_lp_lin_iv")


# Load package data
  ag_data <- ag_data
  sample_start      <- 7
  sample_end        <- dim(ag_data)[1]

# Endogenous data
  endog_data <- ag_data[sample_start:sample_end,3:5]

# 'Instrument' variable
  instrument <- as.data.frame(ag_data$Gov[sample_start:sample_end])


  test_that("Check whether data is a data.frame", {
    endog_data   <- as.matrix(endog_data)
    expect_error(lp_lin_iv(endog_data,
                            instr          = instrument,
                            lags_endog_lin = 4,
                            exog_data      = NULL,
                            lags_exog      = NULL,
                            contemp_data   = NULL,
                            lags_criterion = NaN,
                            max_lags       = NaN,
                            trend          = 0,
                            shock_type     = 1,
                            confint        = 1.96,
                            hor            = 20,
                            num_cores      = 1),
                 'The data has to be a data.frame().', fixed = TRUE)
  })


  test_that("Check whether instrument is given", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = NULL,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 0,
                           shock_type     = 1,
                           confint        = 1.96,
                           hor            = 20,
                           num_cores      = 1),
                 'You have to provide an instrument to shock with.', fixed = TRUE)
  })

  test_that("Check whether lags_endog_lin is falsely NaN", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = NaN,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 0,
                           shock_type     = 1,
                           confint        = 1.96,
                           hor            = 20,
                           num_cores      = 1),
                 '"lags_endog_lin" can only be NaN if a lag length criterion is given.', fixed = TRUE)
  })




  test_that("Check whether message is given when no exogenous data is provided", {

    expect_message(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 0,
                           shock_type     = 1,
                           confint        = 1.96,
                           hor            = 20,
                           num_cores      = 1),
                 'You estimate the model without exogenous data.')
  })

  test_that("Check whether message is given when no contemporaneous data is provided", {
    expect_message(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 0,
                           shock_type     = 1,
                           confint        = 1.96,
                           hor            = 20,
                           num_cores      = 1),
                   'You estimate the model without exogenous data with contemporaneous impact', fixed = TRUE)
  })


  test_that("Check whether lag length criterion is correctly specified", {
    expect_error(lp_lin_iv(endog_data,
                             instr          = instrument,
                             lags_endog_lin       = 4,
                             exog_data      = NULL,
                             lags_exog      = NULL,
                             contemp_data   = NULL,
                             lags_criterion = NULL,
                             max_lags       = NaN,
                             trend          = 0,
                             shock_type     = 1,
                             confint        = 1.96,
                             hor            = 20,
                             num_cores      = 1),
                   '"lags_criterion" has to be NaN or a character, specifying the lag length criterion.', fixed = TRUE)
  })



  test_that("Check whether trend is given", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = NULL,
                           shock_type     = 1,
                           confint        = 1.96,
                           hor            = 20,
                           num_cores      = 1),
                 'Please specify whether and which type of trend to include.', fixed = TRUE)
  })


  test_that("Check whether shock type is given", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 1,
                           shock_type     = NULL,
                           confint        = 1.96,
                           hor            = 20,
                           num_cores      = 1),
                 'Please specify which type of shock to use.', fixed = TRUE)
  })

  test_that("Check whether number of horizons is given", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 0,
                           shock_type     = 0,
                           confint        = 1,
                           hor            = NULL,
                           num_cores      = 1),
                 'Please specify the number of horizons.', fixed = TRUE)
  })


  test_that("Check whether lag lengt criterion is correctly given", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = 'SIC',
                           max_lags       = NaN,
                           trend          = 0,
                           shock_type     = 0,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
                 'Possible lag length criteria are AICc, AIC or BIC. NaN if lag length is specified.', fixed = TRUE)
  })


  test_that("Check whether number of horizons is correctly given", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 0,
                           shock_type     = 0,
                           confint        = 1,
                           hor            = 1.2,
                           num_cores      = 1),
                 'The number of horizons has to be an integer and > 0.', fixed = TRUE)
  })

  test_that("Check whether trend is correctly specified", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 4,
                           shock_type     = 0,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
                 'For trend please enter 0 = no trend, 1 = trend, 2 = trend and quadratic trend.', fixed = TRUE)
  })

  test_that("Check whether shock type is correctly specified", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 1,
                           shock_type     = 4,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
                 'The shock_type has to be 0 = standard deviation shock or 1 = unit shock.', fixed = TRUE)
  })

  test_that("Check whether the width of the confidence bands is correctly specified", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 1,
                           shock_type     = 1,
                           confint        = -0.2,
                           hor            = 12,
                           num_cores      = 1),
                 'The width of the confidence bands has to be >=0.', fixed = TRUE)
  })

  test_that("Check whether exogenous data is a data.frame", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = as.matrix(rnorm(100)),
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 1,
                           shock_type     = 1,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
                 'Exogenous data has to be given as a data.frame.', fixed = TRUE)
  })


  test_that("Check whether instrument is a data.frame", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = as.matrix(rnorm(100)),
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 1,
                           shock_type     = 1,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
                 'Exogenous data has to be given as a data.frame.', fixed = TRUE)
  })


  test_that("Check whether lag length for exogenous data is given", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = as.data.frame(rnorm(dim(endog_data)[1])),
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 1,
                           shock_type     = 1,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
                 'Please provide a lag length for the exogenous data.', fixed = TRUE)
  })

  test_that("Check whether lag length and lag length criterion is given", {
    expect_error(lp_lin_iv(endog_data,
                           instr          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = 'AIC',
                           max_lags       = NULL,
                           trend          = 1,
                           shock_type     = 1,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
         'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.
         Please set lags_endog_lin to NaN if you want to use a lag length criterion.', fixed = TRUE)
  })



  # This example replicates results from the Supplementary Appendix
  # by Ramey and Zubairy (2018), based on results from their provided
  # Matlab code. The results evaluate findings from Auerbach and
  # Gorodnichenko (2012) with local projections by Jordá (2005).
  # The data and Matlab code is available on \url{https://www.journals.uchicago.edu/doi/10.1086/696277}{JoPE}


  # Load and prepare data
  # The sample length of RZ-2018 is 1948:III-2008:III
  ag_data           <- ag_data
  sample_start      <- 8
  sample_end        <- dim(ag_data)[1]
  endog_data        <- ag_data[sample_start:sample_end, 3:5]

  # Choose instrument
  instrument        <- ag_data[sample_start:sample_end, 3]



  # These results are taken from the available Matlab code by Ramey and Zubairy (2018)
  # They coresspond to row 2 and 3 in table 'regg' from the Matlab output
  rz_lin_results <- c(1,	1.27,	1.40,	1.46,	1.38,	1.16,	1.05,	0.90,	0.85,
                      0.82, 0.73, 0.53, 0.45,	0.30,	0.27, 0.28,	0.31,	0.35,	0.36,	0.27)



  test_that("Compare results with RZ-2018", {

    # Estimate linear model
    results_lin_iv <- lp_lin_iv(endog_data,
                                instr          = instrument,
                                lags_endog_lin       = 4,
                                exog_data      = NULL,
                                lags_exog      = NULL,
                                contemp_data   = NULL,
                                lags_criterion = NaN,
                                max_lags       = NaN,
                                trend          = 0,
                                shock_type     = 1,
                                confint        = 1.96,
                                hor            = 20,
                                num_cores      = 1)
    head(results_lin_iv$specs$x_lin)

    lin_results <- round(results_lin_iv$irf_lin_mean[1,], 2)


    testthat::expect_equal(lin_results, rz_lin_results)


  })

