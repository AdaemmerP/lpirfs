context("lp_lin_iv")


# Load package data
  ag_data         <- ag_data
  sample_start    <- 7
  sample_end      <- dim(ag_data)[1]

# Endogenous data
  endog_data       <- ag_data[sample_start:sample_end,3:5]

# 'Instrument' variable
  instrument       <- as.data.frame(ag_data$Gov[sample_start:sample_end])


  test_that("Test whether data is a data.frame", {
    endog_data   <- as.matrix(endog_data)
    testthat::expect_error(lp_lin_iv(endog_data,
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
                            num_cores      = 1),
                 'The data has to be a data.frame().', fixed = TRUE)
  })


  test_that("Test whether instrument is given", {
    testthat::expect_error(lp_lin_iv(endog_data,
                           shock          = NULL,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 0,
                           confint        = 1.96,
                           hor            = 20,
                           num_cores      = 1),
                 'You have to provide an instrument to shock with.', fixed = TRUE)
  })

  test_that("Test whether lags_endog_lin is falsely NaN", {
    testthat::expect_error(lp_lin_iv(endog_data,
                           shock          = instrument,
                           lags_endog_lin       = NaN,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 0,
                           confint        = 1.96,
                           hor            = 20,
                           num_cores      = 1),
                 '"lags_endog_lin" can only be NaN if a lag length criterion is given.', fixed = TRUE)
  })


  test_that("Test whether lag length criterion is correctly specified", {
    testthat::expect_error(lp_lin_iv(endog_data,
                             shock          = instrument,
                             lags_endog_lin       = 4,
                             exog_data      = NULL,
                             lags_exog      = NULL,
                             contemp_data   = NULL,
                             lags_criterion = NULL,
                             max_lags       = NaN,
                             trend          = 0,
                             confint        = 1.96,
                             hor            = 20,
                             num_cores      = 1),
                   '"lags_criterion" has to be NaN or a character, specifying the lag length criterion.', fixed = TRUE)
  })



  test_that("Test whether trend is given", {
    testthat::expect_error(lp_lin_iv(endog_data,
                           shock          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = NULL,
                           confint        = 1.96,
                           hor            = 20,
                           num_cores      = 1),
                 'Please specify whether and which type of trend to include.', fixed = TRUE)
  })


  test_that("Test whether number of horizons is given", {
    testthat::expect_error(lp_lin_iv(endog_data,
                           shock          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 0,
                           confint        = 1,
                           hor            = NULL,
                           num_cores      = 1),
                 'Please specify the number of horizons.', fixed = TRUE)
  })


  test_that("Test whether lag lengt criterion is correctly given", {
    testthat::expect_error(lp_lin_iv(endog_data,
                           shock          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = 'SIC',
                           max_lags       = NaN,
                           trend          = 0,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
                 'Possible lag length criteria are AICc, AIC or BIC. NaN if lag length is specified.', fixed = TRUE)
  })


  test_that("Test whether number of horizons is correctly given", {
    testthat::expect_error(lp_lin_iv(endog_data,
                           shock          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 0,
                           confint        = 1,
                           hor            = 1.2,
                           num_cores      = 1),
                 'The number of horizons has to be an integer and > 0.', fixed = TRUE)
  })

  test_that("Test whether trend is correctly specified", {
    testthat::expect_error(lp_lin_iv(endog_data,
                           shock          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 4,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
                 'For trend please enter 0 = no trend, 1 = trend, 2 = trend and quadratic trend.', fixed = TRUE)
  })


  test_that("Test whether the width of the confidence bands is correctly specified", {
    testthat::expect_error(lp_lin_iv(endog_data,
                           shock          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 1,
                           confint        = -0.2,
                           hor            = 12,
                           num_cores      = 1),
                 'The width of the confidence bands has to be >=0.', fixed = TRUE)
  })

  test_that("Test whether exogenous data is a data.frame", {
    testthat::expect_error(lp_lin_iv(endog_data,
                           shock          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = as.matrix(rnorm(100)),
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 1,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
                 'Exogenous data has to be given as a data.frame.', fixed = TRUE)
  })


  test_that("Test whether instrument is a data.frame", {
    testthat::expect_error(lp_lin_iv(endog_data,
                           shock          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = as.matrix(rnorm(100)),
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 1,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
                 'Exogenous data has to be given as a data.frame.', fixed = TRUE)
  })


  test_that("Test whether lag length for exogenous data is given", {
    testthat::expect_error(lp_lin_iv(endog_data,
                           shock          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = as.data.frame(rnorm(dim(endog_data)[1])),
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = NaN,
                           max_lags       = NaN,
                           trend          = 1,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
                 'Please provide a lag length for the exogenous data.', fixed = TRUE)
  })

  test_that("Test whether lag length and lag length criterion is given", {
    testthat::expect_error(lp_lin_iv(endog_data,
                           shock          = instrument,
                           lags_endog_lin       = 4,
                           exog_data      = NULL,
                           lags_exog      = NULL,
                           contemp_data   = NULL,
                           lags_criterion = 'AIC',
                           max_lags       = NULL,
                           trend          = 1,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
         'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.
         Please set lags_endog_lin to NaN if you want to use a lag length criterion.', fixed = TRUE)
  })


  test_that("Test that model works with automatic lag length selection", {




    testthat::expect_error(lp_lin_iv(endog_data,
                           shock          = instrument,
                           lags_endog_lin = NaN,
                           exog_data      = as.data.frame(rnorm(length(instrument[, 1]))),
                           lags_exog      = 2,
                           contemp_data   = NULL,
                           lags_criterion = 'AIC',
                           max_lags       = 4,
                           trend          = 2,
                           confint        = 1,
                           hor            = 12,
                           num_cores      = 1),
                                           NA)
  })

  test_that("Test that model works with automatic lag length selection AND 2SLS", {

    # Set seed
    set.seed(007)

    # Load data
    ag_data       <- ag_data
    sample_start  <- 7
    sample_end    <- dim(ag_data)[1]

    # Endogenous data
    endog_data    <- ag_data[sample_start:sample_end,3:5]

    # Variable to shock with (government spending)
    shock         <- ag_data[sample_start:sample_end, 3]

    # Generate instrument variable that is correlated with government spending
    instrum       <- as.data.frame(0.9*shock$Gov + rnorm(length(shock$Gov), 0, 0.02) )

    testthat::expect_error(lp_lin_iv(endog_data,
                                     shock          = instrument,
                                     instrum        = instrum,
                                     lags_endog_lin = NaN,
                                     exog_data      = as.data.frame(rnorm(length(instrument[, 1]))),
                                     lags_exog      = 2,
                                     use_twosls     = TRUE,
                                     contemp_data   = NULL,
                                     lags_criterion = 'AIC',
                                     max_lags       = 4,
                                     trend          = 2,
                                     confint        = 1,
                                     hor            = 12,
                                     num_cores      = 1),
                           NA)

    # Estimate linear model
    testthat::expect_error( lp_lin_iv(endog_data,
                                      lags_endog_lin = 4,
                                      shock          = shock,
                                      instrum        = instrum,
                                      use_twosls     = TRUE,
                                      exog_data      = NULL,
                                      lags_exog      = NULL,
                                      contemp_data   = NULL,
                                      lags_criterion = NaN,
                                      max_lags       = NaN,
                                      nw_prewhite    = T,
                                      trend          = 0,
                                      confint        = 1.96,
                                      hor            = 20,
                                      num_cores      = 1),
                            NA )
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


    lin_results <- round(results_lin_iv$irf_lin_mean[1,], 2)


    testthat::expect_equal(lin_results, rz_lin_results)


  })


  test_that("Run use_twosls", {

    # Set seed
    set.seed(007)

    # Load data
    ag_data       <- ag_data
    sample_start  <- 7
    sample_end    <- dim(ag_data)[1]

    # Endogenous data
    endog_data    <- ag_data[sample_start:sample_end,3:5]

    # Variable to shock with (government spending)
    shock         <- ag_data[sample_start:sample_end, 3]

    # Generate instrument variable that is correlated with government spending
    instrum       <- as.data.frame(0.9*shock$Gov + rnorm(length(shock$Gov), 0, 0.02) )

    # Estimate linear model
    testthat::expect_error( lp_lin_iv(endog_data,
                                      lags_endog_lin = 4,
                                      shock          = shock,
                                      instrum        = instrum,
                                      use_twosls     = TRUE,
                                      exog_data      = NULL,
                                      lags_exog      = NULL,
                                      contemp_data   = NULL,
                                      lags_criterion = NaN,
                                      max_lags       = NaN,
                                      trend          = 0,
                                      confint        = 1.96,
                                      hor            = 20,
                                      num_cores      = 1),
                           NA )
  })

  test_that("Test that prewhitening works", {

    # Estimate linear model
    testthat::expect_error(lp_lin_iv(endog_data[, 1],
                                shock          = instrument,
                                lags_endog_lin = 4,
                                exog_data      = NULL,
                                lags_exog      = NULL,
                                contemp_data   = NULL,
                                lags_criterion = NaN,
                                max_lags       = NaN,
                                nw_prewhite    = T,
                                trend          = 0,
                                confint        = 1.96,
                                hor            = 20,
                                num_cores      = 1),
                           NA)


  })


  test_that("Test that prewhitening and 2SLS works", {

    # Set seed
    set.seed(007)

    # Load data
    ag_data       <- ag_data
    sample_start  <- 7
    sample_end    <- dim(ag_data)[1]

    # Endogenous data
    endog_data    <- ag_data[sample_start:sample_end,3:5]

    # Variable to shock with (government spending)
    shock         <- ag_data[sample_start:sample_end, 3]

    # Generate instrument variable that is correlated with government spending
    instrum       <- as.data.frame(0.9*shock$Gov + rnorm(length(shock$Gov), 0, 0.02) )

    # Estimate linear model
    testthat::expect_error( lp_lin_iv(endog_data,
                                      lags_endog_lin = 4,
                                      shock          = shock,
                                      instrum        = instrum,
                                      use_twosls     = TRUE,
                                      exog_data      = NULL,
                                      lags_exog      = NULL,
                                      contemp_data   = NULL,
                                      lags_criterion = NaN,
                                      max_lags       = NaN,
                                      nw_prewhite    = T,
                                      trend          = 0,
                                      confint        = 1.96,
                                      hor            = 20,
                                      num_cores      = 1),
                            NA )
  })


  test_that("Test that 2SLS works and normal standard errors work", {

    # Set seed
    set.seed(007)

    # Load data
    ag_data       <- ag_data
    sample_start  <- 7
    sample_end    <- dim(ag_data)[1]

    # Endogenous data
    endog_data    <- ag_data[sample_start:sample_end,3:5]

    # Variable to shock with (government spending)
    shock         <- ag_data[sample_start:sample_end, 3]

    # Generate instrument variable that is correlated with government spending
    instrum       <- as.data.frame(0.9*shock$Gov + rnorm(length(shock$Gov), 0, 0.02) )

    # Estimate linear model
    testthat::expect_error( lp_lin_iv(endog_data,
                                      lags_endog_lin = 4,
                                      shock          = shock,
                                      instrum        = instrum,
                                      use_twosls     = TRUE,
                                      exog_data      = NULL,
                                      lags_exog      = NULL,
                                      contemp_data   = NULL,
                                      lags_criterion = NaN,
                                      max_lags       = NaN,
                                      use_nw         = F,
                                      trend          = 0,
                                      confint        = 1.96,
                                      hor            = 20,
                                      num_cores      = 1),
                            NA )
  })

