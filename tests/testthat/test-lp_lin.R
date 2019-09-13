context("lp_lin")

# Load data
  data_set_df <- interest_rules_var_data

# # # Create list for input
#   specs <- list()

# Specify inputs
  lags_endog_lin <- NaN
  lags_criterion <- 'AIC'
  max_lags       <- 2L
  trend          <- 0L
  shock_type     <- 1L
  confint        <- 1.96
  hor            <- 12L



test_that("Test whether data is a data.frame", {
    data_set_df   <- as.matrix(data_set_df)
    testthat::expect_error(lp_lin(data_set_df,
                                  lags_endog_lin   = lags_endog_lin,
                                  lags_criterion   = lags_criterion,
                                  max_lags         = max_lags,
                                  trend            = trend,
                                  shock_type       = shock_type,
                                  confint          = confint,
                                  hor              = hor),
                 'The data has to be a data.frame().', fixed = TRUE)
})

test_that("Test whether trend is given", {
    trend   <- NULL
    testthat::expect_error(lp_lin(data_set_df,
                                  lags_endog_lin   = lags_endog_lin,
                                  lags_criterion   = lags_criterion,
                                  max_lags         = max_lags,
                                  trend            = trend,
                                  shock_type       = shock_type,
                                  confint          = confint,
                                  hor              = hor),
                 'Please specify whether and which type of trend to include.', fixed = TRUE)
  })


test_that("Test whether shock_type is given", {
    shock_type   <- NULL
    testthat::expect_error(lp_lin(data_set_df,
                                  lags_endog_lin   = lags_endog_lin,
                                  lags_criterion   = lags_criterion,
                                  max_lags         = max_lags,
                                  trend            = trend,
                                  shock_type       = shock_type,
                                  confint          = confint,
                                  hor              = hor),
                 'Please specify which type of shock to use.', fixed = TRUE)
  })


test_that("Test whether 'confint' is given", {
  confint        <- NULL
  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin   = lags_endog_lin,
                                lags_criterion   = lags_criterion,
                                max_lags         = max_lags,
                                trend            = trend,
                                shock_type       = shock_type,
                                confint          = confint,
                                hor              = hor),
               'Please specify a value for the width of the confidence bands.', fixed = TRUE)
})


test_that("Test whether number of horizons is given", {
  hor          <- NULL
  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin   = lags_endog_lin,
                                lags_criterion   = lags_criterion,
                                max_lags         = max_lags,
                                trend            = trend,
                                shock_type       = shock_type,
                                confint          = confint,
                                hor              = hor),
               'Please specify the number of horizons.', fixed = TRUE)
})


test_that("Test whether wrong lag length is given", {
  lags_criterion <- 'AICCd'
  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin   = lags_endog_lin,
                                lags_criterion   = lags_criterion,
                                max_lags         = max_lags,
                                trend            = trend,
                                shock_type       = shock_type,
                                confint          = confint,
                                hor              = hor),
               'Possible lag length criteria are AICc, AIC or BIC or NaN if lag length is specified.', fixed = TRUE)
} )

test_that("Test whether lag criterion AND fixed number of lags are given", {
  lags_endog_lin <- 1
  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin   = lags_endog_lin,
                                lags_criterion   = lags_criterion,
                                max_lags         = max_lags,
                                trend            = trend,
                                shock_type       = shock_type,
                                confint          = confint,
                                hor              = hor),
               'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.', fixed = TRUE)
} )

test_that("Test whether lag criterion AND maximum number of lags are given", {
  max_lags <- NaN
  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin   = lags_endog_lin,
                                lags_criterion   = lags_criterion,
                                max_lags         = max_lags,
                                trend            = trend,
                                shock_type       = shock_type,
                                confint          = confint,
                                hor              = hor),
               'Please provide a maximum number of lags for the lag length criterion.', fixed = TRUE)
} )

test_that("Test whether values for horizons are correct", {
  hor <- -1
  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin   = lags_endog_lin,
                                lags_criterion   = lags_criterion,
                                max_lags         = max_lags,
                                trend            = trend,
                                shock_type       = shock_type,
                                confint          = confint,
                                hor              = hor),
               'The number of horizons has to be an integer and > 0.', fixed = TRUE)
} )

test_that("Test whether lags are integers", {
  lags_endog_lin <- 1.5
  lags_criterion <- NaN
  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin   = lags_endog_lin,
                                lags_criterion   = lags_criterion,
                                max_lags         = max_lags,
                                trend            = trend,
                                shock_type       = shock_type,
                                confint          = confint,
                                hor              = hor),
               'The numbers of lags have to be a positive integer.', fixed = TRUE)
} )

test_that("Test whether trend is correctly specified", {
  trend <- 12
  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin   = lags_endog_lin,
                                lags_criterion   = lags_criterion,
                                max_lags         = max_lags,
                                trend            = trend,
                                shock_type       = shock_type,
                                confint          = confint,
                                hor              = hor),
               'For trend please enter 0 = no trend, 1 = trend, 2 = trend and quadratic trend.', fixed = TRUE)
} )

test_that("Test shock type is correctly specified", {
  shock_type <- 12
  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin   = lags_endog_lin,
                                lags_criterion   = lags_criterion,
                                max_lags         = max_lags,
                                trend            = trend,
                                shock_type       = shock_type,
                                confint          = confint,
                                hor              = hor),
               'The shock_type has to be 0 = standard deviation shock or 1 = unit shock.', fixed = TRUE)
} )

test_that("Test whether width of confidence bands is correctly specified", {
  confint <- -1
  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin   = lags_endog_lin,
                                lags_criterion   = lags_criterion,
                                max_lags         = max_lags,
                                trend            = trend,
                                shock_type       = shock_type,
                                confint          = confint,
                                hor              = hor),
               'The width of the confidence bands has to be >=0.', fixed = TRUE)
} )

test_that("Test whether maximum lag length is given when no criterion is given", {
  lags_endog_lin <- 3
  lags_criterion <- NaN
  max_lags       <- 3
  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin   = lags_endog_lin,
                                lags_criterion   = lags_criterion,
                                max_lags         = max_lags,
                                trend            = trend,
                                shock_type       = shock_type,
                                confint          = confint,
                                hor              = hor),
               'The maximum number of lags is only used if you provide a lag length criterion.', fixed = TRUE)
} )


test_that("Test whether no lag length criterion and number of lags are given", {
  lags_endog_lin       <- NaN
  lags_criterion <- NaN
  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin   = lags_endog_lin,
                                lags_criterion   = lags_criterion,
                                max_lags         = max_lags,
                                trend            = trend,
                                shock_type       = shock_type,
                                confint          = confint,
                                hor              = hor),
               'You have to at least provide a lag criterion (AICc, AIC or BIC) or a fixed number of lags.', fixed = TRUE)
} )




test_that("Test whether exogenous data is a data.frame", {
  exog_data       <- rnorm(dim(data_set_df)[1])

  testthat::expect_error(lp_lin(data_set_df,
                          lags_endog_lin       = lags_endog_lin,
                          lags_criterion = lags_criterion,
                          max_lags       = max_lags,
                          trend          = trend,
                          shock_type     = shock_type,
                          confint        = confint,
                          hor            = hor,
                          exog_data      = exog_data,
                          lags_exog      = 4),

               'Exogenous data has to be a data.frame.', fixed = TRUE)
} )


test_that("Test whether exogenous data is a data.frame", {
  exog_data       <- rnorm(dim(data_set_df)[1])

  testthat::expect_error(lp_lin(data_set_df,
                      lags_endog_lin       = lags_endog_lin,
                      lags_criterion = lags_criterion,
                      max_lags       = max_lags,
                      trend          = trend,
                      shock_type     = shock_type,
                      confint        = confint,
                      hor            = hor,
                      exog_data      = exog_data,
                      lags_exog      = 4),

               'Exogenous data has to be a data.frame.', fixed = TRUE)
} )


test_that("Test whether lag length criterion is given for exogenous data.", {
  endog_data      <- data_set_df
  exog_data       <- data_set_df[, 1]

  testthat::expect_error(lp_lin(endog_data,
                      lags_endog_lin = lags_endog_lin,
                      lags_criterion = lags_criterion,
                      max_lags       = max_lags,
                      trend          = trend,
                      shock_type     = shock_type,
                      confint        = confint,
                      hor            = hor,
                      exog_data      = exog_data,
                      lags_exog      = NULL),

               'Please provide a lag length for the exogenous data.', fixed = TRUE)
} )

test_that("Test whether results from lp_lin are in region of results from Jordà (2005)", {

  # Load data set
  data_set_df    <- interest_rules_var_data
  results_lin    <- lp_lin(data_set_df,
                              lags_endog_lin = 4L,
                              lags_criterion = NaN,
                              max_lags       = NaN,
                              trend          = 0L,
                              shock_type     = 0L,
                              confint        = 1.96,
                              hor            = 24L,
                              num_cores      = 1)

  # Save results
  results_mean_2 <- results_lin$irf_lin_mean[1,2,1]
  results_low_2  <- results_lin$irf_lin_mean[1,2,1]
  results_up     <- results_lin$irf_lin_mean[1,2,1]


  # Results from Jordà (2005)
  jorda_results_mean_2 <- 0.9   # Approximation from figure 5 in Jordà (2005) plot, p.176
  jorda_results_low_2  <- 0.8   # Approximation from figure 5 in Jordà (2005) plot, p.176
  jorda_results_up_2   <- 1     # Approximation from figure 5 in Jordà (2005) plot, p.176

  expect_equal(results_mean_2, jorda_results_mean_2, tolerance =5e-2)
} )

test_that("Test that prewhitening runs without error", {


  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin = 4L,
                                lags_criterion = NaN,
                                max_lags       = NaN,
                                trend          = 0L,
                                shock_type     = 0L,
                                confint        = 1.96,
                                hor            = 24L,
                                nw_prewhite    = T,
                                num_cores      = 1),
                         NA)
} )



test_that("Test that adjustment to covariance runs without error", {


  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin = 4L,
                                lags_criterion = NaN,
                                max_lags       = NaN,
                                trend          = 0L,
                                shock_type     = 0L,
                                confint        = 1.96,
                                hor            = 24L,
                                nw_prewhite    = T,
                                adjust_se      = T,
                                num_cores      = 1),
                         NA)
} )




test_that("Test that estimating non-robust standard errors runs without error", {


  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin = 4L,
                                lags_criterion = NaN,
                                max_lags       = NaN,
                                trend          = 0L,
                                shock_type     = 0L,
                                confint        = 1.96,
                                hor            = 24L,
                                nw_prewhite    = T,
                                adjust_se      = T,
                                num_cores      = 1),
                         NA)
} )


test_that("Test that estimating non-robust standard errors runs without error", {


  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin = 4L,
                                lags_criterion = NaN,
                                max_lags       = NaN,
                                trend          = 0L,
                                shock_type     = 0L,
                                confint        = 1.96,
                                hor            = 24L,
                                nw_prewhite    = F,
                                adjust_se      = T,
                                num_cores      = 1),
                         NA)
} )



test_that("Test that running with AIC returns no error", {


  testthat::expect_error(lp_lin(data_set_df,
                                lags_endog_lin = NaN,
                                lags_criterion = 'AIC',
                                max_lags       = 2L,
                                trend          = 0L,
                                shock_type     = 0L,
                                confint        = 1.96,
                                use_nw = F,
                                hor            = 24L,
                                nw_prewhite    = T,
                                adjust_se      = T,
                                num_cores      = 1),
                         NA)
} )


test_that("Test that plot() runs without error on object", {


  results_lin     <-           lp_lin(data_set_df,
                                lags_endog_lin = NaN,
                                lags_criterion = 'AIC',
                                max_lags       = 2L,
                                trend          = 0L,
                                shock_type     = 0L,
                                confint        = 1.96,
                                use_nw = F,
                                hor            = 24L,
                                nw_prewhite    = T,
                                adjust_se      = T,
                                num_cores      = 1)

  testthat::expect_error(results_lin,
                         NA)
} )


