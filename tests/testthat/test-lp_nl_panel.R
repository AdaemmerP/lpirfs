context("lp_nl_panel")


library(dplyr)

set.seed(123)

# Simulate panel data
  N  <- 10
  TS <- 30

  cross_section <- sort(LETTERS[rep(seq(1,N, 1), TS)])
  time_section  <- rep(seq(1,TS, 1), N)

  data_set    <- tibble(cross_section, time_section) %>%
                    group_by(cross_section) %>%
                    mutate(x_1 = rnorm(TS)) %>%
                    mutate(x_2 = rnorm(TS)) %>%
                    mutate(x_3 = rnorm(TS)) %>%
                    mutate(x_4 = rnorm(TS)) %>%
                    mutate(y   = 0.3*x_1 + 0.4*x_2 + 0.5*x_3 + 0.6*x_4 + rnorm(TS)) %>%
                    ungroup()


  test_that("Test that data is given", {

              testthat::expect_error( lp_nl_panel(data_set          = NULL,
                                                  data_sample       = 'Full',
                                                  endog_data        = "y",
                                                  cumul_mult        = TRUE,

                                                  shock             = "x_1",
                                                  diff_shock        = FALSE,
                                                  panel_model       = "within",
                                                  panel_effect      = "individual",
                                                  robust_cov        = "vcovSCC",

                                                  switching         = "x_1",
                                                  lag_switching     = TRUE,
                                                  use_logistic      = TRUE,
                                                  use_hp            = TRUE,
                                                  lambda            = 7,
                                                  gamma             = 10,

                                                  c_exog_data       = colnames(data_set)[4:6],
                                                  l_exog_data       = colnames(data_set)[4:6],
                                                  lags_exog_data    = 2,
                                                  c_fd_exog_data    = colnames(data_set)[4:6],
                                                  l_fd_exog_data    = colnames(data_set)[4:6],
                                                  lags_fd_exog_data = 2,

                                                  confint           = 1.67,
                                                  hor               = 10),
                                      "You have to provide the panel data set.", fixed = TRUE)
            })

  test_that("Test that column names are correct", {

    names(data_set)[3:4] <- c("cross_id", "date_id")

    testthat::expect_error( lp_nl_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = "vcovSCC",

                                        switching         = "x_1",
                                        lag_switching     = TRUE,
                                        use_logistic      = TRUE,
                                        use_hp            = TRUE,
                                        lambda            = 7,
                                        gamma             = 10,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 2,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1.67,
                                        hor               = 10),
                            "You cannot use the column names 'cross_id' or 'date_id' besides the first two columns of your data.frame.
         Please rename them." , fixed = TRUE)
  })


  test_that("Test that name for endogenous variable is given", {

    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = NULL,
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = "vcovSCC",

                                        switching         = "x_1",
                                        lag_switching     = TRUE,
                                        use_logistic      = TRUE,
                                        use_hp            = TRUE,
                                        lambda            = 7,
                                        gamma             = 10,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 2,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1.67,
                                        hor               = 10),
                            "You have to provide the name of the endogenous variable." , fixed = TRUE)
  })



  test_that("Test that name for endogenous variable is given", {

    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = TRUE,

                                       shock             = NULL,
                                       diff_shock        = FALSE,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = TRUE,
                                       use_logistic      = TRUE,
                                       use_hp            = TRUE,
                                       lambda            = 7,
                                       gamma             = 10,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1.67,
                                       hor               = 10),
                           "You have to provide the name of the variable to shock with." , fixed = TRUE)
  })



  test_that("Test that name for shock variable is given", {

    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = TRUE,

                                       shock             = NULL,
                                       diff_shock        = FALSE,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = TRUE,
                                       use_logistic      = TRUE,
                                       use_hp            = TRUE,
                                       lambda            = 7,
                                       gamma             = 10,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1.67,
                                       hor               = 10),
                           "You have to provide the name of the variable to shock with." , fixed = TRUE)
  })


  test_that("Test that panel model type is correct", {

    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = TRUE,

                                       shock             = "x_1",
                                       diff_shock        = FALSE,
                                       panel_model       = "soso",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = TRUE,
                                       use_logistic      = TRUE,
                                       use_hp            = TRUE,
                                       lambda            = 7,
                                       gamma             = 10,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1.67,
                                       hor               = 10),
         "The type of the panel model has to be 'within', 'random', 'ht', 'between', 'pooling' or 'fd'. See
         the vignette of the plm package for details."  , fixed = TRUE)
  })



  test_that("Test that panel effect is correct", {

    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = TRUE,

                                       shock             = "x_1",
                                       diff_shock        = FALSE,
                                       panel_model       = "within",
                                       panel_effect      = "soso",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = TRUE,
                                       use_logistic      = TRUE,
                                       use_hp            = TRUE,
                                       lambda            = 7,
                                       gamma             = 10,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1.67),
         "The effect introduced in the model has to be 'individual', 'time', 'twoways' or 'nested'.
         See the vignette of the plm package for details.", fixed = TRUE)
  })


  test_that("Test that covariance estimator is correct", {

    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = TRUE,

                                       shock             = "x_1",
                                       diff_shock        = FALSE,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "soso",

                                       switching         = "x_1",
                                       lag_switching     = TRUE,
                                       use_logistic      = TRUE,
                                       use_hp            = TRUE,
                                       lambda            = 7,
                                       gamma             = 10,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1.67),
         "The choices for robust covariance estimation are 'vcovBK', 'vcovDC', 'vcovHC', 'vcovNW', 'vcovSCC' and 'Vcxt'.
         For details, see the vignette of the plm package and Miller (2017)." , fixed = TRUE)
  })


  test_that("Test that lag length for exogenous data is given", {

    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = TRUE,

                                       shock             = "x_1",
                                       diff_shock        = FALSE,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = TRUE,
                                       use_logistic      = TRUE,
                                       use_hp            = TRUE,
                                       lambda            = 7,
                                       gamma             = 10,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = NULL,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1.67),
                           "You have to provide the lag lengths for the exogenous data with lagged impact." , fixed = TRUE)
  })


  test_that("Test that lag length for exogenous data is given", {

    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = TRUE,

                                       shock             = "x_1",
                                       diff_shock        = FALSE,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = TRUE,
                                       use_logistic      = TRUE,
                                       use_hp            = TRUE,
                                       lambda            = 7,
                                       gamma             = 10,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = NULL,

                                       confint           = 1.67),
        "You have to provide the lag lengths for the exogenous data with lagged impact of first differences." , fixed = TRUE)
  })


  test_that("Test that lag length for exogenous data is given", {

    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = TRUE,

                                       shock             = "x_1",
                                       diff_shock        = FALSE,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = TRUE,
                                       use_logistic      = TRUE,
                                       use_hp            = TRUE,
                                       lambda            = 7,
                                       gamma             = 10,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = NULL),
                          "Please specify a value for the width of the confidence bands." , fixed = TRUE)
  })

  test_that("Test that lag length for exogenous data is given", {

    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = TRUE,

                                       shock             = "x_1",
                                       diff_shock        = FALSE,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = TRUE,
                                       use_logistic      = TRUE,
                                       use_hp            = TRUE,
                                       lambda            = 7,
                                       gamma             = 10,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = -1),
                           "The width of the confidence bands has to be >=0." , fixed = TRUE)
  })


  test_that("Test that lag length for exogenous data is given", {

    hor <- c(NaN, 1.3, -1)

    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = TRUE,

                                       shock             = "x_1",
                                       diff_shock        = FALSE,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = TRUE,
                                       use_logistic      = TRUE,
                                       use_hp            = TRUE,
                                       lambda            = 7,
                                       gamma             = 10,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1,
                                       hor               = sample(hor, 1)),
                           "The number of horizons has to be an integer and >0." , fixed = TRUE)
  })



  test_that("Test that name for switching variable is given", {

    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = TRUE,

                                       shock             = "x_1",
                                       diff_shock        = FALSE,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = NULL,
                                       lag_switching     = TRUE,
                                       use_logistic      = TRUE,
                                       use_hp            = TRUE,
                                       lambda            = 7,
                                       gamma             = 10,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1,
                                       hor               = 2),
                           "You have to provide a name for the switching variable." , fixed = TRUE)
  })


  test_that("Test that 'use_hp' is specified", {


    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = TRUE,

                                       shock             = "x_1",
                                       diff_shock        = FALSE,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = TRUE,
                                       use_logistic      = TRUE,
                                       use_hp            = NULL,
                                       lambda            = 7,
                                       gamma             = 10,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1,
                                       hor               = 2),
                           "Please specify whether to use the HP-filter for the switching variable.", fixed = TRUE)
  })


  test_that("Test that 'use_hp' is specified", {


    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = T,

                                       shock             = "x_1",
                                       diff_shock        = F,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = T,
                                       use_logistic      = T,
                                       use_hp            = T,
                                       lambda            = NULL,
                                       gamma             = 10,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1,
                                       hor               = 2),
                           "Please give a value for lambda for the HP-filter.", fixed = TRUE)
  })


  test_that("Test that gamma value is given", {


    testthat::expect_error(lp_nl_panel(data_set           = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = T,

                                       shock             = "x_1",
                                       diff_shock        = F,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = T,
                                       use_logistic      = T,
                                       use_hp            = T,
                                       lambda            = 3,
                                       gamma             = NULL,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1,
                                       hor               = 2),
                           "Please give a value for gamma (>0).", fixed = TRUE)
  })


  test_that("Test that gmm options are correct", {


    testthat::expect_error(lp_nl_panel(data_set          = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = T,

                                       shock             = "x_1",
                                       diff_shock        = F,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = T,
                                       use_logistic      = T,
                                       use_hp            = T,
                                       lambda            = 3,
                                       gamma             = 3,

                                       use_gmm           = T,
                                       gmm_model         = "soso",

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1,
                                       hor               = 2),
                           "The model type for gmm has to be 'onestep' (default) or 'twosteps'.", fixed = TRUE)
  })

  test_that("Test that gmm options are correct", {


    testthat::expect_error(lp_nl_panel(data_set          = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = T,

                                       shock             = "x_1",
                                       diff_shock        = F,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = T,
                                       use_logistic      = T,
                                       use_hp            = T,
                                       lambda            = 3,
                                       gamma             = 3,

                                       use_gmm           = T,
                                       gmm_effect        = "soso",

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1,
                                       hor               = 2),
                           "The effect for gmm has to be 'twoways' (default) or 'individual'.", fixed = TRUE)
  })


  test_that("Test that gmm options are correct", {


    testthat::expect_error(lp_nl_panel(data_set          = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = T,

                                       shock             = "x_1",
                                       diff_shock        = F,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = T,
                                       use_logistic      = T,
                                       use_hp            = T,
                                       lambda            = 3,
                                       gamma             = 3,

                                       use_gmm           = T,
                                       gmm_transformation = "soso",

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1,
                                       hor               = 2),
                           "The transformation to apply to the model has to either be 'd' (default)
         for the 'difference GMM' model or 'ld' for the 'system GMM'.", fixed = TRUE)
  })


  test_that("Test that shock name is correct", {


    testthat::expect_error(lp_nl_panel(data_set          = data_set,
                                       data_sample       = 'Full',
                                       endog_data        = "y",
                                       cumul_mult        = T,

                                       shock             = "shock",
                                       diff_shock        = F,
                                       panel_model       = "within",
                                       panel_effect      = "individual",
                                       robust_cov        = "vcovSCC",

                                       switching         = "x_1",
                                       lag_switching     = T,
                                       use_logistic      = T,
                                       use_hp            = T,
                                       lambda            = 3,
                                       gamma             = 3,

                                       use_gmm           = F,

                                       c_exog_data       = colnames(data_set)[4:6],
                                       l_exog_data       = colnames(data_set)[4:6],
                                       lags_exog_data    = 2,
                                       c_fd_exog_data    = colnames(data_set)[4:6],
                                       l_fd_exog_data    = colnames(data_set)[4:6],
                                       lags_fd_exog_data = 2,

                                       confint           = 1,
                                       hor               = 2),
                           'Please use another name for your shock variable".
         Your current name would lead to a naming problem during estimation.', fixed = TRUE)
  })


test_that("Test whether output coefficients from 'lp_nl_panel_results' coincide
            with coefficients estimated by using the matrix prepared in 'lp_nl_panel'", {
              suppressWarnings(
              # Estimate panel model
              results_panel <-  lp_nl_panel(data_set          = data_set,
                                            data_sample       = 'Full',
                                            endog_data        = "y",
                                            cumul_mult        = TRUE,

                                            shock             = "x_1",
                                            diff_shock        = FALSE,
                                            panel_model       = "within",
                                            panel_effect      = "individual",
                                            robust_cov        = NULL,

                                            switching         = "x_1",
                                            lag_switching     = FALSE,
                                            use_hp            = TRUE,
                                            lambda            = 7,
                                            gamma             = 10,

                                            c_exog_data       = colnames(data_set)[4:6],
                                            l_exog_data       = colnames(data_set)[4:6],
                                            lags_exog_data    = 2,
                                            c_fd_exog_data    = colnames(data_set)[4:6],
                                            l_fd_exog_data    = colnames(data_set)[4:6],
                                            lags_fd_exog_data = 2,

                                            confint           = 1.67,
                                            hor               = 10))


              lp_nl_panel_results <- results_panel$reg_summaries[[1]]
              yx_data             <- results_panel$xy_data_sets[[1]]

              x_names             <- colnames(yx_data)[!colnames(yx_data) %in% c("cross_id", "date_id", "y")]
              panel_formula       <- as.formula(paste("y~", paste(x_names, collapse = "+")))


              panel_results       <- plm::plm(formula = panel_formula,
                                              data     = yx_data,
                                              index    = c("cross_id", "date_id"),
                                              model    = "within",
                                              effect   = "individual")


              testthat::expect_equal(panel_results$coefficients, results_panel$reg_summaries[[1]]$coefficients[,1])

            })

test_that("Test that model throws no error when estimating robust
            covariance matrix", {
              # Estimate panel model
              testthat::expect_error( lp_nl_panel(data_set          = data_set,
                                                  data_sample       = 'Full',
                                                  endog_data        = "y",
                                                  cumul_mult        = TRUE,

                                                  shock             = "x_1",
                                                  diff_shock        = FALSE,
                                                  panel_model       = "within",
                                                  panel_effect      = "individual",
                                                  robust_cov        = "vcovSCC",

                                                  switching         = "x_1",
                                                  lag_switching     = FALSE,
                                                  use_hp            = TRUE,
                                                  lambda            = 7,
                                                  gamma             = 10,

                                                  c_exog_data       = colnames(data_set)[4:6],
                                                  l_exog_data       = colnames(data_set)[4:6],
                                                  lags_exog_data    = 2,
                                                  c_fd_exog_data    = colnames(data_set)[4:6],
                                                  l_fd_exog_data    = colnames(data_set)[4:6],
                                                  lags_fd_exog_data = 2,

                                                  confint           = 1.67,
                                                  hor               = 10),
                                     NA)
            })


test_that("Test that model works when shock = endog_data", {
              # Estimate panel model
              testthat::expect_error( lp_nl_panel(data_set          = data_set,
                                                  data_sample       = 'Full',
                                                  endog_data        = "y",
                                                  cumul_mult        = TRUE,

                                                  shock             = "y",
                                                  diff_shock        = FALSE,
                                                  panel_model       = "within",
                                                  panel_effect      = "individual",
                                                  robust_cov        = "vcovSCC",

                                                  switching         = "x_1",
                                                  lag_switching     = FALSE,
                                                  use_hp            = TRUE,
                                                  lambda            = 7,
                                                  gamma             = 10,

                                                  c_exog_data       = colnames(data_set)[4:6],
                                                  l_exog_data       = colnames(data_set)[4:6],
                                                  lags_exog_data    = 2,
                                                  c_fd_exog_data    = colnames(data_set)[4:6],
                                                  l_fd_exog_data    = colnames(data_set)[4:6],
                                                  lags_fd_exog_data = 2,

                                                  confint           = 1.67,
                                                  hor               = 10),
                                      NA)
            })

test_that("Test that 'robust_cov = NULL' when using gmm.", {
              # Estimate panel model
              testthat::expect_error( lp_nl_panel(data_set          = data_set,
                                                  data_sample       = 'Full',
                                                  endog_data        = "y",
                                                  cumul_mult        = TRUE,

                                                  shock             = "x_1",
                                                  diff_shock        = FALSE,
                                                  panel_model       = "within",
                                                  panel_effect      = "individual",
                                                  robust_cov        = "vcovSCC",

                                                  switching         = "x_1",
                                                  lag_switching     = FALSE,
                                                  use_hp            = TRUE,
                                                  lambda            = 7,
                                                  gamma             = 10,

                                                  use_gmm           = T,

                                                  c_exog_data       = colnames(data_set)[4:6],
                                                  l_exog_data       = colnames(data_set)[4:6],
                                                  lags_exog_data    = 2,
                                                  c_fd_exog_data    = colnames(data_set)[4:6],
                                                  l_fd_exog_data    = colnames(data_set)[4:6],
                                                  lags_fd_exog_data = 2,

                                                  confint           = 1.67,
                                                  hor               = 10),
                                      "Please set 'robust_cov = NULL' when using gmm.")
            })



test_that("Test gmm model", {

  set.seed(123)

  # Simulate panel data with large cross dimension
  N  <- 300
  TS <- 8

  cross_section <- sort(rep(seq(1,N, 1), TS))
  time_section  <- rep(seq(1,TS, 1), N)

  data_set    <- tibble(cross_section, time_section) %>%
                  group_by(cross_section) %>%
                  mutate(x_1 = rnorm(TS)) %>%
                  mutate(x_2 = rnorm(TS)) %>%
                  mutate(x_3 = rnorm(TS)) %>%
                  mutate(x_4 = rnorm(TS)) %>%
                  mutate(y   = 0.3*x_1 + 0.4*x_2 + 0.5*x_3 + 0.6*x_4 + rnorm(TS)) %>%
                  ungroup()

              # Estimate panel model
              testthat::expect_error(lp_nl_panel(data_set          = data_set,
                                                  data_sample       = 'Full',
                                                  endog_data        = "y",
                                                  cumul_mult        = TRUE,

                                                  shock             = "x_1",
                                                  diff_shock        = FALSE,
                                                  panel_model       = "within",
                                                  panel_effect      = "individual",
                                                  robust_cov        = NULL,

                                                  switching         = "x_1",
                                                  lag_switching     = FALSE,
                                                  use_hp            = TRUE,
                                                  lambda            = 3,
                                                  gamma             = 3,

                                                  use_gmm           = T,

                                                  c_exog_data       = NULL,
                                                  l_exog_data       = "y",
                                                  lags_exog_data    = 1,
                                                  c_fd_exog_data    = NULL,
                                                  l_fd_exog_data    = NULL,
                                                  lags_fd_exog_data = NULL,

                                                  confint           = 1.67,
                                                  hor               = 3),
                                      NA)
            })



test_that("Check output of switching variable I", {


  # Estimate panel model
  results_nl <- lp_nl_panel(data_set                   = data_set,
                                     data_sample       = 'Full',
                                     endog_data        = "y",
                                     cumul_mult        = TRUE,

                                     shock             = "x_1",
                                     diff_shock        = FALSE,
                                     panel_model       = "within",
                                     panel_effect      = "individual",
                                     robust_cov        = "vcovSCC",

                                     switching         = "x_1",
                                     lag_switching     = FALSE,
                                     use_logistic      = FALSE,
                                     use_hp            = FALSE,
                                     lambda            = 3,
                                     gamma             = 3,

                                     use_gmm           = F,

                                     c_exog_data       = NULL,
                                     l_exog_data       = "y",
                                     lags_exog_data    = 1,
                                     c_fd_exog_data    = NULL,
                                     l_fd_exog_data    = NULL,
                                     lags_fd_exog_data = NULL,

                                     confint           = 1.67,
                                     hor               = 3)

  # Test whether shock variable is correct
  testthat::expect_equal(results_nl$fz$fz[, 1], data_set$x_1)



  # States from output
  shock_s1_output <- results_nl$xy_data_sets[[1]]$shock_s1
  shock_s2_output <- results_nl$xy_data_sets[[1]]$shock_s2

  # Make lagged tibble based on data set
  data_set_lag <- data_set %>%
                  arrange(cross_section, time_section) %>%
                  group_by(cross_section)              %>%
                  mutate_at(vars(y), funs(dplyr::lag(., 1))) %>%
                  ungroup()

  lag_indicator <-  !is.na(data_set_lag$y)

  # Manual states
  shock_s1_manual <- data_set$x_1*(1  - data_set$x_1)
  shock_s1_manual <- shock_s1_manual[lag_indicator]

  shock_s2_manual <- data_set$x_1*(data_set$x_1)
  shock_s2_manual <- shock_s2_manual[lag_indicator]

  # Test whether states of shock variable are correctly estimated
  testthat::expect_equal(as.numeric(shock_s1_output), shock_s1_manual)
  testthat::expect_equal(as.numeric(shock_s2_output), shock_s2_manual)


})



test_that("Check output of switching variable II", {



  # Estimate panel model
  results_nl <- lp_nl_panel(data_set          = data_set,
                            data_sample       = 'Full',
                            endog_data        = "y",
                            cumul_mult        = TRUE,

                            shock             = "x_1",
                            diff_shock        = FALSE,
                            panel_model       = "within",
                            panel_effect      = "individual",
                            robust_cov        = "vcovSCC",

                            switching         = "x_1",
                            lag_switching     = TRUE,
                            use_logistic      = FALSE,
                            use_hp            = FALSE,
                            lambda            = 3,
                            gamma             = 3,

                            use_gmm           = F,

                            c_exog_data       = NULL,
                            l_exog_data       = "y",
                            lags_exog_data    = 1,
                            c_fd_exog_data    = NULL,
                            l_fd_exog_data    = NULL,
                            lags_fd_exog_data = NULL,

                            confint           = 1.67,
                            hor               = 3)

  lag_dplyr  <- data_set %>%
                group_by(cross_section) %>%
                           mutate_at(vars(x_1), funs(dplyr::lag(.,1))) %>%
                           ungroup() %>%
                           select(x_1)  %>%
                           as.matrix()  %>%
                           as.numeric()

  fz_output  <- results_nl$fz$fz

  # Test whether switching variable is correctly specified
  testthat::expect_equal(lag_dplyr, fz_output)


  # Test whether states of shock variable are correctly estimated
  # States from output
  shock_s1_output <- results_nl$xy_data_sets[[1]]$shock_s1
  shock_s2_output <- results_nl$xy_data_sets[[1]]$shock_s2


  # Manual states
  shock_s1_manual <- as.numeric(na.omit(data_set$x_1*(1  - lag_dplyr)))
  shock_s2_manual <- as.numeric(na.omit(data_set$x_1*lag_dplyr))

  # Test whether states of shock variable are correctly specified
  testthat::expect_equal(as.numeric(shock_s1_output), shock_s1_manual)
  testthat::expect_equal(as.numeric(shock_s2_output), shock_s2_manual)


})





test_that("Check output of switching variable III", {

  # Estimate panel model
  results_nl <- lp_nl_panel(data_set                   = data_set,
                            data_sample       = 'Full',
                            endog_data        = "y",
                            cumul_mult        = TRUE,

                            shock             = "x_1",
                            diff_shock        = FALSE,
                            panel_model       = "within",
                            panel_effect      = "individual",
                            robust_cov        = "vcovSCC",

                            switching         = "x_1",
                            lag_switching     = FALSE,
                            use_logistic      = TRUE,
                            use_hp            = FALSE,
                            lambda            = 3,
                            gamma             = 3,

                            use_gmm           = F,

                            c_exog_data       = NULL,
                            l_exog_data       = "y",
                            lags_exog_data    = 1,
                            c_fd_exog_data    = NULL,
                            l_fd_exog_data    = NULL,
                            lags_fd_exog_data = NULL,

                            confint           = 1.67,
                            hor               = 3)

  # Use logistic function
  logistic_function <- function(z_0){

    switching_val <- exp(-3*z_0)/
                    (1 + exp(-3*z_0))

  }


  # Compute manual values of switching variable
  logistic_dplyr  <- data_set %>%
                    group_by(cross_section) %>%
                    mutate_at(vars(x_1), funs(logistic_function(.))) %>%
                    ungroup() %>%
                    select(x_1)  %>%
                    as.matrix()  %>%
                    as.numeric()

  fz_output  <- results_nl$fz$fz

  # Test whether switching variable from function and manual values are identical
  testthat::expect_equal(logistic_dplyr, fz_output)


  # States from output
  shock_s1_output <- results_nl$xy_data_sets[[1]]$shock_s1
  shock_s2_output <- results_nl$xy_data_sets[[1]]$shock_s2

  # Make lagged tibble based on data set
  data_set_lag <- data_set %>%
                  arrange(cross_section, time_section) %>%
                  group_by(cross_section)              %>%
                  mutate_at(vars(y), funs(dplyr::lag(., 1))) %>%
                  ungroup()

  lag_indicator <-  !is.na(data_set_lag$y)

  # Manual states
  shock_s1_manual <- data_set$x_1*(1  - logistic_dplyr)
  shock_s1_manual <- shock_s1_manual[lag_indicator]

  shock_s2_manual <- data_set$x_1*(logistic_dplyr)
  shock_s2_manual <- shock_s2_manual[lag_indicator]

  # Test whether states of shock variable are correctly estimated
  testthat::expect_equal(as.numeric(shock_s1_output), shock_s1_manual)
  testthat::expect_equal(as.numeric(shock_s2_output), shock_s2_manual)


})




test_that("Check output of switching variable IV", {


  # Estimate panel model
  results_nl <- lp_nl_panel(data_set                   = data_set,
                            data_sample       = 'Full',
                            endog_data        = "y",
                            cumul_mult        = TRUE,

                            shock             = "x_1",
                            diff_shock        = FALSE,
                            panel_model       = "within",
                            panel_effect      = "individual",
                            robust_cov        = "vcovSCC",

                            switching         = "x_1",
                            lag_switching     = TRUE,
                            use_logistic      = TRUE,
                            use_hp            = FALSE,
                            lambda            = 3,
                            gamma             = 3,

                            use_gmm           = F,

                            c_exog_data       = NULL,
                            l_exog_data       = "y",
                            lags_exog_data    = 1,
                            c_fd_exog_data    = NULL,
                            l_fd_exog_data    = NULL,
                            lags_fd_exog_data = NULL,

                            confint           = 1.67,
                            hor               = 3)

  # Logistic function
  logistic_function <- function(z_0){

    switching_val <- exp(-3*z_0)/
      (1 + exp(-3*z_0))

  }


  # Compute manual switching values based on logistic function
  fz_dplyr  <- data_set %>%
                group_by(cross_section) %>%
                mutate_at(vars(x_1), funs(logistic_function(.))) %>%
                mutate_at(vars(x_1), funs(dplyr::lag(., 1)))     %>%
                ungroup() %>%
                select(x_1)  %>%
                as.matrix()  %>%
                as.numeric()

  # Switching values from output
  fz_output  <- results_nl$fz$fz

  # Test whether switching variable from output and manual are identical
  testthat::expect_equal(fz_dplyr, fz_output)


  # States from output
  shock_s1_output <- results_nl$xy_data_sets[[1]]$shock_s1
  shock_s2_output <- results_nl$xy_data_sets[[1]]$shock_s2

  # Make lagged tibble based on data set
  data_set_lag <- data_set %>%
                  arrange(cross_section, time_section) %>%
                  group_by(cross_section)              %>%
                  mutate_at(vars(y), funs(dplyr::lag(., 1))) %>%
                  ungroup()

  lag_indicator <-  !is.na(data_set_lag$y)

  # Manual states
  shock_s1_manual <- data_set$x_1*(1  - fz_dplyr)
  shock_s1_manual <- shock_s1_manual[lag_indicator]

  shock_s2_manual <- data_set$x_1*(fz_dplyr)
  shock_s2_manual <- shock_s2_manual[lag_indicator]

  # Test whether states of shock variable are correctly estimated
  testthat::expect_equal(as.numeric(shock_s1_output), shock_s1_manual)
  testthat::expect_equal(as.numeric(shock_s2_output), shock_s2_manual)



})

