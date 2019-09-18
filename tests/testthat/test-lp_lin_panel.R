context("lp_lin_panel")


library(dplyr)

  set.seed(123)

  # Simulate panel data
  N  <- 10
  TS <- 30

  cross_section <- sort(LETTERS[rep(seq(1,N, 1), TS)])
  time_section  <- rep(seq(1,TS, 1), N)

  data_set    <- tibble(cross_section, time_section) %>%
                    group_by(cross_section) %>%
                    mutate(x_2 = rnorm(TS)) %>%
                    mutate(x_1 = rnorm(TS)) %>%
                    mutate(x_3 = rnorm(TS)) %>%
                    mutate(x_4 = rnorm(TS)) %>%
                    mutate(y   = 0.3*x_1 + 0.4*x_2 + 0.5*x_3 + 0.6*x_4 + rnorm(TS)) %>%
                    ungroup()


  test_that("Test that column names are correct", {
  names(data_set)[3:4] <- c("cross_id", "date_id")

              testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                                  data_sample       = 'Full',
                                                  endog_data        = "y",
                                                  cumul_mult        = TRUE,

                                                  shock             = "x_1",
                                                  diff_shock        = FALSE,
                                                  iv_reg            = FALSE,
                                                  instrum           = NULL,
                                                  panel_model       = "within",
                                                  panel_effect      = "individual",
                                                  robust_cov        = NULL,

                                                  c_exog_data       = colnames(data_set)[4:6],
                                                  l_exog_data       = colnames(data_set)[4:6],
                                                  lags_exog_data    = 2,
                                                  c_fd_exog_data    = colnames(data_set)[4:6],
                                                  l_fd_exog_data    = colnames(data_set)[4:6],
                                                  lags_fd_exog_data = 2,

                                                  confint           = 1.67,
                                                  hor               = 10),
                                     "You cannot use the column names 'cross_id' or 'date_id' besides the first two columns of your data.frame.
         Please rename them.", fixed = TRUE)
    })



  test_that("Test that column names are correct", {
    names(data_set)[3] <- "Test_lag_name"

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 2,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1.67,
                                        hor               = 10),
                           'Please do not use column names that include the string "lag_" in the name.
         This cause later naming problems', fixed = TRUE)
  })


  test_that("Test that data frame is given", {

    testthat::expect_error(lp_lin_panel(data_set          = NULL,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

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

  test_that("Test that name of endogenous variable is given", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = NULL,
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 2,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1.67,
                                        hor               = 10),
                           "You have to provide the name of the endogenous variable.", fixed = TRUE)
  })

  test_that("Test that name of shock variable is given", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = NULL,
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 2,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1.67,
                                        hor               = 10),
                           "You have to provide the name of the variable to shock with.", fixed = TRUE)
  })

  test_that("Test whether name of instrument variable is given", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = TRUE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 2,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1.67,
                                        hor               = 10),
                           "You have to provide the name of the instrument.", fixed = TRUE)
  })


  test_that("Test whether panel model type is correct", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "soso",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 2,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1.67,
                                        hor               = 10),
                           "The type of the panel model has to be 'within', 'random', 'ht', 'between', 'pooling' or 'fd'. See
           the vignette of the plm package for details.", fixed = TRUE)
  })

  test_that("Test whether panel effect is correct", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "soso",
                                        robust_cov        = NULL,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 2,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1.67,
                                        hor               = 10),
                           "The effect introduced in the model has to be 'individual', 'time', 'twoways' or 'nested'.
           See the vignette of the plm package for details.", fixed = TRUE)
  })

  test_that("Test whether lag lengths for the exogenous data is given", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = NULL,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1.67,
                                        hor               = 10),
        "You have to provide the lag lengths for the exogenous data with lagged impact.", fixed = TRUE)
  })

  test_that("Test whether lag lengths for the exogenous data (first differences) is given", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 1,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = NULL,

                                        confint           = 1.67,
                                        hor               = 10),
                           "You have to provide the lag lengths for the exogenous data with lagged impact of first differences.", fixed = TRUE)
  })

  test_that("Test that width of confidence bands is given", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 1,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = NULL,
                                        hor               = 10),
                           'Please specify a value for the width of the confidence bands.', fixed = TRUE)
  })

  test_that("Test that width of confidence bands is given", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 1,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = -1,
                                        hor               = 10),
                           'The width of the confidence bands has to be >=0.', fixed = TRUE)
  })

  test_that("Test that robust covariance matrix is correctly given", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = "soso",

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 1,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1,
                                        hor               = 10),
                           "The choices for robust covariance estimation are 'vcovBK', 'vcovDC', 'vcovHC', 'vcovNW', 'vcovSCC' and 'Vcxt'.
         For details, see the vignette of the plm package and Miller (2017).", fixed = TRUE)
  })


  test_that("Test that horizon integer is correctly specified", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 1,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1,
                                        hor               = -1.1),
                           'The number of horizons has to be an integer and > 0.', fixed = TRUE)
  })


  test_that("Test that gmm options are correct", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

                                        use_gmm           = TRUE,
                                        gmm_effect        = "soso",

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 1,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1,
                                        hor               = 5),
                           'The effect for gmm has to be "twoways" (default) or "individual".', fixed = TRUE)
  })

  test_that("Test that gmm options are correct", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

                                        use_gmm           = TRUE,
                                        gmm_transformation = "soso",

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 1,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1,
                                        hor               = 5),
                           'The transformation to apply to the model has to either be "d" (default)
         for the "difference GMM" model or "ld" for the "system GMM".', fixed = TRUE)
  })


  test_that("Test that gmm options are correct", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = "vcovSCC",

                                        use_gmm           = TRUE,

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 1,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1,
                                        hor               = 5),
                           'If you want to estimate a gmm model, set "robust_cov = NULL".', fixed = TRUE)
  })

  test_that("Test that gmm options are correct", {

    testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = TRUE,

                                        shock             = "x_1",
                                        diff_shock        = FALSE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = NULL,

                                        use_gmm           = TRUE,
                                        gmm_effect        = "soso",

                                        c_exog_data       = colnames(data_set)[4:6],
                                        l_exog_data       = colnames(data_set)[4:6],
                                        lags_exog_data    = 1,
                                        c_fd_exog_data    = colnames(data_set)[4:6],
                                        l_fd_exog_data    = colnames(data_set)[4:6],
                                        lags_fd_exog_data = 2,

                                        confint           = 1,
                                        hor               = 5),
                           'The effect for gmm has to be "twoways" (default) or "individual".', fixed = TRUE)
  })


# Estimate panel model
results_panel <-  suppressWarnings(
                  lp_lin_panel(data_set          = data_set,
                               data_sample       = 'Full',
                               endog_data        = "y",
                               cumul_mult        = TRUE,

                               shock             = "x_1",
                               diff_shock        = FALSE,
                               iv_reg            = FALSE,
                               instrum           = NULL,
                               panel_model       = "within",
                               panel_effect      = "individual",
                               robust_cov        = NULL,

                               c_exog_data       = colnames(data_set)[4:6],
                               l_exog_data       = colnames(data_set)[4:6],
                               lags_exog_data    = 2,
                               c_fd_exog_data    = colnames(data_set)[4:6],
                               l_fd_exog_data    = colnames(data_set)[4:6],
                               lags_fd_exog_data = 2,

                               confint           = 1.67,
                               hor               = 10))

  lp_lin_panel_results <- results_panel$reg_summaries[[1]]
  yx_data              <- results_panel$xy_data_sets[[1]]

  x_names              <- colnames(yx_data)[!colnames(yx_data) %in% c("cross_id", "date_id", "y")]

  panel_formula        <- as.formula(paste("y~", paste(x_names, collapse = "+")))


  panel_results  <- plm::plm(formula = panel_formula,
                             data     = yx_data,
                             index    = c("cross_id", "date_id"),
                             model    = "within",
                             effect   = "individual")


  test_that("Test whether output coefficients from 'lp_lin_panel' coincide
            with coefficients estimated by using the matrix prepared in 'lp-panel_lin'", {

    testthat::expect_equal(panel_results$coefficients, results_panel$reg_summaries[[1]]$coefficients[,1])



  })


  test_that("Test that model throws no error when estimating robust
            covariance matrix", {

              # All robust cov matrices except of 'vcovDC'
              cov_mat <- c('vcovBK', 'vcovHC', 'vcovNW', 'vcovSCC')

              for (ii in seq_along(cov_mat)){
              # Estimate panel model
              testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                                  data_sample       = 'Full',
                                                  endog_data        = "y",
                                                  cumul_mult        = TRUE,

                                                  shock             = "x_1",
                                                  diff_shock        = FALSE,
                                                  iv_reg            = FALSE,
                                                  instrum           = NULL,
                                                  panel_model       = "within",
                                                  panel_effect      = "individual",
                                                  robust_cov        = cov_mat[ii],

                                                  c_exog_data       = colnames(data_set)[4:6],
                                                  l_exog_data       = colnames(data_set)[4:6],
                                                  lags_exog_data    = 2,
                                                  c_fd_exog_data    = colnames(data_set)[4:6],
                                                  l_fd_exog_data    = colnames(data_set)[4:6],
                                                  lags_fd_exog_data = 2,

                                                  confint           = 1.67,
                                                  hor               = 2),
                                                  NA)
            }


              # Estimate panel model with 'vcovDC'
              testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                                  data_sample       = 'Full',
                                                  endog_data        = "y",
                                                  cumul_mult        = TRUE,

                                                  shock             = "x_1",
                                                  diff_shock        = FALSE,
                                                  iv_reg            = FALSE,
                                                  instrum           = NULL,
                                                  panel_model       = "within",
                                                  panel_effect      = "twoways",
                                                  robust_cov        = 'vcovDC',

                                                  c_exog_data       = colnames(data_set)[4:6],
                                                  l_exog_data       = colnames(data_set)[4:6],
                                                  lags_exog_data    = 2,
                                                  c_fd_exog_data    = colnames(data_set)[4:6],
                                                  l_fd_exog_data    = colnames(data_set)[4:6],
                                                  lags_fd_exog_data = 2,

                                                  confint           = 1.67,
                                                  hor               = 2),
                                     NA)


              # Test 'vcovSCC' with different type
              testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                                  data_sample       = 'Full',
                                                  endog_data        = "y",
                                                  cumul_mult        = TRUE,

                                                  shock             = "x_1",
                                                  diff_shock        = FALSE,
                                                  iv_reg            = FALSE,
                                                  instrum           = NULL,
                                                  panel_model       = "within",
                                                  panel_effect      = "individual",
                                                  robust_cov        = 'vcovSCC',

                                                  robust_type       = "HC1",

                                                  c_exog_data       = colnames(data_set)[4:6],
                                                  l_exog_data       = colnames(data_set)[4:6],
                                                  lags_exog_data    = 2,
                                                  c_fd_exog_data    = colnames(data_set)[4:6],
                                                  l_fd_exog_data    = colnames(data_set)[4:6],
                                                  lags_fd_exog_data = 2,

                                                  confint           = 1.67,
                                                  hor               = 2),
                                     NA)





              })

  test_that("Test that data is correctly computed.", {
    # Estimate panel model
    results_lin_panel <- lp_lin_panel(data_set          = data_set,
                                        data_sample       = 'Full',
                                        endog_data        = "y",
                                        cumul_mult        = FALSE,

                                        shock             = "x_1",
                                        diff_shock        = TRUE,
                                        iv_reg            = FALSE,
                                        instrum           = NULL,
                                        panel_model       = "within",
                                        panel_effect      = "individual",
                                        robust_cov        = "vcovSCC",

                                        c_exog_data       = c("x_2", "x_3", "x_4"), #colnames(data_set)[4:6],
                                        l_exog_data       = c("x_2", "x_3", "x_4"),
                                        lags_exog_data    = 2,
                                        c_fd_exog_data    = c("x_2", "x_3", "x_4"),
                                        l_fd_exog_data    = c("x_2", "x_3", "x_4"),
                                        lags_fd_exog_data = 2,

                                        confint           = 1.67,
                                        hor               = 10)


    # Save data set for h = 0
    data_output <- results_lin_panel$xy_data_sets[[1]]


    # Save contemporaneous output data, h = 0
    c_exog_data_output <-  results_lin_panel$xy_data_sets[[1]][, c("x_2", "x_3", "x_4")]

    # Save lagged exogenous data, h = 0
    lag_names_1        <- paste(c("x_2", "x_3", "x_4"), c("lag_1"), sep = "_")
    lag_names_2        <- paste(c("x_2", "x_3", "x_4"), c("lag_2"), sep = "_")
    lag_names          <- c(lag_names_1, lag_names_2)
    l_exog_data_output <- data_output[, which(names(data_output) %in% lag_names)]

    # Saved lagged data of first differences, h = 0
    dlag_names_1        <- paste(c("dx_2", "dx_3", "dx_4"), c("lag_1"), sep = "_")
    dlag_names_2        <- paste(c("dx_2", "dx_3", "dx_4"), c("lag_2"), sep = "_")
    dlag_names          <- c(dlag_names_1, dlag_names_2)
    dl_exog_data_output <- data_output[, which(names(data_output) %in% dlag_names)]


    # Compute manually contemporaneous data
    # Use lead = 3, because lag length = 2 AND lags of first differences = 2.
    c_exog_data_manual <- data_set %>%
                          dplyr::group_by(cross_section) %>%
                          dplyr::mutate_at(vars(x_2, x_3, x_4), funs(dplyr::lead(., 3))) %>%
                          dplyr::ungroup()               %>%
                          na.omit()                      %>%
                          dplyr::select(-x_1, -y, -cross_section, -time_section)

    # Compare contemporaneous data
    testthat::expect_equal(c_exog_data_manual, c_exog_data_output)


    # Function which takes first differences and sets the first value to NA
    # to be consistent with dplyr
    diff_function <- function(data){

      return(c(NA, diff(data)))

    }



    # Compute manually lagged exogenous data
    # First lag
    l_1_exog_data_manual <- data_set %>%
                          dplyr::group_by(cross_section) %>%
                          dplyr::mutate_at(vars(y), funs(diff_function(.)))  %>% # This constructs first differences
                          dplyr::mutate_at(vars(y), funs(dplyr::lag(., 2)))  %>% # This accounts for the lag = 2 of first differences
                          dplyr::mutate_at(vars(x_2, x_3, x_4), funs(dplyr::lag(., 1))) %>% # This is the first lag of exogenous data
                          dplyr::ungroup()                %>%
                          na.omit()                       %>%
                          dplyr::select(-x_1, -y, -cross_section, -time_section) %>%
                          dplyr::rename(x_2_lag_1 = x_2,
                                        x_3_lag_1 = x_3,
                                        x_4_lag_1 = x_4)

    # Second lag
    l_2_exog_data_manual <- data_set %>%
                            dplyr::group_by(cross_section) %>%
                            dplyr::mutate_at(vars(y), funs(diff_function(.)))  %>% # This constructs first differences
                            dplyr::mutate_at(vars(y), funs(dplyr::lag(., 2)))  %>% # This accounts for the lag = 2 of first differences
                            dplyr::mutate_at(vars(x_2, x_3, x_4), funs(dplyr::lag(., 2))) %>%  # This is the second lag of exogenous data
                            dplyr::ungroup()                                               %>%
                            na.omit()                                                      %>%
                            dplyr::select(-x_1, -y, -cross_section, -time_section)         %>%
                            dplyr::rename(x_2_lag_2 = x_2,
                                          x_3_lag_2 = x_3,
                                          x_4_lag_2 = x_4)


    l_exog_data_manual   <- cbind(l_1_exog_data_manual, l_2_exog_data_manual)



    # Compare contemporaneous data
    testthat::expect_equal(l_exog_data_output, l_exog_data_manual)


    # Compute lagged exogenous data of first differences
    # First lag
    dl_1_exog_data_manual <- data_set %>%
                            dplyr::group_by(cross_section) %>%
                            dplyr::mutate_at(vars(x_2, x_3, x_4), funs(diff_function(.)))  %>% # This constructs first differences
                            dplyr::mutate_at(vars(x_2, x_3, x_4), funs(dplyr::lag(., 1)))  %>% # Take first lag of first differences
                            dplyr::ungroup()                %>%
                            dplyr::select(-x_1, -y, -cross_section, -time_section) %>%
                            dplyr::rename(dx_2_lag_1 = x_2,
                                          dx_3_lag_1 = x_3,
                                          dx_4_lag_1 = x_4)

    # Second lag
    dl_2_exog_data_manual <- data_set %>%
                            dplyr::group_by(cross_section) %>%
                            dplyr::mutate_at(vars(x_2, x_3, x_4), funs(diff_function(.)))  %>% # This constructs first differences
                            dplyr::mutate_at(vars(x_2, x_3, x_4), funs(dplyr::lag(., 2)))  %>% # Take second lag of first differences
                            dplyr::ungroup()                                               %>%
                            dplyr::select(-x_1, -y, -cross_section, -time_section)         %>%
                            dplyr::rename(dx_2_lag_2 = x_2,
                                          dx_3_lag_2 = x_3,
                                          dx_4_lag_2 = x_4)


    dl_exog_data_manual   <- na.omit(cbind(dl_1_exog_data_manual, dl_2_exog_data_manual)) %>%
                             as_tibble()



    # Compare lagged exogenous data of first differences
    testthat::expect_equal(dl_exog_data_manual, dl_exog_data_output)

  })




  test_that("Test model when estimating normal multipliers", {
              # Estimate panel model
              testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                                  data_sample       = 'Full',
                                                  endog_data        = "y",
                                                  cumul_mult        = FALSE,

                                                  shock             = "x_1",
                                                  diff_shock        = TRUE,
                                                  iv_reg            = FALSE,
                                                  instrum           = NULL,
                                                  panel_model       = "within",
                                                  panel_effect      = "individual",
                                                  robust_cov        = "vcovSCC",

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


   test_that("Test gmm approach", {

     set.seed(123)

     # Simulate panel data with large cross dimension
     N  <- 140
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
              testthat::expect_error(lp_lin_panel(data_set          = data_set,
                                                  data_sample       = 'Full',
                                                  endog_data        = "y",
                                                  cumul_mult        = TRUE,

                                                  shock             = "x_1",
                                                  diff_shock        = FALSE,
                                                  iv_reg            = FALSE,
                                                  instrum           = NULL,
                                                  panel_model       = "within",
                                                  panel_effect      = "individual",
                                                  robust_cov        = NULL,

                                                  use_gmm         = TRUE,

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
