context("check_lp_nl_panel")


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
                    mutate(y   = 0.3*x_1 + 0.4*x_2 + 0.5*x_3 + 0.6*x_4) %>%
                    ungroup()

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
                              hor               = 10)


lp_nl_panel_results <- results_panel$reg_summaries[[1]]
yx_data             <- results_panel$xy_data_sets[[1]]

x_names             <- colnames(yx_data)[!colnames(yx_data) %in% c("cross_id", "date_id", "y")]
panel_formula       <- as.formula(paste("y~", paste(x_names, collapse = "+")))


panel_results       <- plm::plm(formula = panel_formula,
                                 data     = yx_data,
                                 index    = c("cross_id", "date_id"),
                                 model    = "within",
                                 effect   = "individual")


test_that("Check whether output coefficients from 'lp_nl_panel_results' coincide
            with coefficients estimated by using the matrix prepared in 'lp_nl_panel'", {

              testthat::expect_equal(panel_results$coefficients, results_panel$reg_summaries[[1]]$coefficients[,1])

            })

test_that("Check that model throws no error when estimating robust
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
                                                  robust_cov        = "Vcx",

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


