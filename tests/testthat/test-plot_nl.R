context("plot_nl")

# Endogenous data
  endog_data     <- interest_rules_var_data

# Specifications for switching variable
  switching      <- endog_data$GDP_gap


test_that("Test whether list length is consistent number of endogenous variables
             for lp_nl model",{


    results_nl <- lp_nl(endog_data,
                        lags_endog_lin  = 4L,
                        lags_endog_nl   = 3L,
                        lags_criterion  = NaN,
                        max_lags        = NaN,
                        trend           = 0L,
                        shock_type      = 1L,
                        confint         = 1.96,
                        hor             = 24L,
                        switching       = switching,
                        use_hp          = TRUE,
                        lambda          = 1600,
                        gamma           = 3,
                        num_cores       = 1)

    plots_nl       <- plot_nl(results_nl)
    num_plots_iv   <- ncol(endog_data)*ncol(endog_data)*2


    expect_equal(length(plots_nl[[1]]) + length(plots_nl[[2]]), num_plots_iv)


})


test_that("Test whether list length is consistent number of endogenous variables
             for lp_nl_iv model",{

               # Load and prepare data
               ag_data           <- ag_data
               sample_start      <- 7
               sample_end        <- dim(ag_data)[1]
               endog_data        <- ag_data[sample_start:sample_end, 3:5]


               shock             <- ag_data[sample_start:sample_end, 7]

               exog_data         <- ag_data[sample_start:sample_end, 6]

               switching_variable <- ag_data$GDP_MA[sample_start:sample_end] - 0.8


    results_nl_iv <- lp_nl_iv(endog_data,
                                         lags_endog_nl     = 3,
                                         shock             = shock,
                                         exog_data         = exog_data,
                                         lags_exog         = 4,
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
                                         num_cores         = 1)

    plots_nl_iv       <- plot_nl(results_nl_iv)
    num_plots_iv      <- ncol(endog_data)*2


    expect_equal(length(plots_nl_iv[[1]]) + length(plots_nl_iv[[2]]), num_plots_iv)


})
