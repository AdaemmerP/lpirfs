context("check_input_lp_nl")

# Load data
  data_set_df <- interest_rules_var_data

# # # Create list for input
#   specs <- list()
#
# Specify inputs
  lags_lin       <- NaN
  lags_criterion <- 'AIC'
  max_lags       <- 2L
  trend          <- 0L
  shock_type     <- 1L
  confint        <- 1.96
  hor            <- 12L



test_that("Check whether data is a data.frame", {
    data_set_df   <- as.matrix(data_set_df)
    expect_error(lp_lin(data_set_df, lags_lin   = lags_lin, lags_criterion = lags_criterion,
                                     max_lags   = max_lags, trend          = trend,
                                     shock_type = shock_type, confint      = confint, hor = hor),
                 'The data has to be a data.frame().', fixed = TRUE)
})

test_that("Check whether trend is given", {
    trend   <- NULL
    expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                                      max_lags   = max_lags, trend          = trend,
                                      shock_type = shock_type, confint      = confint, hor = hor),
                 'Please specify whether and which type of trend to include.', fixed = TRUE)
  })


test_that("Check whether shock_type is given", {
    shock_type   <- NULL
    expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                                      max_lags   = max_lags, trend          = trend,
                                      shock_type = shock_type, confint      = confint,
                                      hor        = hor),
                 'Please specify which type of shock to use.', fixed = TRUE)
  })


test_that("Check whether 'confint' is given", {
  confint        <- NULL
  expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                                    max_lags   = max_lags, trend          = trend,
                                    shock_type = shock_type, confint      = confint,
                                    hor        = hor),
               'Please specify a value for the width of the confidence bands.', fixed = TRUE)
})


test_that("Check whether number of horizons is given", {
  hor          <- NULL
  expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                                    max_lags   = max_lags, trend          = trend,
                                    shock_type = shock_type, confint      = confint,
                                    hor        = hor),
               'Please specify the number of horizons.', fixed = TRUE)
})


test_that("Check whether wrong lag length is given", {
  lags_criterion <- 'AICCd'
  expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                                    max_lags   = max_lags, trend          = trend,
                                    shock_type = shock_type, confint      = confint,
                                    hor        = hor),
               'Possible lag length criteria are AICc, AIC or BIC or NaN if lag length is specified.', fixed = TRUE)
} )

test_that("Check whether lag criterion AND fixed number of lags are given", {
  lags_lin <- 1
  expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                                    max_lags   = max_lags, trend          = trend,
                                    shock_type = shock_type, confint      = confint,
                                    hor        = hor),
               'You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.', fixed = TRUE)
} )

test_that("Check whether lag criterion AND maximum number of lags are given", {
  max_lags <- NaN
  expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                                    max_lags   = max_lags, trend          = trend,
                                    shock_type = shock_type, confint      = confint,
                                    hor        = hor),
               'Please provide a maximum number of lags for the lag length criterion.', fixed = TRUE)
} )

test_that("Check whether values for horizons are correct", {
  hor <- -1
  expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                                    max_lags   = max_lags, trend          = trend,
                                    shock_type = shock_type, confint      = confint,
                                    hor        = hor),
               'The number of horizons has to be an integer and > 0.', fixed = TRUE)
} )

test_that("Check whether lags are integers", {
  lags_lin <- 1.5
  lags_criterion <- NaN
  expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                                    max_lags   = max_lags, trend          = trend,
                                    shock_type = shock_type, confint      = confint,
                                    hor        = hor),
               'The numbers of lags have to be a positive integer.', fixed = TRUE)
} )

test_that("Check whether trend is correctly specified", {
  trend <- 12
  expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                                    max_lags   = max_lags, trend          = trend,
                                    shock_type = shock_type, confint      = confint,
                                    hor        = hor),
               'For trend please enter 0 = no trend, 1 = trend, 2 = trend and quadratic trend.', fixed = TRUE)
} )

test_that("Check shock type is correctly specified", {
  shock_type <- 12
  expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                                    max_lags   = max_lags, trend          = trend,
                                    shock_type = shock_type, confint      = confint,
                                    hor        = hor),
               'The shock_type has to be 0 = standard deviation shock or 1 = unit shock.', fixed = TRUE)
} )

test_that("Check whether width of confidence bands is correctly specified", {
  confint <- -1
  expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                                    max_lags   = max_lags, trend          = trend,
                                    shock_type = shock_type, confint      = confint,
                                    hor        = hor),
               'The width of the confidence bands has to be >=0.', fixed = TRUE)
} )

test_that("Check whether maximum lag length is given when no criterion is given", {
  lags_lin       <- 3
  lags_criterion <- NaN
  max_lags       <- 3
  expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                      max_lags   = max_lags, trend          = trend,
                      shock_type = shock_type, confint      = confint,
                      hor        = hor),
               'The maximum number of lags is only used if you provide a lag length criterion.', fixed = TRUE)
} )


test_that("Check whether no lag length criterion and number of lags are given", {
  lags_lin       <- NaN
  lags_criterion <- NaN
  expect_error(lp_lin(data_set_df, lags_lin    = lags_lin, lags_criterion = lags_criterion,
                      max_lags   = max_lags, trend          = trend,
                      shock_type = shock_type, confint      = confint,
                      hor        = hor),
               'You have to at least provide a lag criterion (AICc, AIC or BIC) or a fixed number of lags.', fixed = TRUE)
} )




# --- Check whether results from lp_lin are in region of results from Jordà (2005)
# Load data set
  data_set_df <- interest_rules_var_data

# Make list for inputs
  specs <- list()

# Specify inputs
  specs$lags_lin       <- 4L      # Number of lags
  specs$lags_criterion <- NaN     # Lag length criterion (AICc, AIC or BIC)
  specs$max_lags       <- NaN     # If lags_criterion is chosen, set maximum number of lags
  specs$trend          <- 0L      # 0 = no trend, 1 = trend, 2 = trend and quadratic trend
  specs$shock_type     <- 0L      # 0 = standard deviation shock, 1 = unit shock
  specs$confint        <- 1.96    # Width of confidence bands: 1 = 68%, 1.67 = 90%, 1.96 = 95%
  specs$hor            <- 24L     # Length of horizon




test_that("Check whether results from lp_lin are in region of results from Jordà (2005)", {
  # Estimate model
  # Safe data frame specifications in 'specs for functions
  specs$starts         <- 1                        # Sample Start
  specs$ends           <- dim(data_set_df)[1]      # Sample end
  specs$columns        <- names(data_set_df)       # Name endogenous variables
  specs$endog          <- ncol(data_set_df)        # Set the number of endogenous variables

  # Construct (lagged) data
  data_lin <- create_lin_data(specs, data_set_df)
  y_lin    <- data_lin[[1]]
  x_lin    <- data_lin[[2]]

  # Construct shock matrix
  d <- get_mat_chol(y_lin, x_lin, data_set_df, specs)

  # Matrices to store OLS parameters
  b1            <- matrix(NaN, specs$endog, specs$endog)
  b1_low        <- matrix(NaN, specs$endog, specs$endog)
  b1_up         <- matrix(NaN, specs$endog, specs$endog)

  # Matrices to store irfs for each horizon
  irf_mean  <-  matrix(NaN, specs$endog, specs$hor + 1)
  irf_low   <-  irf_mean
  irf_up    <-  irf_mean

  # 3D Arrays for all irfs
  irf_lin_mean  <-  array(NaN, dim = c(specs$endog, specs$hor + 1, specs$endog))
  irf_lin_low   <-  irf_lin_mean
  irf_lin_up    <-  irf_lin_mean

  # Make cluster
  numb_cores     <- 1 #min(specs$endog, parallel::detectCores() - 1)
  cl             <- parallel::makeCluster(numb_cores)
  doParallel::registerDoParallel(cl)

  # Decide whether lag lengths are given or have to be estimated
  if(is.nan(specs$lags_criterion) == TRUE){

    # Loops to estimate local projections
    lin_irfs <- foreach(s         = 1:specs$endog,
                        .packages = 'lpirfs')  %dopar%{ # Accounts for the shocks

                          for (h in 1:(specs$hor)){   # Accounts for the horizons
                            I
                            # Create data
                            yy  <-   y_lin[h : dim(y_lin)[1], ]
                            xx  <-   x_lin[1 : (dim(x_lin)[1] - h + 1), ]

                            for (k in 1:specs$endog){ # Accounts for the reactions of the endogenous variables

                              # Estimate coefficients and newey west std.err
                              nw_results     <- lpirfs::newey_west(yy[, k], xx, h)
                              b              <- nw_results[[1]]
                              std_err        <- sqrt(diag(nw_results[[2]]))*specs$confint

                              # Fill coefficient matrix
                              b1[k, ]        <-   b[2:(specs$endog + 1)]
                              b1_low[k, ]    <-   b[2:(specs$endog + 1)] - std_err[2:(specs$endog + 1)]
                              b1_up[k, ]     <-   b[2:(specs$endog + 1)] + std_err[2:(specs$endog + 1)]
                            }

                            # Fill matrices with local projections
                            irf_mean[, h + 1] <- t(b1     %*% d[ , s])
                            irf_low[,  h + 1] <- t(b1_low %*% d[ , s])
                            irf_up[,   h + 1] <- t(b1_up  %*% d[ , s])
                          }

                          # Return irfs
                          return(list(irf_mean,  irf_low,  irf_up))
                        }

    # Fill arrays with irfs
    for(i in 1:specs$endog){

      # Fill irfs
      irf_lin_mean[, , i]   <- as.matrix(do.call(rbind, lin_irfs[[i]][1]))
      irf_lin_low[, ,  i]   <- as.matrix(do.call(rbind, lin_irfs[[i]][2]))
      irf_lin_up[, ,   i]   <- as.matrix(do.call(rbind, lin_irfs[[i]][3]))

      # First value of is merely the shock
      irf_lin_mean[, 1, i]   <- t(d[, i])
      irf_lin_low[,  1, i]   <- irf_lin_mean[, 1, i]
      irf_lin_up[,   1, i]   <- irf_lin_mean[, 1, i]

    }

    ################################################################################
  } else {
    ################################################################################

    # Convert chosen lag criterion to number for loop
    lag_crit     <- switch(specs$lags_criterion,
                           'AICc'= 1,
                           'AIC' = 2,
                           'BIC' = 3)

    # Loops to estimate local projections.
    lin_irfs <- foreach(s          = 1:specs$endog,
                        .packages   = 'lpirfs')  %dopar% {

                          for (h in 1:specs$hor){     # Accounts for the horizon

                            for (k in 1:specs$endog){ # Accounts for endogenous reactions

                              # Find optimal lags
                              val_criterion <- lpirfs::get_vals_lagcrit(y_lin, x_lin, lag_crit, h, k,
                                                                  specs$max_lags)

                              # Set optimal lag length
                              lag_choice  <- which.min(val_criterion)

                              # Extract matrices based on optimal lag length
                              yy <- y_lin[[lag_choice]][, k]
                              yy <- yy[h: length(yy)]

                              xx <- x_lin[[lag_choice]]
                              xx <- xx[1:(dim(xx)[1] - h + 1),]

                              # Estimate coefficients and newey west std.err
                              nw_results   <- lpirfs::newey_west(yy, xx, h)
                              b            <- nw_results[[1]]
                              std_err      <- sqrt(diag(nw_results[[2]]))*specs$confint

                              # Fill coefficient matrix
                              b1[k, ]      <-   b[2:(specs$endog + 1)]
                              b1_low[k, ]  <-   b[2:(specs$endog + 1)] - std_err[2:(specs$endog + 1)]
                              b1_up[k, ]   <-   b[2:(specs$endog + 1)] + std_err[2:(specs$endog + 1)]
                            }

                            # Fill matrices with local projections
                            irf_mean[, h + 1] <- t(b1     %*% d[ , s])
                            irf_low[,  h + 1] <- t(b1_low %*% d[ , s])
                            irf_up[,   h + 1] <- t(b1_up  %*% d[ , s])
                          }

                          list(irf_mean,  irf_low,  irf_up)
                        }

    # Fill arrays with irfs
    for(i in 1:specs$endog){

      # Fill irfs
      irf_lin_mean[, , i] <- as.matrix(do.call(rbind, lin_irfs[[i]][1]))
      irf_lin_low[, ,  i] <- as.matrix(do.call(rbind, lin_irfs[[i]][2]))
      irf_lin_up[, ,   i] <- as.matrix(do.call(rbind, lin_irfs[[i]][3]))

      # First value of horizon is merely the shock
      irf_lin_mean[, 1, i]   <- t(d[, i])
      irf_lin_low[,  1, i]   <- irf_lin_mean[, 1, i]
      irf_lin_up[,   1, i]   <- irf_lin_mean[, 1, i]

    }


    ###################################################################################################

  }

  # Close cluster
  parallel::stopCluster(cl)

  results_lin <- list(irf_lin_mean = irf_lin_mean, irf_lin_low = irf_lin_low,
       irf_lin_up   = irf_lin_up, spes = specs)


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



