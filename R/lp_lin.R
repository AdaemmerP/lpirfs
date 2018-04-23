#' Title
#'
#' @param data_set_estim A data.frame/tibble
#' @param specs List with stuff
#'
#' @return List with mean... SaetÄTER
#' @export
#'
lp_lin <- function(data_set_estim, specs){

  # Construct data for linear model with function 'data_lin'
  data_lin <- create_lin_data(specs, data_set_estim)
  y_lin    <- data_lin[[1]]
  x_lin    <- data_lin[[2]]

  # Construct shock matrix with function reduced_var
  d        <- reduced_var(y_lin, x_lin, data_set_estim, specs)

  # Matrices to store to OLS parameters
  b1        <- matrix(NaN, specs$endog, specs$endog)
  b1_low    <- matrix(NaN, specs$endog, specs$endog)
  b1_up     <- matrix(NaN, specs$endog, specs$endog)

  # Create matrices to store irfs for each horizon
  irf_mean  <-  matrix(NaN, specs$endog, specs$hor + 1)
  irf_low   <-  irf_mean
  irf_up    <-  irf_mean

  # Arrays for all linear irfs
  irf_lin_mean  <-  array(NaN, dim = c(specs$endog, specs$hor + 1, specs$endog))
  irf_lin_low   <-  irf_lin_mean
  irf_lin_up    <-  irf_lin_mean

  ###################################################################################################
  ##################################### For fixed number of lags ####################################
  ###################################################################################################

  # Make cluster
  numb_cores     <- min(specs$endog, detectCores())
  cl             <- makeCluster(numb_cores)
  doSNOW::registerDoSNOW(cl)

  # Switch to decide whether to use manual lag lengths or based on lag lengths criterion
  if(is.nan(specs$lags_criterion) == TRUE) {

    # Loops to estimate local projections. Loop one for parallel computing
    lin_irfs <- foreach(s = 1:specs$endog) %dopar% { # Outer (multicore-) loop to account for the different shocks

      for (h in 1:(specs$hor))  {                  # Account for the horizons

        # Create data
        yy  <-   y_lin[h : dim(y_lin)[1], ]
        xx  <-   x_lin[1 : (dim(x_lin)[1] - h + 1), ]

        for (k in 1:specs$endog)   {               #   Third loop which accounts for the reactions of the endogenous variables

          lm_lin         <-   lm(yy[, k] ~ xx)
          NW_cov         <-   sandwich::NeweyWest(lm_lin, prewhite = FALSE)
          std_err        <-   sqrt(diag(NW_cov))*specs$confint

          b              <-   lm_lin$coefficients

          b1[k, ]        <-   b[2:(specs$endog + 1)]
          b1_low[k, ]    <-   b[2:(specs$endog + 1)] - std_err[2:(specs$endog + 1)]
          b1_up[k, ]     <-   b[2:(specs$endog + 1)] + std_err[2:(specs$endog + 1)]

        }

        # irf_lin_mean[, hh + 1, ss]
        irf_mean[, h + 1] <- t(b1     %*% d[ , s])
        irf_low[,  h + 1] <- t(b1_low %*% d[ , s])
        irf_up[,   h + 1] <- t(b1_up  %*% d[ , s])

      }

      # Return irfs
      list(irf_mean,  irf_low,  irf_up)

    }

    # Fill arrays with irfs
    for(i in 1:specs$endog){

      # Fill irfs
      irf_lin_mean[, , i]   <- as.matrix(do.call(rbind, lin_irfs[[i]][1]))
      irf_lin_low[, ,  i]   <- as.matrix(do.call(rbind, lin_irfs[[i]][2]))
      irf_lin_up[, ,   i]   <- as.matrix(do.call(rbind, lin_irfs[[i]][3]))

      # First value of horizon is merely the shock
      irf_lin_mean[, 1, i]   <- t(d[, i])
      irf_lin_low[,  1, i]   <- irf_lin_mean[, 1, i]
      irf_lin_up[,   1, i]   <- irf_lin_mean[, 1, i]

    }

    ##################################################################################################
                                    } else {
    ###################################################################################################

    lag_crit_val <- matrix(NaN, 1, specs$max_lags)

    # --- Loops to estimate local projections.
    lin_irfs     <- foreach(s = 1:specs$endog,
                            .export = c('find_optim_lags', 'newey_west'),
                            .noexport = c('')) %dopar% {

                              for (h in 1:specs$hor)  { # Accounts for the horizon

                                for (k in 1:specs$endog)   { # Accounts for endogenous reactions

                                  #  Loop for optimal lag length
                                  for(i in 1:specs$max_lags){    # Loop to estimate optimal lag length

                                    yy <- y_lin[[i]][, k]
                                    yy <- yy[h : length(yy)]

                                    xx <- x_lin[[i]]
                                    xx <- xx[1:(dim(xx)[1] - h + 1),]

                                    lag_crit_val[1, i]   <- find_optim_lags(yy, xx, specs)

                                  }

                                  # Set optimal lag length
                                  lag_choice <- which.min(lag_crit_val)

                                  # Extract matrices accordingly
                                  yy         <- y_lin[[lag_choice]][, k]
                                  yy         <- yy[h: length(yy)]
                                  xx         <- x_lin[[lag_choice]]
                                  xx         <- xx[1:(dim(xx)[1] - h + 1),]

                                  # Estimate coefficients and newey west std.err
                                  nw_results       <- newey_west(yy, xx)
                                  b                <- nw_results$coefs
                                  std_err          <- nw_results$nwerr

                                  #       lm_single        <- lm(yy ~ xx)
                                  #       NW_cov           <-   sandwich::NeweyWest(lm_single, prewhite = FALSE)
                                  #       std_err          <-   sqrt(diag(NW_cov))*specs$confint
                                  #       b                <-   lm_single$coefficients

                                  b1[k, ]          <-   b[2:(specs$endog + 1)]
                                  b1_low[k, ]      <-   b[2:(specs$endog + 1)] - std_err[2:(specs$endog + 1)]
                                  b1_up[k, ]       <-   b[2:(specs$endog + 1)] + std_err[2:(specs$endog + 1)]

                                  # cat(h)
                                }

                                # irf_lin_mean[, hh + 1, ss]
                                irf_mean[, h + 1] <- t(b1     %*% d[ , s])
                                irf_low[,  h + 1] <- t(b1_low %*% d[ , s])
                                irf_up[,   h + 1] <- t(b1_up  %*% d[ , s])

                              }

                              # Return irfs
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
  stopCluster(cl)
  #
   list(irf_lin_mean, irf_lin_low, irf_lin_up)

}
