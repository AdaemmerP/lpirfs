#' lp_nl
#'
#' @param data_set_estim A data.frame/tibble
#' @param specs A list with specifications that go into 'lin_lp' or 'nl_lp' function.
#'         specs$lags_criterion: Either NaN (given lag length) or 'AICc'|BICc'|'BIC'
#'         specs$lags_lin:       Lag length
#'         specs$trend:          1 (no trend), 2 (trend), 3 (quadratic trend)
#'         specs$max_lags:       Maximum number of lags to use for lag length criteria
#'         specs$lamdba:    Value of lambda for HP filter
#'         specs$hp_filter: 1 = Use HP-filter to extract standardized time series
#'                          0 = Use pre-defined time series for transition function
#'
#' @return A list with irfs from local projections and newey west standard errors
#' @export
#' @import foreach
#'
lp_nl <- function(data_set_df, specs){

  # Construct data for non-linear model
  data_nl <- create_nl_data(specs, data_set_df)
  y_nl <- data_nl[[1]]
  x_nl <- data_nl[[2]]

  # Construct data for linear model for reduced shocks
  data_lin               <- create_lin_data(specs, data_set_df)
  y_lin                  <- data_lin[[1]]
  x_lin                  <- data_lin[[2]]

  # Construct shock matrix
  d <- reduced_var(y_lin, x_lin, data_set_df, specs)

  # Matrices to store irfs for each horizon
  irf_temp_s1_mean  <-  matrix(NaN, specs$endog, specs$hor + 1)
  irf_temp_s1_low   <-  irf_temp_s1_mean
  irf_temp_s1_up    <-  irf_temp_s1_mean

  irf_temp_s2_mean  <-  matrix(NaN, specs$endog, specs$hor + 1)
  irf_temp_s2_low   <-  irf_temp_s2_mean
  irf_temp_s2_up    <-  irf_temp_s2_mean

  # Arrays to store irfs
  irf_s1_mean  <-  array(NaN, dim = c(specs$endog, specs$hor + 1, specs$endog))
  irf_s1_low   <-  irf_s1_mean
  irf_s1_up    <-  irf_s1_mean

  irf_s2_mean  <-  array(NaN, dim = c(specs$endog, specs$hor + 1, specs$endog))
  irf_s2_low   <-  irf_s2_mean
  irf_s2_up    <-  irf_s2_mean

  # Matrices to store OLS parameters for regime 1 & 2
  b1_s1      <- matrix(NaN, specs$endog, specs$endog)
  b1_low_s1  <- matrix(NaN, specs$endog, specs$endog)
  b1_up_s1   <- matrix(NaN, specs$endog, specs$endog)

  b1_s2      <- matrix(NaN, specs$endog, specs$endog)
  b1_low_s2  <- matrix(NaN, specs$endog, specs$endog)
  b1_up_s2   <- matrix(NaN, specs$endog, specs$endog)

  # Define coefficient position to extract regime_1 and regime_2 parameters in loop
  start_nl_s1   <- 2
  end_nl_s1     <- specs$endog + 1
  samp_nl_s1    <- start_nl_s1:end_nl_s1

  start_nl_s2   <- 2 + specs$endog*specs$lags_nl
  end_nl_s2     <- start_nl_s2 + specs$endog - 1
  samp_nl_s2    <- start_nl_s2:end_nl_s2

  # Make cluster
  numb_cores     <- min(specs$endog, parallel::detectCores() - 1)
  cl             <- makeCluster(numb_cores)
  doSNOW::registerDoSNOW(cl)

 # Determine whether manual lag lengths are given or have to be determined
  if(is.nan(specs$lags_criterion) == TRUE) {

 # Loops to estimate local projections
  nl_irfs <- foreach( s        = 1:specs$endog,
                     .packages = 'lpirfs') %dopar%{ # Accounts for the shocks

        for (h in 1:specs$hor){   # Accounts for the horizons

         yy  <-   y_nl[h:dim(y_nl)[1], ]
         xx  <-   x_nl[1:(dim(x_nl)[1] - h + 1), ]

         for (k in 1:specs$endog){ # Accounts for the reactions of the endogenous variables

           # Estimate coefficients and newey west std.err
           nw_results       <- lpirfs::newey_west_c(yy[, k], xx, h)
           b                <- nw_results[[1]]
           std_err          <- sqrt(diag(nw_results[[2]]))*specs$confint

           # Extract coefficients
           b1_s1[k, ]         <-   b[samp_nl_s1]
           b1_low_s1[k, ]     <-   b[samp_nl_s1] - std_err[samp_nl_s1]
           b1_up_s1[k, ]      <-   b[samp_nl_s1] + std_err[samp_nl_s1]

           b1_s2[k, ]         <-   b[samp_nl_s2]
           b1_low_s2[k, ]     <-   b[samp_nl_s2] - std_err[samp_nl_s2]
           b1_up_s2[k, ]      <-   b[samp_nl_s2] + std_err[samp_nl_s2]
          }

          # Estimate local projections
           irf_temp_s1_mean[, h + 1] <- t(b1_s1        %*%  d[ , s])
           irf_temp_s1_low[,  h + 1] <- t(b1_low_s1    %*%  d[ , s])
           irf_temp_s1_up[,   h + 1] <- t(b1_up_s1     %*%  d[ , s])

           irf_temp_s2_mean[, h + 1] <- t(b1_s2        %*%  d[ , s])
           irf_temp_s2_low[,  h + 1] <- t(b1_low_s2    %*%  d[ , s])
           irf_temp_s2_up[,   h + 1] <- t(b1_up_s2     %*%  d[ , s])
   }

    list(irf_temp_s1_mean, irf_temp_s1_low, irf_temp_s1_up,
         irf_temp_s2_mean, irf_temp_s2_low, irf_temp_s2_up)

}

 # Fill arrays with irfs
 for(i in 1:specs$endog){

   # Fill irfs
   irf_s1_mean[, , i] <- as.matrix(do.call(rbind, nl_irfs[[i]][1]))
   irf_s1_low[, ,  i] <- as.matrix(do.call(rbind, nl_irfs[[i]][2]))
   irf_s1_up[, ,   i] <- as.matrix(do.call(rbind, nl_irfs[[i]][3]))

   irf_s2_mean[, , i] <- as.matrix(do.call(rbind, nl_irfs[[i]][4]))
   irf_s2_low[,  , i] <- as.matrix(do.call(rbind, nl_irfs[[i]][5]))
   irf_s2_up[,   , i] <- as.matrix(do.call(rbind, nl_irfs[[i]][6]))

   # First value of is merely the shock
   irf_s1_mean[, 1, i]   <- t(d[, i])
   irf_s1_low[,  1, i]   <- irf_s1_mean[, 1, i]
   irf_s1_up[,   1, i]   <- irf_s1_mean[, 1, i]

   irf_s2_mean[, 1, i]   <- t(d[, i])
   irf_s2_low[,  1, i]   <- irf_s1_mean[, 1, i]
   irf_s2_up[,   1, i]   <- irf_s1_mean[, 1, i]
}

################################################################################
                             } else {
################################################################################

 # Convert lag length criterion to number for Rcpp loop
  lag_crit     <- switch(specs$lags_criterion,
                           'AICc'= 1,
                           'AIC' = 2,
                           'BIC' = 3)

 # Set starting values for parameters of regime 1
  start_nl_s1      <- 2
  end_nl_s1        <- specs$endog + 1
  samp_nl_s1       <- start_nl_s1:end_nl_s1

 # --- Loops to estimate local projections.
  nl_irfs <- foreach(s         = 1:specs$endog,
                     .packages = 'lpirfs') %dopar% { # Accounts for shocks

         for (h in 1:specs$hor){      # Accounts for the horizons
            for (k in 1:specs$endog){ # Accounts for the reactions of the endogenous variables

             # Find optimal lag length and select matrices from lists accordingly
              val_criterion   <- lpirfs::find_lag_c(y_lin, x_lin, lag_crit, h, k,
                                                        specs$max_lags)
              lag_choice      <- which.min(val_criterion)

              yy              <- y_nl[[lag_choice]][, k]
              yy              <- yy[h: length(yy)]

              xx              <- x_nl[[lag_choice]]
              xx              <- xx[1:(dim(xx)[1] - h + 1),]

             # Estimate parameters and newey west standard errors
              nw_results      <- lpirfs::newey_west_c(yy, xx, h)
              b               <- nw_results[[1]]
              std_err         <- sqrt(diag(nw_results[[2]]))

             # Set start and values of parameters for regime 2
              start_nl_s2     <- 2 + specs$endog*lag_choice
              end_nl_s2       <- start_nl_s2 + specs$endog - 1
              samp_nl_s2      <- start_nl_s2:end_nl_s2

             # Fill paramater matrices
              b1_s1[k, ]      <-   b[samp_nl_s1]
              b1_low_s1[k, ]  <-   b[samp_nl_s1] - std_err[samp_nl_s1]
              b1_up_s1[k, ]   <-   b[samp_nl_s1] + std_err[samp_nl_s1]

              b1_s2[k, ]      <-   b[samp_nl_s2]
              b1_low_s2[k, ]  <-   b[samp_nl_s2] - std_err[samp_nl_s2]
              b1_up_s2[k, ]   <-   b[samp_nl_s2] + std_err[samp_nl_s2]
              }

             # Estimate local projections
              irf_temp_s1_mean[, h + 1] <- t(b1_s1        %*%  d[ , s])
              irf_temp_s1_low[,  h + 1] <- t(b1_low_s1    %*%  d[ , s])
              irf_temp_s1_up[,   h + 1] <- t(b1_up_s1     %*%  d[ , s])

              irf_temp_s2_mean[, h + 1] <- t(b1_s2        %*%  d[ , s])
              irf_temp_s2_low[,  h + 1] <- t(b1_low_s2    %*%  d[ , s])
              irf_temp_s2_up[,   h + 1] <- t(b1_up_s2     %*%  d[ , s])
             }

          list(irf_temp_s1_mean, irf_temp_s1_low, irf_temp_s1_up,
               irf_temp_s2_mean, irf_temp_s2_low, irf_temp_s2_up)
        }

# Fill arrays with local projection irfs
  for(i in 1:specs$endog){

  # Fill irfs
   irf_s1_mean[, , i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][1]))
   irf_s1_low[,  , i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][2]))
   irf_s1_up[,  ,  i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][3]))

   irf_s2_mean[, , i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][4]))
   irf_s2_low[,  , i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][5]))
   irf_s2_up[, ,   i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][6]))

  # First value of horizon is merely the shock
   irf_s1_mean[, 1, i]   <- t(d[, i])
   irf_s1_low[,  1, i]   <- irf_s1_mean[, 1, i]
   irf_s1_up[,   1, i]   <- irf_s1_mean[, 1, i]

   irf_s2_mean[, 1, i]   <- t(d[, i])
   irf_s2_low[,  1, i]   <- irf_s2_mean[, 1, i]
   irf_s2_up[,   1, i]   <- irf_s2_mean[, 1, i]

    }

 }

# Close cluster
  stopCluster(cl)

  list(irf_s1_mean = irf_s1_mean, irf_s1_low = irf_s1_low, irf_s1_up = irf_s1_up,
       irf_s2_mean = irf_s2_mean, irf_s2_low = irf_s2_low, irf_s2_up = irf_s2_up)

}
