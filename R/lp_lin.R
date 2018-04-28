#' @name lp_lin
#' @title Compute (linear) impulse responses
#' @description Compute impulse responses with local projections by Jordà (2005).
#'
#' @param data_set_df A \link{data.frame}, containing all endogenous variabls for the VAR.
#' @param specs_list A list with the following inputs: \cr
#' \itemize{
#'   \item \emph{lags_criterion:}   NaN (Lag length is given), 'AICc', 'AIC' or 'BIC'.
#'   \item \emph{lags_lin:}         Number of lags for VAR (if \emph{lags_criterion} = NaN).
#'   \item \emph{max_lags:}         Maximum number of lags (if \emph{lags_criterion} = 'AICc', BICc' or 'BIC').
#'   \item \emph{trend:}            No trend =  1 , Include trend = 2, Include trend and quadratic trend = 3.
#'   \item \emph{shock_type:}       No trend =  1 , Include trend = 2, Include trend and quadratic trend = 3.
#'   \item \emph{confint:}          Width of confidence bands. 68% = 1, 90% = 1.65, 95% = 1.96.
#'   \item \emph{hor:}              Horizons for irfs.
#' }

#' @return A list with estimated impulse responses their newey west standard errors and
#' a list with specifications of the data frame for the plot function.
#' @export
#' @import foreach
#' @examples
#' # Make specification list
#' specs_list <- list()
#'
#' # Specify inputs
#' specs_list$lags_lin       <- 12
#' specs_list$lags_criterion <- NaN
#' specs_list$max_lags       <- 2
#' specs_list$trend          <- 1
#' specs_list$shock_type     <- 1
#' specs_list$confint        <- 1.96
#' specs_list$hor            <- 24
#'
#' # Estimate model and save results
#'  results_lin <- lpirfs::lp_lin(data_set_df, specs_list)
lp_lin <- function(data_set_df, specs_list){

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
  d <- reduced_var(y_lin, x_lin, data_set_df, specs)

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
  numb_cores     <- min(specs$endog, parallel::detectCores() - 1)
  cl             <- makeCluster(numb_cores)
  doSNOW::registerDoSNOW(cl)

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
       nw_results     <- lpirfs::newey_west_c(yy[, k], xx, h)
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
         val_criterion <- lpirfs::find_lag_c(y_lin, x_lin, lag_crit, h, k,
                                                 specs$max_lags)

        # Set optimal lag length
         lag_choice  <- which.min(val_criterion)

        # Extract matrices based on optimal lag length
         yy <- y_lin[[lag_choice]][, k]
         yy <- yy[h: length(yy)]

         xx <- x_lin[[lag_choice]]
         xx <- xx[1:(dim(xx)[1] - h + 1),]

        # Estimate coefficients and newey west std.err
         nw_results   <- lpirfs::newey_west_c(yy, xx, h)
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
  stopCluster(cl)

  list(irf_lin_mean, irf_lin_low, irf_lin_up, specs)

}
