#' @name lp_lin
#' @title Compute (linear) impulse responses
#' @description Compute impulse responses with local projections by Jordà (2005).
#'
#' @param data_set_df A \link{data.frame}() containing all endogenous variables for the VAR. The column order
#'                    is used for the Cholesky decomposition.
#' @param specs A \link{list}() with the following inputs:
#'
#' \itemize{
#' \item{\strong{lags_criterion} NaN or character. NaN means that the number of lags
#'         has to be given at \emph{lags_lin}. The character refers to the corresponding lag length criterion ('AICc', 'AIC' or 'BIC').}
#' \item{\strong{lags_lin} Integer. Number of lags for (linear) VAR (if \emph{lags_criterion} = NaN).}
#' \item{\strong{max_lags} Integer. Maximum number of lags (if \emph{lags_criterion} = 'AICc', 'AIC', 'BIC').}
#' \item{\strong{trend} Integer. No trend =  0 , Include trend = 1, Include trend and quadratic trend = 2.}
#' \item{\strong{shock_type} Integer. Standard deviation shock = 0, Unit shock = 1.}
#' \item{\strong{confint} Double. Width of confidence bands. 68\% = 1, 90\% = 1.65, 95\% = 1.96.}
#' \item{\strong{hor} Integer. Number of horizons for impulse responses. }
#' }
#'
#' @return A list with impulse responses and their robust confidence bands.
#' It also returns an updated list of \emph{specs} with further properties of \emph{data_set_df} for the plot function.
#'
#'\item{irf_lin_mean}{A three 3D \link{array}, containing all impulse responses for all endogenous variables.
#'                    The last dimension denotes the shock variable. The row in each matrix
#'                    denotes the respones of the \emph{ith} variable as ordered in data_set_df. The columns denote the horizon.
#'                    For example, if \emph{results_lin} contains the results, results_lin$irf_lin_mean[, , 1] returns a KXH matrix,
#'                    where K is the number of variables and H the number of horizons. '1' means that the rows are the responses to
#'                    the shock of the first variable.}
#'
#'\item{irf_lin_low}{A three 3D \link{array}, containing all lower confidence bands of the responses,
#'                    based on robust standard errors by Newey and West (1987). Properties are equal to irf_lin_mean.}
#'
#'\item{irf_lin_up}{A three 3D \link{array}, containing all upper confidence bands of the responses,
#'                    robust standard errors by Newey and West (1987). Properties are equal to irf_lin_mean.}
#'
#'\item{specs}{An updated list of \emph{specs} for the plot function.}
#'
#' @export
#' @references
#' Jordà, O. (2005) "Estimation and Inference of Impulse Responses by Local Projections."
#' \emph{American Economic Review}, 95 (1): 161-182.
#'
#'
#' Newey W.K., and West K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
#' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55, 703–708.

#' @import foreach
#' @examples
#' \dontrun{
#'# Load packages
#'   library(dplyr)
#'   library(doSNOW)
#'   library(parallel)
#'   library(Rcpp)
#'   library(lpirfs)
#'
#'
#'# Load data
#'   data_set_df <- interest_rules_var_data
#'
#'# Create list for input
#'   specs <- list()
#'
#'# Specify inputs
#'   specs$lags_lin       <- 4L
#'   specs$lags_criterion <- NaN
#'   specs$max_lags       <- 2L
#'   specs$trend          <- 0L
#'   specs$shock_type     <- 1L
#'   specs$confint        <- 1.96
#'   specs$hor            <- 12L
#'
#'# Estimate model
#'   results_lin  <- lp_lin(data_set_df, specs)
#'
#'# Make plots
#'   linear_plots <- plot_lin_irfs(results_lin)
#'
#'# Show single plots
#'   linear_plots[[1]]
#'   linear_plots[[2]]
#'
#'# Show all plots
#'   library(ggpubr)
#'   library(gridExtra)
#'
#'   lin_plots_all <- sapply(linear_plots, ggplotGrob)
#'   marrangeGrob(lin_plots_all, nrow = ncol(data_set_df), ncol = ncol(data_set_df), top=NULL)
#'
#'  }
lp_lin <- function(data_set_df, specs){

  # Check whether 'trend' is given
  if(is.null(specs$trend) == TRUE){
    stop('Please specify whether and which type of trend to include.')
  }

  # Check whether 'shock_type' is given
  if(is.null(specs$shock_type) == TRUE){
    stop('Please specify which type of shock to use.')
  }

  # Check whether 'shock_type' is given
  if(is.null(specs$confint) == TRUE){
    stop('Please specify a value for the width of the confidence bands.')
  }

  # Check whether 'shock_type' is given
  if(is.null(specs$hor) == TRUE){
    stop('Please specify the number of horizons.')
  }


  # Check whether wrong lag length criterion is given
  if(!(is.nan(specs$lags_criterion) == TRUE | specs$lags_criterion == 'AICc'|
       specs$lags_criterion == 'AIC' | specs$lags_criterion == 'BIC'    ) == TRUE){
    stop('Possible lag length criteria are AICc, AIC or BIC.')
  }


  # Check whether lags criterion and maximum number of lags is given
  if((is.character(specs$lags_criterion) == TRUE) &
      (!is.na(specs$lags_lin) == TRUE)){
     stop('You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.')
    }


  # Check whether no lag length criterion and number of lags is given
  if((is.na(specs$lags_criterion)  == TRUE) &
      (is.na(specs$lags_lin)        == TRUE)){
    stop('You have to at least provide a lag criterion (AICc, AIC or BIC) or a fixed number of lags.')
  }


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

  list(irf_lin_mean = irf_lin_mean, irf_lin_low = irf_lin_low,
       irf_lin_up   = irf_lin_up, spes = specs)

}
