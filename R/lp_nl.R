#' @name lp_nl
#' @title Compute (non-linear) impulse responses
#' @description Compute non-linear impulse responses with local projections by Jordà (2005).
#'
#' @param data_set_df A \link{data.frame}() containing all endogenous variabls for the VAR. The column order
#'                    is used for the Cholesky decomposition.
#' @param specs A \link{list}() with the following inputs:
#'
#' \itemize{
#' \item{\strong{lags_criterion} NaN or character ('AICc', 'AIC' or 'BIC'). NaN means that the number of lags will be given. The
#'       character refers to the corresponding lag length criterion.}
#' \item{\strong{lags_lin} Integer. Number of lags for (linear) VAR (if lags_criterion = NaN).}
#' \item{\strong{max_lags} Integer. Maximum number of lags (if lags_criterion = 'AICc'|'AIC'|'BIC').}
#' \item{\strong{trend} Integer. No trend =  0 , Include trend = 1, Include trend and quadratic trend = 2.}
#' \item{\strong{shock_type} Integer. Standard deviation shock = 0, Unit shock = 1.}
#' \item{\strong{confint} Double. Width of confidence bands. 68\% = 1, 90\% = 1.65, 95\% = 1.96.}
#' \item{\strong{hor} Integer. Horizons for irfs. Wieso das denn?}
#' \item{\strong{switching} Vector. A vector with the same row dimension as 'data_set_df'. This series is either
#'               decomposed by the Hodrick-Prescott filter or directly included into the the switching function
#'               \deqn{(F_{z_t}) = \frac{exp(-\gamma z_t)}{1 + exp(-\gamma z_t)}}    }
#' }
#'
#' @return A list with impulse responses and their robust confidence bands.
#' It also returns an updated list with further properties of 'data_set_df' for the plot function.
#'
#'\item{irf_lin_mean:}{A three 3D \link{array}, containing all impulse responses for all endogenous variables.
#'                    The last dimension denotes the shock variable. The row in each matrix
#'                    denotes the respones of the \emph{ith} variable as ordered in data_set_df. The columns denote the horizon.
#'                    For example, if \emph{results_lin} contains the results, results_lin$irf_lin_mean[, , 1] returns a KXH matrix,
#'                    where K is the number of variables and H the number of horizons. '1' means that the rows are the responses to
#'                    the shock of the first variable.}
#'
#'\item{irf_lin_low:}{A three 3D \link{array}, containing all lower confidence bands of the responses,
#'                    based on robust standard errors by Newey and West (1987). Properties are equal to irf_lin_mean.}
#'
#'\item{irf_lin_up:}{A three 3D \link{array}, containing all upper confidence bands of the responses,
#'                    robust standard errors by Newey and West (1987). Properties are equal to irf_lin_mean.}
#'
#'\item{specs:}{An updated list of \emph{specs} with updated entries for the plot function.}
#'
#'
#'
#' @export
#' @references
#' Jordà, O. (2005) "Estimation and Inference of Impulse Responses by Local Projections."
#' \emph{American Economic Review}, 95 (1): 161-182.
#'
#' Newey W.K., West K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
#' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55, 703–708.

#' @import foreach
#' @examples
#' # Create list for input
#'   specs <- list()
#'
#' # Specify inputs
#'   specs$lags_lin       <- 12L
#'   specs$lags_criterion <- NaN
#'   specs$max_lags       <- 2L
#'   specs$trend          <- 1L
#'   specs$shock_type     <- 1L
#'   specs$confint        <- 1.96
#'   specs$hor            <- 24L
#'
#' # Estimate model and save results
#'  results_lin <- lpirfs::lp_lin(data_set_df, specs)
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
