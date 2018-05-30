#' @name reduced_var
#' @title Estimate structural shock matrix via Cholesky decomposition
#' @description Estimate structural shock matrix via Cholesky decomposition
#' @param y_lin A matrix with all endogenous variables
#' @param x_lin A matrix with lagged endogenous variables
#' @param data_set_df A data.frame with all endogenous variables
#' @param specs A list with specifications from \link{lp_lin} or \link{lp_nl}
#' @return Shock matrix (d)
#' @author Philipp Adämmer

reduced_var  <- function(y_lin, x_lin, data_set_df, specs){

 # Check whether lag criterion is given
 if (is.nan(specs$lags_criterion) == TRUE) {

  # Estimates reduced VAR with pre-defined lag length
    y_data        <- lapply(seq_len(ncol(y_lin)), function(i) y_lin[,i])
    resids_all    <- (lapply(y_data, lm_function, x_lin))

################################################################################
                                } else {
################################################################################

 # Estimate lag criteria with VARselect from vars package
  lag_criterion <- vars::VARselect(data_set_df, lag.max =  specs$max_lags)

  if (specs$lags_criterion == 'AICc'){

    # Estimate and save AIC values
    AIC_values  <- lag_criterion$criteria[1,]

    # Calculate number of 'exogenous' variables and add 2 (constant and variance)
    p           <- unlist(lapply(1:specs$max_lags, function(lag, K){lag*K},
                                   ncol(data_set_df))) + 2

    # Calculate number of observations for each regression
    n           <- unlist(lapply(1:specs$max_lags, function(lag, data){
                                   length(data[(lag+1):dim(data)[1], 1])},
                                   as.matrix(data_set_df[,1])))


    # Calculate AICc, see Cavanaugh (1997) <doi:10.1016/S0167-7152(96)00128-9>
    #                     Burnham et. al (2011) <doi:10.1007/s00265-010-1029-6>
      specs$lags_lin  <- which.min(AIC_values + 2*p*(p+1)/n - p - 1)


              } else if (specs$lags_criterion == 'AIC'){


      specs$lags_lin  <- which.min(lag_criterion$criteria[1,])

                       } else {

      specs$lags_lin  <- which.min(lag_criterion$criteria[3,]) }


    # Build data based on 'optimal lag length
    y_data      <- as.list(dplyr::as_tibble(y_lin[[specs$lags_lin]]))
    x_data      <- x_lin[[specs$lags_lin]]

    # Estimate OLS model and calculate residuals
    resids_all  <- lapply(y_data, lm_function, x_data)

  }

  # Make matrix of residuals
  resid_all     <- matrix(unlist(resids_all), ncol = specs$endog, byrow = F )

  # Make covariance matrix
  cov_var       <- stats::cov(resid_all)

  # Cholesky decomposition
  A             <- t(chol(cov_var))
  D             <- diag(sqrt(diag(cov_var)))

  # Shock Matrix
  d <- matrix(NaN, specs$endog, specs$endog)

      if (specs$shock_type == 0){

        for (i in 1:specs$endog){

             d[, i]     <-  A[, i]/A[i, i]*D[i, i]

        }

                } else {

        for (i in 1:specs$endog){
          d[, i]     <-  A[, i]/A[i, i]

        }
      }

  # Return shock matrix
  return(d)

}
