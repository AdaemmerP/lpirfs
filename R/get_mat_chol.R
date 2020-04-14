#' @name get_mat_chol
#' @title Compute structural shock matrix via Cholesky decomposition
#' @description Compute structural shock matrix via Cholesky decomposition with input variables
#' created in \link{lp_lin} or \link{lp_nl}.
#' @param y_lin A matrix with all endogenous variables.
#' @param x_lin A matrix with lagged endogenous variables.
#' @param endog_data A \link{data.frame} with all endogenous variables.
#' @param specs A list with specifications from \link{lp_lin} or \link{lp_nl}.
#' @return Shock matrix (d)
#' @keywords internal
#' @author Philipp Adämmer

get_mat_chol  <- function(y_lin, x_lin, endog_data, specs){

 # Check whether lag criterion is given
 if (is.nan(specs$lags_criterion) == TRUE) {

  # Estimates reduced VAR with pre-defined lag length
    y_data        <- lapply(seq_len(ncol(y_lin)), function(i) y_lin[,i])
    resids_all    <- (lapply(y_data, get_resids_ols, x_lin))

################################################################################
                                } else {
################################################################################

 # Estimate lag criteria
  lag_criterion <- get_var_lagcrit(endog_data, specs = specs)

  if (specs$lags_criterion == 'AICc'){

      specs$lags_endog_lin  <- lag_criterion$order_vals[1]


              } else if (specs$lags_criterion == 'AIC'){


      specs$lags_endog_lin  <- lag_criterion$order_vals[2]

                       } else {

      specs$lags_endog_lin  <- lag_criterion$order_vals[3]  }


    # Build data based on 'optimal lag length
    y_data      <- as.list(as.data.frame(y_lin[[specs$lags_endog_lin]]))
    x_data      <- x_lin[[specs$lags_endog_lin]]

    # Estimate OLS model and calculate residuals
    resids_all  <- lapply(y_data, get_resids_ols, x_data)

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
