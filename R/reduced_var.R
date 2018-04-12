#' Function to estimate reduced VAR equation for equation and then shock matrix.
#'
#' @param y_data A data frame for the exogenous variables.
#' @param x_lin A list with exogenous variables.
#' @param y_lin A list with endogenous variables.
#' @param lags Numeric value with number of lags.
#' @return List with output from lm object.



reduced_var <- function(y_lin, x_lin, data_set_estim, specs){

  # Estimate OLS equation for equation
  if (specs$lags_criterion == 'NA') {

    lm_all   <- dplyr::as_tibble(lapply(y_lin, lm_function))

                         } else {

      lag_criterion <- vars::VARselect(data_set_estim, lag.max =  specs$max_lags)

    if (specs$lags_criterion == 'AIC'){

      specs$lags_lin  <- which.min(lag_criterion$criteria[1,])
      lm_all          <- dplyr::as_tibble(lapply(y_lin[[specs$lags_lin]],
                                                 lm_function))

                          } else {

      specs$lags_lin  <- which.min(lag_criterion$criteria[3,])
      lm_all          <- dplyr::as_tibble(lapply(y_lin[[specs$lags_lin ]],
                                                          lm_function)) }

  }


  # Rename columns
  names(lm_all) <- 1:specs$endog

  # Extract residuals
  resid_all     <- dplyr::as_tibble(lapply(lm_all[2,], unlist))

  # Make Covariance matrix
  cov_var       <- cov(resid_all)

  # Cholesky decomposition
  A             <- t(chol(cov_var))
  D             <- diag(sqrt(diag(cov_var)))

  # Shock Matrix
  d <- matrix(NaN, specs$endog, specs$endog)

  # Loop to create shocks
  for (jj in 1:specs$endog){

  # Making a unit shock
    d[, jj]                          <-  A[, jj]/A[jj, jj]*D[jj, jj]
  }

  # Return shock matrix
  d

}
