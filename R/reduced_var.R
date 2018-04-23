#' Function to estimate structural shock matrix via Cholesky decomposition.
#'
#' @param y_lin A data frame for the exogenous variables.
#' @param x_lin A list with exogenous variables.
#' @param data_set_estim Data with all endogenous variables.
#' @param specs A list with specifications that go into 'lin_lp' or 'nl_lp' function.
#'        specs$lags_criterion: Either NaN (given lag length) or 'BIC'|'AIC'
#'        specs$lags_lin:       Lag length
#'        specs$max_lags:       Maximum number of lags to use for lag length criteria
#'        specs$shock_type:     1 = Standard deviation shock, 2 = Unit shock
#' @return Shock matrix (d)
#' @export
#' @author Philipp Adämmer


reduced_var  <- function(y_lin, x_lin, data_set_estim, specs){

  # Estimate OLS equation for equation
  if (is.nan(specs$lags_criterion) == TRUE) {

    # Estimates reduced VAR with pre-defined lag length
    lm_all        <- dplyr::as_tibble(lapply(y_lin, lm_function, x_lin))

                               } else {

    # Estimates reduced VAR with AIC or BIC criterion
    lag_criterion <- vars::VARselect(data_set_estim, lag.max =  specs$max_lags)

    if (specs$lags_criterion == 'AIC'){

      specs$lags_lin  <- which.min(lag_criterion$criteria[1,])

                               } else {

      specs$lags_lin  <- which.min(lag_criterion$criteria[3,]) }


    # Build data based on 'optimal lag length
    y_data <- as_tibble(y_lin[[specs$lags_lin]])
    x_data <- as_tibble(x_lin[[specs$lags_lin]])

    # Estimate OLS model
    lm_all  <- dplyr::as_tibble(lapply(y_data, lm_function, x_data))

  }


  # Rename columns
  names(lm_all) <- 1:specs$endog

  # Extract residuals
  resid_all     <- matrix(unlist(lm_all[2,]), ncol = specs$endog)


  # Make Covariance matrix
  cov_var       <- cov(resid_all)

  # Cholesky decomposition
  A             <- t(chol(cov_var))
  D             <- diag(sqrt(diag(cov_var)))

  # Shock Matrix
  d <- matrix(NaN, specs$endog, specs$endog)

  # Loop to create shocks

  if (specs$shock_type == 1){

    for (i in 1:specs$endog){
      d[, i]     <-  A[, i]/A[i, i]*D[i, i]
    }

  } else {

    for (i in 1:specs$endog){
      d[, i]     <-  A[, i]/A[i, i]

    }
  }

  # Return shock matrix
  d

}
