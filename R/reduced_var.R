#' Function to estimate structural shock matrix via Cholesky decomposition.
#'
#' @param y_lin A data frame for the exogenous variables.
#' @param x_lin A list with exogenous variables.
#' @param data_set Data with all endogenous variables.
#' @param specs A list with specifications that go into 'lin_lp' or 'nl_lp' function.
#'        specs$lags_criterion: Either NaN (given lag length) or 'BIC'|'AIC'
#'        specs$lags_lin:       Lag length
#'        specs$max_lags:       Maximum number of lags to use for lag length criteria
#'        specs$shock_type:     1 = Standard deviation shock, 2 = Unit shock
#' @return Shock matrix (d).



reduced_var <- function(y_lin, x_lin, data_set, specs){

  # Estimate OLS equation for equation
  if (specs$lags_criterion == 'NA') {

    lm_all   <- dplyr::as_tibble(lapply(y_lin, lm_function))

                         } else {

      lag_criterion <- vars::VARselect(data_set, lag.max =  specs$max_lags)

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


  # Loop to create standard deviation or unit shock matrix

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
