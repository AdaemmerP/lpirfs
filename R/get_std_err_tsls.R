#' @name get_std_err_tsls
#' @title Compute standard errors for 2SLS
#' @description Compute 2SLS standard errors.#'
#' @param yy A matrix with all endogenous variables.
#' @param xx A matrix with lagged endogenous variables.
#' @param k Integer. Value of column to choose from
#' @param lag_nw Integer. Number of lags for Newey and West
#' @param specs A list with specifications from \link{lp_lin} or \link{lp_nl}.
#' @return List. First element contains the estimated standard errors, the second element contains the OLS point estimates.
#' @keywords internal
#' @author Philipp Adämmer


get_std_err_tsls <- function(yy, xx, lag_nw, k, zz,  specs){


  if(!is.matrix(yy)){
    yy <-  matrix(yy)
  }

  # Newey West standard errors
  if(isTRUE(specs$use_nw)){

    # Check whether prewhitening shall be applied
    if(isTRUE(specs$nw_prewhite)){

      # Estimate coefficients
      nw_results      <- lpirfs::newey_west_tsls(yy[, k], xx, zz, lag_nw)
      b               <- nw_results[[1]]

      x_u             <- nw_results[[3]]
      xpxi            <- nw_results[[4]]

      resid_pw        <- var_one(x_u)[[2]]
      D_mat           <- var_one(x_u)[[3]]

      cov_nw_pw       <- newey_west_pw(resid_pw, xpxi, D_mat, 1)[[1]]

      # Make finite sample adjustment?
      if(isTRUE(specs$adjust_se)) cov_nw_pw  <- cov_nw_pw*nrow(yy)/(nrow(yy) - ncol(xx) - 1)

      # Get standard errors
      std_err        <- sqrt(diag(cov_nw_pw))*specs$confint


         }    else    {

      #
      nw_results   <- lpirfs::newey_west_tsls(yy[, k], xx, zz, lag_nw)
      b            <- nw_results[[1]]
      cov_mat      <- nw_results[[2]]

      # Make finite sample adjustment
      if(isTRUE(specs$adjust_se)) cov_mat  <- cov_mat*nrow(yy)/(nrow(yy) - ncol(xx) - 1)

      # Get NW standard errors
      std_err        <- sqrt(diag(cov_mat))*specs$confint


    }

    # Normal standard errors
  }                         else        {

    ols_output         <- lpirfs::newey_west_tsls(yy[, k], xx, zz, lag_nw)


    # Get parameters and normal standard errors
    b                  <- ols_output[[1]]
    beta_cov           <- ols_output[[6]]


    # Finite sample adjustment?
    if(isTRUE(specs$adjust_se)) beta_cov <- beta_cov*nrow(yy)/(nrow(yy) - ncol(xx) - 1)

    std_err           <- sqrt(diag(beta_cov))

  }

  return(list(std_err, b))


}
