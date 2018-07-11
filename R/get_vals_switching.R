#' @name get_vals_switching
#' @title Compute values of transition function to separate regimes
#' @description Function to estimate transition values, which uses a smooth transition function as
#' used in Auerbach and Gorodnichenko (2012). The time series used in the transition function
#' can be de-trended via the Hodrick-Prescott (see Auerbach and Gorodnichenko, 2013).
#' @param switching_data A numeric vector.
#' @param specs A \link{list}() with inputs as in \link{lp_nl}().
#' @return \item{fz}{A numeric vector with values from smooth transition function \eqn{F(z_{t-1})}.}
#' @keywords internal
#' @references
#' Auerbach, A. J., and  Gorodnichenko Y. (2012). "Measuring the Output Responses to Fiscal Policy."
#' \emph{American Economic Journal: Economic Policy}, 4 (2): 1-27.
#'
#' Auerbach, A. J., and Gorodnichenko Y. (2013). "Fiscal Multipliers in Recession and Expansion."
#' \emph{NBER Working Paper Series}. Nr 17447.
#'
#'
#' @author Philipp Adämmer



get_vals_switching <- function(switching_data, specs){

 # Decide whether to use HP filter.
  if(specs$hp_filter == 1){

  # Use HP-filter to decompose switching variable.
   filter_results  <-   hp_filter(matrix(switching_data), specs$lambda)
   gamma_fz        <-   specs$gamma
   z_0             <-   as.numeric(scale(filter_results[[1]], center = TRUE))
   fz              <-   exp((-1)*gamma_fz*z_0)/(1 + exp((-1)*gamma_fz*z_0))
  # return(fz)

                    }  else  {

    fz              <-   exp( (-1)*specs$gamma*switching_data)/(1 + exp((-1)*specs$gamma*switching_data))

  }

  return(fz)

}
