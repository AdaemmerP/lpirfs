#' Function to estimate switching series/variables. It uses a smooth transition function as
#' Auerbach & Gorodnichenko (2012)  <doi:10.1257/pol.4.2.1>.
#' The standardized time series is either pre-defined or will be estimated via
#' the Hodrick-Prescott as suggested by Ramey & Zubairy (2018) <doi:10.1086/696277>
#'
#' @param switching_data A vector with normalized time series to construct switching variable
#' @param specs A list with specifications that go into 'lin_lp' or 'nl_lp' function.
#'        specs$lamdba:    Value of lambda for HP filter
#'        specs$hp_filter: 1 = Use HP-filter to extract standardized time series
#'                         0 = Use pre-defined time series for transition function
#' @return fz: Vector with values from smooth transition function
#' @export
#' @author Philipp Adämmer
#' @examples


switching_series <- function(switching_data, specs){

  if(specs$hp_filter == 1){

    # Uses HP-filter to decompose switching time series.
      filter_results  <-   mFilter::hpfilter(switching_data, freq = specs$lambda, type = 'lambda')
      gamma_fz        <-   specs$gamma
      z_0             <-   as.numeric(scale(filter_results$cycle, center = TRUE))

    # Estimate values based on smooth tranisition function.
      fz              <-   exp(gamma_fz*z_0)/(1 + exp(gamma_fz*z_0))

    # Use merely pre-set time series.

                              }  else  {

   # Estimate values based on smooth tranisition function.
     fz              <-   exp(specs$gamma*switching_data)/(1 + exp(specs$gamma*switching_data))

  }

}
