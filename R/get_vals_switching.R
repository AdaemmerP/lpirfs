#' @name get_vals_switching
#' @title Compute values of transition function to separate regimes
#' @description Computes transition values by using a smooth transition function as
#' used in Auerbach and Gorodnichenko (2012). The time series used in the transition function
#' can be detrended via the Hodrick-Prescott filter (see Auerbach and Gorodnichenko, 2013).
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

 # Use switching variable for non-panel data
 if(specs$model_type == 0 | specs$model_type == 1){

  # Decide whether to use HP filter.
    if(specs$use_hp == TRUE){

    # Use HP-filter to decompose switching variable.
     filter_results  <-   hp_filter(matrix(switching_data), specs$lambda)
     gamma_fz        <-   specs$gamma
     z_0             <-   as.numeric(scale(filter_results[[1]], center = TRUE))
     fz              <-   exp((-1)*gamma_fz*z_0)/(1 + exp((-1)*gamma_fz*z_0))

     # Use first lag of value from switching function?
     if(isTRUE(specs$lag_switching)){

       fz            <-   create_lags(as.data.frame(fz), 1)   %>%
                          as.matrix() %>%
                          as.numeric()

     }


                          }  else  {

      fz              <-   exp((-1)*specs$gamma*switching_data)/(1 + exp((-1)*specs$gamma*switching_data))

      # Use first lag of value from switching function?
      if(isTRUE(specs$lag_switching)){

        fz            <-   create_lags(as.data.frame(fz), 1)   %>%
                            as.matrix() %>%
                            as.numeric()

      }


                      }

########################### For panel data #####################################
                              } else  {
################################################################################

  # Estimate switching values for panel data
  if(specs$model_type == 2){

    # Decide whether to use HP filter.
    if(specs$use_hp == TRUE){

    # Function to use hp_filter in dplyr
    use_hp_dplyr <- function(data, lambda){

         hp_values <- hp_filter(matrix(data), lambda)
         return(hp_values[[1]])
    }

    switching_tbl <- switching_data                                         %>%
                      dplyr::select(cross_id, date_id, specs$switching)     %>%
                      dplyr::group_by(cross_id)                             %>%
                      dplyr::mutate_at(vars(specs$switching), funs(use_hp_dplyr(., specs$lambda))) %>%
                      dplyr::rename(switching = specs$switching)            %>%
                      dplyr::ungroup()

    # Plug values from HP-filter into switching function
    gamma_fz        <-   specs$gamma
    z_0             <-   as.numeric(scale(switching_tbl$switching, center = TRUE))
    fz              <-   exp((-1)*gamma_fz*z_0)/(1 + exp((-1)*gamma_fz*z_0))

    # Use first lag of value from switching function?
    if(isTRUE(specs$lag_switching)){

      fz            <-   create_lags(as.data.frame(fz), 1)   %>%
                         as.matrix() %>%
                         as.numeric()

    }


    return(fz)
                             }  else  {

    fz              <-   exp((-1)*specs$gamma*switching_data)/(1 + exp((-1)*specs$gamma*switching_data))

    # Use first lag of value from switching function?
    if(isTRUE(specs$lag_switching)){

      fz            <-   create_lags(as.data.frame(fz), 1)   %>%
                         as.matrix() %>%
                         as.numeric()

    }

    }
 }




  }


  return(fz)

}
