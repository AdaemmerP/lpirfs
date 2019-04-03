#' @name get_vals_switching
#' @title Compute values of transition function to separate regimes
#' @description Computes transition values by using a smooth transition function as
#' used in Auerbach and Gorodnichenko (2012). The time series used in the transition function
#' can be detrended via the Hodrick-Prescott filter (see Auerbach and Gorodnichenko, 2013).
#' @param data_set A numeric vector or a panel data set, depending on the model to estimate.
#' @param specs A \link{list} with inputs as in \link{lp_nl}().
#' @return \item{fz}{A numeric vector with values from the smooth transition function \eqn{F(z_{t-1})}.}
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



get_vals_switching <- function(data_set, specs){


  # Function which creates lags to be consistent with dplyr
  lag_function  <- function(data, lag_nr){

    lag_data  <- dplyr::lag(data, lag_nr)

  }


 # Use switching variable for non-panel data
 if(specs$model_type == 0 | specs$model_type == 1){

  # Convert switching data to tibble
   data_set        <- as_tibble(data_set)
   names(data_set) <- "switch_name"

  # Decide whether to use HP filter.
    if(specs$use_hp == TRUE){

    # Use HP-filter to decompose switching variable.
     filter_results  <-   hp_filter(as.matrix(data_set), specs$lambda)
     gamma_fz        <-   specs$gamma
     z_0             <-   as.numeric(scale(filter_results[[1]], center = TRUE))
     fz              <-   exp((-1)*gamma_fz*z_0)/(1 + exp((-1)*gamma_fz*z_0))

     # Use first lag of value from switching function?
     if(isTRUE(specs$lag_switching)){

       fz            <-    dplyr::lag(fz, 1)

     }


                          }  else  {

      fz              <-  exp((-1)*specs$gamma*data_set$switch_name)/(1 + exp((-1)*specs$gamma*data_set$switch_name))

      # Use first lag of value from switching function?
      if(isTRUE(specs$lag_switching)){


        fz            <-    dplyr::lag(fz, 1)



      }


                      }

########################### For panel data #####################################
                              } else  {
################################################################################

  # Estimate switching values for panel data
  if(specs$model_type == 2){

    # Decide whether to use HP filter.
    if(isTRUE(specs$use_hp)){

    # Function to use hp_filter in dplyr
    use_hp_dplyr <- function(data, lambda){

         hp_values <- hp_filter(matrix(data), lambda)
         return(hp_values[[1]])
    }

    switching_tbl <- data_set                                         %>%
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


      fz_df <- tibble(cross_id = data_set$cross_id, date_id = data_set$date_id,
                      fz = fz)

      fz_df <- fz_df %>%
                dplyr::group_by(cross_id)                              %>%
                dplyr::mutate_at(vars(fz), funs(lag_function(., 1)))   %>%
                dplyr::ungroup()

      fz    <- fz_df$fz

    }


    return(fz)
                             }  else  {


    fz              <-   exp((-1)*specs$gamma*data_set[specs$switching])/
                         (1 + exp((-1)*specs$gamma*data_set[specs$switching]))

    fz              <- as.numeric(fz[, 1])

    # Use first lag of value from switching function?
    if(isTRUE(specs$lag_switching)){


      fz_df <- tibble(cross_id = data_set$cross_id, date_id = data_set$date_id,
                      fz = fz)

      fz_df <- fz_df %>%
                dplyr::group_by(cross_id)                              %>%
                dplyr::mutate_at(vars(fz), funs(lag_function(., 1)))   %>%
                dplyr::ungroup()

      fz    <- fz_df$fz


     }
   }
 }




  }


  return(fz)

}
