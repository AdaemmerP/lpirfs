#' @name create_lin_data
#' @title Function to create data for linear model.
#' @description Function to create data for linear model.
#' @param specs A \link{list}(). Inputs are outlined in \link{lp_lin}.
#' @param data_set_df A \link{data.frame} with all endogenous variables.
#' @return List with filled matrices of lagged left (y_lin) and right hand side (x_lin) variables
#' @author Philipp Adämmer

create_lin_data     <- function(specs, data_set_df){

 # Check whether lag length is provided or has to be determined
 if (is.nan(specs$lags_criterion) == TRUE) {

    # Data for endogenous variables
    y_lin <- data_set_df[(specs$lags_lin + 1):dim(data_set_df)[1],]  %>%
                                                                as.matrix()

    # Make exogenous lagged data
    x_lin <- create_lags(data_set_df, specs$lags_lin)

    # Include no trend, trend or quadratic trend?
    if (specs$trend == 0){

      x_lin            <-   x_lin %>%
                              as.matrix()
             } else if (specs$trend == 1){

      x_lin            <-   x_lin                                        %>%
                             dplyr::mutate(trend    = row_number())      %>%
                                                                   as.matrix()

             } else {

       x_lin            <-   x_lin               %>%
                                dplyr::mutate(trend    = row_number())  %>%
                                dplyr::mutate(sq_trend = trend^2)       %>%
                                                                        as.matrix()
}
################################################################################
                               } else {
################################################################################

 # Create list to store lagged data
  y_lin     <- rep(list(NaN), specs$max_lags)
  x_lin     <- rep(list(NaN), specs$max_lags)

 # Make lag data based on max lag lengths
  for(i in 1:specs$max_lags){

    y_lin[[i]] <-  data_set_df[(i + 1):dim(data_set_df)[1],]   %>%
                                                          as.matrix()

    x_lin[[i]] <-  dplyr::as_tibble(create_lags(data_set_df, i))

   # Include no trend, trend or quadratic trend
    if (specs$trend == 0){

     x_lin[[i]]          <-   x_lin[[i]]        %>%
                                                as.matrix()
             } else if  (specs$trend == 1){

     x_lin[[i]]          <-   x_lin[[i]]       %>%
                                          dplyr::mutate(trend = row_number())  %>%
                                                            as.matrix()
             } else {

     x_lin[[i]]          <-   x_lin[[i]]                            %>%
                                          dplyr::mutate(trend = row_number())  %>%
                                          dplyr::mutate(sq_trend = trend^2)    %>%
                                                            as.matrix()
    }
   }
  }

  # Return list with exogenous and endogenous data
  return(list(y_lin, x_lin))

}
