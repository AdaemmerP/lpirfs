#' Function to create data for linear model.
#'
#' @param specs A list with specifications that go into 'lp_lin' or 'lp_nl' function.
#'        specs$lags_criterion: Either NaN (given lag length) or 'AICc'|'AIC'|'BIC'|
#'        specs$lags_lin:       Lag length
#'        specs$trend:          1 (no trend), 2 (trend), 3 (quadratic trend)
#'        specs$max_lags:       Maximum number of lags to determine lag length criteria
#' @param data_set_df A data frame with all endogenous variables for VAR.
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
    switch(specs$trend,
           x_lin            <-   x_lin               %>%
                                              as.matrix(),

           x_lin            <-   x_lin                                     %>%
                                   dplyr::mutate(trend    = row_number()   %>%
                                                                   as.matrix()),

           x_lin            <-   x_lin               %>%
                                   dplyr::mutate(trend    = row_number())  %>%
                                   dplyr::mutate(sq_trend = trend^2)       %>%
                                                                   as.matrix())

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
   switch(specs$trend,

             x_lin[[i]]            <-   x_lin[[i]]                 %>%
                                                            as.matrix(),

             x_lin[[i]]            <-   x_lin[[i]]                             %>%
                                          dplyr::mutate(trend = row_number())  %>%
                                                            as.matrix(),

             x_lin[[i]]            <-   x_lin[[i]]                 %>%
                                          dplyr::mutate(trend = row_number())   %>%
                                          dplyr::mutate(sq_trend = trend^2) %>%
                                                            as.matrix())
    }
  }

  # Return list with exogenous and endogenous data
  return(list(y_lin, x_lin))

}
