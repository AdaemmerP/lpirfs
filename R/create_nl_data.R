#' Function to create data for non-linear model.
#'
#' @param specs A list with specifications that go into 'lin_lp' or 'nl_lp' function.
#'        specs$lags_criterion: Either NaN (given lag length) or 'BIC'|'AIC'
#'        specs$lags_lin:       Lag length
#'        specs$trend:          1 (no trend), 2 (trend), 3 (quadratic trend)
#'        specs$max_lags:       Maximum number of lags to use for lag length criteria
#' @param data_set_df A data frame with all endogenous variables
#'
#' @return A list with lagged data for non-linear model.
#' @author Philipp Adämmer
#'
#'
create_nl_data <- function(specs, data_set_df){

  # Check whether lag lengths have to be determined
  if (is.nan(specs$lags_criterion) == TRUE) {

   # Load switching variable
    fz  <- switching_series(specs$switching, specs)
    fz  <- fz[specs$lags_nl:(length(fz) - 1)]  # Make lagged data so that (F_{z_(t-1)})

   # Select data for endogenous variables
    y_nl         <- data_set_df[(specs$lags_nl + 1):dim(data_set_df)[1],]

   # Make exogenous lagged data
    x_nl         <- create_lags(data_set_df, specs$lags_nl)

   # Save names of exogenous variables
    linear_names <- names(x_nl)

   # Create tibbles with exogenous regime data and combine them to one data set
    x_nl_s1      <- x_nl %>%
                      dplyr::mutate_all(funs(expans      = .*(1 - fz)))  %>%
                      dplyr::select(-one_of(linear_names))

    x_nl_s2      <- x_nl %>%
                      dplyr::mutate_all(funs(recess      = .*fz))  %>%
                      dplyr::select(-one_of(linear_names))

    x_nl         <- cbind(x_nl_s1, x_nl_s2)

    # Include no trend, trend or quadratic trend
    switch(specs$trend,

     # Only constant
       x_nl            <-   x_nl %>%
                                as.matrix(),

     # Constant and trend
       x_nl            <-   x_nl %>%
                                   dplyr::mutate(trend    = row_number()) %>%
                                                                     as.matrix() ,

     # Constant, trend and quadratice trend
       x_nl            <-   x_nl %>%
                                   dplyr::mutate(trend    = row_number())  %>%
                                   dplyr::mutate(sq_trend = trend^2)       %>%
                                                                      as.matrix()

    )

################################################################################
                              } else {
################################################################################

 # Create list to store matrices
    y_nl     <- rep(list(NaN), specs$max_lags)
    x_nl     <- rep(list(NaN), specs$max_lags)

  for(i in 1:specs$max_lags){

   # Switching_data
     fz  <- switching_series(specs$switching[i:(nrow(data_set_df) - 1)], specs)

   # Create list with endogenous variables
      y_nl[[i]]      <- data_set_df[(i + 1):dim(data_set_df)[1],]  %>%
                                                                as.matrix()

   # Create exogenous variables
      x_nl_temp      <- create_lags(data_set_df, i)

      linear_names   <- names(x_nl_temp)

      x_nl_s1        <- x_nl_temp %>%
                           dplyr::mutate_all(funs(expans      = .*(1 - fz))) %>%
                           dplyr::select(-one_of(linear_names))

      x_nl_s2        <- x_nl_temp %>%
                           dplyr::mutate_all(funs(recess      = .*fz))        %>%
                           dplyr::select(-one_of(linear_names))

      x_nl[[i]]      <- cbind(x_nl_s1, x_nl_s2)

      # Include no trend, trend or quadratic trend
      switch(specs$trend,

             x_nl[[i]]            <-   x_nl[[i]]                                 %>%
                                                                    as.matrix() ,

             x_nl[[i]]            <-   x_nl[[i]]                                 %>%
                                            dplyr::mutate(trend  = row_number()  %>%
                                                                     as.matrix()),

             x_nl[[i]]            <-   x_nl[[i]]                                     %>%
                                            dplyr::mutate(trend    = row_number())   %>%
                                            dplyr::mutate(sq_trend = trend^2)        %>%
                                                                     as.matrix()  )


    }
   }

  list(y_nl = y_nl, x_nl = x_nl, fz = fz)
}
