#' @name create_nl_data
#' @title Compute data for nonlinear model with instrument variable approach
#' @description Function to create data for nonlinear model with instrument variable approach.
#' @param specs A \link{list}(). Inputs are created in \link{lp_nl_iv}.
#' @param endog_data A \link{data.frame} with dependent variables.
#' @return List with filled matrices of lagged left (y_nl) and right hand side (x_nl) variables.
#' @import dplyr
#' @keywords internal
#' @author Philipp Adämmer


create_nl_data <- function(specs, endog_data){

  # Check whether lag lengths have to be determined
  if (is.nan(specs$lags_criterion) == TRUE) {

    # Load switching variable
    fz  <- get_vals_switching(specs$switching, specs)
    fz  <- create_lags(as.data.frame(fz), 1)        %>%
                                        as.matrix() %>%
                                        as.numeric()

    # Select data for endogenous variables
    y_nl    <- endog_data

    # Make exogenous lagged data
    x_nl    <- create_lags(endog_data, specs$lags_nl)


    # Save names of exogenous variables
    linear_names <- names(x_nl)

    # Create tibbles with exogenous regime data and combine them to one data set
    x_nl_s1      <- x_nl %>%
                        dplyr::mutate_all(funs(s1 = .*(1 - fz)))  %>%
                        dplyr::select(-one_of(linear_names))

    x_nl_s2      <- x_nl %>%
                        dplyr::mutate_all(funs(s2 = .*fz))  %>%
                        dplyr::select(-one_of(linear_names))

    x_nl         <- cbind(x_nl_s1, x_nl_s2)


    # Check whether model type is 'iv'.
    # 0 = Normal model, 1 = IV model
    # Prepare instrument variable and add to exogenous data
    if(specs$model_type == 1){
    # Prepare instrument variable
    instrum           <- specs$instr
    colnames(instrum) <- 'instrum'
    instrum_names     <- colnames(instrum)

    # Make states of instrument

    instrum_s1        <- instrum %>%
                          dplyr::mutate_all(funs(instrum_s1 = .*(1 - fz)))  %>%
                          dplyr::select(-one_of(instrum_names))

    instrum_s2        <- instrum %>%
                          dplyr::mutate_all(funs(instrum_s2 = .*fz))        %>%
                          dplyr::select(-one_of(instrum_names))

    # Add instrument variable
    x_nl              <- cbind(instrum_s1, instrum_s2,  x_nl)
                      }

    # Include no trend, trend or quadratic trend
    if(specs$trend == 0){

      # Only constant
      x_nl             <-   x_nl %>%
                            as.matrix()

                    } else if (specs$trend == 1){

      # Constant and trend
      x_nl            <-   x_nl %>%
                            dplyr::mutate(trend = row_number()) %>%
                                                              as.matrix()

                      }  else {

      x_nl             <-   x_nl %>%
                             dplyr::mutate(trend = row_number())     %>%
                             dplyr::mutate(sq_trend = trend^2)       %>%
                                                              as.matrix()
                      }

    # Construct (lagged) exogenous data and merge it with lagged endogenous data
    if(!(is.null(specs$exog_data))){

      # Create lagged exogenous data and merge with endogenous data
      x_exog    <- create_lags(specs$exog_data, specs$lags_exog)     %>%
                                                        `rownames<-`(NULL)


      x_nl      <- cbind(x_nl, x_exog)

    }

    # Add contemporaneous data if supplied
    if(!(is.null(specs$contemp_data))){

      x_nl                         <- cbind(x_nl , specs$contemp_data)

    }


    # Combine endogenous and exogenous data
    yx_all    <-  cbind(y_nl, fz, x_nl) %>%
                                      stats::na.omit()


    y_nl      <-  yx_all[, 1:ncol(endog_data)]  %>%
                                                as.matrix()

    fz        <- yx_all[, (1 + ncol(endog_data))]

    x_nl      <-  yx_all[, (2 + ncol(endog_data)): dim(yx_all)[2]]   %>%
                                                 as.matrix()



################################################################################
                                } else {
################################################################################

    # Create list to store matrices
      y_nl_store     <- rep(list(NaN), specs$max_lags)
      x_nl_store     <- rep(list(NaN), specs$max_lags)
      fz_store       <- rep(list(NaN), specs$max_lags)

    # Prepare data outside of loop

    # Create list with endogenous variables
      y_nl      <- endog_data

    # Load switching variable
      fz  <- get_vals_switching(specs$switching, specs)
      fz  <- create_lags(as.data.frame(fz), 1)            %>%
                            as.matrix() %>%
                            as.numeric()


    # Prepare instrument variable
      # Check whether model type is 'iv'.
      # 0 = Normal model, 1 = IV model
      # Prepare instrument variable and add to exogenous data
      if(specs$model_type == 1){
      instrum           <- specs$instr
      colnames(instrum) <- 'instrum'
      instrum_names     <- colnames(instrum)
      }


    for(i in 1:specs$max_lags){

  # Create lagged variables
      x_nl_temp      <- create_lags(endog_data, i)

      linear_names   <- names(x_nl_temp)

      x_nl_s1        <- x_nl_temp %>%
                        dplyr::mutate_all(funs(s1 = .*(1 - fz))) %>%
                        dplyr::select(-one_of(linear_names))

      x_nl_s2        <- x_nl_temp %>%
                        dplyr::mutate_all(funs(s2 = .*fz))        %>%
                        dplyr::select(-one_of(linear_names))

      x_nl            <- cbind(x_nl_s1, x_nl_s2)


  # Prepare states of instrument
      if(specs$model_type == 1){
      instrum_s1      <- instrum %>%
                         dplyr::mutate_all(funs(instrum_s1 = .*(1 - fz)))  %>%
                         dplyr::select(-one_of(instrum_names))

      instrum_s2        <- instrum %>%
                           dplyr::mutate_all(funs(instrum_s2 = .*fz))        %>%
                           dplyr::select(-one_of(instrum_names))

  # Add state instruments
        x_nl              <- cbind(instrum_s1, instrum_s2,  x_nl)
      }

  # Add trend if set
     if(specs$trend == 0){

       x_nl             <-   x_nl               %>%
                            as.matrix()


                    } else if (specs$trend == 1){

       x_nl            <-   x_nl                                    %>%
                             dplyr::mutate(trend = row_number())     %>%
                             as.matrix()


                       }  else {

       x_nl            <-   x_nl                                %>%
                              dplyr::mutate(trend = row_number())      %>%
                              dplyr::mutate(sq_trend = trend^2)        %>%
                              as.matrix()
    }


  # Construct (lagged) exogenous data and merge it with lagged endogenous data
        if(!(is.null(specs$exog_data))){

          x_exog    <- create_lags(specs$exog_data, specs$lags_exog)     %>%
                       `rownames<-`(NULL)
          x_nl      <- cbind(x_nl, x_exog)

      }

  # Add contemporaneous data if set
        if(!(is.null(specs$contemp_data))){

          x_nl      <- cbind(x_nl , specs$contemp_data)

        }

  # Merge endogenous and exogenous data
        yx_all               <-  cbind(y_nl, fz, x_nl)         %>%
                                 stats::na.omit()

        y_nl_store[[i]]      <-  yx_all[, 1:ncol(endog_data)]  %>%
                                 as.matrix()

        fz_store[[i]]        <-  yx_all[, (1 + ncol(endog_data))]

        x_nl_store[[i]]      <-  yx_all[, (2 + ncol(endog_data)): dim(yx_all)[2]]   %>%
                                 as.matrix()




    }
      # Save values te return in list
        y_nl <- y_nl_store
        x_nl <- x_nl_store
        fz   <- fz_store
  }

  return(list(y_nl = y_nl, x_nl = x_nl, fz = fz))
}
