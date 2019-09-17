#' @name lp_lin
#' @title Compute linear impulse responses
#' @description Compute linear impulse responses with local projections by Jordà (2005).
#'
#' @param endog_data A \link{data.frame}, containing the endogenous variables for the VAR. The Cholesky decomposition is based on the
#'          column order.
#' @param lags_criterion NaN or character. NaN (default) means that the number of lags
#'         has to be given at \emph{lags_endog_lin}. The character specifies the lag length criterion ('AICc', 'AIC' or 'BIC').
#' @param lags_endog_lin NaN or integer. NaN if lag length criterion is used. Integer for number of lags for \emph{endog_data}.
#' @param max_lags NaN or integer. Maximum number of lags if \emph{lags_criterion} is given. NaN (default) otherwise.
#' @param trend Integer. No trend =  0 , include trend = 1, include trend and quadratic trend = 2.
#' @param shock_type Integer. Standard deviation shock = 0, unit shock = 1.
#' @param confint Double. Width of confidence bands. 68\% = 1; 90\% = 1.65; 95\% = 1.96.
#' @param use_nw Boolean. Use Newey-West (1987) standard errors for impulse responses? TRUE (default) or FALSE.
#' @param nw_lag Integer. Specifies the maximum lag with positive weight for the Newey-West estimator. If set to NULL (default), the lag increases with
#'               with the number of horizon.
#' @param nw_prewhite Boolean. Should the estimators be pre-whitened? TRUE of FALSE (default).
#' @param adjust_se Boolen. Should a finite sample adjsutment be made to the covariance matrix estimators? TRUE or FALSE (default).
#' @param hor Integer. Number of horizons for impulse responses.
#' @param exog_data A \link{data.frame}, containing exogenous variables for the VAR. The row length has to be the same as \emph{endog_data}.
#'                 Lag lengths for exogenous variables have to be given and will no be determined via a lag length criterion.
#' @param lags_exog Integer. Number of lags for the exogenous variables.
#' @param contemp_data A \link{data.frame}, containing exogenous data with contemporaneous impact. The row length has to be the same as \emph{endog_data}.
#' @param num_cores NULL or Integer. The number of cores to use for the estimation. If NULL, the function will
#'                 use the maximum number of cores minus one.
#'
#' @seealso \url{https://adaemmerp.github.io/lpirfs/README_docs.html}
#'
#' @return A list containing:
#'
#'
#'\item{irf_lin_mean}{A three 3D \link{array}, containing all impulse responses for all endogenous variables.
#'                    The last dimension denotes the shock variable. The row in each matrix
#'                    gives the responses of the \emph{ith} variable, ordered as in endog_data. The columns denote the horizons.
#'                    For example, if \emph{results_lin} contains the list with results, results_lin$irf_lin_mean[, , 1] returns a KXH matrix,
#'                    where K is the number of variables and H the number of horizons. '1' is the shock variable, corresponding to the
#'                   first variable in \emph{endog_data}.}
#'
#'\item{irf_lin_low}{A three 3D \link{array} containing all lower confidence bands of the responses,
#'                    based on robust standard errors by Newey and West (1987). Properties are equal to irf_lin_mean.}
#'
#'\item{irf_lin_up}{A three 3D \link{array} containing all upper confidence bands of the responses,
#'                    based on robust standard errors by Newey and West (1987). Properties are equal to \emph{irf_lin_mean}.}
#'
#'\item{diagnostic_list}{A list OLS diagnostics. To see everything you can simply use summary() or results$diagnostic_list. The first entry
#' the shock variable. The rows of each shown matrix then denotes the endogenous variable that reacts to the shock.}
#'
#'\item{specs}{A list with properties of \emph{endog_data} for the plot function. It also contains
#'             lagged data (y_lin and x_lin) used for the irf estimations.}
#'
#' @export
#' @references
#' Akaike, H. (1974). "A new look at the statistical model identification", \emph{IEEE Transactions on Automatic Control}, 19 (6): 716–723.
#'
#' Hurvich, C. M., and Tsai, C.-L. (1989), "Regression and time series model selection in small samples",
#' \emph{Biometrika}, 76(2): 297–307
#'
#' Jordà, Ò. (2005). "Estimation and Inference of Impulse Responses by Local Projections."
#' \emph{American Economic Review}, 95 (1): 161-182.
#'
#' Newey, W.K., and West, K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
#' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55: 703–708.
#'
#' Schwarz, Gideon E. (1978). "Estimating the dimension of a model", \emph{Annals of Statistics}, 6 (2): 461–464.
#'
#' @author Philipp Adämmer
#' @importFrom foreach foreach
#' @examples
#'\donttest{
#'           ## Example without exogenous variables
#'
#'# Load package
#'   library(lpirfs)
#'
#'# Load (endogenous) data
#'   endog_data <- interest_rules_var_data
#'
#'# Estimate linear model
#'   results_lin <- lp_lin(endog_data,
#'                              lags_endog_lin = 4,
#'                              trend          = 0,
#'                              shock_type     = 1,
#'                              confint        = 1.96,
#'                              hor            = 12)
#'
#'# Show all impule responses
#'# Compare with Figure 5 in Jordà (2005)
#'  plot(results_lin)
#'
#'# Make individual plots
#'  linear_plots <- plot_lin(results_lin)
#'
#'# Show single plots
#'  # * The first element of 'linear_plots' shows the response of the first
#'  #   variable (GDP_gap) to a shock in the first variable (GDP_gap).
#'  # * The second element of 'linear_plots' shows the response of the first
#'  #   variable (GDP_gap) to a shock in the second variable (inflation).
#'  # * ...
#'
#'   linear_plots[[1]]
#'   linear_plots[[2]]
#'
#'
#'# Show diagnostics. The first element correponds to the first shock variable.
#'  summary(results_lin)
#'
#'
#'                       ## Example with exogenous variables ##
#'
#'# Load (endogenous) data
#'  endog_data <- interest_rules_var_data
#'
#'# Create exogenous data and data with contemporaneous impact (for illustration purposes only)
#'  exog_data    <- endog_data$GDP_gap*endog_data$Infl*endog_data$FF + rnorm(dim(endog_data)[1])
#'  contemp_data <- endog_data$GDP_gap*endog_data$Infl*endog_data$FF + rnorm(dim(endog_data)[1])
#'
#'# Exogenous data has to be a data.frame
#'  exog_data    <- data.frame(xx = exog_data )
#'  contemp_data <- data.frame(cc =  contemp_data)
#'
#'# Estimate linear model
#'   results_lin <- lp_lin(endog_data,
#'                                lags_endog_lin = 4,
#'                                trend          = 0,
#'                                shock_type     = 1,
#'                                confint        = 1.96,
#'                                hor            = 12,
#'                                exog_data      = exog_data,
#'                                lags_exog      = 4,
#'                                contemp_data   = contemp_data)
#'
#'# Show all impulse responses
#'  plot(results_lin)
#'
#'# Show diagnostics. The first element correponds to the first shock variable.
#'  summary(results_lin)
#'
#'  }
lp_lin <- function(endog_data,
                        lags_endog_lin = NULL,
                        lags_criterion = NaN,
                        max_lags       = NaN,
                        trend          = NULL,
                        shock_type     = NULL,
                        confint        = NULL,
                        use_nw         = TRUE,
                        nw_lag         = NULL,
                        nw_prewhite    = FALSE,
                        adjust_se      = FALSE,
                        hor            = NULL,
                        exog_data      = NULL,
                        lags_exog      = NULL,
                        contemp_data   = NULL,
                        num_cores      = NULL){


  # Create list to store inputs
    specs <- list()

  # Specify inputs
    specs$lags_endog_lin <- lags_endog_lin
    specs$lags_criterion <- lags_criterion
    specs$max_lags       <- max_lags
    specs$trend          <- trend
    specs$shock_type     <- shock_type
    specs$confint        <- confint
    specs$hor            <- hor
    specs$exog_data      <- exog_data
    specs$lags_exog      <- lags_exog
    specs$use_nw         <- use_nw
    specs$nw_prewhite    <- nw_prewhite
    specs$adjust_se      <- adjust_se
    specs$nw_lag         <- nw_lag


  # Set 2SLS option to FALSE
    specs$use_twosls <- FALSE

  # Add 'contempranoeus' as NULL for data construction
    specs$contemp_data   <- contemp_data

  # Set model type for plot function
    specs$model_type     <- 0

  # Check whether data is a data.frame()
  if(!(is.data.frame(endog_data))){
    stop('The data has to be a data.frame().')
  }

  # Check whether 'trend' is given
  if(is.null(specs$trend)){
    stop('Please specify whether and which type of trend to include.')
  }

  # Check whether 'shock_type' is given
  if(is.null(specs$shock_type)){
    stop('Please specify which type of shock to use.')
  }

  # Check whether width for confidence intervals is given
  if(is.null(specs$confint)){
    stop('Please specify a value for the width of the confidence bands.')
  }

  # Check whether number of horizons is given
  if(is.null(specs$hor)){
    stop('Please specify the number of horizons.')
  }


  # Check whether wrong lag length criterion is given
  if(!(is.nan(specs$lags_criterion)          | specs$lags_criterion == 'AICc'|
       specs$lags_criterion         == 'AIC' | specs$lags_criterion == 'BIC')){
    stop('Possible lag length criteria are AICc, AIC or BIC or NaN if lag length is specified.')
  }


  # Check whether lags criterion and maximum number of lags are given
  if((is.character(specs$lags_criterion)) &
      (!is.na(specs$lags_endog_lin))){
     stop('You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.')
    }


  # Check whether no lag length criterion and number of lags are given
  if((is.na(specs$lags_criterion)) &
      (is.na(specs$lags_endog_lin))){
    stop('You have to at least provide a lag criterion (AICc, AIC or BIC) or a fixed number of lags.')
  }


  # Check whether maximum number of lags is given for lag length criterion
  if((is.character(specs$lags_criterion)) &
     (is.na(specs$max_lags)            )){
    stop('Please provide a maximum number of lags for the lag length criterion.')
  }


  # Check whether values for horizons are correct
  if(!(specs$hor > 0) | is.nan(specs$hor) | !(specs$hor %% 1 == 0)){
    stop('The number of horizons has to be an integer and > 0.')
  }

  # Check whether lags for linear model are integers
  if(is.numeric(specs$lags_endog_lin) & !is.nan(specs$lags_endog_lin)){
    if(!(specs$lags_endog_lin %% 1 == 0)  | specs$lags_endog_lin < 0){
      stop('The numbers of lags have to be a positive integer.')
    }
  } else {}

  # Check whether trend is correctly specified
  if(!(specs$trend %in% c(0,1,2))){
    stop('For trend please enter 0 = no trend, 1 = trend, 2 = trend and quadratic trend.')
  }

  # Check whether shock type is correctly specified
  if(!(specs$shock_type %in% c(0,1))){
    stop('The shock_type has to be 0 = standard deviation shock or 1 = unit shock.')
  }


  # Check whether width of confidence bands is >=0
  if(!(specs$confint >=0)){
    stop('The width of the confidence bands has to be >=0.')
  }

  # Check whether maximum lag length is given when no criterion is given
    if(!is.character(specs$lags_criterion) & is.numeric(specs$max_lags) & !is.nan(specs$max_lags)){
      stop('The maximum number of lags is only used if you provide a lag length criterion.')
    }

  # Check whether exogenous data is a data.frame
    if(!is.null(specs$exog_data) & !is.data.frame(specs$exog_data)){
      stop('Exogenous data has to be a data.frame.')
    }

  # Check whether lag length for exogenous data is given
    if(!is.null(specs$exog_data) & is.null(specs$lags_exog)){
      stop('Please provide a lag length for the exogenous data.')
    }


  # Safe data frame specifications in 'specs for functions
   specs$starts         <- 1                        # Sample Start
   specs$ends           <- dim(endog_data)[1]      # Sample end
   specs$column_names   <- names(endog_data)       # Name endogenous variables
   specs$endog          <- ncol(endog_data)        # Set the number of endogenous variables

 # Construct (lagged) endogenous data
  data_lin <- create_lin_data(specs, endog_data)
  y_lin    <- data_lin[[1]]
  x_lin    <- data_lin[[2]]


  # Save endogenous and lagged exogenous data in specs
  specs$y_lin        <- y_lin
  specs$x_lin        <- x_lin


 # Construct shock matrix
  d <- get_mat_chol(y_lin, x_lin, endog_data, specs)

 # Matrices to store OLS parameters
  b1            <- matrix(NaN, specs$endog, specs$endog)
  b1_low        <- matrix(NaN, specs$endog, specs$endog)
  b1_up         <- matrix(NaN, specs$endog, specs$endog)

 # Matrices to store irfs for each horizon
  irf_mean  <-  matrix(NaN, specs$endog, specs$hor + 1)
  irf_low   <-  irf_mean
  irf_up    <-  irf_mean

 # 3D Arrays for all irfs
  irf_lin_mean  <-  array(NaN, dim = c(specs$endog, specs$hor + 1, specs$endog))
  irf_lin_low   <-  irf_lin_mean
  irf_lin_up    <-  irf_lin_mean

 # Make list to store OLS diagnostics for each horizon
  diagnost_ols_each_h <- list()

 # Make matrix to store OLS diagnostics for each endogenous variable k
  diagnost_each_k           <- matrix(NaN, specs$endog,  4)
  rownames(diagnost_each_k) <- specs$column_names
  colnames(diagnost_each_k) <- c("R-sqrd.", "Adj. R-sqrd.", "F-stat", " p-value")

  # Make cluster?
  if(is.null(num_cores)){
    num_cores    <- min(specs$endog, parallel::detectCores() - 1)
  }

  cl             <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)

 # Decide whether lag lengths are given or have to be estimated
  if(is.nan(specs$lags_criterion) == TRUE){

 # Loops to estimate local projections
  lin_irfs <- foreach(s         = 1:specs$endog,
                      .packages = 'lpirfs')  %dopar%{ # Accounts for the shocks

   for (h in 1:(specs$hor)){   # Accounts for the horizons

    # Create data
     yy  <-   y_lin[h : dim(y_lin)[1], ]
     xx  <-   x_lin[1 : (dim(x_lin)[1] - h + 1), ]

     # Set lag number for Newey-West (1987)
     if(is.null(nw_lag)){

          lag_nw <- h

               } else {

          lag_nw <- nw_lag

     }

     for (k in 1:specs$endog){ # Accounts for the reactions of the endogenous variables


       # Get standard errors and point estimates
       get_ols_vals <- lpirfs::get_std_err(yy, xx, lag_nw, k, specs)
       std_err <- get_ols_vals[[1]]
       b       <- get_ols_vals[[2]]

       # Fill coefficient matrix
       b1[k, ]        <-   b[2:(specs$endog + 1)]
       b1_low[k, ]    <-   b[2:(specs$endog + 1)] - std_err[2:(specs$endog + 1)]
       b1_up[k, ]     <-   b[2:(specs$endog + 1)] + std_err[2:(specs$endog + 1)]

       # Get diagnostocs for summary
       get_diagnost              <- lpirfs::ols_diagnost(yy[, k], xx)
       diagnost_each_k[k, 1]     <- get_diagnost[[3]]
       diagnost_each_k[k, 2]     <- get_diagnost[[4]]
       diagnost_each_k[k, 3]     <- get_diagnost[[5]]
       diagnost_each_k[k, 4]     <- stats::pf(diagnost_each_k[k, 3], get_diagnost[[6]], get_diagnost[[7]], lower.tail = F)


      }

      # Fill matrices with local projections
       irf_mean[, h + 1] <- t(b1     %*% d[ , s])
       irf_low[,  h + 1] <- t(b1_low %*% d[ , s])
       irf_up[,   h + 1] <- t(b1_up  %*% d[ , s])

      # Save full summary matrix in list for each horizon
       diagnost_ols_each_h[[h]]             <- diagnost_each_k

   }

       # Give names to horizon
       names(diagnost_ols_each_h)    <- paste("h", 1:specs$hor, sep = " ")

       # Return irfs and diagnostics
       return(list(irf_mean,  irf_low,  irf_up, diagnost_ols_each_h))
}


  # List to save diagnostics
  diagnostic_list <- list()

 # Fill arrays with irfs
  for(i in 1:specs$endog){

    # Fill irfs
    irf_lin_mean[, , i]   <- as.matrix(do.call(rbind, lin_irfs[[i]][1]))
    irf_lin_low[, ,  i]   <- as.matrix(do.call(rbind, lin_irfs[[i]][2]))
    irf_lin_up[, ,   i]   <- as.matrix(do.call(rbind, lin_irfs[[i]][3]))

    # First value of is merely the shock
    irf_lin_mean[, 1, i]   <- t(d[, i])
    irf_lin_low[,  1, i]   <- irf_lin_mean[, 1, i]
    irf_lin_up[,   1, i]   <- irf_lin_mean[, 1, i]

    # Fill list with all OLS diagnostics
    diagnostic_list[[i]]        <- lin_irfs[[i]][4]

  }

  # Give names to diagnostic List
  names(diagnostic_list) <- paste("Shock:", specs$column_names, sep = " ")

################################################################################
                               } else {
################################################################################

 # Convert chosen lag criterion to number for loop
  lag_crit     <- switch(specs$lags_criterion,
                                         'AICc'= 1,
                                         'AIC' = 2,
                                         'BIC' = 3)

 # Loops to estimate local projections.
  lin_irfs <- foreach(s          = 1:specs$endog,
                     .packages   = 'lpirfs')  %dopar% {

    for (h in 1:specs$hor){     # Accounts for the horizon

      for (k in 1:specs$endog){ # Accounts for endogenous reactions

        # Find optimal lags
         n_obs         <- nrow(y_lin[[1]]) - h + 1 # Number of observations for model with lag one
         val_criterion <- lpirfs::get_vals_lagcrit(y_lin, x_lin, lag_crit, h, k,
                                                   specs$max_lags, n_obs)

        # Set optimal lag length
         lag_choice    <- which.min(val_criterion)

        # Extract matrices based on optimal lag length
         yy            <- y_lin[[lag_choice]][, k]
         yy            <- yy[h: length(yy)]

         xx            <- x_lin[[lag_choice]]
         xx            <- xx[1:(dim(xx)[1] - h + 1),]


         # Set lag number for Newey-West (1987)
         if(is.null(nw_lag)){

           lag_nw <- h

                      } else {

           lag_nw <- nw_lag

                      }


         # Get standard errors and point estimates. Set k = 1 because endogenous variable is numeric vector
         get_ols_vals <- lpirfs::get_std_err(yy, xx, lag_nw, 1, specs)
         std_err <- get_ols_vals[[1]]
         b       <- get_ols_vals[[2]]

         # Fill coefficient matrix
         b1[k, ]        <-   b[2:(specs$endog + 1)]
         b1_low[k, ]    <-   b[2:(specs$endog + 1)] - std_err[2:(specs$endog + 1)]
         b1_up[k, ]     <-   b[2:(specs$endog + 1)] + std_err[2:(specs$endog + 1)]

         # Get diagnostocs for summary
         get_diagnost              <- lpirfs::ols_diagnost(yy, xx)
         diagnost_each_k[k, 1]     <- get_diagnost[[3]]
         diagnost_each_k[k, 2]     <- get_diagnost[[4]]
         diagnost_each_k[k, 3]     <- get_diagnost[[5]]
         diagnost_each_k[k, 4]     <- stats::pf(diagnost_each_k[k, 3], get_diagnost[[6]], get_diagnost[[7]], lower.tail = F)

       }


        # Fill matrices with local projections
         irf_mean[, h + 1] <- t(b1     %*% d[ , s])
         irf_low[,  h + 1] <- t(b1_low %*% d[ , s])
         irf_up[,   h + 1] <- t(b1_up  %*% d[ , s])

         # Save full summary matrix in list for each horizon
         diagnost_ols_each_h[[h]]        <- diagnost_each_k

    }

     # Give names to horizon
       names(diagnost_ols_each_h)    <- paste("h", 1:specs$hor, sep = " ")

        return(list(irf_mean,  irf_low,  irf_up, diagnost_ols_each_h))
    }


    diagnostic_list      <- list()

    # Fill arrays with irfs
    for(i in 1:specs$endog){

      # Fill irfs
      irf_lin_mean[, , i] <- as.matrix(do.call(rbind, lin_irfs[[i]][1]))
      irf_lin_low[, ,  i] <- as.matrix(do.call(rbind, lin_irfs[[i]][2]))
      irf_lin_up[, ,   i] <- as.matrix(do.call(rbind, lin_irfs[[i]][3]))

      # First value of horizon is merely the shock
      irf_lin_mean[, 1, i]   <- t(d[, i])
      irf_lin_low[,  1, i]   <- irf_lin_mean[, 1, i]
      irf_lin_up[,   1, i]   <- irf_lin_mean[, 1, i]

      # Fill list with all OLS diagnostics
      diagnostic_list[[i]]        <- lin_irfs[[i]][4]

    }

    # Give names to diagnostic List
    names(diagnostic_list) <- paste("Shock:", specs$column_names, sep = " ")


###################################################################################################

  }

  # Close cluster
  parallel::stopCluster(cl)

  result <-  list(irf_lin_mean      = irf_lin_mean,
                  irf_lin_low       = irf_lin_low,
                  irf_lin_up        = irf_lin_up,
                  diagnostic_list   = diagnostic_list,
                  specs             = specs)

  # Give object S3 name
  class(result) <- "lpirfs_lin_obj"
  return(result)

}
