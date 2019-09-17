#' @name lp_lin_iv
#' @title Compute linear impulse responses with identified shock and/or with 2SLS
#' @description Compute linear impulse responses with identified shock and/or with 2SLS.
#' @param endog_data A \link{data.frame}, containing the values of the dependent variable(s).
#' @param shock A one column \link{data.frame}, including the variable to shock with. The row length has to be the same as \emph{endog_data}.
#' When \emph{use_twosls = TRUE}, this variable will be approximated/regressed on the instrument variable(s) given in \emph{instrum}.
#' @param instr Deprecated input name. Use \emph{shock} instead. See \emph{shock} for details.
#' @param use_twosls Boolean. Use two stage least squares? TRUE or FALSE (default).
#' @param instrum A \link{data.frame}, containing the instrument(s) to use for 2SLS. This instrument will be used for the
#'  variable in \emph{shock}.
#' @param lags_endog_lin NaN or integer. NaN if lags are chosen by a lag length criterion. Integer for number of lags for \emph{endog_data}.
#' @param exog_data A \link{data.frame}, containing exogenous variables. The row length has to be the same as \emph{endog_data}.
#'                  Lag lengths for exogenous variables have to be given and will no be determined via a lag length criterion.
#' @param lags_exog NULL or Integer. Integer for the number of lags for the exogenous data.
#' @param contemp_data A \link{data.frame}, containing exogenous data with contemporaneous impact.
#'                      The row length has to be the same as \emph{endog_data}.
#' @param lags_criterion NaN or character. NaN means that the number of lags
#'         will be given at \emph{lags_endog_lin}. Possible lag length criteria are 'AICc', 'AIC' or 'BIC'.
#'         Note that when \emph{use_twosls = TRUE}, the lag lengths are chosen based on normal OLS regressions, without using the instruments.
#' @param max_lags NaN or integer. Maximum number of lags if \emph{lags_criterion} is a character denoting the lag length criterion. NaN otherwise.
#' @param trend Integer. No trend =  0 , include trend = 1, include trend and quadratic trend = 2.
#' @param confint Double. Width of confidence bands. 68\% = 1; 90\% = 1.65; 95\% = 1.96.
#' @param use_nw Boolean. Use Newey-West (1987) standard errors for impulse responses? TRUE (default) or FALSE.
#' @param nw_lag Integer. Specifies the maximum lag with positive weight for the Newey-West estimator. If set to NULL (default), the lag increases with
#'               with the number of horizon.
#' @param nw_prewhite Boolean. Should the estimators be pre-whitened? TRUE of FALSE (default).
#' @param adjust_se Boolen. Should a finite sample adjsutment be made to the covariance matrix estimators? TRUE or FALSE (default).
#' @param hor Integer. Number of horizons for impulse responses.
#' @param num_cores NULL or Integer. The number of cores to use for the estimation. If NULL, the function will
#'                  use the maximum number of cores minus one.
#'
#' @seealso \url{https://adaemmerp.github.io/lpirfs/README_docs.html}
#'
#' @return A list containing:
#'
#'
#'
#'\item{irf_lin_mean}{A \link{matrix}, containing the impulse responses.
#'                    The row in each matrix denotes the response of the \emph{ith}
#'                    variable to the shock. The columns are the horizons.}
#'
#'\item{irf_lin_low}{A \link{matrix}, containing all lower confidence bands of
#'                    the impulse responses, based on robust standard errors by Newey and West (1987).
#'                    Properties are equal to \emph{irf_lin_mean}.}
#'
#'\item{irf_lin_up}{A \link{matrix}, containing all upper confidence bands of
#'                    the impulse responses, based on robust standard errors by Newey and West (1987).
#'                    Properties are equal to \emph{irf_lin_mean}.}
#'
#'\item{specs}{A list with properties of \emph{endog_data} for the plot function. It also contains
#'             lagged data (y_lin and x_lin) used for the estimations of the impulse responses.}
#'
#'
#'
#' @export
#' @references
#' Akaike, H. (1974). "A new look at the statistical model identification", \emph{IEEE Transactions on Automatic Control}, 19 (6): 716–723.
#'
#' Auerbach, A. J., and  Gorodnichenko, Y. (2012). "Measuring the Output Responses to Fiscal Policy."
#' \emph{American Economic Journal: Economic Policy}, 4 (2): 1-27.
#'
#' Blanchard, O., and Perotti, R. (2002). “An Empirical Characterization of the
#' Dynamic Effects of Changes in Government Spending and Taxes on Output.” \emph{Quarterly
#' Journal of Economics}, 117(4): 1329–1368.
#'
#' Hurvich, C. M., and  Tsai, C.-L. (1989), "Regression and time series model selection in small samples",
#' \emph{Biometrika}, 76(2): 297–307
#'
#' Jordà, Ò. (2005). "Estimation and Inference of Impulse Responses by Local Projections."
#' \emph{American Economic Review}, 95 (1): 161-182.
#'
#' Jordà, Ò, Schularick, M., Taylor, A.M. (2015), "Betting the house", \emph{Journal of International Economics},
#' 96, S2-S18.
#'
#' Newey, W.K., and West, K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
#' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55: 703–708.
#'
#' Ramey, V.A., and Zubairy, S. (2018). "Government Spending Multipliers in Good Times
#' and in Bad: Evidence from US Historical Data." \emph{Journal of Political Economy},
#' 126(2): 850 - 901.
#'
#' Schwarz, Gideon E. (1978). "Estimating the dimension of a model", \emph{Annals of Statistics}, 6 (2): 461–464.
#'
#'@author Philipp Adämmer
#'@import foreach
#'@examples
#'\donttest{
#'
#'# This example replicates a result from the Supplementary Appendix
#'# by Ramey and Zubairy (2018) (RZ-18)
#'
#'# Load data
#'  ag_data       <- ag_data
#'  sample_start  <- 7
#'  sample_end    <- dim(ag_data)[1]
#'
#'# Endogenous data
#'  endog_data    <- ag_data[sample_start:sample_end,3:5]
#'
#'# Variable to shock with. Here government spending due to
#'# Blanchard and Perotti (2002) framework
#'  shock         <- ag_data[sample_start:sample_end, 3]
#'
#'# Estimate linear model
#'  results_lin_iv <- lp_lin_iv(endog_data,
#'                                lags_endog_lin = 4,
#'                                shock          = shock,
#'                                trend          = 0,
#'                                confint        = 1.96,
#'                                hor            = 20)
#'
#'# Show all impulse responses
#'  plot(results_lin_iv)
#'
#'# Make and save plots
#'  iv_lin_plots    <- plot_lin(results_lin_iv)
#'
#'# * The first element of 'iv_lin_plots' shows the response of the first
#'#   variable (Gov) to the  shock (Gov).
#'# * The second element of 'iv_lin_plots' shows the response of the second
#'#   variable (Tax) to the shock (Gov).
#'# * ...
#'
#'# This plot replicates the left plot in the mid-panel of Figure 12 in the
#'# Supplementary Appendix by RZ-18.
#'  iv_lin_plots[[1]]
#'
#'
#'# Show diagnostics. The first element shows the reaction of the first given endogenous variable.
#'  summary(results_lin_iv)
#'
#'
#'## Add lags of the identified shock ##
#'
#'# Endogenous data but now exclude government spending
#'  endog_data    <- ag_data[sample_start:sample_end, 4:5]
#'
#'# Variable to shock with (government spending)
#'  shock         <- ag_data[sample_start:sample_end, 3]
#'
#'# Add the shock variable to exogenous data
#'  exog_data     <- shock
#'
#'# Estimate linear model with lagged shock variable
#'  results_lin_iv <- lp_lin_iv(endog_data,
#'                                lags_endog_lin = 4,
#'                                shock          = shock,
#'                                exog_data      = exog_data,
#'                                lags_exog      = 2,
#'                                trend          = 0,
#'                                confint        = 1.96,
#'                                hor            = 20)
#'
#'
#'# Show all responses
#'  plot(results_lin_iv)
#'
#'# Show diagnostics. The first element shows the reaction of the first endogenous variable.
#'  summary(results_lin_iv)
#'
#'
#'##############################################################################
#'#####                         Use 2SLS                               #########
#'##############################################################################
#'
#'# Set seed
#'  set.seed(007)
#'
#'# Load data
#'  ag_data       <- ag_data
#'  sample_start  <- 7
#'  sample_end    <- dim(ag_data)[1]
#'
#'# Endogenous data
#'  endog_data    <- ag_data[sample_start:sample_end,3:5]
#'
#'# Variable to shock with (government spending)
#'  shock         <- ag_data[sample_start:sample_end, 3]
#'
#'# Generate instrument variable that is correlated with government spending
#'  instrum       <- as.data.frame(0.9*shock$Gov + rnorm(length(shock$Gov), 0, 0.02) )
#'
#'# Estimate linear model via 2SLS
#'  results_lin_iv <- lp_lin_iv(endog_data,
#'                             lags_endog_lin = 4,
#'                             shock          = shock,
#'                             instrum        = instrum,
#'                             use_twosls     = TRUE,
#'                             trend          = 0,
#'                             confint        = 1.96,
#'                             hor            = 20)
#'
#'# Show all responses
#'  plot(results_lin_iv)
#'
#' }
#'
#'
lp_lin_iv <- function(endog_data,
                   shock          = NULL,
                   instr          = NULL,
                   use_twosls     = FALSE,
                   instrum        = NULL,
                   lags_endog_lin = NULL,
                   exog_data      = NULL,
                   lags_exog      = NULL,
                   contemp_data   = NULL,
                   lags_criterion = NaN,
                   max_lags       = NaN,
                   trend          = NULL,
                   confint        = NULL,
                   use_nw         = TRUE,
                   nw_lag         = NULL,
                   nw_prewhite    = FALSE,
                   adjust_se      = FALSE,
                   hor            = NULL,
                   num_cores      = NULL){

  # Give warning if 'instr' is used as input name
  if(!is.null(instr)){
    shock <- instr
    warning("'instr' is a deprecated input name. Use 'shock' instead.")
  }

  # Check whether data is a data.frame
  if(!(is.data.frame(endog_data))){
    stop('The data has to be a data.frame().')
  }

  # Check whether data is a data.frame
  if(is.nan(lags_endog_lin) & !is.character(lags_criterion)){
    stop('"lags_endog_lin" can only be NaN if a lag length criterion is given.')
  }

  # Check whether instrument for shock is given
  if(is.null(shock)){
    stop('You have to provide an instrument to shock with.')
  }

  # Check whether instrument for shock is given
  if(!is.data.frame(shock)){
    stop('The instrument has to be given as a data.frame().')
  }

  # Check whether exogenous data is a data.frame
  if(!is.null(exog_data) & !is.data.frame(exog_data)){
    stop('Exogenous data has to be given as a data.frame.')
  }

  # Check whether lag length for exogenous data is given
  if(!is.null(exog_data) & is.null(lags_exog)){
    stop('Please provide a lag length for the exogenous data.')
  }

  # Check whether 'lags_criterion' is correctly specified
  if(is.null(lags_criterion)){
    stop('"lags_criterion" has to be NaN or a character, specifying the lag length criterion.')
  }


  # Give error when no trend is given
  if(is.null(trend)){
    stop('Please specify whether and which type of trend to include.')
  }


  # Check whether width for confidence intervals is given
  if(is.null(confint)){
    stop('Please specify a value for the width of the confidence bands.')
  }

  # Check whether number of horizons is given
  if(is.null(hor)){
    stop('Please specify the number of horizons.')
  }

  # Check whether wrong lag length criterion is given
  if(!(is.nan(lags_criterion)          | lags_criterion == 'AICc'|
       lags_criterion         == 'AIC' | lags_criterion == 'BIC')){
    stop('Possible lag length criteria are AICc, AIC or BIC. NaN if lag length is specified.')
  }

  # Check whether lags criterion and maximum number of lags are given
  if((is.character(lags_criterion)) &
     (!is.na(lags_endog_lin))){
    stop('You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.
         Please set lags_endog_lin to NaN if you want to use a lag length criterion.')
  }

  # Check whether values for horizons are correct
  if(!(hor > 0) | is.nan(hor) | !(hor %% 1 == 0)){
    stop('The number of horizons has to be an integer and > 0.')
  }

  # Check whether trend is correctly specified
  if(!(trend %in% c(0,1,2))){
    stop('For trend please enter 0 = no trend, 1 = trend, 2 = trend and quadratic trend.')
  }


  # Check whether width of confidence bands is >=0
  if(!(confint >=0)){
    stop('The width of the confidence bands has to be >=0.')
  }


  # Give error when use_twosls = T but instrum = NULL
  if(isTRUE(use_twosls) & is.null(instrum)){
    stop('Please specify at least one instrument to use for 2SLS.')
  }




  # Create list to store inputs
  specs <- list()

  # Specify inputs
  specs$shock              <- shock
  specs$use_twosls         <- use_twosls
  specs$instrum            <- instrum
  specs$lags_endog_lin     <- lags_endog_lin
  specs$exog_data          <- exog_data
  specs$lags_exog          <- lags_exog
  specs$contemp_data       <- contemp_data
  specs$lags_criterion     <- lags_criterion
  specs$max_lags           <- max_lags
  specs$trend              <- trend
  specs$confint            <- confint
  specs$hor                <- hor

  specs$use_nw             <- use_nw
  specs$nw_prewhite        <- nw_prewhite
  specs$adjust_se          <- adjust_se
  specs$nw_lag             <- nw_lag


  specs$model_type         <- 1


# Function start

# Safe data frame specifications in 'specs for functions
  specs$starts         <- 1                       # Sample Start
  specs$ends           <- dim(endog_data)[1]      # Sample end
  specs$column_names   <- names(endog_data)       # Name endogenous variables
  specs$endog          <- ncol(endog_data)        # Set the number of endogenous variables

# Construct (lagged) endogenous data
  data_lin <- create_lin_data(specs, endog_data)

  y_lin    <- data_lin[[1]]
  x_lin    <- data_lin[[2]]
  z_lin    <- data_lin[[3]]

# Save endogenous and lagged exogenous data in specs
  specs$y_lin  <- y_lin
  specs$x_lin  <- x_lin
  specs$z_lin  <- z_lin


# Matrices to store OLS parameters
  b1        <- matrix(NaN, specs$endog, specs$endog)
  b1_low    <- matrix(NaN, specs$endog, specs$endog)
  b1_up     <- matrix(NaN, specs$endog, specs$endog)

# Matrices to store irfs for each horizon
  irf_mean  <-  matrix(NaN, 1, specs$hor)
  irf_low   <-  irf_mean
  irf_up    <-  irf_mean

# 3D Arrays for all irfs
  irf_lin_mean  <-  matrix(NaN, nrow = specs$endog, ncol = specs$hor)
  irf_lin_low   <-  irf_lin_mean
  irf_lin_up    <-  irf_lin_mean

# Make matrix to store OLS diagnostics for each endogenous variable k
  diagnost_ols_each_h           <- matrix(NaN, specs$hor, 4)
  rownames(diagnost_ols_each_h) <- paste("h", 1:specs$hor, sep = " ")
  colnames(diagnost_ols_each_h) <- c("R-sqrd.", "Adj. R-sqrd.", "F-stat", " p-value")

# Make cluster
  if(is.null(num_cores)){
    num_cores    <- min(specs$endog, parallel::detectCores() - 1)
  }

  cl             <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)

# Decide whether lag lengths are given or have to be estimated
if(is.nan(specs$lags_criterion) == TRUE){

  # Loops to estimate local projections
  lin_irfs <- foreach(s         = 1:specs$endog,
                      .packages = 'lpirfs')  %dopar%{ # Accounts for the reaction of the endogenous variable

                        for (h in 1:(specs$hor)){   # Accounts for the horizons

                          # Create data
                          yy  <-   y_lin[h : dim(y_lin)[1], ]
                          xx  <-   x_lin[1 : (dim(x_lin)[1] - h + 1), ]

                          # Check whether data are matrices to correctly extract values
                          if(!is.matrix(xx)){
                             xx  <- as.matrix(xx)
                          }

                          if(!is.matrix(yy)){
                             yy  <- matrix(yy)
                          }

                          # Set lag number for Newey-West (1987)
                          if(is.null(nw_lag)){

                            lag_nw <- h

                             } else {

                            lag_nw <- nw_lag

                          }

                            # Check whether use OLS or 2sls
                            if(specs$use_twosls == FALSE){

                              # Get standard errors and point estimates
                              get_ols_vals <- lpirfs::get_std_err(yy, xx, lag_nw, s, specs)
                              std_err      <- get_ols_vals[[1]]
                              b            <- get_ols_vals[[2]]

                              # Get diagnostocs for summary
                              get_diagnost                  <- lpirfs::ols_diagnost(yy[, s], xx)
                              diagnost_ols_each_h[h, 1]     <- get_diagnost[[3]]
                              diagnost_ols_each_h[h, 2]     <- get_diagnost[[4]]
                              diagnost_ols_each_h[h, 3]     <- get_diagnost[[5]]
                              diagnost_ols_each_h[h, 4]     <- stats::pf(get_diagnost[[5]], get_diagnost[[6]], get_diagnost[[7]], lower.tail = F)

                                         }   else   {

                               # Extract instrument matrix and save as matrix
                                zz <- specs$z_lin[1 : (dim(z_lin)[1] - h + 1), ] %>%
                                      as.matrix()


                              get_tsls_vals  <-   get_std_err_tsls(yy, xx, lag_nw, s, zz,  specs)
                              b              <-   get_tsls_vals[[2]]
                              std_err        <-   get_tsls_vals[[1]]


                              # Get diagnostocs for summary
                              get_diagnost                  <- lpirfs::ols_diagnost(yy[, s], xx)
                              diagnost_ols_each_h[h, 1]     <- get_diagnost[[3]]
                              diagnost_ols_each_h[h, 2]     <- get_diagnost[[4]]
                              diagnost_ols_each_h[h, 3]     <- get_diagnost[[5]]
                              diagnost_ols_each_h[h, 4]     <- stats::pf(get_diagnost[[5]], get_diagnost[[6]], get_diagnost[[7]], lower.tail = F)


                            }

                            # Fill matrices with local projections
                            irf_mean[1, h]  <-  b[2]
                            irf_low[1,  h]  <-  b[2] - std_err[2]
                            irf_up[1,   h]  <-  b[2] + std_err[2]


                        }

                        # Return irfs
                        return(list(irf_mean,  irf_low,  irf_up, diagnost_ols_each_h))
                      }



  # List for OLS diagnostics
  diagnostic_list             <- list()

  # Fill arrays with irfs
  for(i in 1:specs$endog){

    irf_lin_mean[i , ]   <- as.matrix(do.call(rbind, lin_irfs[[i]][1]))
    irf_lin_low[i  , ]   <- as.matrix(do.call(rbind, lin_irfs[[i]][2]))
    irf_lin_up[i   , ]   <- as.matrix(do.call(rbind, lin_irfs[[i]][3]))

    diagnostic_list[[i]] <-  lin_irfs[[i]][[4]]

  }

  # Name the list of diagnostics
   names(diagnostic_list)  <- paste("Endog. Variable:", specs$column_names , sep = " ")


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
                      .packages   = 'lpirfs')  %dopar% { # Accounts for the reaction of the endogenous variable

                        for (h in 1:specs$hor){     # Accounts for the horizon

                          # Set lag number for Newey-West (1987)
                          if(is.null(nw_lag)){

                            lag_nw <- h

                          } else {

                            lag_nw <- nw_lag

                          }

                            # Find optimal lags
                            n_obs         <- nrow(y_lin[[1]]) - h + 1 # Number of observations for model with lag one
                            val_criterion <- lpirfs::get_vals_lagcrit(y_lin, x_lin, lag_crit, lag_nw, s,
                                                                      specs$max_lags, n_obs)

                            # Set optimal lag length
                            lag_choice  <- which.min(val_criterion)

                            # Extract matrices based on optimal lag length
                            yy <- y_lin[[lag_choice]][, s]
                            yy <- yy[h: length(yy)]

                            xx <- x_lin[[lag_choice]]
                            xx <- xx[1:(dim(xx)[1] - h + 1),]

                            # Check whether use OLS or 2sls
                            if(specs$use_twosls == FALSE){

                              # Get standard errors and point estimates
                              get_ols_vals <- lpirfs::get_std_err(yy, xx, lag_nw, 1, specs)
                              std_err      <- get_ols_vals[[1]]
                              b            <- get_ols_vals[[2]]

                              # Get diagnostocs for summary
                              get_diagnost                  <- lpirfs::ols_diagnost(yy, xx)
                              diagnost_ols_each_h[h, 1]     <- get_diagnost[[3]]
                              diagnost_ols_each_h[h, 2]     <- get_diagnost[[4]]
                              diagnost_ols_each_h[h, 3]     <- get_diagnost[[5]]
                              diagnost_ols_each_h[h, 4]     <- stats::pf(get_diagnost[[5]], get_diagnost[[6]], get_diagnost[[7]], lower.tail = F)


                                        } else {

                                # Extract instrument matrix z_lin
                                zz         <- z_lin[[lag_choice]]
                                zz         <- zz[1:(dim(zz)[1] - h + 1),]


                                get_tsls_vals  <-   get_std_err_tsls(yy, xx, lag_nw, 1, zz,  specs)
                                b              <-   get_tsls_vals[[2]]
                                std_err        <-   get_tsls_vals[[1]]

                                # Get diagnostocs for summary
                                get_diagnost                  <- lpirfs::ols_diagnost(yy, xx)
                                diagnost_ols_each_h[h, 1]     <- get_diagnost[[3]]
                                diagnost_ols_each_h[h, 2]     <- get_diagnost[[4]]
                                diagnost_ols_each_h[h, 3]     <- get_diagnost[[5]]
                                diagnost_ols_each_h[h, 4]     <- stats::pf(get_diagnost[[5]], get_diagnost[[6]], get_diagnost[[7]], lower.tail = F)




                            }

                            # Fill matrices with local projections
                            irf_mean[1, h]  <-  b[2]
                            irf_low[1,  h]  <-  b[2] - std_err[2]
                            irf_up[1,   h]  <-  b[2] + std_err[2]

                        }

                        return(list(irf_mean,  irf_low,  irf_up, diagnost_ols_each_h))
                      }

  # Fill list with all OLS diagnostics
  diagnostic_list             <- list()


  # Fill arrays with irfs
  for(i in 1:specs$endog){

    # Fill irfs
    irf_lin_mean[i , ]   <- as.matrix(do.call(rbind, lin_irfs[[i]][1]))
    irf_lin_low[i  , ]   <- as.matrix(do.call(rbind, lin_irfs[[i]][2]))
    irf_lin_up[i   , ]   <- as.matrix(do.call(rbind, lin_irfs[[i]][3]))


    diagnostic_list[[i]] <-  lin_irfs[[i]][[4]]

  }

  # Name the list of diagnostics
  names(diagnostic_list)  <- paste("Endog. Variable:", specs$column_names , sep = " ")

}

# Close cluster
parallel::stopCluster(cl)

result <- list(irf_lin_mean      = irf_lin_mean,
               irf_lin_low       = irf_lin_low,
               irf_lin_up        = irf_lin_up,
               diagnostic_list   = diagnostic_list,
               specs             = specs)

# Give object S3 name
class(result) <- "lpirfs_lin_iv_obj"
return(result)


}

