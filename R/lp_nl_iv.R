#' @name lp_nl_iv
#' @title Compute nonlinear impulse responses with instrument variable approach
#' @description Compute nonlinear impulse responses with local projections by Jordà (2005) and impulse variable approach as in
#' Ramey and Zubairy (2018). The can be separated into two states via a smooth transition function, applied in Auerbach and Gorodnichenko (2012).
#'
#' @param endog_data A \link{data.frame} containing all endogenous variables for the VAR. The column order
#'                     is used for the Cholesky decomposition.
#' @param lags_nl NaN or integer. Number of lags for nonlinear VAR (if \emph{lags_criterion} = NaN). NaN if lag length criterion is given.
#' @param instr One column \link{data.frame} including the instrument to shock with.
#'              The row length has to be the same as \emph{endog_data}.
#' @param exog_data Null or a \link{data.frame}, containing exogenous data. The row length has to be the same as \emph{endog_data}.
#'                  The exogenous variables will not be separated into two regimes as the endogenous variables.
#' @param lags_exog Null or an integer, indicating the number of lags for exogenous data.
#' @param contemp_data A \link{data.frame}, containing exogenous data with contemporaneous impact. This data will not be lagged.
#'                      The row length has to be the same as \emph{endog_data}.
#' @param lags_criterion NaN or character. NaN means that the number of lags
#'         will be given at \emph{lags_nl} and \emph{lags_lin}. The lag length criteria are 'AICc', 'AIC' and 'BIC'.
#' @param max_lags NaN or integer. Maximum number of lags (if \emph{lags_criterion} = 'AICc', 'AIC', 'BIC'). NaN otherwise.
#' @param trend Integer. Include no trend =  0 , include trend = 1, include trend and quadratic trend = 2.
#' @param shock_type Integer. Standard deviation shock = 0, unit shock = 1.
#' @param confint Double. Width of confidence bands. 68\% = 1; 90\% = 1.65; 95\% = 1.96.
#' @param hor Integer. Number of horizons for impulse responses.
#' @param switching Numeric vector. A column vector with the same length as \emph{endog_data}. This series can either
#'               be decomposed via the Hodrick-Prescott filter (see Auerbach and Gorodnichenko, 2013) or
#'               directly plugged into the following smooth transition function:
#'               \deqn{ F_{z_t} = \frac{exp(-\gamma z_t)}{1 + exp(-\gamma z_t)} }
#'               Warning: \eqn{F_{z_t}} will be lagged in \link{create_nl_data} by one and then multiplied with the data.
#'               If the variable shall not be lagged, the vector has to be given with a lead of one.
#'               The data for the two regimes are: \cr
#'               Regime 1 = (1-\eqn{F(z_{t-1})})*y_{t-p}, \cr
#'               Regime 2 = \eqn{F(z_{t-1})}*y_{t-p}.
#'@param gamma Double. Positive number which is used in the transition function.
#'@param use_hp Integer. No HP-filter = 0. Use HP-filter = 1.
#'@param lambda Double. Value of \eqn{\lambda} for the Hodrick-Prescott filter if HP-filter is applied.
#'@param num_cores Integer. The number of cores to use for the estimation. If no number is set, the function will
#'                 use the maximum number of available cores less one.
#'@seealso \url{https://adaemmerp.github.io/lpirfs/README_docs.html}
#'
#'@return results_list A list with impulse responses and their robust confidence bands.
#' It also contains a list named \emph{specs} with properties of \emph{endog_data} for the plot function and the computed
#' lagged matrices.
#'
#'\item{irf_s1_mean}{A \link{matrix} containing the impulse responses of the first regime.
#'                    The row in each matrix denotes the responses of the \emph{ith}
#'                    variable to the instrument shock. The columns are the horizons.}
#'
#'\item{irf_s1_low}{A \link{matrix} containing all lower confidence bands of
#'                    the impulse responses, based on robust standard errors by Newey and West (1987).
#'                    Properties are equal to \emph{irf_s1_mean}.}
#'
#'\item{irf_s1_up}{A \link{matrix} containing all upper confidence bands of the
#'                    impulse responses, based on robust standard errors by Newey and West (1987).
#'                    Properties are equal to \emph{irf_s1_mean}.}
#'
#'\item{irf_s2_mean}{A \link{matrix} containing all impulse responses for the second regime.
#'                    The row in each matrix denotes the responses of the \emph{ith} variableto the shock.
#'                    The columns denote the horizon.
#'
#'\item{irf_s2_low}{A three \link{matrix} containing all lower confidence bands of the responses,
#'                    based on robust standard errors by Newey and West (1987). Properties are equal to \emph{irf_s2_mean}.}
#'
#'\item{irf_s2_up}{A three \link{matrix}, containing all upper confidence bands of the responses, based on
#'                    robust standard errors by Newey and West (1987). Properties are equal to \emph{irf_s2_mean}.}
#'
#'\item{specs}{A list with properties of \emph{endog_data} for the plot function.}
#'
#'\item{fz}{A vector containing the values of the transition function F(z_{t-1}).}
#'
#' @export
#'
#' @references
#'
#' Akaike, H. (1974). "A new look at the statistical model identification", \emph{IEEE Transactions on Automatic Control}, 19 (6): 716–723.
#'
#' Auerbach, A. J., and  Gorodnichenko Y. (2012). "Measuring the Output Responses to Fiscal Policy."
#' \emph{American Economic Journal: Economic Policy}, 4 (2): 1-27.
#'
#' Auerbach, A. J., and Gorodnichenko Y. (2013). "Fiscal Multipliers in Recession and Expansion."
#' \emph{NBER Working Paper Series}. Nr 17447.
#'
#' Hurvich, C. M., and Tsai, C.-L. (1993) “A Corrected Akaike Information Criterion for
#' Vector Autoregressive Model Selection.” \emph{Journal of Time Series Analysis}, 1993, 14(3):
#' 271–79.
#'
#' Jordà, Ò. (2005) "Estimation and Inference of Impulse Responses by Local Projections."
#' \emph{American Economic Review}, 95 (1): 161-182.
#'
#' Newey W.K., and West K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
#' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55, 703–708.
#'
#' Ramey, V.A., Zubairy, S. (2018). "Government Spending Multipliers in Good Times
#' and in Bad: Evidence from US Historical Data." \emph{Journal of Political Economy},
#' 126(2): 850 - 901.
#'
#' Schwarz, Gideon E. (1978). "Estimating the dimension of a model", \emph{Annals of Statistics}, 6 (2): 461–464.
#'
#' @import foreach
#' @examples
#'\donttest{
#'
#'# This example replicates results from the Supplementary Appendix
#'# by Ramey and Zubairy (2018) (RZ-18). It evaluates results from Auerbach and
#'# Gorodnichenko (2012) (AG-12) with local projections by Jordá (2005).
#'# The data is taken from \url{https://www.journals.uchicago.edu/doi/10.1086/696277}{JoPE}
#'
#'
#'# Load and prepare data
#'# The sample length of RZ-2018 is 1948:III-2008:III
#'  ag_data           <- ag_data
#'  sample_start      <- 7
#'  sample_end        <- dim(ag_data)[1]
#'  endog_data        <- ag_data[sample_start:sample_end, 3:5]
#'
#'# The shock is created by RZ-18 and available as supplementary data
#'# to their paper
#'  shock             <- ag_data[sample_start:sample_end, 7]
#'
#'# AG-12 also include four lags of the 7-quarter moving average growth rate
#'# as exogenous regressors in their model (see RZ-18)
#'  exog_data         <- ag_data[sample_start:sample_end, 6]
#'
#'# Choose the 7-quarter moving average growth rate as switching variable.
#'# and adjust it to have suffiently long recession periods.
#'  switching_variable <- ag_data$GDP_MA[sample_start:sample_end] - 0.8
#'
#'# Estimate local projections
#'  results_nl_iv <- lp_nl_iv(endog_data,
#'                            lags_nl           = 3,
#'                            instr             = shock,
#'                            exog_data         = exog_data,
#'                            lags_exog         = 4,
#'                            contemp_data      = NULL,
#'                            lags_criterion     = NaN,
#'                            max_lags          = NaN,
#'                            trend             = 0,
#'                            shock_type        = 1,
#'                            confint           = 1.96,
#'                            hor               = 20,
#'                            switching         = switching_variable,
#'                            use_hp            = 0,
#'                            lambda            = NaN, # Ravn and Uhlig (2002):
#'                                                     # Anuual data    = 6.25
#'                                                     # Quarterly data = 1600
#'                                                     # Monthly data   = 129,600
#'                            gamma             = 3)
#'
#'# Make and save plots
#'  plots_nl_iv <- plot_nl(results_nl_iv)
#'
#'# Show single impulse responses
#'# Compare with Figure 12 from Supplementary Appendix of RZ-18.
#'  plot(plots_nl_iv$gg_s1[[1]])
#'  plot(plots_nl_iv$gg_s2[[1]])
#'
#'# Prepare to show all plots
#'  library(ggpubr)
#'  library(gridExtra)
#'
#'  s1_plots <- sapply(plots_nl_iv$gg_s1, ggplotGrob)
#'  s2_plots <- sapply(plots_nl_iv$gg_s2, ggplotGrob)
#'
#'# Show all responses of state 1
#'  marrangeGrob(s1_plots, nrow = ncol(endog_data), ncol = 1, top = NULL)
#'
#'# Show all responses of state 2
#'  marrangeGrob(s2_plots, nrow = ncol(endog_data), ncol = 1, top = NULL)
#'
#'}
#'@author Philipp Adämmer
#'
lp_nl_iv <- function(endog_data,
                            lags_nl           = NULL,
                            instr             = NULL,
                            exog_data         = NULL,
                            lags_exog         = NULL,
                            contemp_data      = NULL,
                            lags_criterion    = NULL,
                            max_lags          = NULL,
                            trend             = NULL,
                            shock_type        = NULL,
                            confint           = NULL,
                            hor               = NULL,
                            switching         = NULL,
                            use_hp            = NULL,
                            lambda            = NULL,
                            gamma             = NULL,
                            num_cores         = NULL){

  # Check whether data is a data.frame
  if(!(is.data.frame(endog_data))){
    stop('The endogenous data has to be a data.frame.')
  }


  if(!is.null(exog_data) & !(is.data.frame(exog_data))){
    stop('The exogenous data has to be a data.frame.')
  }


  if(!is.null(contemp_data) & !(is.data.frame(contemp_data))){
    stop('The exogenous data with contemporary impact has to be a data.frame.')
  }

  # Give message when no linear model is provided
  if(is.null(exog_data)){
    message('You estimate the model without exogenous data.')
  }

  # Give message when no contemporaneous data is provided
  if(is.null(contemp_data)){
    message('You estimate the model without exogenous data with contemporaneous impact')
  }



  # Check whether 'trend' is given
  if(is.null(trend) == TRUE){
    stop('Please specify whether and which type of trend to include.')
  }

  # Check whether trend is correctly specified
  if(!(trend %in% c(0,1,2))){
    stop('For trend please set 0 = no trend, 1 = trend, 2 = trend and quadratic trend.')
  }


  # Check whether 'shock_type' is given
  if(is.null(shock_type) == TRUE){
    stop('Please specify which type of shock to use.')
  }

  # Check whether shock type is correctly specified
  if(!(shock_type %in% c(0,1))){
    stop('The shock_type has to be 0 = standard deviation shock or 1 = unit shock.')
  }


  # Check whether switching variable is given
  if(is.null(switching) == TRUE){
    stop('Please provide a switching variable.')
  }

  # Check whether 'use_hp' is given
  if(is.null(use_hp) == TRUE){
    stop('Please specify whether to use the HP-filter for the switching variable.')
  }

  # Check whether lambda is given if 'use_hp == 1'
  if((use_hp == 1) &
     (is.null(lambda) == TRUE)){
    stop('Please specify lambda for the HP-filter.')
  }

  # Check whether 'gamma' is given
  if(is.null(gamma) == TRUE){
    stop('Please specify gamma for the transition function.')
  }

  # Check whether gamma is positive
  if((gamma < 0)){
    stop('Gamma has to be a positive number.')
  }

  # Check whether 'confint' is given
  if(is.null(confint) == TRUE){
    stop('Please specify a value for the width of the confidence bands.')
  }

  # Check whether width of confidence bands is >=0
  if(!(confint >=0)){
    stop('The width of the confidence bands has to be >=0.')
  }

  # Check whether number of horizons is given
  if(is.null(hor) == TRUE){
    stop('Please specify the number of horizons.')
  }

  # Check whether wrong lag length criterion is given
  if(!(is.nan(lags_criterion)          | lags_criterion == 'AICc'|
       lags_criterion         == 'AIC' | lags_criterion == 'BIC') == TRUE){
    stop('Possible lag length criteria are AICc, AIC or BIC or NaN if lag length is specified.')
  }

  # Check whether lags criterion and fixed number of lags for nonlinear model is given
  if((is.character(lags_criterion) == TRUE) &
     (!is.na(lags_nl) == TRUE)){
    stop('You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.')
  }

  # Check whether maximum number of lags is given for lag length criterion
  if((is.character(lags_criterion)) &
     (is.na(max_lags))){
    stop('Please provide a maximum number of lags for the lag length criterion.')
  }

  # Check whether values for horizons are correct
  if(!(hor > 0) | is.nan(hor) |  !(hor %% 1 == 0)){
    stop('The number of horizons has to be an integer and > 0.')
  }




  # Create list to store inputs
  specs <- list()

  # Specify inputs
  specs$lags_nl               <- lags_nl

  if(is.data.frame(instr)){
     specs$instr   <- instr  }  else {
     specs$instr   <- as.data.frame(instr)
  }

  if(is.null(exog_data) | is.data.frame(exog_data)){
     specs$exog_data    <- exog_data}  else  {
     specs$exog_data    <- as.data.frame(exog_data)
  }

  if(is.null(contemp_data) | is.data.frame(contemp_data)){
     specs$contemp_data <- contemp_data}  else {
     specs$contemp_data <- as.data.frame(contemp_data)
    }


    specs$lags_exog      <- lags_exog
    specs$lags_criterion <- lags_criterion
    specs$max_lags       <- max_lags
    specs$trend          <- trend
    specs$shock_type     <- shock_type
    specs$confint        <- confint
    specs$hor            <- hor
    specs$switching      <- switching
    specs$use_hp         <- use_hp
    specs$lambda         <- lambda
    specs$gamma          <- gamma

    specs$model_type     <- 1


  # Safe data frame specifications in 'specs for functions
    specs$starts         <- 1                       # Sample Start
    specs$ends           <- dim(endog_data)[1]      # Sample end
    specs$column_names   <- names(endog_data)       # Name endogenous variables
    specs$endog          <- ncol(endog_data)        # Set the number of endogenous variables

  # Construct data for nonlinear model
    data_nl <- create_nl_data(specs, endog_data)
    y_nl    <- data_nl[[1]]
    x_nl    <- data_nl[[2]]
    fz      <- data_nl[[3]]

  # Save endogenous and lagged exogenous data in specs
    specs$y_nl        <- y_nl
    specs$x_nl        <- x_nl


    # Matrices to store irfs for each horizon
    irf_temp_s1_mean  <-  matrix(NaN, specs$endog, specs$hor)
    irf_temp_s1_low   <-  irf_temp_s1_mean
    irf_temp_s1_up    <-  irf_temp_s1_mean

    irf_temp_s2_mean  <-  matrix(NaN, specs$endog, specs$hor)
    irf_temp_s2_low   <-  irf_temp_s2_mean
    irf_temp_s2_up    <-  irf_temp_s2_mean

  # Arrays to store irfs
    irf_s1_mean  <-  matrix(NaN, nrow = specs$endog, ncol = specs$hor)
    irf_s1_low   <-  irf_s1_mean
    irf_s1_up    <-  irf_s1_mean

    irf_s2_mean  <-  matrix(NaN, nrow = specs$endog, ncol = specs$hor)
    irf_s2_low   <-  irf_s2_mean
    irf_s2_up    <-  irf_s2_mean

  # Matrices to store OLS parameters for regime 1 & 2
    b1_s1        <- matrix(NaN, specs$endog, specs$endog)
    b1_low_s1    <- matrix(NaN, specs$endog, specs$endog)
    b1_up_s1     <- matrix(NaN, specs$endog, specs$endog)

    b1_s2        <- matrix(NaN, specs$endog, specs$endog)
    b1_low_s2    <- matrix(NaN, specs$endog, specs$endog)
    b1_up_s2     <- matrix(NaN, specs$endog, specs$endog)

  # Define coefficient position to extract regime_1 and regime_2 parameters in loop
    start_nl_s1   <- 2
    end_nl_s1     <- specs$endog + 1
    samp_nl_s1    <- start_nl_s1:end_nl_s1


  # Make cluster
  if(is.null(num_cores)){
  num_cores     <- min(specs$endog, parallel::detectCores() - 1)
  }

  cl            <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)

  # Determine whether manual lag lengths are given or have to be determined
  if(is.nan(specs$lags_criterion) == TRUE) {

    # Loops to estimate local projections
    nl_irfs <- foreach( s        = 1:specs$endog,
                        .packages = 'lpirfs') %dopar%{ # Accounts for the shocks

                          for (h in 1:specs$hor){   # Accounts for the horizons

                            yy  <-   y_nl[h:dim(y_nl)[1], ]
                            xx  <-   x_nl[1:(dim(x_nl)[1] - h + 1), ]

                            for (k in 1:specs$endog){ # Accounts for the reactions of the endogenous variables

                              # Estimate coefficients and newey west std.err
                              if(specs$endog == 1 ){
                                nw_results   <- lpirfs::newey_west(yy, xx, h)
                                                  } else {
                                nw_results   <- lpirfs::newey_west(yy[, k], xx, h)
                                                  }

                              b                <- nw_results[[1]]
                              std_err          <- sqrt(diag(nw_results[[2]]))*specs$confint

                              irf_temp_s1_mean[k, h] <- b[2]
                              irf_temp_s1_low[k,  h] <- b[2] - std_err[2]
                              irf_temp_s1_up[k,   h] <- b[2] + std_err[2]

                              irf_temp_s2_mean[k, h] <- b[3]
                              irf_temp_s2_low[k,  h] <- b[3] - std_err[3]
                              irf_temp_s2_up[k,   h] <- b[3] + std_err[3]
                            }
                          }

                          list(irf_temp_s1_mean, irf_temp_s1_low, irf_temp_s1_up,
                               irf_temp_s2_mean, irf_temp_s2_low, irf_temp_s2_up)

                        }

      # Fill irfs
      irf_s1_mean[ , ]  <- as.matrix(do.call(rbind, nl_irfs[[1]][1]))
      irf_s1_low[ , ]   <- as.matrix(do.call(rbind, nl_irfs[[1]][2]))
      irf_s1_up[ , ]    <- as.matrix(do.call(rbind, nl_irfs[[1]][3]))

      irf_s2_mean[ , ]  <- as.matrix(do.call(rbind, nl_irfs[[1]][4]))
      irf_s2_low[ , ]   <- as.matrix(do.call(rbind, nl_irfs[[1]][5]))
      irf_s2_up[ , ]    <- as.matrix(do.call(rbind, nl_irfs[[1]][6]))

################################################################################
                               } else {
################################################################################

    # Convert lag length criterion to number for Rcpp loop
    lag_crit     <- switch(specs$lags_criterion,
                           'AICc'= 1,
                           'AIC' = 2,
                           'BIC' = 3)

    # --- Loops to estimate local projections.
    nl_irfs <- foreach(s         = 1:specs$endog,
                       .packages = 'lpirfs') %dopar% { # Accounts for shocks

                         for (h in 1:specs$hor){      # Accounts for the horizons
                           for (k in 1:specs$endog){ # Accounts for the reactions of the endogenous variables

                             # Find optimal lag length and select matrices from lists accordingly
                             n_obs           <- nrow(endog_data) - h # Number of maximum observations
                             val_criterion   <- lpirfs::get_vals_lagcrit(y_nl, x_nl, lag_crit, h, k,
                                                                         specs$max_lags, n_obs)

                             lag_choice      <- which.min(val_criterion)

                             yy              <- y_nl[[lag_choice]][, k]
                             yy              <- yy[h: length(yy)]

                             xx              <- x_nl[[lag_choice]]
                             xx              <- xx[1:(dim(xx)[1] - h + 1),]

                             nw_results      <- lpirfs::newey_west(yy, xx, h)

                             b               <- nw_results[[1]]
                             std_err         <- sqrt(diag(nw_results[[2]]))*specs$confint

                             irf_temp_s1_mean[k, h] <- b[2]
                             irf_temp_s1_low[k,  h] <- b[2] - std_err[2]
                             irf_temp_s1_up[k,   h] <- b[2] + std_err[2]

                             irf_temp_s2_mean[k, h] <- b[3]
                             irf_temp_s2_low[k,  h] <- b[3] - std_err[3]
                             irf_temp_s2_up[k,   h] <- b[3] + std_err[3]

                           }
                          }

                         list(irf_temp_s1_mean, irf_temp_s1_low, irf_temp_s1_up,
                              irf_temp_s2_mean, irf_temp_s2_low, irf_temp_s2_up)
                       }

      # Fill irfs
      irf_s1_mean[ , ]  <- as.matrix(do.call(rbind, nl_irfs[[1]][1]))
      irf_s1_low[ , ]   <- as.matrix(do.call(rbind, nl_irfs[[1]][2]))
      irf_s1_up[ , ]    <- as.matrix(do.call(rbind, nl_irfs[[1]][3]))

      irf_s2_mean[ , ]  <- as.matrix(do.call(rbind, nl_irfs[[1]][4]))
      irf_s2_low[ , ]   <- as.matrix(do.call(rbind, nl_irfs[[1]][5]))
      irf_s2_up[ , ]    <- as.matrix(do.call(rbind, nl_irfs[[1]][6]))



  }

  # Close cluster
  parallel::stopCluster(cl)

  results_list <- list(irf_s1_mean = irf_s1_mean, irf_s1_low = irf_s1_low, irf_s1_up = irf_s1_up,
                       irf_s2_mean = irf_s2_mean, irf_s2_low = irf_s2_low, irf_s2_up = irf_s2_up,
                       fz          = fz,
                       specs       = specs)
  return(results_list)

}
