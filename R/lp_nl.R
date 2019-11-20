#' @name lp_nl
#' @title Compute nonlinear impulse responses
#' @description Compute nonlinear impulse responses with local projections by Jordà (2005). The
#' data can be separated into two states by a smooth transition function as applied in Auerbach and Gorodnichenko (2012),
#' or by a simple dummy approach.
#'
#' @param endog_data A \link{data.frame}, containing all endogenous variables for the VAR. The Cholesky decomposition is based on the
#'                   column order.
#' @param lags_criterion NaN or character. NaN (default) means that the number of lags
#'         will be given at \emph{lags_endog_nl} and \emph{lags_endog_lin}. The lag length criteria are 'AICc', 'AIC' and 'BIC'.
#' @param lags_endog_lin NaN or integer. NaN if lag length criterion is used.
#'                                Integer for number of lags for linear VAR to identify shock.
#' @param lags_endog_nl NaN or integer. Number of lags for nonlinear VAR. NaN if lag length criterion is given.
#' @param max_lags NaN or integer. Maximum number of lags (if \emph{lags_criterion} = 'AICc', 'AIC', 'BIC'). NaN (default) otherwise.
#' @param trend Integer. Include no trend =  0 , include trend = 1, include trend and quadratic trend = 2.
#' @param shock_type Integer. Standard deviation shock = 0, unit shock = 1.
#' @param confint Double. Width of confidence bands. 68\% = 1; 90\% = 1.65; 95\% = 1.96.
#' @param use_nw Boolean. Use Newey-West (1987) standard errors for impulse responses? TRUE (default) or FALSE.
#' @param nw_lag Integer. Specifies the maximum lag with positive weight for the Newey-West estimator. If set to NULL (default), the lag increases with
#'               with the number of horizon.
#' @param nw_prewhite Boolean. Should the estimators be pre-whitened? TRUE of FALSE (default).
#' @param adjust_se Boolen. Should a finite sample adjsutment be made to the covariance matrix estimators? TRUE or FALSE (default).
#' @param hor Integer. Number of horizons for impulse responses.
#' @param switching Numeric vector. A column vector with the same length as \emph{endog_data}. If 'use_logistic = TRUE', this series can either
#'               be decomposed via the Hodrick-Prescott filter (see Auerbach and Gorodnichenko, 2013) or
#'               directly plugged into the following logistic function:
#'               \deqn{ F_{z_t} = \frac{exp(-\gamma z_t)}{1 + exp(-\gamma z_t)}. }
#'               Important: \eqn{F_{z_t}} will be lagged by one and then multiplied with the data.
#'               If the variable shall not be lagged, use 'lag_switching = FALSE': \cr
#'               Regime 1 = (1-\eqn{F(z_{t-1})})*y_{(t-p)}, \cr
#'               Regime 2 = \eqn{F(z_{t-1})}*y_{(t-p)}.
#'@param lag_switching Boolean. Use the first lag of the values of the transition function? TRUE (default) or FALSE.
#'@param gamma Double. Positive number which is used in the transition function.
#'@param use_logistic Boolean. Use logistic function to separate states? TRUE (default) or FALSE. If FALSE, the values of the switching variable
#'                     have to be binary (0/1).
#'@param use_hp Boolean. Use HP-filter? TRUE or FALSE.
#'@param lambda Double. Value of \eqn{\lambda} for the Hodrick-Prescott filter (if use_hp = TRUE).
#'@param exog_data A \link{data.frame}, containing exogenous variables for the VAR. The row length has to be the same as \emph{endog_data}.
#'                  Lag lengths for exogenous variables have to be given and will no be determined via a lag length criterion.
#'@param lags_exog Integer. Number of lags for the exogenous variables.
#'@param contemp_data A \link{data.frame}, containing exogenous data with contemporaneous impact. This data will not be lagged.
#'                      The row length has to be the same as \emph{endog_data}.
#'@param num_cores Integer. The number of cores to use for the estimation. If NULL, the function will
#'                 use the maximum number of cores minus one.
#'
#'@seealso \url{https://adaemmerp.github.io/lpirfs/README_docs.html}
#'
#'
#' @return A list containing:
#'
#'\item{irf_s1_mean}{A three 3D \link{array}, containing all impulse responses for all endogenous variables of the first state.
#'                    The last dimension denotes the shock variable. The row in each matrix
#'                    denotes the responses of the \emph{ith} variable, ordered as in \emph{endog_data}. The columns are the horizons.
#'                    For example, if the results are saved in \emph{results_nl}, results_nl$irf_s1_mean[, , 1] returns a KXH matrix,
#'                    where K is the number of variables and H the number of horizons. '1' is the shock variable, corresponding to the
#'                    variable in the first column of \emph{endog_data}.}
#'
#'\item{irf_s1_low}{A three 3D \link{array}, containing all lower confidence bands of the impulse responses, based on
#'                    robust standard errors by Newey and West (1987). Properties are equal to \emph{irf_s1_mean}.}
#'
#'\item{irf_s1_up}{A three 3D \link{array}, containing all upper confidence bands of the impulse responses, based on
#'                    robust standard errors by Newey and West (1987). Properties are equal to \emph{irf_s1_mean}.}
#'
#'\item{irf_s2_mean}{A three 3D \link{array}, containing all impulse responses for all endogenous variables of the second state.
#'                    The last dimension denotes the shock variable. The row in each matrix
#'                    denotes the responses of the \emph{ith} variable, ordered as in endog_data. The columns denote the horizon.
#'                    For example, if the results are saved in \emph{results_nl}, results_nl$irf_s2_mean[, , 1] returns a KXH matrix,
#'                    where K is the number of variables and H the number of horizons. '1' is the first shock variable corresponding to the
#'                    variable in the first column of \emph{endog_data}.}
#'
#'\item{irf_s2_low}{A three 3D \link{array}, containing all lower confidence bands of the responses,
#'                    based on robust standard errors by Newey and West (1987). Properties are equal to \emph{irf_s2_mean}.}
#'
#'\item{irf_s2_up}{A three 3D \link{array}, containing all upper confidence bands of the responses, based on
#'                    robust standard errors by Newey and West (1987). Properties are equal to \emph{irf_s2_mean}.}
#'
#'\item{specs}{A list with properties of \emph{endog_data} for the plot function. It also contains
#'             lagged data (y_nl and x_nl) used for the irf estimations.}
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
#' \emph{NBER Working Paper Series}. Nr. 17447.
#'
#' Hurvich, C. M., and Tsai, C.-L. (1989), "Regression and time series model selection in small samples",
#' \emph{Biometrika}, 76(2): 297–307
#'
#' Jordà, Ò. (2005) "Estimation and Inference of Impulse Responses by Local Projections."
#' \emph{American Economic Review}, 95 (1): 161-182.
#'
#' Newey, W.K., and West, K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
#' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55, 703–708.
#'
#' Schwarz, Gideon E. (1978). "Estimating the dimension of a model", \emph{Annals of Statistics}, 6 (2): 461–464.
#'
#' Ravn, M.O., Uhlig, H. (2002). "On Adjusting the Hodrick-Prescott Filter for the Frequency of Observations."
#' \emph{Review of Economics and Statistics}, 84(2), 371-376.
#'
#' @import foreach
#' @examples
#'\donttest{
#'                   ## Example without exogenous variables ##
#'
#'# Load package
#'   library(lpirfs)
#'   library(ggpubr)
#'   library(gridExtra)
#'
#'# Load (endogenous) data
#'   endog_data <- interest_rules_var_data
#'
#'# Choose data for switching variable (here Federal Funds Rate)
#'# Important: The switching variable does not have to be used within the VAR!
#'  switching_data <-  endog_data$Infl
#'
#'# Estimate model and save results
#'   results_nl    <- lp_nl(endog_data,
#'                                 lags_endog_lin  = 4,
#'                                 lags_endog_nl   = 3,
#'                                 trend           = 0,
#'                                 shock_type      = 1,
#'                                 confint         = 1.96,
#'                                 hor             = 24,
#'                                 switching       = switching_data,
#'                                 use_hp          = TRUE,
#'                                 lambda          = 1600,
#'                                 gamma           = 3)
#'
#'# Show all plots
#'  plot(results_nl)
#'
#'# Make and save all plots
#'   nl_plots <- plot_nl(results_nl)
#'
#'# Save plots based on states
#'   s1_plots <- sapply(nl_plots$gg_s1, ggplotGrob)
#'   s2_plots <- sapply(nl_plots$gg_s2, ggplotGrob)
#'
#'# Show first irf of each state
#'   plot(s1_plots[[1]])
#'   plot(s2_plots[[1]])
#'
#'# Show diagnostics. The first element correponds to the first shock variable.
#'  summary(results_nl)
#'
#'
#'                      ## Example with exogenous variables ##
#'
#'# Load (endogenous) data
#'   endog_data <- interest_rules_var_data
#'
#'# Choose data for switching variable (here Federal Funds Rate)
#'# Important: The switching variable does not have to be used within the VAR!
#'  switching_data <-  endog_data$FF
#'
#'# Create exogenous data and data with contemporaneous impact (for illustration purposes only)
#'  exog_data    <- endog_data$GDP_gap*endog_data$Infl*endog_data$FF + rnorm(dim(endog_data)[1])
#'  contemp_data <- endog_data$GDP_gap*endog_data$Infl*endog_data$FF + rnorm(dim(endog_data)[1])
#'
#'# Exogenous data has to be a data.frame
#'  exog_data    <- data.frame(xx  = exog_data)
#'  contemp_data <- data.frame(cc  = contemp_data)
#'
#'# Estimate model and save results
#'  results_nl <- lp_nl(endog_data,
#'                           lags_endog_lin  = 4,
#'                           lags_endog_nl   = 3,
#'                           trend           = 0,
#'                           shock_type      = 1,
#'                           confint         = 1.96,
#'                           hor             = 24,
#'                           switching       = switching_data,
#'                           use_hp          = TRUE,
#'                           lambda          = 1600, # Ravn and Uhlig (2002):
#'                                                   # Anuual data    = 6.25
#'                                                   # Quarterly data = 1600
#'                                                   # Monthly data   = 129 600
#'                           gamma           = 3,
#'                           exog_data       = exog_data,
#'                           lags_exog       = 3)
#'
#'
#'# Show all plots
#'  plot(results_nl)
#'
#'
#'# Show diagnostics. The first element correponds to the first shock variable.
#'  summary(results_nl)
#'
#'
#'
#'}
#' @author Philipp Adämmer
#'
lp_nl <- function(endog_data,
                               lags_endog_lin = NULL,
                               lags_endog_nl  = NULL,
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
                               switching      = NULL,
                               lag_switching  = TRUE,
                               use_logistic   = TRUE,
                               use_hp         = NULL,
                               lambda         = NULL,
                               gamma          = NULL,
                               exog_data      = NULL,
                               lags_exog      = NULL,
                               contemp_data   = NULL,
                               num_cores      = 1){

  # Create list to store inputs
    specs <- list()

  # Specify inputs
    specs$lags_endog_lin <- lags_endog_lin
    specs$lags_endog_nl  <- lags_endog_nl
    specs$lags_criterion <- lags_criterion
    specs$max_lags       <- max_lags
    specs$trend          <- trend
    specs$shock_type     <- shock_type
    specs$confint        <- confint
    specs$hor            <- hor
    specs$switching      <- switching
    specs$lag_switching  <- lag_switching
    specs$use_logistic   <- use_logistic
    specs$use_hp         <- use_hp
    specs$lambda         <- lambda
    specs$gamma          <- gamma
    specs$exog_data      <- exog_data
    specs$lags_exog      <- lags_exog

    specs$use_nw         <- use_nw
    specs$nw_prewhite    <- nw_prewhite
    specs$adjust_se      <- adjust_se
    specs$nw_lag         <- nw_lag


    # Add 'contempranoeus' as NULL for data construction
    specs$contemp_data   <- NULL
    # Set model type for lag construction
    specs$model_type     <- 0

    # Set 2SLS option to FALSE
    specs$use_twosls <- FALSE

#--- Check inputs

  # Check whether data is a data.frame
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

  # Check whether switching variable is given
  if(is.null(specs$switching)){
    stop('Please provide a switching variable.')
  }

  # Check whether 'use_hp' is given
  if(isTRUE(specs$use_logistic) & is.null(specs$use_hp)){
    stop('Please specify whether to use the HP-filter for the switching variable.')
  }

  # Check whether lambda is given if 'use_hp == 1'
  if(isTRUE(specs$use_hp) & (is.null(specs$lambda))){
    stop('Please specify lambda for the HP-filter.')
  }

  # Check whether 'gamma' is given
  if(isTRUE(specs$use_logistic) & is.null(specs$gamma)){
    stop('Please specify gamma for the transition function.')
  }

  # Check whether 'confint' is given
  if(is.null(specs$confint)){
    stop('Please specify a value for the width of the confidence bands.')
  }


  # Check whether number of horizons is given
  if(is.null(specs$hor)){
    stop('Please specify the number of horizons.')
  }

  # Check whether wrong lag length criterion is given
  if(!(is.nan(specs$lags_criterion)          | specs$lags_criterion == 'AICc'|
       specs$lags_criterion         == 'AIC' | specs$lags_criterion == 'BIC') == TRUE){
    stop('Possible lag length criteria are AICc, AIC or BIC or NaN if lag length is specified.')
  }


  # Check whether lags criterion and fixed number of lags for nonlinear model is given
  if((is.character(specs$lags_criterion)) &
     (!is.na(specs$lags_endog_nl))){
    stop('You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.')
  }


  # Check whether lags criterion and fixed number of lags for linear model is given
  if((is.character(specs$lags_criterion)) &
     (!is.na(specs$lags_endog_lin))){
    stop('You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.')
  }


  # Check whether maximum number of lags is given for lag length criterion
  if((is.character(specs$lags_criterion)) &
     (is.na(specs$max_lags))){
    stop('Please provide a maximum number of lags for the lag length criterion.')
  }


  # Check whether lin_lags is given if nl_lags is given
  if((is.numeric(specs$lags_endog_nl)) &
     (is.null(specs$lags_endog_lin))){
    stop('Please provide a lag length for the linear model to identify the shock.')
  }


  # Check whether values for horizons are correct
  if(!(specs$hor > 0) | is.nan(specs$hor) |  !(specs$hor %% 1 == 0)){
    stop('The number of horizons has to be an integer and > 0.')
  }

  # Check whether lags for linear model are integers
  if(is.numeric(specs$lags_endog_nl) & !is.nan(specs$lags_endog_nl)){
    if(!(specs$lags_endog_nl %% 1 == 0) | specs$lags_endog_nl < 0){
      stop('The number of lags have to be a positive integer.')
    }
  } else {}


  # Check whether trend is correctly specified
  if(!(specs$trend %in% c(0,1,2))){
    stop('For trend please set 0 = no trend, 1 = trend, 2 = trend and quadratic trend.')
  }


  # Check whether shock type is correctly specified
  if(!(specs$shock_type %in% c(0,1))){
    stop('The shock_type has to be 0 = standard deviation shock or 1 = unit shock.')
  }


  # Check whether width of confidence bands is >=0
  if(!(specs$confint >=0)){
    stop('The width of the confidence bands has to be >=0.')
  }

  # Check whether 'gamma' is given
   if(isTRUE(use_logistic) & is.null(gamma) == TRUE){
     stop('Please specify gamma for the transition function.')
   }

  # Check whether gamma is positive
  if(isTRUE(use_logistic)){
     if(specs$gamma < 0){
    stop('Gamma has to be a positive number.')
   }
  }

  # Check whether use_hp is either 0 or 1 is positive
  if(isTRUE(use_logistic)){
    if(!(specs$use_hp %in% c(0, 1))){
      stop('Please set use_hp = 0 (do not use HP-filter), or use_hp = 1 (use HP-filter).')
   }
  }

  # Check whether use_hp is either 0 or 1 is positive
   if(!is.na(specs$max_lags) & specs$max_lags < 0 ){
    stop('The maximum number of lags has to be a positive integer.')
  }

  # Check whether whether no lag length criterion is given but maximum number of lags.
    if(is.nan(specs$lags_criterion) & !is.nan(specs$max_lags)& is.numeric(specs$max_lags)){
      stop('The maximum number of lags can only be used if a lag length criterion is given.')
    }



  # Safe data frame specifications in 'specs for functions
  specs$starts         <- 1                        # Sample Start
  specs$ends           <- dim(endog_data)[1]      # Sample end
  specs$column_names   <- names(endog_data)       # Name endogenous variables
  specs$endog          <- ncol(endog_data)        # Set the number of endogenous variables

  # Construct data for nonlinear model
  data_nl     <- create_nl_data(specs, endog_data)  %>%
                 stats::na.omit()                   %>%
                 `rownames<-`(NULL)

  y_nl        <- data_nl[[1]]
  x_nl        <- data_nl[[2]]
  fz          <- data_nl[[3]]

  # Save endogenous and lagged exogenous of nonlinear data in specs
  specs$y_nl  <- y_nl
  specs$x_nl  <- x_nl

  # Construct data for linear model for reduced shocks
  data_lin    <- create_lin_data(specs, endog_data)
  y_lin       <- data_lin[[1]]
  x_lin       <- data_lin[[2]]

  # Save endogenous and lagged exogenous of linear data in specs
  specs$y_lin <- y_lin
  specs$x_lin <- x_lin

  # Construct shock matrix
  d                 <- get_mat_chol(y_lin, x_lin, endog_data, specs)

  # Matrices to store irfs for each horizon
  irf_temp_s1_mean  <-  matrix(NaN, specs$endog, specs$hor + 1)
  irf_temp_s1_low   <-  irf_temp_s1_mean
  irf_temp_s1_up    <-  irf_temp_s1_mean

  irf_temp_s2_mean  <-  matrix(NaN, specs$endog, specs$hor + 1)
  irf_temp_s2_low   <-  irf_temp_s2_mean
  irf_temp_s2_up    <-  irf_temp_s2_mean

  # Arrays to store irfs
  irf_s1_mean  <-  array(NaN, dim = c(specs$endog, specs$hor + 1, specs$endog))
  irf_s1_low   <-  irf_s1_mean
  irf_s1_up    <-  irf_s1_mean

  irf_s2_mean  <-  array(NaN, dim = c(specs$endog, specs$hor + 1, specs$endog))
  irf_s2_low   <-  irf_s2_mean
  irf_s2_up    <-  irf_s2_mean

  # Matrices to store OLS parameters for regime 1 & 2
  b1_s1      <- matrix(NaN, specs$endog, specs$endog)
  b1_low_s1  <- matrix(NaN, specs$endog, specs$endog)
  b1_up_s1   <- matrix(NaN, specs$endog, specs$endog)

  b1_s2      <- matrix(NaN, specs$endog, specs$endog)
  b1_low_s2  <- matrix(NaN, specs$endog, specs$endog)
  b1_up_s2   <- matrix(NaN, specs$endog, specs$endog)

  # Define coefficient position to extract regime_1 and regime_2 parameters in loop
  start_nl_s1   <- 2
  end_nl_s1     <- specs$endog + 1
  samp_nl_s1    <- start_nl_s1:end_nl_s1

  # Make list to store OLS diagnostics for each horizon
  diagnost_ols_each_h <- list()

  # Make matrix to store OLS diagnostics for each endogenous variable k
  diagnost_each_k           <- matrix(NaN, specs$endog,  4)
  rownames(diagnost_each_k) <- specs$column_names
  colnames(diagnost_each_k) <- c("R-sqrd.", "Adj. R-sqrd.", "F-stat", " p-value")


  # Make cluster
  if(is.null(num_cores)){
    num_cores     <- min(specs$endog, parallel::detectCores() - 1)
  }

  cl            <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)

 # Determine whether manual lag lengths are given or have to be determined
  if(is.nan(specs$lags_criterion) == TRUE) {

# Determine parameter position for regime 2
   start_nl_s2   <- 2 + specs$endog*specs$lags_endog_nl
   end_nl_s2     <- start_nl_s2 + specs$endog - 1
   samp_nl_s2    <- start_nl_s2:end_nl_s2

 # Loops to estimate local projections
  nl_irfs <- foreach( s        = 1:specs$endog,
                     .packages = 'lpirfs') %dopar%{ # Accounts for the shocks

        for (h in 1:specs$hor){   # Accounts for the horizons

         yy  <-   y_nl[h:dim(y_nl)[1], ]
         xx  <-   x_nl[1:(dim(x_nl)[1] - h + 1), ]


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

           # Extract coefficients
           b1_s1[k, ]       <-   b[samp_nl_s1]
           b1_low_s1[k, ]   <-   b[samp_nl_s1] - std_err[samp_nl_s1]
           b1_up_s1[k, ]    <-   b[samp_nl_s1] + std_err[samp_nl_s1]

           b1_s2[k, ]       <-   b[samp_nl_s2]
           b1_low_s2[k, ]   <-   b[samp_nl_s2] - std_err[samp_nl_s2]
           b1_up_s2[k, ]    <-   b[samp_nl_s2] + std_err[samp_nl_s2]

           # Get diagnostocs for summary
           get_diagnost              <- lpirfs::ols_diagnost(yy[, k], xx)
           diagnost_each_k[k, 1]     <- get_diagnost[[3]]
           diagnost_each_k[k, 2]     <- get_diagnost[[4]]
           diagnost_each_k[k, 3]     <- get_diagnost[[5]]
           diagnost_each_k[k, 4]     <- stats::pf(diagnost_each_k[k, 3], get_diagnost[[6]], get_diagnost[[7]], lower.tail = F)

          }

          # Estimate local projections
           irf_temp_s1_mean[, h + 1] <- t(b1_s1        %*%  d[ , s])
           irf_temp_s1_low[,  h + 1] <- t(b1_low_s1    %*%  d[ , s])
           irf_temp_s1_up[,   h + 1] <- t(b1_up_s1     %*%  d[ , s])

           irf_temp_s2_mean[, h + 1] <- t(b1_s2        %*%  d[ , s])
           irf_temp_s2_low[,  h + 1] <- t(b1_low_s2    %*%  d[ , s])
           irf_temp_s2_up[,   h + 1] <- t(b1_up_s2     %*%  d[ , s])

           # Give rownames
           rownames(diagnost_each_k) <- paste("h", h, ":", specs$column_names, sep ="")

           # Save full summary matrix in list for each horizon
           diagnost_ols_each_h[[h]]             <- diagnost_each_k
        }


  # Give names to horizon
 #   names(diagnost_ols_each_h)    <- paste("h", 1:specs$hor, sep = " ")

    list(irf_temp_s1_mean, irf_temp_s1_low, irf_temp_s1_up,
         irf_temp_s2_mean, irf_temp_s2_low, irf_temp_s2_up,
         diagnost_ols_each_h)

}

  # List to save diagnostics
  diagnostic_list <- list()

 # Fill arrays with irfs
 for(i in 1:specs$endog){

   # Fill irfs
   irf_s1_mean[, , i] <- as.matrix(do.call(rbind, nl_irfs[[i]][1]))
   irf_s1_low[, ,  i] <- as.matrix(do.call(rbind, nl_irfs[[i]][2]))
   irf_s1_up[, ,   i] <- as.matrix(do.call(rbind, nl_irfs[[i]][3]))

   irf_s2_mean[, , i] <- as.matrix(do.call(rbind, nl_irfs[[i]][4]))
   irf_s2_low[,  , i] <- as.matrix(do.call(rbind, nl_irfs[[i]][5]))
   irf_s2_up[,   , i] <- as.matrix(do.call(rbind, nl_irfs[[i]][6]))

   # First value of is merely the shock
   irf_s1_mean[, 1, i]   <- t(d[, i])
   irf_s1_low[,  1, i]   <- irf_s1_mean[, 1, i]
   irf_s1_up[,   1, i]   <- irf_s1_mean[, 1, i]

   irf_s2_mean[, 1, i]   <- t(d[, i])
   irf_s2_low[,  1, i]   <- irf_s1_mean[, 1, i]
   irf_s2_up[,   1, i]   <- irf_s1_mean[, 1, i]

   # Fill list with all OLS diagnostics
   diagnostic_list[[i]]        <- nl_irfs[[i]][[7]]


 }

  # Give names to diagnostic List
  names(diagnostic_list) <- paste("Shock:", specs$column_names, sep = " ")

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

             # Find optimal lag length
              n_obs           <- nrow(y_nl[[1]]) - h + 1 # Number of observations for model with lag one
              val_criterion   <- lpirfs::get_vals_lagcrit(y_nl, x_nl, lag_crit, h, k,
                                                          specs$max_lags, n_obs)

              lag_choice      <- which.min(val_criterion)

              yy              <- y_nl[[lag_choice]][, k]
              yy              <- yy[h: length(yy)]

              xx              <- x_nl[[lag_choice]]
              xx              <- xx[1:(dim(xx)[1] - h + 1),]


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

             # Set start and values of parameters for regime 2
              start_nl_s2     <- 2 + specs$endog*lag_choice
              end_nl_s2       <- start_nl_s2 + specs$endog - 1
              samp_nl_s2      <- start_nl_s2:end_nl_s2

             # Fill paramater matrices
              b1_s1[k, ]      <-   b[samp_nl_s1]
              b1_low_s1[k, ]  <-   b[samp_nl_s1] - std_err[samp_nl_s1]
              b1_up_s1[k, ]   <-   b[samp_nl_s1] + std_err[samp_nl_s1]

              b1_s2[k, ]      <-   b[samp_nl_s2]
              b1_low_s2[k, ]  <-   b[samp_nl_s2] - std_err[samp_nl_s2]
              b1_up_s2[k, ]   <-   b[samp_nl_s2] + std_err[samp_nl_s2]



              # Get diagnostocs for summary
              get_diagnost              <- lpirfs::ols_diagnost(yy, xx)
              diagnost_each_k[k, 1]     <- get_diagnost[[3]]
              diagnost_each_k[k, 2]     <- get_diagnost[[4]]
              diagnost_each_k[k, 3]     <- get_diagnost[[5]]
              diagnost_each_k[k, 4]     <- stats::pf(diagnost_each_k[k, 3], get_diagnost[[6]], get_diagnost[[7]], lower.tail = F)


              }

             # Estimate local projections
              irf_temp_s1_mean[, h + 1] <- t(b1_s1        %*%  d[ , s])
              irf_temp_s1_low[,  h + 1] <- t(b1_low_s1    %*%  d[ , s])
              irf_temp_s1_up[,   h + 1] <- t(b1_up_s1     %*%  d[ , s])

              irf_temp_s2_mean[, h + 1] <- t(b1_s2        %*%  d[ , s])
              irf_temp_s2_low[,  h + 1] <- t(b1_low_s2    %*%  d[ , s])
              irf_temp_s2_up[,   h + 1] <- t(b1_up_s2     %*%  d[ , s])

              # Save full summary matrix in list for each horizon
              diagnost_ols_each_h[[h]]             <- diagnost_each_k
         }



          list(irf_temp_s1_mean, irf_temp_s1_low, irf_temp_s1_up,
               irf_temp_s2_mean, irf_temp_s2_low, irf_temp_s2_up,
               diagnost_ols_each_h)
        }


  # List to save diagnostics
  diagnostic_list <- list()

# Fill arrays with local projection irfs
  for(i in 1:specs$endog){

  # Fill irfs
   irf_s1_mean[, , i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][1]))
   irf_s1_low[,  , i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][2]))
   irf_s1_up[,  ,  i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][3]))

   irf_s2_mean[, , i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][4]))
   irf_s2_low[,  , i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][5]))
   irf_s2_up[, ,   i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][6]))

  # First value of horizon is merely the shock
   irf_s1_mean[, 1, i]   <- t(d[, i])
   irf_s1_low[,  1, i]   <- irf_s1_mean[, 1, i]
   irf_s1_up[,   1, i]   <- irf_s1_mean[, 1, i]

   irf_s2_mean[, 1, i]   <- t(d[, i])
   irf_s2_low[,  1, i]   <- irf_s2_mean[, 1, i]
   irf_s2_up[,   1, i]   <- irf_s2_mean[, 1, i]


   # Fill list with all OLS diagnostics
   diagnostic_list[[i]]        <- nl_irfs[[i]][[7]]

  }

  # Give names to diagnostic List
  names(diagnostic_list) <- paste("Shock:", specs$column_names, sep = " ")

 }

# Close cluster
  parallel::stopCluster(cl)

 result         <-  list(irf_s1_mean     = irf_s1_mean,
                         irf_s1_low      = irf_s1_low,
                         irf_s1_up       = irf_s1_up,
                         irf_s2_mean     = irf_s2_mean,
                         irf_s2_low      = irf_s2_low,
                         irf_s2_up       = irf_s2_up,
                         fz              = fz,
                         specs           = specs,
                         diagnostic_list = diagnostic_list)

 # Give object S3 name
 class(result) <- "lpirfs_nl_obj"

 return(result)



}
