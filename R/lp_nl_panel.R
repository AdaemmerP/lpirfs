#' @name lp_nl_panel
#' @title Compute nonlinear impulse responses for panel data
#' @description This function estimates nonlinear impulse responses with local projections for panel data with an
#'              identified shock.
#' @inheritParams lp_lin_panel
#'
#' @param switching Character. Name of the column to use for the switching variable. This series can either
#'               be decomposed via the Hodrick-Prescott filter (see Auerbach and Gorodnichenko, 2013) or
#'               directly plugged into the following smooth transition function:
#'               \deqn{F_{z_t} = \frac{exp(-\gamma z_t)}{1 + exp(-\gamma z_t)}.}
#'               The data for the two regimes are lagged by default: \cr
#'               Regime 1 = (1-\eqn{F(z_{t-1})})*y_{(t-p)}, \cr
#'               Regime 2 = \eqn{F(z_{t-1})}*y_{(t-p)}.
#' @param lag_switching Boolean. Use the first lag of the values of the transition function? TRUE (default) or FALSE.
#' @param gamma Double. Positive number which is used in the transition function.
#' @param use_logistic Boolean. Use logistic function to separate states? TRUE (default) of FALSE. If FALSE, the values of the switching variable
#'                     have to be zero or one.
#' @param use_hp Boolean. Use HP-filter? TRUE or FALSE (default).
#' @param lambda Double. Value of \eqn{\lambda} for the Hodrick-Prescott filter (if use_hp = TRUE).
#'
#' @author Philipp Adämmer
#'
#' @return A list containing:
#'
#'\item{irf_lin_mean}{A \link{matrix}, containing the impulse responses.
#'                   The columns are the horizons.}
#'
#'\item{irf_lin_low}{A \link{matrix}, containing all lower confidence bands.
#'                   The columns are the horizons.}
#'
#'\item{irf_lin_up}{A \link{matrix}, containing all upper confidence bands.
#'                                   The columns are the horizons.}
#'
#'\item{reg_summaries}{Regression output for each horizon.}
#'
#'\item{xy_data_sets}{Panel data sets with endogenous and exogenous variables for each horizon.}
#'
#'\item{specs}{A list with data properties for the plot function.}
#'
#' @importFrom dplyr lead lag filter
#' @importFrom plm plm
#' @importFrom lmtest coeftest
#' @export
#'
#' @references
#'
#' Croissant, Y., Millo, G. (2008). “Panel Data Econometrics in R: The plm Package.” \emph{Journal of Statistical Software}, 27(2), 1-43. doi:
#' 10.18637/jss.v027.i02 (URL: \url{http://doi.org/10.18637/jss.v027.i02}).
#'
#' Jordà, Ò. (2005). "Estimation and Inference of Impulse Responses by Local Projections."
#' \emph{American Economic Review}, 95 (1): 161-182.
#'
#' Jordà, Ò., Schualrick, M., Taylor, A.M. (2018). "Large and State-Dependent Effects of Quasi-Random Monetary Experiments",
#' \emph{NBER} working paper 23074, \emph{FRBSF} working paper 2017-02.
#'
#' Millo G (2017). “Robust Standard Error Estimators for Panel Models: A Unifying Approach.” \emph{Journal of Statistical Software}, 82(3), 1-27. doi:
#' 10.18637/jss.v082.i03 (URL: \url{http://doi.org/10.18637/jss.v082.i03}).
#' @examples
#'\donttest{
#'
#'# This example is based on the STATA code 'LPs_basic_doall.do', provided on
#'# Òscar Jordà's website (https://sites.google.com/site/oscarjorda/home/local-projections)
#'# It estimates impulse reponses of the ratio of (mortgage lending/GDP) to a
#'# +1% change in the short term interest rate
#'
#'# Load libraries to download and read excel file from the website
#'  library(lpirfs)
#'  library(httr)
#'  library(readxl)
#'  library(dplyr)
#'
#'# Retrieve the JST Macrohistory Database
#'  url_jst <-"http://www.macrohistory.net/JST/JSTdatasetR3.xlsx"
#'  GET(url_jst, write_disk(jst_link <- tempfile(fileext = ".xlsx")))
#'  jst_data <- read_excel(jst_link, 2L)
#'
#'# Swap the first two columns so that 'country' is the first (cross section) and 'year' the
#'# second (time section) column
#'  jst_data <- jst_data %>%
#'              dplyr::select(country, year, everything())
#'
#'# Prepare variables. This is based on the 'data.do' file
#'   data_set <- jst_data %>%
#'                mutate(stir     = stir)                         %>%
#'                mutate(mortgdp  = 100*(tmort/gdp))              %>%
#'                mutate(hpreal   = hpnom/cpi)                    %>%
#'                group_by(country)                               %>%
#'                mutate(hpreal   = hpreal/hpreal[year==1990][1]) %>%
#'                mutate(lhpreal  = log(hpreal))                  %>%
#'
#'                mutate(lhpy     = lhpreal - log(rgdppc))        %>%
#'                mutate(lhpy     = lhpy - lhpy[year == 1990][1]) %>%
#'                mutate(lhpreal  = 100*lhpreal)                  %>%
#'                mutate(lhpy     = 100*lhpy)                     %>%
#'                ungroup()                                       %>%
#'
#'                mutate(lrgdp    = 100*log(rgdppc))              %>%
#'                mutate(lcpi     = 100*log(cpi)) 		            %>%
#'                mutate(lriy     = 100*log(iy*rgdppc))           %>%
#'                mutate(cay      = 100*(ca/gdp))                 %>%
#'                mutate(tnmort   = tloans - tmort)               %>%
#'                mutate(nmortgdp = 100*(tnmort/gdp))             %>%
#'
#'              # Prepare instrument
#'                mutate(lnarrow  = log(narrowm))                 %>%
#'                mutate(rlnarrow = lnarrow - lcpi)               %>%
#'                dplyr::select(country, year, mortgdp, stir, ltrate, lhpy,
#'                              lrgdp, lcpi, lriy, cay, nmortgdp, rlnarrow)
#'
#'
#'# Use data_sample from 1870 to 2013 and exclude WWI and WWII
#'   data_sample <-   seq(1870, 2016)[which(!(seq(1870, 2016) %in%
#'                               c(seq(1914, 1918),
#'                                 seq(1939, 1947),
#'                                 seq(2014, 2016))))]
#'
#'# Estimate panel model
#' results_panel <-  lp_nl_panel(data_set           = data_set,
#'                                data_sample       = data_sample,
#'                                endog_data        = "mortgdp",
#'                                cumul_mult        = TRUE,
#'
#'                                shock             = "stir",
#'                                diff_shock        = TRUE,
#'                                panel_model       = "within",
#'                                panel_effect      = "individual",
#'                                robust_cov        = "vcovSCC",
#'
#'                                switching         = "lrgdp",
#'                                lag_switching     = TRUE,
#'                                use_hp            = TRUE,
#'                                lambda            = 6.25,
#'                                gamma             = 10,
#'
#'                                c_exog_data       = "cay",
#'                                l_exog_data       = NULL,
#'                                lags_exog_data    = NULL,
#'                                c_fd_exog_data    = colnames(data_set)[c(seq(4,9),11)],
#'                                l_fd_exog_data    = colnames(data_set)[c(seq(3,9),11)],
#'                                lags_fd_exog_data = 2,
#'
#'                                confint           = 1.67,
#'                                hor               = 5)
#'
#'# Create and plot irfs
#'  nl_plots <- plot_nl(results_panel)
#'
#'  plot(nl_plots$gg_s1[[1]])
#'  plot(nl_plots$gg_s2[[1]])
#'
#'# Plot values of the transition function for USA between 1950 and 2016
#'  library(ggplot2)
#'  library(dplyr)
#'
#'  data_set %>%
#'     mutate(fz = results_panel$fz) %>%
#'     select(country, year, fz)     %>%
#'     filter(country == "USA" & year > 1950  & year <= 2016) %>%
#'     ggplot()+
#'     geom_line(aes(x = year, y = fz)) +
#'     scale_x_continuous(breaks = seq(1950, 2016, 5))
#'
#'                               ### Use GMM ###
#'
#' # Use a much smaller sample to have fewer T than N
#' data_sample <-   seq(2000, 2012)
#'
#'
#' # Estimate panel model with gmm
#' # This example gives a warning at each iteration. The data set is not well suited for
#' # GMM as GMM is based on N-asymptotics and the data set only contains 27 countries
#'
#' results_panel <-  lp_nl_panel(data_set           = data_set,
#'                                data_sample       = data_sample,
#'                                endog_data        = "mortgdp",
#'                                cumul_mult        = TRUE,
#'
#'                                shock             = "stir",
#'                                diff_shock        = TRUE,
#'                                panel_model       = "within",
#'                                panel_effect      = "individual",
#'                                robust_cov        = NULL,
#'
#'                                panel_gmm         = TRUE,
#'                                gmm_model         = "onestep",
#'                                gmm_effect        = "twoways",
#'                                gmm_transformation = "ld",
#'
#'                                switching         = "lrgdp",
#'                                lag_switching     = TRUE,
#'                                use_hp            = TRUE,
#'                                lambda            = 6.25,
#'                                gamma             = 10,
#'
#'                                c_exog_data       = "cay",
#'                                l_exog_data       = NULL,
#'                                lags_exog_data    = NULL,
#'                                c_fd_exog_data    = colnames(data_set)[c(seq(4,9),11)],
#'                                l_fd_exog_data    = colnames(data_set)[c(seq(3,9),11)],
#'                                lags_fd_exog_data = 2,
#'
#'                                confint           = 1.67,
#'                                hor               = 5)
#'
#'# Create and plot irfs
#'  nl_plots <- plot_nl(results_panel)
#'
#'  plot(nl_plots$gg_s1[[1]])
#'  plot(nl_plots$gg_s2[[1]])
#'
#'}
#'
lp_nl_panel <- function(
                      data_set          = NULL,
                      data_sample       = "Full",
                      endog_data        = NULL,
                      cumul_mult        = TRUE,

                      shock             = NULL,
                      diff_shock        = TRUE,
                      panel_model       = "within",
                      panel_effect      = "individual",
                      robust_cov        = NULL,

                      panel_gmm         = FALSE,
                      gmm_model         = "onestep",
                      gmm_effect        = "twoways",
                      gmm_transformation = "d",


                      c_exog_data       = NULL,
                      l_exog_data       = NULL,
                      lags_exog_data    = NaN,
                      c_fd_exog_data    = NULL,
                      l_fd_exog_data    = NULL,
                      lags_fd_exog_data = NaN,

                      switching         = NULL,
                      use_logistic      = TRUE,
                      lag_switching     = TRUE,
                      use_hp            = FALSE,
                      lambda            = NULL,
                      gamma             = NULL,

                      confint           = NULL,
                      hor               = NULL){


  # Check whether column names are named properly
  if(any(colnames(data_set)[3:dim(data_set)[2]] %in% c("cross_id", "date_id"))){
    stop("You cannot use the column names 'cross_id' or 'date_id' besides the first two columns of your data.frame.
         Please rename them." )
  }

  # Check whether data_set is given
  if(is.null(data_set)){
    stop("You have to provide the panel data set." )
  }


  # Check whether name for endogenous variable is given
  if(is.null(endog_data)){
    stop("You have to provide the name of the endogenous variable." )
  }


  # Check whether shock is given
  if(is.null(shock)){
    stop("You have to provide the name of the variable to shock with." )
  }

  # Check whether panel model type is correct
  if(!panel_model %in% c("within", "random", "ht", "between", "pooling", "fd")){
    stop("The type of the panel model has to be 'within', 'random', 'ht', 'between', 'pooling' or 'fd'. See
         the vignette of the plm package for details." )
  }

  # Check whether the panel effect is correctly specified
  if(!panel_effect %in% c("individual", "time", "twoways", "nested")){
    stop("The effect introduced in the model has to be 'individual', 'time', 'twoways' or 'nested'.
         See the vignette of the plm package for details." )
  }


  # Check whether robust covariance estimator is correctly specified
  if(!is.null(robust_cov)){
    if(!robust_cov %in% c("Vw", "Vcx", "Vct", "Vcxt", "vcovBK", "vcovDC", "vcovG", "vcovHC", "vcovNW", "vcovSCC")){
      stop("The choices for robust covariance estimation are 'Vw', 'Vcx', 'Vct', 'Vcxt', 'vcovBK', 'vcovDC', 'vcovG', 'vcovHC', 'vcovNW', 'vcovSCC'.
         See the vignette of the plm package for details." )
    }
  }

  # Check whether lag lengths are given if necessary
  if(!is.null(l_exog_data)){
    if(is.null(lags_exog_data)){
      stop("You have to provide the lag lengths for the exogenous data with lagged impact." )
    }
  }

  # Check whether lag lengths are given if necessary
  if(!is.null(l_fd_exog_data)){
    if(is.null(lags_fd_exog_data)){
      stop("You have to provide the lag lengths for the exogenous data with lagged impact of first differences." )
    }
  }

  # Check whether width of confidence intervals is given
  if(is.null(confint)){
    stop('Please specify a value for the width of the confidence bands.')
  }

  # Check whether width of confidence bands is >=0
  if(!(confint >=0)){
    stop('The width of the confidence bands has to be >=0.')
  }

  # Check whether values for horizons are correct
  if(!(hor > 0) | is.nan(hor) | !(hor %% 1 == 0)){
    stop('The number of horizons has to be an integer and >0.')
  }

  # Check whether switching variable is given
  if(is.null(switching)){
    stop("You have to provide a switching variable.")
  }

  # Check whether to use the HP-filter
  if(is.null(use_hp)){
    stop("Please specify whether to use the HP-filter for the switching variable.")
  }

  # Check whether value for lamda is given
  if(isTRUE(use_hp) & is.null(lambda)){
    stop("Please give a value for lambda for the HP-filter.")
  }

  # Check whether value for gamma is given
  if(is.null(gamma)){
    stop("Please give a value for gamma > 0.")
  }

  # Check whether input for gmm is correct
  if(isTRUE(gmm_model) & !gmm_model %in% c("onestep", "twosteps")){
    stop('The model type for gmm has to be "onestep" (default) or "twosteps".')
  }

  # Check whether input for gmm is correct
  if(isTRUE(gmm_model) & !gmm_effect %in% c("twoways", "individual")){
    stop('The effect for gmm has to be "twoways" (default) or "individual".')
  }

  # Check whether input for gmm is correct
  if(isTRUE(gmm_model) & !gmm_transformation %in% c("d", "ld")){
    stop('The transformation to apply to the model has to either be "d" (default)
         for the "difference GMM" model or "ld" for the "system GMM".')
  }


  # Rename first two column names of data.frame
  colnames(data_set)[1]     <- "cross_id"
  colnames(data_set)[2]     <- "date_id"

  # Create list to store inputs
  specs <- list()

  # Fill list with inputs
  specs$data_sample        <- data_sample
  specs$endog_data         <- endog_data
  specs$cumul_mult         <- cumul_mult

  specs$shock              <- shock
  specs$diff_shock         <- diff_shock

  specs$panel_model        <- panel_model
  specs$panel_effect       <- panel_effect
  specs$robust_cov         <- robust_cov

  specs$panel_gmm          <- panel_gmm
  specs$gmm_model          <- gmm_model
  specs$gmm_effect         <- gmm_effect
  specs$gmm_transformation <- gmm_transformation

  specs$switching          <- switching
  specs$lag_switching      <- lag_switching
  specs$use_logistic       <- use_logistic
  specs$use_hp             <- use_hp
  specs$lambda             <- lambda
  specs$gamma              <- gamma

  specs$exog_data          <- colnames(data_set)[which(!colnames(data_set) %in%
                                                         c("cross_id", "date_id"))]
  specs$c_exog_data        <- c_exog_data
  specs$l_exog_data        <- l_exog_data
  specs$lags_exog_data     <- lags_exog_data

  specs$c_fd_exog_data     <- c_fd_exog_data
  specs$l_fd_exog_data     <- l_fd_exog_data
  specs$lags_fd_exog_data  <- lags_fd_exog_data

  specs$confint            <- confint
  specs$hor                <- hor

  specs$model_type         <- 2        # Model type for panel data
  specs$endog              <- 1        # Set the number of endogenous variables for plot function
  specs$column_names       <- endog_data

  specs$is_nl              <- TRUE     # Indicator to use in 'create_panel_data'


  # Create data
  nl_panel_data   <- create_panel_data(specs, data_set)

  # Extract endogenous and exogenous data
  specs            <- nl_panel_data$specs
  x_reg_data       <- nl_panel_data$x_reg_data
  y_data           <- nl_panel_data$y_data

  fz               <- nl_panel_data$fz


  # Prepare matrices to store irfs
  irf_s1_mean   <- matrix(NaN, 1, specs$hor)
  irf_s1_up     <- matrix(NaN, 1, specs$hor)
  irf_s1_low    <- matrix(NaN, 1, specs$hor)

  irf_s2_mean   <- matrix(NaN, 1, specs$hor)
  irf_s2_up     <- matrix(NaN, 1, specs$hor)
  irf_s2_low    <- matrix(NaN, 1, specs$hor)


  # List to store regression results
  reg_summaries  <- list(rep(NaN), specs$hor)
  xy_data_sets   <- list(rep(NaN), specs$hor)


  # Prepare formula names
  y_reg_name     <- specs$endog_data
  x_reg_names    <- names(x_reg_data)


  # Make formula for panel estimation
  ols_formula    <- paste(y_reg_name, "~",
                          paste(x_reg_names[!(x_reg_names %in% c("cross_id", "date_id"))],
                                collapse = " + "))


  # Check whether to use GMM
  if(isTRUE(specs$panel_gmm)){
    gmm_formula <-  stats::as.formula(paste(ols_formula, "|", "plm::lag(",y_reg_name,", 2:99)" , sep=""))

                     } else {

    # Convert ols string to formula
    plm_formula    <- stats::as.formula(ols_formula)
  }


  # Loop to estimate irfs with local projections
  for(ii in 1:specs$hor){


    # Join endogenous and exogenous data
    yx_data        <- suppressMessages(
                      left_join(y_data[[ii]], x_reg_data)  %>%
                      stats::na.omit()
      )


    # Choose data_sample if specified
    if(!(specs$data_sample[1] == 'Full')){

      yx_data      <-   yx_data %>%
                        dplyr::filter(date_id %in% specs$data_sample)

    }


    # Do panel estimation
    # Check whether to use gmm
    if(isTRUE(specs$panel_gmm)){

      panel_results  <- plm::pgmm(gmm_formula,
                                  data           = yx_data,
                                  index          = c("cross_id", "date_id"),
                                  model          = specs$gmm_model,
                                  effect         = specs$gmm_effect,
                                  transformation = specs$gmm_transformation)

                            } else {

      panel_results  <- plm::plm(formula  = plm_formula,
                                 data     = yx_data,
                                 index    = c("cross_id", "date_id"),
                                 model    = specs$panel_model,
                                 effect   = specs$panel_effect)
    }


    # Estimate confidence bands with robust standard errors?
    if(is.character(specs$robust_cov)){

      # Estimate robust covariance matrices
      if(specs$robust_cov %in% c("vcovBK", "vcovDC", "vcovG", "vcovHC", "vcovNW", "vcovSCC")){

        reg_results <-  lmtest::coeftest(panel_results, vcov. = get(specs$robust_cov, envir = environment(plm)))

                                                } else {

        reg_results <-  lmtest::coeftest(panel_results,  vcov = se_hc_panel_cluster(specs$robust_cov))

      }

      # Estimate irfs and confidence bands
      irf_s1_mean[1, ii]   <- reg_results[1, 1]
      irf_s1_up[1,   ii]   <- reg_results[1, 1] + specs$confint*reg_results[1,2]
      irf_s1_low[1,  ii]   <- reg_results[1, 1] - specs$confint*reg_results[1,2]

      irf_s2_mean[1, ii]   <- reg_results[2, 1]
      irf_s2_up[1,   ii]   <- reg_results[2, 1] + specs$confint*reg_results[2,2]
      irf_s2_low[1,  ii]   <- reg_results[2, 1] - specs$confint*reg_results[2,2]

      # Estimate model without robust standard errors
                               }      else      {

      reg_results <- summary(panel_results)

      # Estimate irfs and confidence bands
      irf_s1_mean[1, ii]   <- reg_results$coefficients[1, 1]
      irf_s1_up[1,   ii]   <- reg_results$coefficients[1, 1] + specs$confint*reg_results$coefficients[1,2]
      irf_s1_low[1,  ii]   <- reg_results$coefficients[1, 1] - specs$confint*reg_results$coefficients[1,2]

      irf_s2_mean[1, ii]   <- reg_results$coefficients[2, 1]
      irf_s2_up[1,   ii]   <- reg_results$coefficients[2, 1] + specs$confint*reg_results$coefficients[2,2]
      irf_s2_low[1,  ii]   <- reg_results$coefficients[2, 1] - specs$confint*reg_results$coefficients[2,2]

    }


    # Save regression results and data_sets
    reg_summaries[[ii]]      <- reg_results
    xy_data_sets[[ii]]       <- yx_data

  }

  # List to return
  return(list(irf_s1_mean    = irf_s1_mean,
              irf_s1_up      = irf_s1_up,
              irf_s1_low     = irf_s1_low,

              irf_s2_mean    = irf_s2_mean,
              irf_s2_up      = irf_s2_up,
              irf_s2_low     = irf_s2_low,

              fz             = fz,

              reg_summaries  = reg_summaries,
              xy_data_sets   = xy_data_sets,
              specs          = specs))




  }
