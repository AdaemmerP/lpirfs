#' @name lp_lin_panel
#' @title Compute linear impulse responses with local projections for panel data
#' @description This function estimates impulse responses with local projections for panel data, either with an
#'              identified shock or by an instrument variable approach. It further allows to estimate irfs by GMM.
#' @param data_set A \link{data.frame}, containing the panel data set. The first column has to be the
#'                 variable denoting the cross section. The second column has to be the
#'                 variable denoting the time section.
#' @param data_sample   Boolean or character. Use full data_sample? TRUE (default). To estimate a subset, you have to provide
#'                 the sequence of dates to use. This sequence has to be in the same format as the second column (time-section).
#' @param endog_data Character. Name of the endogenous variable. You can only provide one endogenous variable at a time.
#' @param cumul_mult Boolean. Estimate cumulative multipliers? TRUE or FALSE. If TRUE, cumulative responses
#'                   are estimated via: \deqn{y_(t+h) - y_(t-1)} for h = 0,..., H-1.
#' @param shock      Character. The column name of the variable to shock with.
#' @param diff_shock Boolean. Take first differences of the shock variable? TRUE or FALSE.
#' @param iv_reg     Boolean. Use instrument variable approach? TRUE or FALSE.
#' @param instrum    NULL or Character. The name(s) of the instrument variable(s) if iv_reg = TRUE.
#' @param panel_model Character. Type of panel model. The default is "within" (fixed effects). See vignette of the plm package for options and details.
#' @param panel_effect Character. The effects introduced in the model: "individual", "time", "twoways",
#' or "nested". See the vignette of the plm package for details.
#'
#' @param panel_gmm  Boolean. Use GMM for estimation? TRUE or FALSE (default). See vignette of plm package for details.
#' @param gmm_effect Character. The effects introduced in the model: "twoways" (default) or "individual". See vignette of the plm package for details.
#' @param gmm_model Character. Either "onestep" (default) or "twosteps". See vignette of the plm package for details.
#' @param gmm_transformation Character. Either "d" (default) for the "difference GMM" model or "ld" for the "system GMM".
#' See vignette of the plm package for details.
#'
#' @param robust_cov NULL or Character. The character specifies the method how to estimate robust standard errors: "vcovBK", "vcovDC",
#'                    "vcovG", "vcovHC", "vcovNW", "vcovSCC". See vignette of plm() package for details.
#' @param c_exog_data NULL or Character. Names of exogenous variables with contemporaneous impact.
#' @param l_exog_data NULL or Character. Names of exogenous variables with lagged impact.
#' @param lags_exog_data Integer. Lag length for the exogenous variables with lagged impact.
#' @param c_fd_exog_data NULL or Character. Names of exogenous variables with contemporaneous impact of first differences.
#' @param l_fd_exog_data NULL or Character. Names of exogenous variables with lagged impact of first differences.
#' @param lags_fd_exog_data NaN or Integer. Number of lags for variables with impact of first differences.
#' @param confint Double. Width of confidence bands. 68\% = 1; 90\% = 1.65; 9\% = 1.96.
#' @param hor Integer. Number of horizons for impulse responses.
#' @param num_cores NULL or Integer. The number of cores to use for the estimation. If NULL, the function will
#'                 use the maximum number of cores minus one.
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
#' @importFrom foreach foreach
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
#'# Use data_sample from 1870 to 2013 BUT exclude WWI and WWII
#'   data_sample <-   seq(1870, 2016)[which(!(seq(1870, 2016) %in%
#'                               c(seq(1914, 1918),
#'                                 seq(1939, 1947),
#'                                 seq(2014, 2016))))]
#'
#'# Estimate panel model
#' results_panel <-  lp_lin_panel(data_set          = data_set,
#'                                data_sample       = data_sample,
#'                                endog_data        = "mortgdp",
#'                                cumul_mult        = TRUE,
#'
#'                                shock             = "stir",
#'                                diff_shock        = TRUE,
#'                                iv_reg            = FALSE,
#'                                instrum           = NULL,
#'                                panel_model       = "within",
#'                                panel_effect      = "individual",
#'                                robust_cov        = NULL,
#'
#'                                c_exog_data       = "cay",
#'                                l_exog_data       = NULL,
#'                                lags_exog_data    = NULL,
#'                                c_fd_exog_data    = colnames(data_set)[c(seq(4,9),11)],
#'                                l_fd_exog_data    = colnames(data_set)[c(seq(3,9),11)],
#'                                lags_fd_exog_data = 2,
#'
#'                                confint           = 1.67,
#'                                hor               = 10)
#'
#'# Create and plot irfs
#'  plot_lin_panel <- plot_lin(results_panel)
#'  plot(plot_lin_panel[[1]])
#'
#'
#'# Create and add instrument to data_set
#'  set.seed(123)
#'  data_set   <- data_set %>%
#'                group_by(country) %>%
#'                mutate(instrument = 0.8*stir + rnorm(length(stir), 0, sd(na.omit(stir))/10)) %>%
#'                ungroup()
#'
#'
#' # Estimate panel model with iv
#' results_panel <-  lp_lin_panel(data_set          = data_set,
#'                                data_sample       = data_sample,
#'                                endog_data        = "mortgdp",
#'                                cumul_mult        = TRUE,
#'
#'                                shock             = "stir",
#'                                diff_shock        = TRUE,
#'                                iv_reg            = TRUE,
#'                                instrum           = "instrument",
#'                                panel_model       = "within",
#'                                panel_effect      = "individual",
#'                                robust_cov        = NULL,
#'
#'                                c_exog_data       = "cay",
#'                                l_exog_data       = NULL,
#'                                lags_exog_data    = NULL,
#'                                c_fd_exog_data    = colnames(data_set)[c(seq(4,9),11)],
#'                                l_fd_exog_data    = colnames(data_set)[c(seq(3,9),11)],
#'                                lags_fd_exog_data = 2,
#'
#'                                confint           = 1.67,
#'                                hor               = 10)
#'
#'# Create and plot irfs
#'  plot_lin_panel <- plot_lin(results_panel)
#'  plot(plot_lin_panel[[1]])
#'
#'
#'                     ### Use GMM ###
#'# Simulate panel data (AR(1) with fixed effects)
#'  N  <- 80
#'  TS <- 10
#'
#' cross_id <- sort(rep(seq(1,N, 1), TS))
#' date_id  <- rep(seq(1,TS, 1), N)
#'
#' set.seed(007)
#' data_set    <- tibble(cross_id, date_id)             %>%
#'                       group_by(cross_id)             %>%
#'                 mutate(x_1   = rnorm(TS))            %>%
#'                 mutate(l_x_1 = dplyr::lag(x_1, 1))   %>%
#'                 mutate(fe    = runif(1, 0, 1))       %>%
#'                 mutate(x_1   = 0.8*l_x_1 + fe)       %>%
#'                 ungroup()                            %>%
#'                 na.omit()
#'
#' # Estimate model with gmm
#' results_panel <-  lp_lin_panel(data_set          = data_set,
#'                               data_sample       = "Full",
#'                               endog_data        = "x_1",
#'                               cumul_mult        = TRUE,
#'
#'                               shock             = "l_x_1",
#'                               diff_shock        = FALSE,
#'                               iv_reg            = FALSE,
#'                               instrum           = NULL,
#'                               panel_model       = "within",
#'                               panel_effect      = "individual",
#'                               robust_cov        = NULL,
#'
#'                               panel_gmm          = TRUE,
#'                               gmm_model          = "onestep",
#'                               gmm_effect         = "twoways",
#'                               gmm_transformation = "ld",
#'
#'                               c_exog_data       = NULL,
#'                               l_exog_data       = "x_1",
#'                               lags_exog_data    = 1,
#'                               c_fd_exog_data    = NULL,
#'                               l_fd_exog_data    = NULL,
#'                               lags_fd_exog_data = NULL,
#'
#'                               confint           = 1.96,
#'                               hor               = 10)
#'
#' # Create and plot irfs
#' plot_lin_panel <- plot_lin(results_panel)
#' plot(plot_lin_panel[[1]])
#' }
#'
lp_lin_panel <- function(
                    data_set          = NULL,
                    data_sample       = "Full",
                    endog_data        = NULL,
                    cumul_mult        = TRUE,

                    shock             = NULL,
                    diff_shock        = TRUE,
                    iv_reg            = FALSE,
                    instrum           = NULL,
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
                    confint           = NULL,
                    hor               = NULL,
                    num_cores         = NULL){


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

  # Check whether instrument is given if iv_reg = TRUE
  if(isTRUE(iv_reg) & is.null(instrum)){
    stop("You have to provide the name of the instrument." )
  }

  # Check panel model type is correct
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
    if(!robust_cov %in% c("vcovBK", "vcovDC", "vcovG", "vcovHC", "vcovNW", "vcovSCC")){
    stop("The choices for robust covariance estimation are 'vcovBK', 'vcovDC', 'vcovG', 'vcovHC', 'vcovNW', 'vcovSCC'.
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

  # Check whether width for confidence intervals is given
  if(is.null(confint)){
    stop('Please specify a value for the width of the confidence bands.')
  }

  # Check whether width of confidence bands is >=0
  if(!(confint >=0)){
    stop('The width of the confidence bands has to be >=0.')
  }

  # Check whether values for horizons are correct
  if(!(hor > 0) | is.nan(hor) | !(hor %% 1 == 0)){
    stop('The number of horizons has to be an integer and > 0.')
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

  # Specify inputs
  specs$data_sample         <- data_sample
  specs$endog_data          <- endog_data
  specs$cumul_mult          <- cumul_mult

  specs$shock               <- shock
  specs$diff_shock          <- diff_shock

  specs$instrum             <- instrum
  specs$panel_model         <- panel_model
  specs$panel_effect        <- panel_effect
  specs$iv_reg              <- iv_reg

  specs$panel_gmm           <- panel_gmm
  specs$gmm_model           <- gmm_model
  specs$gmm_effect          <- gmm_effect
  specs$gmm_transformation  <- gmm_transformation

  specs$robust_cov          <- robust_cov

  specs$exog_data           <- colnames(data_set)[which(!colnames(data_set) %in%
                                                         c("cross_id", "date_id"))]
  specs$c_exog_data         <- c_exog_data
  specs$l_exog_data         <- l_exog_data
  specs$lags_exog_data      <- lags_exog_data

  specs$c_fd_exog_data      <- c_fd_exog_data
  specs$l_fd_exog_data      <- l_fd_exog_data
  specs$lags_fd_exog_data   <- lags_fd_exog_data

  specs$confint             <- confint
  specs$hor                 <- hor

  specs$model_type          <- 2
  specs$endog               <- 1        # Set the number of endogenous variables for plot function
  specs$column_names        <- endog_data

  specs$is_nl               <- FALSE    # For create panel data function


  #--- Create data
  lin_panel_data            <- create_panel_data(specs, data_set)

  # Extract endogenous and exogenous data
  specs            <- lin_panel_data$specs
  x_reg_data       <- lin_panel_data$x_reg_data
  y_data           <- lin_panel_data$y_data

  x_instrument     <- lin_panel_data$x_instrument


  # Prepare matrices to store irfs
  irf_panel_mean   <- matrix(NaN, 1, specs$hor)
  irf_panel_up     <- matrix(NaN, 1, specs$hor)
  irf_panel_low    <- matrix(NaN, 1, specs$hor)


  # List to store regression results
  reg_summaries    <- list(rep(NaN), specs$hor)
  xy_data_sets     <- list(rep(NaN), specs$hor)


  # Prepare formula names
  y_reg_name     <- specs$endog_data
  x_reg_names    <- names(x_reg_data)


  # Make formula for normal panel estimation without iv
  ols_formula    <- paste(y_reg_name, "~",
                          paste(x_reg_names[!(x_reg_names %in% c("cross_id", "date_id"))], collapse = " + "))


  # Make formula for iv_regression
  if(isTRUE(specs$iv_reg)){

    # Make formula string for iv_regression
    iv_formula     <- paste(ols_formula, paste(x_reg_names[!(x_reg_names %in% c("cross_id",
                                                                                "date_id", y_reg_name, specs$shock))],
                                               collapse = " + "), sep =  "| ")
    iv_formula     <- paste(iv_formula, "+", paste(specs$instrum, collapse =  " + "))

    # Convert string to formula
    plm_formula    <- stats::as.formula(iv_formula)

               } else {

   # Check whether to use GMM
    if(isTRUE(specs$panel_gmm)){
    gmm_formula <-  stats::as.formula(paste(ols_formula, "|", "plm::lag(",y_reg_name,", 2:99)" , sep=""))

             } else {

    # Convert ols string to formula
    plm_formula    <- stats::as.formula(ols_formula)
    }
  }


  # Loop to estimate irfs
  for(ii in 1:specs$hor){

    if(isTRUE(specs$iv_reg)){

      yx_data        <- suppressMessages(
                        dplyr::left_join(y_data[[ii]], x_reg_data)  %>%
                        dplyr::left_join(x_instrument)              %>%
                        stats::na.omit()
                        )

                     }   else    {


      yx_data        <- suppressMessages(
                        dplyr::left_join(y_data[[ii]], x_reg_data)  %>%
                        stats::na.omit()
                        )

    }


    # Choose data_sample if specified
    if(!(specs$data_sample[1] == 'Full')){

      yx_data      <-   yx_data %>%
                        dplyr::filter(date_id %in% specs$data_sample)

    }


    # Do panel estimation
    # Check whether to use GMM
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

      # Estimate model robust standard errors a la Driscoll and Kraay (1998)
      reg_results <-  lmtest::coeftest(panel_results, vcov. = plm::vcovSCC)

      # Estimate irfs and confidence bands
      irf_panel_mean[[1, ii]]   <- reg_results[1, 1]
      irf_panel_up[[1,   ii]]   <- reg_results[1, 1] + specs$confint*reg_results[1,2]
      irf_panel_low[[1,  ii]]   <- reg_results[1, 1] - specs$confint*reg_results[1,2]

                             }      else      {

      reg_results <- summary(panel_results)

      # Estimate irfs and confidence bands
      irf_panel_mean[[1, ii]]   <- reg_results$coefficients[1, 1]
      irf_panel_up[[1,   ii]]   <- reg_results$coefficients[1, 1] + specs$confint*reg_results$coefficients[1,2]
      irf_panel_low[[1,  ii]]   <- reg_results$coefficients[1, 1] - specs$confint*reg_results$coefficients[1,2]

    }


    # Save regression results and data_sets
    reg_summaries[[ii]]       <- reg_results
    xy_data_sets[[ii]]        <- yx_data
  }


  return(list(irf_panel_mean   = irf_panel_mean,
              irf_panel_low    = irf_panel_low,
              irf_panel_up     = irf_panel_up,
              reg_summaries    = reg_summaries,
              xy_data_sets     = xy_data_sets,
              specs            = specs
              ))




}
