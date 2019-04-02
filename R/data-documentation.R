#' @title Data to estimate fiscal multipliers
#' @description A tibble, containing data to estimate fiscal multipliers.
#'              This data was originally used by Auerbach and Gorodnichenko (2012).
#'              Sarah and Zubairy (2018) use this data to re-evaluate their results with local projections.
#'
#' @format A \link{tibble} with 248 quarterly observations (rows) and 7 variables (columns):
#' \describe{
#'   \item{Year}{Year of observation.}
#'   \item{Quarter}{Quarter of observation.}
#'   \item{Gov}{Logs of real government (federal, state, and local) purchases (consumption and investment).}
#'   \item{Tax}{Logs of real government receipts of direct and indirect taxes net of transfers to businesses
#'  and individuals.}
#'   \item{GDP}{Logs of real gross domestic product.}
#'   \item{GDP_MA}{7-quarter moving average growth rate of GDP.}
#'   \item{Gov_shock_mean}{Identified government spending shock. For details see Supplementary Appendix of Ramey and Zubairy (2018).}
#' }
#' Sample: 1948:IV - 2008:IV
#' @references
#'
#' Auerbach, A. J., and  Gorodnichenko Y. (2012). "Measuring the Output Responses to Fiscal Policy."
#' \emph{American Economic Journal: Economic Policy}, 4 (2): 1-27.
#'
#' Jordà, Ò. (2005) "Estimation and Inference of Impulse Responses by Local Projections."
#' \emph{American Economic Review}, 95 (1): 161-182.
#'
#' Ramey, V.A., Zubairy, S. (2018). "Government Spending Multipliers in Good Times
#' and in Bad: Evidence from US Historical Data." \emph{Journal of Political Economy},
#' 126(2): 850 - 901.
#'
#' @source \url{https://www.journals.uchicago.edu/doi/10.1086/696277}
"ag_data"



#' @title Data to estimate the effects of interest rate rules for monetary policy
#' @description A tibble, containing data to estimate the effects of interest rate rules for monetary policy.
#'              The data are used by Jordà (2005).
#' @format A \link{tibble} with 193 quarterly observations (rows) and 3 variables (columns):
#' \describe{
#'   \item{GDP_gap}{Percentage difference between real GDP and potential GDP (Congressional Budget Office).}
#'   \item{Infl}{Inflation: Percentage change in the GDP, chain weighted price index at annual rate.}
#'   \item{FF}{Federal funds rate: quarterly average of daily rates.}
#' }
#' Sample: 1955:I - 2003:I
#' @references
#'
#' Jordà, Ò. (2005) "Estimation and Inference of Impulse Responses by Local Projections."
#' \emph{American Economic Review}, 95 (1): 161-182.
#'
#' @source \url{https://www.aeaweb.org/articles?id=10.1257/0002828053828518}
"interest_rules_var_data"


#' @title Data to estimate a standard monetary VAR
#'
#' @description A tibble, containing data to estimate a standard monetary VAR.
#'
#' @format A \link{tibble} with 494 monthly observations (rows) and 6 variables (columns):
#' \describe{
#'   \item{EM}{Log of non-agricultural payroll employment.}
#'   \item{P}{Log of personal consumption expenditures deflator (1996 = 100).}
#'   \item{POCM}{Annual growth rate of the index of sensitive materials
#'               prices issued by the Conference Board.}
#'    \item{FF}{Federal funds rate.}
#'    \item{NBRX}{Ratio of nonborrowed reserves plus extended credit to total reserves.}
#'    \item{M2}{Annual growth rate of M2 stock.}
#'
#'   }
#' Sample: 1960:01 - 2001:02.
#'
#' @references
#'
#' Jordà, Ò. (2005) "Estimation and Inference of Impulse Responses by Local Projections."
#' \emph{American Economic Review}, 95 (1): 161-182.
#'
#' @source \url{https://www.aeaweb.org/articles?id=10.1257/0002828053828518}
"monetary_var_data"


