#' @title Data to estimate the effects of interest rate rules for monetary policy
#' @description A \link{tibble}() containing data to estimate the effects of interest rate rules for monetary policy.
#'              The data are used in Jordà (2005).
#' @format A data frame with 193 quarterly observations (rows) and 3 variables (columns):
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
#' @description A \link{tibble}() containing data to estimate a standard monetary VAR.
#'
#' @format A data frame with 494 monthly observations (rows) and 6 variables (columns):
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
