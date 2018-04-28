#' Data to estimate the effects of interest rate rules for monetary policy, applied in the AER article by Jordà (2005).
#' <doi:10.1257/0002828053828518>
#' Quarterly data to estimate the effects of interest rate rules for monetary policy.
#'
#'
#' @format A data frame with 193 observations (rows) and 3 variables:
#' \describe{
#'   \item{GDP_gap}{Percentage difference between real GDP and potential GDP (Congressional Budget Office.}
#'   \item{Infl}{Inflation: Percentage change in the GDP, chain weighted price index at annual rate.}
#'   \item{FF}{Federal funds rate: quarterly average of daily rates}

#'   ...
#' }
#' Sample: 1955:I - 2003:I
#' @source \url{https://www.aeaweb.org/articles?id=10.1257/0002828053828518}
"monetary_var_data"




#' Data for for Evans and Marshall (1998) VAR, applied in the AER article by Jordà (2005).
#' <doi:10.1257/0002828053828518>
#'
#'
#' @format A data frame with 494 observations (rows) and 6 variables:
#' \describe{
#'   \item{EM}{Log of non-agricultural payroll employment.}
#'   \item{P}{Log of personal consumption expenditures deflator (1996 = 100).}
#'   \item{POCM}{Annual growth rate of the index of sensitive materials
#'               prices issued by the Conference Board.}
#'    \item{FF}{Federal funds rate.}
#'    \item{NBRX}{Ratio of nonborrowed reserves plus extended credit to total reserves.}
#'    \item{M2}{Annual growth rate of M2 stock.}
#'   ...
#' }
#' Sample: 1960:01 - 2001:02.
#' @source \url{https://www.aeaweb.org/articles?id=10.1257/0002828053828518}
"monetary_var_data"
