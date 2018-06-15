#' Local Projection Impulse Response Functions
#'
#' Contains functions to estimate linear and nonlinear impulse
#' responses based on local projections by Jordà (2005) <doi:10.1257/0002828053828518>.
#' To estimate nonlinear impulse responses the data are separated into two regimes by a
#' a transition function, applied in Auerbach and Gorodnichenko (2012) <doi:10.1257/pol.4.2.1>.
#' @docType package
#' @author Philipp Adämmer
#' @importFrom Rcpp evalCpp
#' @useDynLib lpirfs, .registration = TRUE
#' @exportPattern "^[[:alpha:]]+"
#' @name lpirfs-package
NULL
