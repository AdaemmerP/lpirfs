#' Local Projection Impulse Response Functions
#'
#' lpirfs provides functions to estimate and plot linear as well as nonlinear impulse
#' responses based on local projections by Jordà (2005) <doi:10.1257/0002828053828518>.
#' Nonlinear impulse responses are estimated for two regimes which can be separated by a smooth transition function as
#' applied in Auerbach and Gorodnichenko (2012) <doi:10.1257/pol.4.2.1>, or by a simple dummy approach.
#'
#' @docType package
#' @author Philipp Adämmer
#' @importFrom Rcpp evalCpp
#' @useDynLib lpirfs, .registration = TRUE
#' @exportPattern "^[[:alpha:]]+"
#' @name lpirfs-package
NULL
