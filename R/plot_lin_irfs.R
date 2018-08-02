#' @name plot_lin_irfs
#' @title Compute and display plots of linear impulse responses
#' @description Compute and display linear impulse responses estimated with \link{lp_lin}.
#' @param results_lin A \link{list}() with 3D arrays estimated in \link{lp_lin}.
#' @return A list with (gg-)plots for linear impulse responses.
#' @import ggplot2
#' @author Philipp Adämmer
#' @keywords internal
#' @examples
#'\donttest{
#'
#'# This function is deprecated. See \link{plot_lin}.
#'
#'
#'
#'  }
plot_lin_irfs <- function(results_lin){

  .Deprecated("plot_lin")
   plot_lin(results_lin = results_lin)
}
