#' @name plot_nl_irfs
#' @title Compute and display plots of nonlinear impulse responses
#' @description Compute and display (nonlinear) impulse responses estimated with \link{lp_nl}.
#' @param results_nl A \link{list} with 3D arrays estimated in \link{lp_nl}.
#' @return A list with (gg-)plots for nonlinear impulse responses.
#' @export
#' @import ggplot2
#' @keywords internal
#' @author Philipp Adämmer
#' @examples
#'\donttest{
#'# Load package
#'
#'
#'  }
plot_nl_irfs <- function(results_nl){


  .Deprecated("plot_nl")
  plot_nl(results_nl = results_nl)
}


