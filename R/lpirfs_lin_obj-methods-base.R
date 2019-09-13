#' Base methods for lpirfs_obj objects
#' @name lpirfs_obj-class

NULL

#' @name plot.lpirfs_lin_obj
#' @title Base print() function to plot all impulse responses from linear lpirfs object
#' @export
#' @param x An object of type 'lpirfs_lin_obj
#' @param ... Additional arguments to be consistent with S3 print() function
#' @importFrom gridExtra marrangeGrob
#' @importFrom ggplot2 ggplotGrob
#' @method plot lpirfs_lin_obj

plot.lpirfs_lin_obj <- function(x, ...){

  # Extract number for column names
  nr_cols       <- x$specs$endog

  lin_plots     <- plot_lin(x)

  # Save plots based on states
  lin_plots_all <- sapply(lin_plots, ggplotGrob)

  # Show all plots
  gridExtra::marrangeGrob(lin_plots_all, nrow = nr_cols, ncol = nr_cols, top = NULL)


}

#' @name summary.lpirfs_lin_obj
#' @title Summary for linear lpirfs object
#' @export
#' @param object An object of type 'lpirfs_lin_obj'
#' @param ... Additional arguments to be consistent with S3 print() function
#' @importFrom gridExtra marrangeGrob
#' @importFrom ggplot2 ggplotGrob
#' @method summary lpirfs_lin_obj
summary.lpirfs_lin_obj <- function(object, ...){

  object[[4]]


}




#' @name plot.lpirfs_nl_obj
#' @title Base print() function to plot all impulse responses from nonlinear lpirfs object
#' @export
#' @param x An object of type 'lpirfs_nl_obj'
#' @param ... Additional arguments to be consistent with S3 print() function
#' @importFrom gridExtra marrangeGrob
#' @importFrom ggplot2 ggplotGrob
#' @method plot lpirfs_nl_obj

plot.lpirfs_nl_obj <- function(x, ...){


  # Make and save all plots
  nl_plots <- plot_nl(x)

  # Save plots based on states
  s1_plots <- sapply(nl_plots$gg_s1, ggplotGrob)
  s2_plots <- sapply(nl_plots$gg_s2, ggplotGrob)

  # Show all plots
  result_1 <- marrangeGrob(s1_plots, nrow = ncol(endog_data), ncol = ncol(endog_data), top = NULL)
  result_2 <- marrangeGrob(s2_plots, nrow = ncol(endog_data), ncol = ncol(endog_data), top = NULL)

  list(result_1, result_2)


}

#' @name summary.lpirfs_nl_obj
#' @title Summary for nonlinear lpirfs object
#' @export
#' @param object An object of type 'lpirfs_nl_obj'
#' @param ... Additional arguments to be consistent with S3 print() function
#' @importFrom gridExtra marrangeGrob
#' @importFrom ggplot2 ggplotGrob
#' @method summary lpirfs_nl_obj
summary.lpirfs_nl_obj <- function(object, ...){

  object[[9]]


}

