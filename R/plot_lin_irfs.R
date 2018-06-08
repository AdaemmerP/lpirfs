#' @name plot_lin_irfs
#' @title Compute and display (linear) impulse responses
#' @description Compute and display (linear) impulse responses estimated with \link{lp_lin}.
#' @param results_lin A list with 3D arrays estimated in \link{lp_lin}.
#' @return A list with (gg-)plots for linear impulse responses
#' @export
#' @import ggplot2
#' @author Philipp Adämmer
#' @examples
#'  \dontrun{
#'# Load packages
#'   library(dplyr)
#'   library(doParallel)
#'   library(vars)
#'   library(parallel)
#'   library(Rcpp)
#'
#'# Load data
#'   data_set_df <- interest_rules_var_data
#'
#'# Create list for input
#'   specs <- list()
#'
#'# Specify inputs
#'   specs$lags_lin       <- 4L
#'   specs$lags_criterion <- NaN
#'   specs$max_lags       <- 2L
#'   specs$trend          <- 0L
#'   specs$shock_type     <- 1L
#'   specs$confint        <- 1.96
#'   specs$hor            <- 12L
#'
#'# Estimate model and save results
#'   results_lin  <- lp_lin(data_set_df, specs)
#'
#'# Make and save plots
#'   linear_plots <- plot_lin_irfs(results_lin)
#'
#'# Show single plots
#'   linear_plots[[1]]
#'   linear_plots[[2]]
#'
#'# Show all plots
#'   library(ggpubr)
#'   library(gridExtra)
#'
#'   lin_plots_all <- sapply(linear_plots, ggplotGrob)
#'   marrangeGrob(lin_plots, nrow = ncol(data_set_df), ncol = ncol(data_set_df))
#'
#'  }
plot_lin_irfs <- function(results_lin){

  irf_lin_mean <- results_lin[[1]]
  irf_lin_low  <- results_lin[[2]]
  irf_lin_up   <- results_lin[[3]]
  specs        <- results_lin[[4]]

  plot_num     <- 1
  gg_lin       <- rep(list(NaN), specs$endog*specs$endog)

  # Loop to fill to create plots
  for(rr in 1:(specs$endog)){
    for (ss in 1:(specs$endog)){

      # Tibbles for linear irfS
      tbl_lin_mean <- as.matrix(t(irf_lin_mean[, 1:specs$hor , ss]))[, rr]
      tbl_lin_low  <- as.matrix(t(irf_lin_low[,  1:specs$hor , ss]))[, rr]
      tbl_lin_up   <- as.matrix(t(irf_lin_up[,   1:specs$hor , ss]))[, rr]

      tbl_lin      <- tibble(x   = 1:(specs$hor),  mean = tbl_lin_mean,
                             low   = tbl_lin_low, up = tbl_lin_up)

      gg_lin[[plot_num]] <- ggplot()+
                            geom_line(data     = tbl_lin, aes(y = mean, x = x)) +
                            geom_ribbon(data   = tbl_lin, aes(x = x, ymin = low, ymax = up), col = 'grey',
                                        fill   = 'grey', alpha  = 0.3) +
                            theme_classic() +
                            ggtitle(paste(specs$columns[ss], 'on', specs$columns[rr], sep=" ")) +
                            xlab('') +
                            ylab('') +
                            theme(title = element_text(size = 6),
                                  plot.title = element_text(hjust = 0.5)) +
                            scale_y_continuous(expand = c(0, 0))          +
                            scale_x_continuous(expand = c(0, 0),
                                               breaks = seq(0, specs$hor, 2))  +
                            geom_hline(yintercept = 0, col = "red")

    # Add one to count variable
      plot_num     <- plot_num + 1

    }
      }

  return(gg_lin)

}
