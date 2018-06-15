#' @name plot_nl_irfs
#' @title Compute and display plots of nonlinear impulse responses
#' @description Compute and display (nonlinear) impulse responses estimated with \link{lp_nl}().
#' @param results_nl A list with 3D arrays estimated in \link{lp_nl}().
#' @return A list with (gg-)plots for nonlinear impulse responses.
#' @export
#' @import ggplot2
#' @author Philipp Adämmer
#' @examples
#' \dontrun{
#'# Load packages
#'   library(dplyr)
#'   library(doParallel)
#'   library(parallel)
#'   library(vars)
#'   library(mFilter)
#'   library(Rcpp)
#'   library(lpirfs)
#'
#'# Load data
#'   data_set_df <- monetary_var_data
#'
#'# Create list for input
#'   specs <- list()
#'
#'# Fill list
#'   specs$lags_lin       <- NaN
#'   specs$lags_nl        <- NaN
#'   specs$lags_criterion <- 'AIC'
#'   specs$max_lags       <- 2
#'   specs$trend          <- 1
#'   specs$shock_type     <- 1
#'
#'# Specifications for switching variable
#'   specs$switching      <- data_set_df$FF
#'   specs$hp_filter      <- 1
#'   specs$lambda         <- 129600 # Monthly   = 129600,
#'                                  # Quarterly = 1600,
#'                                  # Annual    = 6.25
#'   specs$gamma          <- 3
#'
#'# Horizons and confidence intervals
#'   specs$confint        <- 1.96
#'   specs$hor            <- 24
#'
#'# Estimate model and save results
#'   results_nl <- lp_nl(data_set_df, specs)
#'
#'# Make and save all plots
#'   nl_plots <- plot_nl_irfs(results_nl)
#'
#'# Show all plots
#'   library(ggpubr)
#'   library(gridExtra)
#'
#'# Save plots based on states
#'   s1_plots <- sapply(nl_plots$gg_s1, ggplotGrob)
#'   s2_plots <- sapply(nl_plots$gg_s2, ggplotGrob)
#'
#'# Show first irf of each state
#'   plot(s1_plots[[1]])
#'   plot(s2_plots[[1]])
#'
#'# Display plots
#'   marrangeGrob(s1_plots, nrow = ncol(data_set_df), ncol = ncol(data_set_df), top=NULL)
#'   marrangeGrob(s2_plots, nrow = ncol(data_set_df), ncol = ncol(data_set_df), top=NULL)
#'
#'  }
plot_nl_irfs <- function(results_nl){

  col_regime_1 <-"#21618C"
  col_regime_2 <- "#D68910"

  irf_s1_mean <- results_nl[[1]]
  irf_s1_low  <- results_nl[[2]]
  irf_s1_up   <- results_nl[[3]]

  irf_s2_mean <- results_nl[[4]]
  irf_s2_low  <- results_nl[[5]]
  irf_s2_up   <- results_nl[[6]]

  specs      <- results_nl$specs

  gg_s1      <- rep(list(NaN), specs$endog*specs$endog)
  gg_s2      <- rep(list(NaN), specs$endog*specs$endog)
  plot_num   <- 1



  for(rr in 1:(specs$endog)){
    for (ss in 1:(specs$endog)){

      # Tibbles for expansion irfs
      tbl_s1_mean <- as.matrix(t(irf_s1_mean[,  1:specs$hor , ss]))[, rr]
      tbl_s1_low  <- as.matrix(t(irf_s1_low[,   1:specs$hor , ss]))[, rr]
      tbl_s1_up   <- as.matrix(t(irf_s1_up[,    1:specs$hor , ss]))[, rr]

      tbl_s1     <- data.frame(x   = 1:specs$hor,  mean = tbl_s1_mean,
                               low = tbl_s1_low,   up   = tbl_s1_up)

      # Tibbles for recessions irfs
      tbl_s1_mean <- as.matrix(t(irf_s2_mean[,  1:specs$hor , ss]))[, rr]
      tbl_s2_low  <- as.matrix(t(irf_s2_low[,   1:specs$hor , ss]))[, rr]
      tbl_s2_up   <- as.matrix(t(irf_s2_up[,    1:specs$hor , ss]))[, rr]

      tbl_s2      <- data.frame(x   = 1:specs$hor,  mean   = tbl_s1_mean,
                               low  = tbl_s2_low,   up     = tbl_s2_up)


      gg_s1[[plot_num]] <- ggplot() +
                  geom_line(data     = tbl_s1, aes(y = mean, x = x), col = col_regime_1) +
                  geom_ribbon(data   = tbl_s1, aes(x = x, ymin = low, ymax = up), col = 'grey',
                              fill = 'grey', alpha = 0.3) +
                  theme_classic() +
                  ggtitle(paste(specs$columns[ss], 'on', specs$columns[rr], sep=" ")) +
                  xlab('') +
                  ylab('') +
                  theme(title = element_text(size = 6),
                        plot.title = element_text(hjust = 0.5)) +
                  scale_y_continuous(expand = c(0, 0))  +
                  scale_x_continuous(expand = c(0, 0),
                                     breaks = seq(0, specs$hor, 2))  +
                  geom_hline(yintercept = 0, col = "black", size = 0.25, linetype = "dashed")


      gg_s2[[plot_num]] <- ggplot() +
                  geom_line(data     = tbl_s2, aes(y = mean, x = x) , col = col_regime_2) +
                  geom_ribbon(data   = tbl_s2, aes(x = x, ymin = low, ymax = up), col = 'grey',
                              fill = 'grey', alpha = 0.3) +
                  theme_classic() +
                  ggtitle(paste(specs$columns[ss], 'on', specs$columns[rr], sep=" ")) +
                  xlab('') +
                  ylab('') +
                  theme(title = element_text(size = 6),
                        plot.title = element_text(hjust = 0.5)) +
                  scale_y_continuous(expand = c(0, 0))  +
                  scale_x_continuous(expand = c(0, 0),
                                     breaks = seq(0, specs$hor, 2))  +
                  geom_hline(yintercept = 0, col = "black", size = 0.25, linetype = "dashed")

      plot_num <- plot_num + 1


    }

  }


  list (gg_s1 = gg_s1, gg_s2 = gg_s2)

}
