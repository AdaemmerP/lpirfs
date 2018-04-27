#' Function to plot irfs, estimated with lin_lp function
#'
#' @param results_lin List with results from lp_lin function
#'
#' @return
#' @export
#'
#' @examples
plot_lin_irfs <- function(results_lin){

  irf_lin_mean <- results_lin[[1]]
  irf_lin_low  <- results_lin[[2]]
  irf_lin_up   <- results_lin[[3]]

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
                                        fill = 'grey', alpha = 0.6) +
                            theme_classic() +
                            ggtitle(paste(specs$columns[ss], 'on', specs$columns[rr], sep=" ")) +
                            xlab('') +
                            ylab('') +
                            theme(title = element_text(size = 6)) +
                            scale_y_continuous(expand = c(0, 0))  +
                            scale_x_continuous(expand = c(0, 0),
                                               breaks = seq(0, specs$hor, 2))  +
                            geom_hline(yintercept = 0, col = "red")

    # Add one to count variable
      plot_num     <- plot_num + 1

    }
      }

  # Plot results
  lin_plots <- sapply(gg_lin, ggplotGrob)
  marrangeGrob(lin_plots, nrow = specs$endog, ncol = specs$endog)
}
