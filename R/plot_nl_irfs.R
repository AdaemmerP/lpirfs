#'  Function to plot irfs, estimated with lp_lin function
#'
#' @param results_nl List with estimated results from lp_nl function
#'
#' @return A list with (gg-)plots of impulse responses for two regimes
#' @export
#'
plot_nl_irfs <- function(results_nl){

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

      tbl_s1     <- tibble(x   = 1:specs$hor,  mean = tbl_s1_mean,
                           low = tbl_s1_low,   up   = tbl_s1_up)

      # Tibbles for recessions irfs
      tbl_s1_mean <- as.matrix(t(irf_s2_mean[,  1:specs$hor , ss]))[, rr]
      tbl_s2_low  <- as.matrix(t(irf_s2_low[,   1:specs$hor , ss]))[, rr]
      tbl_s2_up   <- as.matrix(t(irf_s2_up[,    1:specs$hor , ss]))[, rr]

      tbl_s2      <- tibble(x   = 1:specs$hor,  mean   = tbl_s1_mean,
                            low = tbl_s2_low,   up     = tbl_s2_up)


      gg_s1[[plot_num]] <- ggplot() +
                  geom_line(data     = tbl_s1, aes(y = mean, x = x), col = 'darkgreen') +
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
                  geom_hline(yintercept = 0, col = "red", size = .02)


      gg_s2[[plot_num]] <- ggplot() +
                  geom_line(data     = tbl_s2, aes(y = mean, x = x), col = 'darkred') +
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
                  geom_hline(yintercept = 0, col = "red")

      plot_num <- plot_num + 1


    }

  }


  list (gg_s1 = gg_s1, gg_s2 = gg_s2)

}
