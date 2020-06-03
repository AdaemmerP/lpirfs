#' @name plot_lin
#' @title Compute and display plots of linear impulse responses
#' @description Compute and display linear impulse responses, estimated with \link{lp_lin}() and \link{lp_lin_iv}().
#' @param results_lin A \link{list} created with \link{lp_lin}() or \link{lp_lin_iv}().
#' @return A list with (gg-)plots for linear impulse responses.
#' @export
#' @import ggplot2 ggpubr
#' @author Philipp Adämmer
#' @examples
#'\donttest{
#'
#'# See examples for lp_lin() and lp_lin_iv().
#'  }
plot_lin <- function(results_lin){


  irf_lin_mean <- results_lin[[1]]
  irf_lin_low  <- results_lin[[2]]
  irf_lin_up   <- results_lin[[3]]


  specs        <- results_lin$specs

  # Plots for lin function
  if(specs$model_type == 0){

  plot_num     <- 1
  gg_lin       <- rep(list(NaN), specs$endog*specs$endog)

  # Loop to fill to create plots
  for(rr in 1:(specs$endog)){
    for (ss in 1:(specs$endog)){

      # Tibbles for linear irfS
      tbl_lin_mean <- as.matrix(t(irf_lin_mean[, 1:specs$hor , ss]))[, rr]
      tbl_lin_low  <- as.matrix(t(irf_lin_low[,  1:specs$hor , ss]))[, rr]
      tbl_lin_up   <- as.matrix(t(irf_lin_up[,   1:specs$hor , ss]))[, rr]

      tbl_lin      <- tibble(x     = seq_along(tbl_lin_mean),  mean = tbl_lin_mean,
                             low   = tbl_lin_low, up = tbl_lin_up)

      gg_lin[[plot_num]] <- ggplot()+
                              geom_line(data     = tbl_lin, aes(y = mean, x = x)) +
                              geom_ribbon(data   = tbl_lin, aes(x = x, ymin = low, ymax = up), col = 'grey',
                                          fill   = 'grey', alpha  = 0.3) +
                              theme_classic() +
                              ggtitle(paste(specs$column_names[ss], 'on', specs$column_names[rr], sep=" ")) +
                              xlab('') +
                              ylab('') +
                              theme(title = element_text(size = 6),
                                    plot.title = element_text(hjust = 0.5)) +
                              scale_y_continuous(expand = c(0, 0))          +
                              scale_x_continuous(expand = c(0, 0),
                                                 breaks = seq(0, specs$hor, 2))  +
                              geom_hline(yintercept = 0, col = "black", size = 0.25, linetype = "dashed")

      # Add one to count variable
      plot_num     <- plot_num + 1

 }
}

         } else if(specs$model_type == 1| specs$model_type == 2){

      gg_lin       <- rep(list(NaN), specs$endog)

      # Loop to fill to create plots
      for(rr in 1:(specs$endog)){

          # Tibbles for linear irfS
          tbl_lin_mean <- irf_lin_mean[rr, ]
          tbl_lin_low  <- irf_lin_low[rr, ]
          tbl_lin_up   <- irf_lin_up[rr, ]

          tbl_lin      <- tibble(x     = seq_along(tbl_lin_mean),  mean = tbl_lin_mean,     # 1:(specs$hor)
                                 low   = tbl_lin_low,    up   = tbl_lin_up)

          gg_lin[[rr]] <- ggplot()+
                                  geom_line(data     = tbl_lin, aes(y = mean, x = x)) +
                                  geom_ribbon(data   = tbl_lin, aes(x = x, ymin = low, ymax = up), col = 'grey',
                                              fill   = 'grey', alpha  = 0.3) +
                                  theme_classic() +
                                  ggtitle(paste('Shock', 'on', specs$column_names[rr], sep=" ")) +
                                  xlab('') +
                                  ylab('') +
                                  theme(title = element_text(size = 6),
                                        plot.title = element_text(hjust = 0.5)) +
                                  scale_y_continuous(expand = c(0, 0))          +
                                  scale_x_continuous(expand = c(0, 0),
                                                     breaks = seq(0, specs$hor, 2))  +
                                  geom_hline(yintercept = 0, col = "black", size = 0.25, linetype = "dashed")



    }
}

  return(gg_lin)

}
