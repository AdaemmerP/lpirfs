#' @name plot_nl
#' @title Compute and display plots of nonlinear impulse responses
#' @description Compute and display (nonlinear) impulse responses, estimated with \link{lp_nl}() and \link{lp_nl_iv}().
#' @param results_nl A \link{list} created with \link{lp_nl}() or \link{lp_nl_iv}().
#' @return A list with (gg-)plots for nonlinear impulse responses.
#' @export
#' @import ggplot2
#' @author Philipp Adämmer
#' @examples
#'\donttest{
#'# Load package
#'
#'  # See examples for lp_nl() and lp_nl_iv().
#'
#'  }
plot_nl <- function(results_nl){

  col_regime_1 <- "#21618C"
  col_regime_2 <- "#D68910"

  specs   <- results_nl$specs

  if(specs$model_type == 0){

  irf_s1_mean <- results_nl[[1]]
  irf_s1_low  <- results_nl[[2]]
  irf_s1_up   <- results_nl[[3]]

  irf_s2_mean <- results_nl[[4]]
  irf_s2_low  <- results_nl[[5]]
  irf_s2_up   <- results_nl[[6]]

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
                            ggtitle(paste(specs$column_names[ss], 'on', specs$column_names[rr], sep=" ")) +
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
                              ggtitle(paste(specs$column_names[ss], 'on', specs$column_names[rr], sep=" ")) +
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
                    } else if(specs$model_type == 1| specs$model_type == 2){



  gg_s1        <- rep(list(NaN), specs$endog)
  gg_s2        <- rep(list(NaN), specs$endog)
  plot_num     <- 1

  for(rr in 1:(specs$endog)){

   # Tibbles for expansion irfs
   tbl_s1_mean <-  results_nl$irf_s1_mean[rr, ]
   tbl_s1_low  <-  results_nl$irf_s1_low[rr, ]
   tbl_s1_up   <-  results_nl$irf_s1_up[rr, ]

   tbl_s1      <- data.frame(x   = 1:specs$hor,  mean = tbl_s1_mean,
                            low = tbl_s1_low,   up   = tbl_s1_up)

   # Tibbles for recessions irfs
   tbl_s2_mean <- results_nl$irf_s2_mean[rr, ]
   tbl_s2_low  <- results_nl$irf_s2_low[rr, ]
   tbl_s2_up   <- results_nl$irf_s2_up[rr, ]

   tbl_s2      <- data.frame(x   = 1:specs$hor,  mean   = tbl_s2_mean,
                             low = tbl_s2_low,   up     = tbl_s2_up)


   gg_s1[[rr]] <- ggplot() +
                    geom_line(data     = tbl_s1, aes(y = mean, x = x), col = col_regime_1) +
                    geom_ribbon(data   = tbl_s1, aes(x = x, ymin = low, ymax = up), col = 'grey',
                                                      fill = 'grey', alpha = 0.3) +
                    theme_classic() +
                    ggtitle(paste('Shock', 'on', specs$column_names[rr], sep=" ")) +
                    xlab('') +
                    ylab('') +
                    theme(title = element_text(size = 6),
                          plot.title = element_text(hjust = 0.5)) +
                    scale_y_continuous(expand = c(0, 0))  +
                    scale_x_continuous(expand = c(0, 0),
                                                             breaks = seq(0, specs$hor, 2))  +
                    geom_hline(yintercept = 0, col = "black", size = 0.25, linetype = "dashed")


   gg_s2[[rr]] <- ggplot() +
                     geom_line(data     = tbl_s2, aes(y = mean, x = x) , col = col_regime_2) +
                     geom_ribbon(data   = tbl_s2, aes(x = x, ymin = low, ymax = up), col = 'grey',
                                                    fill = 'grey', alpha = 0.3) +
                     theme_classic() +
                     ggtitle(paste('Shock', 'on', specs$column_names[rr], sep=" ")) +
                     xlab('') +
                     ylab('') +
                     theme(title      = element_text(size = 6),
                           plot.title = element_text(hjust = 0.5)) +
                     scale_y_continuous(expand = c(0, 0))  +
                     scale_x_continuous(expand = c(0, 0),
                                                           breaks = seq(0, specs$hor, 2))  +
                     geom_hline(yintercept = 0, col = "black", size = 0.25, linetype = "dashed")


                                    }









}

  list (gg_s1 = gg_s1, gg_s2 = gg_s2)

}
