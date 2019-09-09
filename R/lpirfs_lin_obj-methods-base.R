#' Base methods for lpirfs_lin_obj objects
#' @name lpirfs_lin_obj-class

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


  # Extract impulse responses from lpirfs object
  irf_lin_mean <- x[[1]]
  irf_lin_low  <- x[[2]]
  irf_lin_up   <- x[[3]]


  specs        <- x$specs

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


 lin_plots_all <- sapply(gg_lin, ggplotGrob)
 marrangeGrob(lin_plots_all, nrow = ncol(endog_data), ncol = ncol(endog_data), top = NULL)



}
