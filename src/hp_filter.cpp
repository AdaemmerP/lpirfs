#include <RcppArmadillo.h>
using namespace Rcpp;
//' @name hp_filter
//' @title Decompose a times series via the Hodrick-Prescott filter
//' @description  Estimate cyclical and trend component with filter by Hodrick and Prescott (1997).
//' The function is based on the function \emph{hpfilter} from the archived \emph{mFilter}-package.
//' @param x One column matrix with numeric values.
//' @param lambda Numeric value.
//' @return A list. The first element contains the cyclical component and the second element the trend component.
//' @examples
//' \donttest{
//' library(lpirfs)
//'
//'
//'# Decompose the Federal Funds Rate
//'  data_set     <- as.matrix(interest_rules_var_data$FF)
//'  hp_results   <- hp_filter(data_set, 1600)
//'
//'# Extract results and save as data.frame
//'  hp_cyc    <- as.data.frame(hp_results[[1]])
//'  hp_trend  <- as.data.frame(hp_results[[2]])
//'
//'# Make data.frames for plots
//'  cyc_df     <- data.frame(yy = hp_cyc$V1,   xx = seq(as.Date('1955-01-01'),
//'                             as.Date('2003-01-01') , "quarter"))
//'  trend_df   <- data.frame(yy = hp_trend$V1, xx = seq(as.Date('1955-01-01'),
//'                             as.Date('2003-01-01') , "quarter"))
//'
//'# Make plots
//'  library(ggplot2)
//'
//'# Plot cyclical part
//'  ggplot(data = cyc_df) +
//'  geom_line(aes(y = yy, x = xx))
//'
//'# Plot trend component
//'  ggplot(trend_df) +
//'  geom_line(aes(y = yy, x = xx))
//'
//'}
//' @references
//' Hodrick, R.J., and Prescott, E. C. (1997). "Postwar U.S. Business Cycles: An Empirical Investigation."
//' \emph{Journal of Money, Credit and Banking}, 29(1), 1-16.
//'
//' Ravn, M.O., Uhlig, H. (2002). "On Adjusting the Hodrick-Prescott Filter for the Frequency of Observations."
//' \emph{Review of Economics and Statistics}, 84(2), 371-376.
//' @author Philipp Adämmer
// [[Rcpp::export]]
List hp_filter(NumericVector x, double lambda){

  arma::mat xx, ln, q, sigma_r, g, b_inv, b;
  arma::vec x_cycle, x_trend;
  int    n;
  List ret(2);

  xx        = as<arma::mat>(x);

  n         = xx.n_rows;
  arma::mat imat(n,n);
  imat      = imat.eye();

  arma::mat ln_1(xx.n_rows, 1);
  ln_1      = ln_1.fill(0);
  ln_1      = ln_1.t();

  arma::mat ln_2(n-1, n);
  ln_2      = ln_2.eye();

  ln        = join_cols(ln_1, ln_2);
  ln        = (imat - ln)*(imat - ln);

  q         = ln.rows(2,n-1);
  q         = q.t();

  sigma_r   = q.t()*q;
  arma::mat sigma_n(n-2, n-2);
  sigma_n   = sigma_n.eye();

  g         = q.t()*xx;
  b_inv     = inv(sigma_n + lambda*sigma_r);
  b         = b_inv*g;

  x_cycle   = lambda*q*b;
  x_trend   = xx - x_cycle;


  ret[0]  = wrap(x_cycle) ;
  ret[1]  = wrap(x_trend);
  return (ret);
}
