#include <RcppArmadillo.h>
using namespace Rcpp;
//' @name ols_diagnost
//' @title Compute diagnostics for OLS models
//' @description  Compute OLS diagnostics such as R^2, adjusted R^2, AIC, etc.
//' @param y Numeric vector.
//' @param x Numeric matrix.
//' @return A list:
//'
//'\item{beta}{Point estimates of OLS regression.}
//'
//'\item{beta_cov}{Covariance matrix of point estimates.}
//'
//'\item{R^2}{The R^2 statistic from OLS regression. }
//'
//'\item{Adj.R^2}{The adjusted R^2 staistic from OLS regression.}
//'
//'\item{F-stat}{The computed F-statistic.}
//'
//'\item{df1}{First degress of freedom for F-statistic.}
//'
//'\item{df2}{Second degrees of freedom for F-staitisc.}
//'
//'\item{AIC_c}{The AIC_c criterion by Hurvich and Tsai (1989)}
//'
//'\item{AIC}{The AIC criterion by Akaike (1974)}
//'
//'\item{BIC}{The BIC criterion by Schwarz and Gideon (1978)}
//'
//' @keywords internal
//' @references
//' Akaike, H. (1974). "A new look at the statistical model identification", \emph{IEEE Transactions on Automatic Control}, 19 (6): 716–723.
//'
//' Hurvich, C. M., and Tsai, C.-L. (1989). "Regression and time series model selection
//' in small samples", Biometrika, 76(2): 297–307,
//'
//' Schwarz, G.(1978). "Estimating the dimension of a model", \emph{Annals of Statistics}, 6 (2): 461–464.
// [[Rcpp::export]]
List ols_diagnost(NumericVector y, NumericMatrix x){
  arma::mat xx, xx_one, yy, xpxi, xpxi_00, beta_cov;
  arma::vec w1, beta, beta_00, resids, resids_mean, resids_00, resids_sq, resids_mean_sq, resids_sq_00;
  int nobs, num_exog, num_restr, df_1, df_2 ;
  double sigma_hat, y_bar, ssr, ssr_00, ssm, r_sq, r_sq_adj, var_eps, tp, ll, aic_c, aic, bic, f_stat;
  double pi = 3.141593;
  List ret(10);


  // X Matrices
  xx       = as<arma::mat>(x);
  xx_one   = arma::ones<arma::mat>(xx.n_rows, 1); // Insert ones for constant
  xx.insert_cols(0, xx_one);

  // Y Matrices
  yy       = as<arma::vec>(y);
  num_exog = xx.n_cols;
  nobs     = xx.n_rows;

  // Point estimates of OLS model
  xpxi     = inv(xx.t()*xx);
  beta     = xpxi*xx.t()*yy;

  // Squared residuals from observations
  resids    = yy - xx*beta;
  resids_sq = resids%resids;
  ssr       = sum(resids_sq);

  // Estimate cov-matrix of estimators
  sigma_hat = ssr/double(nobs - num_exog);
  beta_cov  = sigma_hat*xpxi;

  // Squared residuals from mean
  y_bar          = mean(y);
  resids_mean    = y - y_bar;
  resids_mean_sq = resids_mean%resids_mean;
  ssm            = sum(resids_mean_sq);

  // R^2
  r_sq      = 1 - ssr/ssm;

  // Adjusted R^2
  r_sq_adj  = 1 - (1 - r_sq)*((nobs - 1)/double(nobs - num_exog));


  // Estimate F-statistic
     // Number of restrictions
     num_restr    = num_exog - 1;

     // Estimate F-statistic
     f_stat       = ((ssm - ssr)/double(num_restr))/(ssr/double(nobs - num_exog));

     // Get p-value of F statistic
      df_1 = num_restr ;
      df_2 = (nobs - num_exog)  ;

  // Section for information criteria
  var_eps  = ssr/nobs;

  tp       = xx.n_cols + 1;    // Number of parameters to estimate. Plus one,
                               // because of variance

  // Estimate log-likelihood
  ll       =  - nobs/2*log(2*pi) - nobs/2*log(var_eps) - ssr/(2*var_eps);


  // Estimate information criteria
    aic_c = (-2*ll + 2*tp) + (2*pow(tp,2) + 2*tp)/(nobs - tp -1);

    aic   = -2*ll + 2*tp ;

    bic   = -2*ll + log(nobs)*tp;


  ret[0]  = beta;
  ret[1]  = beta_cov;
  ret[2]  = r_sq;
  ret[3]  = r_sq_adj;
  ret[4]  = f_stat;
  ret[5]  = df_1;
  ret[6]  = df_2;
  ret[7]  = aic_c;
  ret[8]  = aic;
  ret[9]  = bic;
  return (ret);

}
