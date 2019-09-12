#include <RcppArmadillo.h>
using namespace Rcpp;
//' @name var_one
//' @title Compute VAR to prewhite estimating functions for Newey West estimator.
//' @description  Compute Newey-West estimator with prewhitened estimation functions.
//' @param VAR_Data Matrix.
//' @return A list. The first element contains the slope parameters of the VAR(1), the sedond element contains
//' the residuals and the third element the inverted slope parameter matrix.
//' @keywords internal
//' @references
//'
//' Andrews, D.W. and Monahan, J.C. (1992). An improved heteroskedasticity and
//' autocorrelation consistent covariance matrix estimator. \emph{Econometrica}, pp.953-966.
//'
//' Newey, W.K., and West, K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
//' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55, 703–708.
//'
//'
// [[Rcpp::export]]
List var_one(NumericMatrix VAR_Data){
    NumericMatrix V;
    arma::mat XY_Data, beta, beta_mat, mat_resids, yy, xx, xpxi, D_diag, D_mat;
    int nobs, num_exog;
    List ret(4);

    XY_Data  = as<arma::mat>(VAR_Data);
    nobs     = XY_Data.n_rows - 1;
    num_exog = XY_Data.n_cols;
    D_diag   = arma::eye<arma::mat>(num_exog, num_exog);


    beta_mat   = arma::zeros<arma::mat>(num_exog, num_exog);
    mat_resids = arma::zeros<arma::mat>(nobs, num_exog);

    // Make Matrix with lagged data
    xx   = XY_Data.rows(0, (nobs - 1));

    for (int i = 0; i < num_exog; ++i){

      yy                = XY_Data(arma::span(1, (nobs)), arma::span(i, i));
      beta              = inv(xx.t()*xx)*xx.t()*yy;
      beta_mat.row(i)   = beta.t();

      mat_resids.col(i) = yy - xx*beta;

    }

    D_mat = inv(D_diag - beta_mat);

    ret[0]  = wrap(beta_mat);
    ret[1]  = wrap(mat_resids);
    ret[2]  = wrap(D_mat);
    return (ret);
  }

