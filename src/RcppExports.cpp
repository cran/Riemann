// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cpp_ipot20
Rcpp::List cpp_ipot20(arma::vec a, arma::vec b, arma::mat dab, double lambda, double p, int maxiter, double abstol, int L);
RcppExport SEXP _Riemann_cpp_ipot20(SEXP aSEXP, SEXP bSEXP, SEXP dabSEXP, SEXP lambdaSEXP, SEXP pSEXP, SEXP maxiterSEXP, SEXP abstolSEXP, SEXP LSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type a(aSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type b(bSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type dab(dabSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    Rcpp::traits::input_parameter< double >::type abstol(abstolSEXP);
    Rcpp::traits::input_parameter< int >::type L(LSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_ipot20(a, b, dab, lambda, p, maxiter, abstol, L));
    return rcpp_result_gen;
END_RCPP
}
// cpp_pdist
arma::mat cpp_pdist(arma::mat& X);
RcppExport SEXP _Riemann_cpp_pdist(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_pdist(X));
    return rcpp_result_gen;
END_RCPP
}
// macg_mle
arma::mat macg_mle(Rcpp::List& data, int maxiter, double abstol);
RcppExport SEXP _Riemann_macg_mle(SEXP dataSEXP, SEXP maxiterSEXP, SEXP abstolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    Rcpp::traits::input_parameter< double >::type abstol(abstolSEXP);
    rcpp_result_gen = Rcpp::wrap(macg_mle(data, maxiter, abstol));
    return rcpp_result_gen;
END_RCPP
}
// macg_sample
arma::cube macg_sample(int n, int r, arma::mat sigma);
RcppExport SEXP _Riemann_macg_sample(SEXP nSEXP, SEXP rSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type r(rSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(macg_sample(n, r, sigma));
    return rcpp_result_gen;
END_RCPP
}
// macg_density
arma::vec macg_density(Rcpp::List& data, arma::mat sigma);
RcppExport SEXP _Riemann_macg_density(SEXP dataSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(macg_density(data, sigma));
    return rcpp_result_gen;
END_RCPP
}
// acg_mle
arma::mat acg_mle(Rcpp::List& data, int maxiter, double abstol);
RcppExport SEXP _Riemann_acg_mle(SEXP dataSEXP, SEXP maxiterSEXP, SEXP abstolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    Rcpp::traits::input_parameter< double >::type abstol(abstolSEXP);
    rcpp_result_gen = Rcpp::wrap(acg_mle(data, maxiter, abstol));
    return rcpp_result_gen;
END_RCPP
}
// acg_density
arma::vec acg_density(Rcpp::List& data, arma::mat A);
RcppExport SEXP _Riemann_acg_density(SEXP dataSEXP, SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(acg_density(data, A));
    return rcpp_result_gen;
END_RCPP
}
// cppdist_int_1toN
arma::vec cppdist_int_1toN(arma::vec x, arma::mat& Y);
RcppExport SEXP _Riemann_cppdist_int_1toN(SEXP xSEXP, SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(cppdist_int_1toN(x, Y));
    return rcpp_result_gen;
END_RCPP
}
// cppdist_ext_1toN
arma::vec cppdist_ext_1toN(arma::vec x, arma::mat& Y);
RcppExport SEXP _Riemann_cppdist_ext_1toN(SEXP xSEXP, SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(cppdist_ext_1toN(x, Y));
    return rcpp_result_gen;
END_RCPP
}
// basic_pdist
arma::mat basic_pdist(std::string mfdname, Rcpp::List& data, std::string dtype);
RcppExport SEXP _Riemann_basic_pdist(SEXP mfdnameSEXP, SEXP dataSEXP, SEXP dtypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::string >::type dtype(dtypeSEXP);
    rcpp_result_gen = Rcpp::wrap(basic_pdist(mfdname, data, dtype));
    return rcpp_result_gen;
END_RCPP
}
// basic_pdist2
arma::mat basic_pdist2(std::string mfdname, Rcpp::List& data1, Rcpp::List& data2, std::string dtype);
RcppExport SEXP _Riemann_basic_pdist2(SEXP mfdnameSEXP, SEXP data1SEXP, SEXP data2SEXP, SEXP dtypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data1(data1SEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data2(data2SEXP);
    Rcpp::traits::input_parameter< std::string >::type dtype(dtypeSEXP);
    rcpp_result_gen = Rcpp::wrap(basic_pdist2(mfdname, data1, data2, dtype));
    return rcpp_result_gen;
END_RCPP
}
// basic_interpolate
arma::cube basic_interpolate(std::string mfdname, std::string dtype, arma::mat mat1, arma::mat mat2, arma::vec vect);
RcppExport SEXP _Riemann_basic_interpolate(SEXP mfdnameSEXP, SEXP dtypeSEXP, SEXP mat1SEXP, SEXP mat2SEXP, SEXP vectSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< std::string >::type dtype(dtypeSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type mat1(mat1SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type mat2(mat2SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type vect(vectSEXP);
    rcpp_result_gen = Rcpp::wrap(basic_interpolate(mfdname, dtype, mat1, mat2, vect));
    return rcpp_result_gen;
END_RCPP
}
// inference_mean_intrinsic
Rcpp::List inference_mean_intrinsic(std::string mfdname, Rcpp::List& data, arma::vec myweight, int myiter, double myeps);
RcppExport SEXP _Riemann_inference_mean_intrinsic(SEXP mfdnameSEXP, SEXP dataSEXP, SEXP myweightSEXP, SEXP myiterSEXP, SEXP myepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type myweight(myweightSEXP);
    Rcpp::traits::input_parameter< int >::type myiter(myiterSEXP);
    Rcpp::traits::input_parameter< double >::type myeps(myepsSEXP);
    rcpp_result_gen = Rcpp::wrap(inference_mean_intrinsic(mfdname, data, myweight, myiter, myeps));
    return rcpp_result_gen;
END_RCPP
}
// inference_mean_extrinsic
Rcpp::List inference_mean_extrinsic(std::string mfdname, Rcpp::List& data, arma::vec myweight, int myiter, double myeps);
RcppExport SEXP _Riemann_inference_mean_extrinsic(SEXP mfdnameSEXP, SEXP dataSEXP, SEXP myweightSEXP, SEXP myiterSEXP, SEXP myepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type myweight(myweightSEXP);
    Rcpp::traits::input_parameter< int >::type myiter(myiterSEXP);
    Rcpp::traits::input_parameter< double >::type myeps(myepsSEXP);
    rcpp_result_gen = Rcpp::wrap(inference_mean_extrinsic(mfdname, data, myweight, myiter, myeps));
    return rcpp_result_gen;
END_RCPP
}
// inference_median_intrinsic
Rcpp::List inference_median_intrinsic(std::string mfdname, Rcpp::List& data, arma::vec myweight, int myiter, double myeps);
RcppExport SEXP _Riemann_inference_median_intrinsic(SEXP mfdnameSEXP, SEXP dataSEXP, SEXP myweightSEXP, SEXP myiterSEXP, SEXP myepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type myweight(myweightSEXP);
    Rcpp::traits::input_parameter< int >::type myiter(myiterSEXP);
    Rcpp::traits::input_parameter< double >::type myeps(myepsSEXP);
    rcpp_result_gen = Rcpp::wrap(inference_median_intrinsic(mfdname, data, myweight, myiter, myeps));
    return rcpp_result_gen;
END_RCPP
}
// inference_median_extrinsic
Rcpp::List inference_median_extrinsic(std::string mfdname, Rcpp::List& data, arma::vec myweight, int myiter, double myeps);
RcppExport SEXP _Riemann_inference_median_extrinsic(SEXP mfdnameSEXP, SEXP dataSEXP, SEXP myweightSEXP, SEXP myiterSEXP, SEXP myepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type myweight(myweightSEXP);
    Rcpp::traits::input_parameter< int >::type myiter(myiterSEXP);
    Rcpp::traits::input_parameter< double >::type myeps(myepsSEXP);
    rcpp_result_gen = Rcpp::wrap(inference_median_extrinsic(mfdname, data, myweight, myiter, myeps));
    return rcpp_result_gen;
END_RCPP
}
// clustering_nmshift
Rcpp::List clustering_nmshift(std::string mfdname, Rcpp::List& data, double h, int iter, double eps);
RcppExport SEXP _Riemann_clustering_nmshift(SEXP mfdnameSEXP, SEXP dataSEXP, SEXP hSEXP, SEXP iterSEXP, SEXP epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type h(hSEXP);
    Rcpp::traits::input_parameter< int >::type iter(iterSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    rcpp_result_gen = Rcpp::wrap(clustering_nmshift(mfdname, data, h, iter, eps));
    return rcpp_result_gen;
END_RCPP
}
// clustering_kmeans_lloyd
Rcpp::List clustering_kmeans_lloyd(std::string mfdname, std::string geotype, Rcpp::List& data, int iter, double eps, arma::uvec initlabel);
RcppExport SEXP _Riemann_clustering_kmeans_lloyd(SEXP mfdnameSEXP, SEXP geotypeSEXP, SEXP dataSEXP, SEXP iterSEXP, SEXP epsSEXP, SEXP initlabelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< std::string >::type geotype(geotypeSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type iter(iterSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type initlabel(initlabelSEXP);
    rcpp_result_gen = Rcpp::wrap(clustering_kmeans_lloyd(mfdname, geotype, data, iter, eps, initlabel));
    return rcpp_result_gen;
END_RCPP
}
// clustering_kmeans_macqueen
Rcpp::List clustering_kmeans_macqueen(std::string mfdname, std::string geotype, Rcpp::List& data, int iter, double eps, arma::uvec initlabel);
RcppExport SEXP _Riemann_clustering_kmeans_macqueen(SEXP mfdnameSEXP, SEXP geotypeSEXP, SEXP dataSEXP, SEXP iterSEXP, SEXP epsSEXP, SEXP initlabelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< std::string >::type geotype(geotypeSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type iter(iterSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type initlabel(initlabelSEXP);
    rcpp_result_gen = Rcpp::wrap(clustering_kmeans_macqueen(mfdname, geotype, data, iter, eps, initlabel));
    return rcpp_result_gen;
END_RCPP
}
// clustering_clrq
Rcpp::List clustering_clrq(std::string mfdname, Rcpp::List& data, arma::uvec init_label, double par_a, double par_b);
RcppExport SEXP _Riemann_clustering_clrq(SEXP mfdnameSEXP, SEXP dataSEXP, SEXP init_labelSEXP, SEXP par_aSEXP, SEXP par_bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type init_label(init_labelSEXP);
    Rcpp::traits::input_parameter< double >::type par_a(par_aSEXP);
    Rcpp::traits::input_parameter< double >::type par_b(par_bSEXP);
    rcpp_result_gen = Rcpp::wrap(clustering_clrq(mfdname, data, init_label, par_a, par_b));
    return rcpp_result_gen;
END_RCPP
}
// clustering_sup_intrinsic
Rcpp::List clustering_sup_intrinsic(std::string mfdname, Rcpp::List& data, arma::vec weight, double multiplier, int maxiter, double eps);
RcppExport SEXP _Riemann_clustering_sup_intrinsic(SEXP mfdnameSEXP, SEXP dataSEXP, SEXP weightSEXP, SEXP multiplierSEXP, SEXP maxiterSEXP, SEXP epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type weight(weightSEXP);
    Rcpp::traits::input_parameter< double >::type multiplier(multiplierSEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    rcpp_result_gen = Rcpp::wrap(clustering_sup_intrinsic(mfdname, data, weight, multiplier, maxiter, eps));
    return rcpp_result_gen;
END_RCPP
}
// clustering_kmeans18B
Rcpp::List clustering_kmeans18B(std::string mfdname, std::string geotype, Rcpp::List& data, int K, int M, int maxiter);
RcppExport SEXP _Riemann_clustering_kmeans18B(SEXP mfdnameSEXP, SEXP geotypeSEXP, SEXP dataSEXP, SEXP KSEXP, SEXP MSEXP, SEXP maxiterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< std::string >::type geotype(geotypeSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type K(KSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    rcpp_result_gen = Rcpp::wrap(clustering_kmeans18B(mfdname, geotype, data, K, M, maxiter));
    return rcpp_result_gen;
END_RCPP
}
// cvi_internal_score
double cvi_internal_score(std::string mfd, std::string dtype, Rcpp::List& data, arma::uvec mylabel);
RcppExport SEXP _Riemann_cvi_internal_score(SEXP mfdSEXP, SEXP dtypeSEXP, SEXP dataSEXP, SEXP mylabelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfd(mfdSEXP);
    Rcpp::traits::input_parameter< std::string >::type dtype(dtypeSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type mylabel(mylabelSEXP);
    rcpp_result_gen = Rcpp::wrap(cvi_internal_score(mfd, dtype, data, mylabel));
    return rcpp_result_gen;
END_RCPP
}
// cvi_internal_gdxx
double cvi_internal_gdxx(std::string mfd, std::string dtype, Rcpp::List& data, arma::uvec mylabel, int delta, int Delta);
RcppExport SEXP _Riemann_cvi_internal_gdxx(SEXP mfdSEXP, SEXP dtypeSEXP, SEXP dataSEXP, SEXP mylabelSEXP, SEXP deltaSEXP, SEXP DeltaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfd(mfdSEXP);
    Rcpp::traits::input_parameter< std::string >::type dtype(dtypeSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type mylabel(mylabelSEXP);
    Rcpp::traits::input_parameter< int >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< int >::type Delta(DeltaSEXP);
    rcpp_result_gen = Rcpp::wrap(cvi_internal_gdxx(mfd, dtype, data, mylabel, delta, Delta));
    return rcpp_result_gen;
END_RCPP
}
// cvi_internal_db
double cvi_internal_db(std::string mfd, std::string dtype, Rcpp::List& data, arma::uvec mylabel);
RcppExport SEXP _Riemann_cvi_internal_db(SEXP mfdSEXP, SEXP dtypeSEXP, SEXP dataSEXP, SEXP mylabelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfd(mfdSEXP);
    Rcpp::traits::input_parameter< std::string >::type dtype(dtypeSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type mylabel(mylabelSEXP);
    rcpp_result_gen = Rcpp::wrap(cvi_internal_db(mfd, dtype, data, mylabel));
    return rcpp_result_gen;
END_RCPP
}
// cvi_internal_ci
double cvi_internal_ci(std::string mfd, std::string dtype, Rcpp::List& data, arma::uvec mylabel);
RcppExport SEXP _Riemann_cvi_internal_ci(SEXP mfdSEXP, SEXP dtypeSEXP, SEXP dataSEXP, SEXP mylabelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfd(mfdSEXP);
    Rcpp::traits::input_parameter< std::string >::type dtype(dtypeSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type mylabel(mylabelSEXP);
    rcpp_result_gen = Rcpp::wrap(cvi_internal_ci(mfd, dtype, data, mylabel));
    return rcpp_result_gen;
END_RCPP
}
// cvi_internal_ch
double cvi_internal_ch(std::string mfd, std::string dtype, Rcpp::List& data, arma::uvec mylabel);
RcppExport SEXP _Riemann_cvi_internal_ch(SEXP mfdSEXP, SEXP dtypeSEXP, SEXP dataSEXP, SEXP mylabelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfd(mfdSEXP);
    Rcpp::traits::input_parameter< std::string >::type dtype(dtypeSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type mylabel(mylabelSEXP);
    rcpp_result_gen = Rcpp::wrap(cvi_internal_ch(mfd, dtype, data, mylabel));
    return rcpp_result_gen;
END_RCPP
}
// cvi_internal_dunn
double cvi_internal_dunn(std::string mfd, std::string dtype, Rcpp::List& data, arma::uvec mylabel);
RcppExport SEXP _Riemann_cvi_internal_dunn(SEXP mfdSEXP, SEXP dtypeSEXP, SEXP dataSEXP, SEXP mylabelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfd(mfdSEXP);
    Rcpp::traits::input_parameter< std::string >::type dtype(dtypeSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type mylabel(mylabelSEXP);
    rcpp_result_gen = Rcpp::wrap(cvi_internal_dunn(mfd, dtype, data, mylabel));
    return rcpp_result_gen;
END_RCPP
}
// visualize_pga
Rcpp::List visualize_pga(std::string mfdname, Rcpp::List& data);
RcppExport SEXP _Riemann_visualize_pga(SEXP mfdnameSEXP, SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(visualize_pga(mfdname, data));
    return rcpp_result_gen;
END_RCPP
}
// visualize_kpca
Rcpp::List visualize_kpca(std::string mfdname, Rcpp::List& data, double sigma, int ndim);
RcppExport SEXP _Riemann_visualize_kpca(SEXP mfdnameSEXP, SEXP dataSEXP, SEXP sigmaSEXP, SEXP ndimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    rcpp_result_gen = Rcpp::wrap(visualize_kpca(mfdname, data, sigma, ndim));
    return rcpp_result_gen;
END_RCPP
}
// visualize_isomap
arma::mat visualize_isomap(std::string mfdname, Rcpp::List& data, std::string geometry, int nnbd);
RcppExport SEXP _Riemann_visualize_isomap(SEXP mfdnameSEXP, SEXP dataSEXP, SEXP geometrySEXP, SEXP nnbdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::string >::type geometry(geometrySEXP);
    Rcpp::traits::input_parameter< int >::type nnbd(nnbdSEXP);
    rcpp_result_gen = Rcpp::wrap(visualize_isomap(mfdname, data, geometry, nnbd));
    return rcpp_result_gen;
END_RCPP
}
// visualize_cmds
Rcpp::List visualize_cmds(std::string mfd, std::string geo, Rcpp::List& data, int ndim);
RcppExport SEXP _Riemann_visualize_cmds(SEXP mfdSEXP, SEXP geoSEXP, SEXP dataSEXP, SEXP ndimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfd(mfdSEXP);
    Rcpp::traits::input_parameter< std::string >::type geo(geoSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    rcpp_result_gen = Rcpp::wrap(visualize_cmds(mfd, geo, data, ndim));
    return rcpp_result_gen;
END_RCPP
}
// visualize_sammon
Rcpp::List visualize_sammon(std::string mfd, std::string geo, Rcpp::List& data, int ndim, int maxiter, double abstol);
RcppExport SEXP _Riemann_visualize_sammon(SEXP mfdSEXP, SEXP geoSEXP, SEXP dataSEXP, SEXP ndimSEXP, SEXP maxiterSEXP, SEXP abstolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfd(mfdSEXP);
    Rcpp::traits::input_parameter< std::string >::type geo(geoSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    Rcpp::traits::input_parameter< double >::type abstol(abstolSEXP);
    rcpp_result_gen = Rcpp::wrap(visualize_sammon(mfd, geo, data, ndim, maxiter, abstol));
    return rcpp_result_gen;
END_RCPP
}
// curvedist_lp
double curvedist_lp(std::string mfd, std::string geo, Rcpp::List& data1, Rcpp::List& data2, arma::vec vect, double myp);
RcppExport SEXP _Riemann_curvedist_lp(SEXP mfdSEXP, SEXP geoSEXP, SEXP data1SEXP, SEXP data2SEXP, SEXP vectSEXP, SEXP mypSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfd(mfdSEXP);
    Rcpp::traits::input_parameter< std::string >::type geo(geoSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data1(data1SEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data2(data2SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type vect(vectSEXP);
    Rcpp::traits::input_parameter< double >::type myp(mypSEXP);
    rcpp_result_gen = Rcpp::wrap(curvedist_lp(mfd, geo, data1, data2, vect, myp));
    return rcpp_result_gen;
END_RCPP
}
// curvedist_dtwbasic
double curvedist_dtwbasic(std::string mfd, std::string geo, Rcpp::List& data1, Rcpp::List& data2);
RcppExport SEXP _Riemann_curvedist_dtwbasic(SEXP mfdSEXP, SEXP geoSEXP, SEXP data1SEXP, SEXP data2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfd(mfdSEXP);
    Rcpp::traits::input_parameter< std::string >::type geo(geoSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data1(data1SEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data2(data2SEXP);
    rcpp_result_gen = Rcpp::wrap(curvedist_dtwbasic(mfd, geo, data1, data2));
    return rcpp_result_gen;
END_RCPP
}
// learning_seb
Rcpp::List learning_seb(std::string mfdname, Rcpp::List& data, int myiter, double myeps, std::string method);
RcppExport SEXP _Riemann_learning_seb(SEXP mfdnameSEXP, SEXP dataSEXP, SEXP myiterSEXP, SEXP myepsSEXP, SEXP methodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type myiter(myiterSEXP);
    Rcpp::traits::input_parameter< double >::type myeps(myepsSEXP);
    Rcpp::traits::input_parameter< std::string >::type method(methodSEXP);
    rcpp_result_gen = Rcpp::wrap(learning_seb(mfdname, data, myiter, myeps, method));
    return rcpp_result_gen;
END_RCPP
}
// learning_rmml
arma::mat learning_rmml(std::string mfdname, Rcpp::List& data, double lambda, arma::uvec label);
RcppExport SEXP _Riemann_learning_rmml(SEXP mfdnameSEXP, SEXP dataSEXP, SEXP lambdaSEXP, SEXP labelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type label(labelSEXP);
    rcpp_result_gen = Rcpp::wrap(learning_rmml(mfdname, data, lambda, label));
    return rcpp_result_gen;
END_RCPP
}
// learning_coreset18B
Rcpp::List learning_coreset18B(std::string mfdname, std::string geoname, Rcpp::List& data, int M, int myiter, double myeps);
RcppExport SEXP _Riemann_learning_coreset18B(SEXP mfdnameSEXP, SEXP geonameSEXP, SEXP dataSEXP, SEXP MSEXP, SEXP myiterSEXP, SEXP myepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mfdname(mfdnameSEXP);
    Rcpp::traits::input_parameter< std::string >::type geoname(geonameSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    Rcpp::traits::input_parameter< int >::type myiter(myiterSEXP);
    Rcpp::traits::input_parameter< double >::type myeps(myepsSEXP);
    rcpp_result_gen = Rcpp::wrap(learning_coreset18B(mfdname, geoname, data, M, myiter, myeps));
    return rcpp_result_gen;
END_RCPP
}
// runif_sphere
arma::mat runif_sphere(int n, int p);
RcppExport SEXP _Riemann_runif_sphere(SEXP nSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(runif_sphere(n, p));
    return rcpp_result_gen;
END_RCPP
}
// runif_stiefel
arma::cube runif_stiefel(int p, int k, int N);
RcppExport SEXP _Riemann_runif_stiefel(SEXP pSEXP, SEXP kSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(runif_stiefel(p, k, N));
    return rcpp_result_gen;
END_RCPP
}
// spdwass_sylvester
arma::mat spdwass_sylvester(arma::mat A, arma::mat X);
RcppExport SEXP _Riemann_spdwass_sylvester(SEXP ASEXP, SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(spdwass_sylvester(A, X));
    return rcpp_result_gen;
END_RCPP
}
// spdwass_log
arma::mat spdwass_log(arma::mat C, arma::mat X);
RcppExport SEXP _Riemann_spdwass_log(SEXP CSEXP, SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type C(CSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(spdwass_log(C, X));
    return rcpp_result_gen;
END_RCPP
}
// spdwass_exp
arma::mat spdwass_exp(arma::mat C, arma::mat V, double t);
RcppExport SEXP _Riemann_spdwass_exp(SEXP CSEXP, SEXP VSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type C(CSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type V(VSEXP);
    Rcpp::traits::input_parameter< double >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(spdwass_exp(C, V, t));
    return rcpp_result_gen;
END_RCPP
}
// spdwass_metric
double spdwass_metric(arma::mat S, arma::mat X, arma::mat Y);
RcppExport SEXP _Riemann_spdwass_metric(SEXP SSEXP, SEXP XSEXP, SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type S(SSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(spdwass_metric(S, X, Y));
    return rcpp_result_gen;
END_RCPP
}
// spdwass_baryRU02
arma::mat spdwass_baryRU02(arma::field<arma::mat> spdlist, arma::vec weight, int maxiter, double abstol);
RcppExport SEXP _Riemann_spdwass_baryRU02(SEXP spdlistSEXP, SEXP weightSEXP, SEXP maxiterSEXP, SEXP abstolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::field<arma::mat> >::type spdlist(spdlistSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type weight(weightSEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    Rcpp::traits::input_parameter< double >::type abstol(abstolSEXP);
    rcpp_result_gen = Rcpp::wrap(spdwass_baryRU02(spdlist, weight, maxiter, abstol));
    return rcpp_result_gen;
END_RCPP
}
// spdwass_baryAE16
arma::mat spdwass_baryAE16(arma::field<arma::mat> spdlist, arma::vec weight, int maxiter, double abstol);
RcppExport SEXP _Riemann_spdwass_baryAE16(SEXP spdlistSEXP, SEXP weightSEXP, SEXP maxiterSEXP, SEXP abstolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::field<arma::mat> >::type spdlist(spdlistSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type weight(weightSEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    Rcpp::traits::input_parameter< double >::type abstol(abstolSEXP);
    rcpp_result_gen = Rcpp::wrap(spdwass_baryAE16(spdlist, weight, maxiter, abstol));
    return rcpp_result_gen;
END_RCPP
}
// src_spd_pdist
arma::mat src_spd_pdist(arma::cube& data, std::string geometry);
RcppExport SEXP _Riemann_src_spd_pdist(SEXP dataSEXP, SEXP geometrySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cube& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::string >::type geometry(geometrySEXP);
    rcpp_result_gen = Rcpp::wrap(src_spd_pdist(data, geometry));
    return rcpp_result_gen;
END_RCPP
}
// mat_rank
arma::uword mat_rank(arma::mat A);
RcppExport SEXP _Riemann_mat_rank(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(mat_rank(A));
    return rcpp_result_gen;
END_RCPP
}
// mat_symm
arma::mat mat_symm(arma::mat A, bool diag);
RcppExport SEXP _Riemann_mat_symm(SEXP ASEXP, SEXP diagSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< bool >::type diag(diagSEXP);
    rcpp_result_gen = Rcpp::wrap(mat_symm(A, diag));
    return rcpp_result_gen;
END_RCPP
}
// mat_diaghalf
arma::mat mat_diaghalf(arma::mat D);
RcppExport SEXP _Riemann_mat_diaghalf(SEXP DSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type D(DSEXP);
    rcpp_result_gen = Rcpp::wrap(mat_diaghalf(D));
    return rcpp_result_gen;
END_RCPP
}
// mat_diaginvhalf
arma::mat mat_diaginvhalf(arma::mat D);
RcppExport SEXP _Riemann_mat_diaginvhalf(SEXP DSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type D(DSEXP);
    rcpp_result_gen = Rcpp::wrap(mat_diaginvhalf(D));
    return rcpp_result_gen;
END_RCPP
}
// mat_cov2cor
arma::mat mat_cov2cor(arma::mat A);
RcppExport SEXP _Riemann_mat_cov2cor(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(mat_cov2cor(A));
    return rcpp_result_gen;
END_RCPP
}
// cpp_rmvnorm
arma::mat cpp_rmvnorm(int n, arma::vec mu, arma::mat sigma);
RcppExport SEXP _Riemann_cpp_rmvnorm(SEXP nSEXP, SEXP muSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_rmvnorm(n, mu, sigma));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_Riemann_cpp_ipot20", (DL_FUNC) &_Riemann_cpp_ipot20, 8},
    {"_Riemann_cpp_pdist", (DL_FUNC) &_Riemann_cpp_pdist, 1},
    {"_Riemann_macg_mle", (DL_FUNC) &_Riemann_macg_mle, 3},
    {"_Riemann_macg_sample", (DL_FUNC) &_Riemann_macg_sample, 3},
    {"_Riemann_macg_density", (DL_FUNC) &_Riemann_macg_density, 2},
    {"_Riemann_acg_mle", (DL_FUNC) &_Riemann_acg_mle, 3},
    {"_Riemann_acg_density", (DL_FUNC) &_Riemann_acg_density, 2},
    {"_Riemann_cppdist_int_1toN", (DL_FUNC) &_Riemann_cppdist_int_1toN, 2},
    {"_Riemann_cppdist_ext_1toN", (DL_FUNC) &_Riemann_cppdist_ext_1toN, 2},
    {"_Riemann_basic_pdist", (DL_FUNC) &_Riemann_basic_pdist, 3},
    {"_Riemann_basic_pdist2", (DL_FUNC) &_Riemann_basic_pdist2, 4},
    {"_Riemann_basic_interpolate", (DL_FUNC) &_Riemann_basic_interpolate, 5},
    {"_Riemann_inference_mean_intrinsic", (DL_FUNC) &_Riemann_inference_mean_intrinsic, 5},
    {"_Riemann_inference_mean_extrinsic", (DL_FUNC) &_Riemann_inference_mean_extrinsic, 5},
    {"_Riemann_inference_median_intrinsic", (DL_FUNC) &_Riemann_inference_median_intrinsic, 5},
    {"_Riemann_inference_median_extrinsic", (DL_FUNC) &_Riemann_inference_median_extrinsic, 5},
    {"_Riemann_clustering_nmshift", (DL_FUNC) &_Riemann_clustering_nmshift, 5},
    {"_Riemann_clustering_kmeans_lloyd", (DL_FUNC) &_Riemann_clustering_kmeans_lloyd, 6},
    {"_Riemann_clustering_kmeans_macqueen", (DL_FUNC) &_Riemann_clustering_kmeans_macqueen, 6},
    {"_Riemann_clustering_clrq", (DL_FUNC) &_Riemann_clustering_clrq, 5},
    {"_Riemann_clustering_sup_intrinsic", (DL_FUNC) &_Riemann_clustering_sup_intrinsic, 6},
    {"_Riemann_clustering_kmeans18B", (DL_FUNC) &_Riemann_clustering_kmeans18B, 6},
    {"_Riemann_cvi_internal_score", (DL_FUNC) &_Riemann_cvi_internal_score, 4},
    {"_Riemann_cvi_internal_gdxx", (DL_FUNC) &_Riemann_cvi_internal_gdxx, 6},
    {"_Riemann_cvi_internal_db", (DL_FUNC) &_Riemann_cvi_internal_db, 4},
    {"_Riemann_cvi_internal_ci", (DL_FUNC) &_Riemann_cvi_internal_ci, 4},
    {"_Riemann_cvi_internal_ch", (DL_FUNC) &_Riemann_cvi_internal_ch, 4},
    {"_Riemann_cvi_internal_dunn", (DL_FUNC) &_Riemann_cvi_internal_dunn, 4},
    {"_Riemann_visualize_pga", (DL_FUNC) &_Riemann_visualize_pga, 2},
    {"_Riemann_visualize_kpca", (DL_FUNC) &_Riemann_visualize_kpca, 4},
    {"_Riemann_visualize_isomap", (DL_FUNC) &_Riemann_visualize_isomap, 4},
    {"_Riemann_visualize_cmds", (DL_FUNC) &_Riemann_visualize_cmds, 4},
    {"_Riemann_visualize_sammon", (DL_FUNC) &_Riemann_visualize_sammon, 6},
    {"_Riemann_curvedist_lp", (DL_FUNC) &_Riemann_curvedist_lp, 6},
    {"_Riemann_curvedist_dtwbasic", (DL_FUNC) &_Riemann_curvedist_dtwbasic, 4},
    {"_Riemann_learning_seb", (DL_FUNC) &_Riemann_learning_seb, 5},
    {"_Riemann_learning_rmml", (DL_FUNC) &_Riemann_learning_rmml, 4},
    {"_Riemann_learning_coreset18B", (DL_FUNC) &_Riemann_learning_coreset18B, 6},
    {"_Riemann_runif_sphere", (DL_FUNC) &_Riemann_runif_sphere, 2},
    {"_Riemann_runif_stiefel", (DL_FUNC) &_Riemann_runif_stiefel, 3},
    {"_Riemann_spdwass_sylvester", (DL_FUNC) &_Riemann_spdwass_sylvester, 2},
    {"_Riemann_spdwass_log", (DL_FUNC) &_Riemann_spdwass_log, 2},
    {"_Riemann_spdwass_exp", (DL_FUNC) &_Riemann_spdwass_exp, 3},
    {"_Riemann_spdwass_metric", (DL_FUNC) &_Riemann_spdwass_metric, 3},
    {"_Riemann_spdwass_baryRU02", (DL_FUNC) &_Riemann_spdwass_baryRU02, 4},
    {"_Riemann_spdwass_baryAE16", (DL_FUNC) &_Riemann_spdwass_baryAE16, 4},
    {"_Riemann_src_spd_pdist", (DL_FUNC) &_Riemann_src_spd_pdist, 2},
    {"_Riemann_mat_rank", (DL_FUNC) &_Riemann_mat_rank, 1},
    {"_Riemann_mat_symm", (DL_FUNC) &_Riemann_mat_symm, 2},
    {"_Riemann_mat_diaghalf", (DL_FUNC) &_Riemann_mat_diaghalf, 1},
    {"_Riemann_mat_diaginvhalf", (DL_FUNC) &_Riemann_mat_diaginvhalf, 1},
    {"_Riemann_mat_cov2cor", (DL_FUNC) &_Riemann_mat_cov2cor, 1},
    {"_Riemann_cpp_rmvnorm", (DL_FUNC) &_Riemann_cpp_rmvnorm, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_Riemann(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
