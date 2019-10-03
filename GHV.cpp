#include<Rcpp.h>
using namespace Rcpp;

/* 
Following R code is replaced:
 
V <- df$NAV / df$Numeraire.Index

H <- cumsum(df$CashFlow / df$Numeraire.Index)

G <- cumsum(apply(PreCoef, 1, function(x){
(df[, x["Factor"]] - perc.fee) * df[, x["Predictor"]]
}) %*% PreCoef[, "Coef"] * df$NAV.lag1  / df$Numeraire.Index)
 
pos <- min(length(H), 15*12) # 15 years after vintage
NAV.adjustment <- 1
R <- (H[pos] + NAV.adjustment * V[pos] - G[pos])^2

R.list[[vin]] <- sum(R)/length(R)
*/

// [[Rcpp::export]]
double GHV_cpp(DataFrame df, DataFrame PreCoef, double perc_fee) {
  
  int nrow_df = std::min(df.nrow(), 180); // 180 = 12 * 15
  NumericVector VectorG(nrow_df, 0.0);
  
  int nrow_PreCoef = PreCoef.nrow();
  
  CharacterVector FactorVector = as<CharacterVector>(PreCoef["Factor"]);
  CharacterVector PredictorVector = as<CharacterVector>(PreCoef["Predictor"]);
  NumericVector CoefVector = as<NumericVector>(PreCoef["Coef"]);
  
  for(int i = 0; i < nrow_PreCoef; ++i) {
    double Coef = CoefVector[i];
    if (Coef == 0) continue;
    
    String Factor = FactorVector[i];
    NumericVector FactorValue = as<NumericVector>(df[Factor]);
    String Predictor = PredictorVector[i];
    NumericVector PredictorValue = as<NumericVector>(df[Predictor]);
    
    for(int j = 0; j < nrow_df; ++j) {
      VectorG[j] += (FactorValue[j] - perc_fee) * PredictorValue[j] * Coef;
    }
  }
  
  NumericVector NAV_lag1 = as<NumericVector>(df["NAV.lag1"]);
  NumericVector Numeraire_Index = as<NumericVector>(df["Numeraire.Index"]);
  
  for(int j = 0; j < nrow_df; ++j) {
    VectorG[j] *= NAV_lag1[j] / Numeraire_Index[j];
  }
  
  // G (numeraire-discounted replication cash flow)
  double FinalG = 0.0;
  for(int j = 0; j < nrow_df; ++j) {
    FinalG += VectorG[j];
  }
  
  // V (numeraire-discounted observed NAV)
  NumericVector NAV = as<NumericVector>(df["NAV"]);
  double FinalV = NAV[nrow_df-1] / Numeraire_Index[nrow_df-1];
  
  // H (numeraire-discounted obseved cash flow)
  NumericVector CashFlow = as<NumericVector>(df["CashFlow"]);
  double FinalH = 0.0;
  for(int j = 0; j < nrow_df; ++j) {
    FinalH += CashFlow[j] / Numeraire_Index[j];
  }
  
  // R <- (H[pos] + NAV.adjustment * V[pos] - G[pos])^2 // in empirical.loss()
  double R = std::pow(FinalH + FinalV - FinalG, 2);

  return R;
}
