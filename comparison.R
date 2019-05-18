##########################
## Simple Hedge Comparison
##########################
# Prologue ------------
if(sys.nframe() == 0L) rm(list = ls())
single_vintage_example <- new.env()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source("global_opts.R")
source("base_procedure.R")

# Estimate One Factor Model ----
perc.fee <- 2 / 100 / 12

preqin.basis <- readRDS("preqin_basis/MSCI.World_preqin_basis.RDS")

predictor.coefs <- base_procedure$base.procedure(
  fp.coefs = base_procedure$create.predictor.coefs(preqin.basis = preqin.basis), 
  perc.fee = perc.fee, preqin.basis = preqin.basis, 
  step.len = 1, p.set = "One", f.set = "NASDAQ.Numeraire")

print(predictor.coefs$Coef)

One.Factor.hedge <- function(beta, driessen){
  list.of.dfs <- preqin.basis$df.vc
  
  hedge.list <- list()
  for(vintage.year in names(list.of.dfs)) {
    df <- list.of.dfs[[vintage.year]]
    df$LinearReturn <- (df$NASDAQ.Numeraire - perc.fee) * beta
    
    df$ReplicationCF <- df$LinearReturn * df$NAV.lag1 / df$Numeraire.Index
    
    hedge.list[[vintage.year]] <- data.frame(Vintage = vintage.year, 
                                             Observed = sum(df$CashFlow / df$Numeraire.Index),
                                             Replication = sum(df$ReplicationCF),
                                             Error = sum(df$CashFlow / df$Numeraire.Index) - sum(df$ReplicationCF))
  }
  df.out <- data.frame(do.call(rbind, hedge.list))
  df.out[nrow(df.out)+1, 2:4] <- colSums(df.out[, 2:4])
  rownames(df.out)[nrow(df.out)] <- "Sum"
  return(df.out)
}

df.boost.one.factor <- One.Factor.hedge(predictor.coefs$Coef)

# Driessen et al (2012) Method ----

npv.sqr <- function(par){
list.of.dfs <- preqin.basis$df.vc

discounted.cashflow <- list()
for(vintage.year in names(list.of.dfs)) {
  df <- list.of.dfs[[vintage.year]]
  df$LinearReturn <- df$Numeraire.Return + df$NASDAQ.Numeraire * par - perc.fee
  df$CompundReturn <- exp(cumsum(log(1+df$LinearReturn)))
  
  discounted.cashflow[[vintage.year]] <- sum(df$CashFlow / df$CompundReturn)
}
dcf <- as.numeric(discounted.cashflow)
return(sum(dcf^2))
}
npv.sqr(1)

beta.start <- 0
res <- optimx::optimx(par = beta.start, fn = npv.sqr, control = list(all.methods=TRUE))
beta.estimate <- res['BFGS', 'p1']

df.driessen <- One.Factor.hedge(beta.estimate)


# merge to tex table ----

tex.comparison <- function(df.b = df.boost.one.factor, df.d = df.driessen) {
  colnames(df.b)[3:4] <- paste(colnames(df.b)[3:4], "Boost", sep = ".")
  colnames(df.d)[3:4] <- paste(colnames(df.d)[3:4], "Driessen", sep = ".")
  
  df <- cbind(df.b[2], df.b[, 3:4], df.d[, 3:4])
  
  print(xtable::xtable(df))
  return(df)
}
df.comparison <- tex.comparison()


