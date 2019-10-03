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
beta <- list()
preqin.basis <- readRDS("preqin_basis/MSCI.World_preqin_basis.RDS")

predictor.coefs <- base_procedure$base.procedure(
  fp.coefs = base_procedure$create.predictor.coefs(preqin.basis = preqin.basis), 
  perc.fee = perc.fee, preqin.basis = preqin.basis, 
  step.len = 1, p.set = "One", f.set = "NASDAQ.Numeraire")

print(predictor.coefs$Coef)
beta[['boost']] <- predictor.coefs$Coef

One.Factor.hedge <- function(beta){
  list.of.dfs <- preqin.basis$df.vc
  
  hedge.list <- list()
  for(vintage.year in names(list.of.dfs)) {
    df <- list.of.dfs[[vintage.year]]
    df <- df[1:(15*12), ] # cut after 15 years
    
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
  df <- df[1:(15*12), ] # cut after 15 years
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
beta.estimate
beta[['driessen']] <- beta.estimate

df.driessen <- One.Factor.hedge(beta[['driessen']])


# mPME one factor model ------
par <- 1
mPME.sqr <- function(par) {
  list.of.dfs <- preqin.basis$df.vc
  
  discounted.cashflow <- list()
  for(vintage.year in names(list.of.dfs)) {
    df <- list.of.dfs[[vintage.year]]
    df <- df[1:(15*12), ] # cut after 15 years
    
    df$LinearReturn <- df$Numeraire.Return + df$NASDAQ.Numeraire * par - perc.fee

    # modified PME
    df$Call <- ifelse(df$CashFlow <= 0, - df$CashFlow, 0)
    df$Dist <- ifelse(df$CashFlow >= 0, df$CashFlow, 0)
    df$weight.Dist <- ifelse(df$NAV == 0, 0, df$Dist / (df$Dist + df$NAV))
    df$NAV.mPME <- 0
    df$Dist.mPME <- 0
    for(i in 1:nrow(df)) {
      if(i == 1) {
        a <- 0
      } else {
        a <- 1
      }
      df$NAV.mPME[i] <- (1 - df$weight.Dist[i]) * (df$NAV.mPME[i-a] * (1+df$LinearReturn[i]) + df$Call[i] )
      df$Dist.mPME[i] <- df$weight.Dist[i] * (df$NAV.mPME[i-a] * (1+df$LinearReturn[i]) + df$Call[i] )
    }
    
    realized <- sum(df$CashFlow / df$Numeraire.Index)
    # use observed Call cash flows to finance trading strategy
    replication <- sum( (df$Dist.mPME - df$Call) / df$Numeraire.Index)
    discounted.cashflow[[vintage.year]] <- realized - replication
  }
  dcf <- as.numeric(discounted.cashflow)
  return(sum(dcf^2))
}
mPME.sqr(1)

res <- optimize(mPME.sqr, c(-10, 10))
beta.estimate <- res$minimum
beta.estimate
beta[['mPME']] <- beta.estimate
  
mPME.hedge <- function(beta){
  list.of.dfs <- preqin.basis$df.vc
  
  hedge.list <- list()
  for(vintage.year in names(list.of.dfs)) {
    df <- list.of.dfs[[vintage.year]]
    df <- df[1:(15*12), ] # cut after 15 years
    
    df$LinearReturn <- df$Numeraire.Return + df$NASDAQ.Numeraire * par - perc.fee
    
    # modified PME
    df$Call <- ifelse(df$CashFlow <= 0, - df$CashFlow, 0)
    df$Dist <- ifelse(df$CashFlow >= 0, df$CashFlow, 0)
    df$weight.Dist <- ifelse(df$NAV == 0, 0, df$Dist / (df$Dist + df$NAV))
    df$NAV.mPME <- 0
    df$Dist.mPME <- 0
    for(i in 1:nrow(df)) {
      if(i == 1) {
        a <- 0
      } else {
        a <- 1
      }
      df$NAV.mPME[i] <- (1 - df$weight.Dist[i]) * (df$NAV.mPME[i-a] * (1+df$LinearReturn[i]) + df$Call[i] )
      df$Dist.mPME[i] <- df$weight.Dist[i] * (df$NAV.mPME[i-a] * (1+df$LinearReturn[i]) + df$Call[i] )
    }
    
    realized <- sum(df$CashFlow / df$Numeraire.Index)
    replication <- sum( (df$Dist.mPME - df$Call) / df$Numeraire.Index)
    
    hedge.list[[vintage.year]] <- data.frame(Vintage = vintage.year, 
                                             Observed = realized,
                                             Replication = replication,
                                             Error = realized - replication)
  }
  df.out <- data.frame(do.call(rbind, hedge.list))
  df.out[nrow(df.out)+1, 2:4] <- colSums(df.out[, 2:4])
  rownames(df.out)[nrow(df.out)] <- "Sum"
  return(df.out)
}
df.mPME <- mPME.hedge(beta[['mPME']])

# merge to tex table ----

tex.comparison1 <- function(df.b = df.boost.one.factor, df.d = df.driessen) {
  colnames(df.b)[3:4] <- paste(colnames(df.b)[3:4], "Boost", sep = ".")
  colnames(df.d)[3:4] <- paste(colnames(df.d)[3:4], "Driessen", sep = ".")

  df <- cbind(df.b[2], df.b[, 3:4], df.d[, 3:4])
  
  print(xtable::xtable(df))
  return(df)
}
df.comparison1 <- tex.comparison()

tex.comparison2 <- function(df.b = df.boost.one.factor, df.d = df.driessen, df.m = df.mPME) {
  colnames(df.b)[3:4] <- paste(colnames(df.b)[3:4], "Boost", sep = ".")
  colnames(df.d)[3:4] <- paste(colnames(df.d)[3:4], "Driessen", sep = ".")
  colnames(df.m)[3:4] <- paste(colnames(df.m)[3:4], "mPME", sep = ".")
  
  df <- cbind(df.b[2], df.b[, 3:4], df.d[, 3:4], df.m[, 3:4])
  
  print(xtable::xtable(df))
  return(df)
}
df.comparison2 <- tex.comparison()

beta
