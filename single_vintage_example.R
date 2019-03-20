#########################
## single vintage example
#########################
# Prologue  ------------
if(sys.nframe() == 0L) rm(list = ls())
single_vintage_example <- new.env()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source("global_opts.R")
source("base_procedure.R")


# Gain function vintage year wrapper  ---------
single_vintage_example$gain.process.vin <- function(vintage, PreCoef, preqin.basis, perc.fee) {
  base_procedure$gain.process(0, 
               PreCoef = PreCoef,
               df = preqin.basis$df.vc[[as.character(vintage)]],                             
               factor = "NASDAQ.Numeraire",
               predictor = "One", 
               perc.fee = perc.fee)
}


# Plot single vintage (perfect ex-post) hedge ------
single_vintage_example$plot.single.vin <- function(do.eps, PreCoef, preqin.basis, perc.fee, tag=""){
  cost.list <- single_vintage_example$gain.process.vin(vintage = vintage, 
                                                       PreCoef = PreCoef, 
                                                       preqin.basis = preqin.basis, 
                                                       perc.fee = perc.fee)
  H <- cost.list$H
  V <- cost.list$V
  G <- cost.list$G
  
  df.vc.us <- preqin.basis$df.vc[[as.character(vintage)]]
  
  if(do.eps) {
    setEPS()
    postscript(paste("EPS/", Sys.Date(), "SingleVintage", tag,".eps", sep=""), width = 5.5, height = 4,
               family = "Helvetica", pointsize = 5)
    par(mfrow=c(2,2), cex=1.1)
  }
  par(mar = c(4.5, 4.1, 4.1, 1))
  plot(df.vc.us$Date, V, type="l", main = "Value", xlab = "Date", ylab = "Value")
  abline(h=0, col="darkgrey", lty=2)
  plot(df.vc.us$Date, G, type="l", main = "Gain", xlab = "Date", ylab = "Gain")
  abline(h=0, col="darkgrey", lty=2)
  max.val <- max(c(H, G-V)) ; min.val <- min(c(H, G-V))
  plot(df.vc.us$Date, H, type="l", main = "Cash Flow & Replication", xlab = "Date", 
       ylab = "Cash Flow Replication", ylim = c(min.val, max.val))
  lines(df.vc.us$Date, G - V, col="blue")
  abline(h=0, col="darkgrey", lty=2)
  legend("bottomright", bty="n", legend = c("Cash Flow", "Gain - Value"), col=c("black","blue"), lty=1)
  plot(df.vc.us$Date, (H + V - G), type = "l", main = "Hedging Error", xlab = "Date", ylab = "Cost")
  abline(h=0, col="darkgrey", lty=2)
  abline(h=tail((H + V - G),1),col="red")
  
  if(do.eps) {
    par(mfrow=c(1,1), cex=1)
    dev.off() 
  }
}


# Plot PE Fund Dynamics (Example) --------
single_vintage_example$plot.example.dynamics <- function(preqin.basis) {
  
  cost.list <- single_vintage_example$gain.process.vin(vintage = vintage, preqin.basis = preqin.basis, perc.fee = 0,
                                PreCoef = create.predictor.coefs(preqin.basis = preqin.basis))
  
  df.vc.us <- preqin.basis$df.vc[[as.character(vintage)]]
  
  H <- cost.list$H
  V <- cost.list$V

  if(create.EPS) {
    setEPS()
    postscript("EPS/", Sys.Date(), "ValueCashFlows.eps", width = 5.5, height = 4.5,
               family = "Helvetica", pointsize = 8)
    par(mfrow=c(1,1), cex=1.4)
  }
  
  CF <- df.vc.us$CashFlow / df.vc.us$Numeraire.Index
  max.val <- max(c(H, V, CF)) ; min.val <- min(c(H, V, CF))
  plot(df.vc.us$Date, V, type="l", ylim = c(min.val, max.val), col = "grey",
       main = "Private Equity Fund Dynamics (US VC, Vintage 1992)", xlab = "Date", ylab = "Amount")
  abline(h=0, col="darkgrey", lty=2)
  lines(df.vc.us$Date, H)
  
  points(df.vc.us$Date, CF, type = "h", col = "red")
  
  legend("bottomright", bty="n", legend = c("Cumulative CashFlow", "CashFlows", "Value"), col=c("black", "red", "grey"), lty=1)
  
  if(create.EPS) {
    par(mfrow=c(1,1), cex=1)
    dev.off() 
  }
}


# Epilogue -----------

if(sys.nframe() == 0L) {
  # Init: Options & preqin.basis
  perc.fee <- 0 / 100 / 12
  
  preqin.basis <- readRDS("preqin_basis/MSCI.World_preqin_basis.RDS")
  
  
  # Estimate perfect ex-post single vintage year hedge
  predictor.coefs <- base.procedure(fp.coefs = create.predictor.coefs(preqin.basis = preqin.basis), 
                                    perc.fee = perc.fee, preqin.basis = preqin.basis, 
                                    step.len = 1, p.set = "One", f.set = "NASDAQ.Numeraire")
  print(predictor.coefs)
  
  # test gain function
  tail(single_vintage_example$gain.process.vin(vintage = vintage, 
                                               PreCoef = predictor.coefs, 
                                               preqin.basis = preqin.basis, 
                                               perc.fee = perc.fee)$G, 1)
  
  # Plot single vintage year result
  single_vintage_example$plot.single.vin(do.eps=create.EPS, 
                                         PreCoef = predictor.coefs, 
                                         preqin.basis = preqin.basis, 
                                         perc.fee = perc.fee)
  
  # Plot corresponding vintage year dynamics
  single_vintage_example$plot.example.dynamics(preqin.basis = preqin.basis)
  
}

