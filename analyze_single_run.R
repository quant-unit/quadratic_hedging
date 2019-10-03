#####################
## Analyze Single Run
#####################
# Prologue --------
if(sys.nframe() == 0L) rm(list = ls())
analyze_single_run <- new.env()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source("global_opts.R")


# Extract run by run.name ----------
analyze_single_run$extract.run <- function(run.name) {
  out.list <- list()
  
  cv.list <- list()
  for(filename in list.files("results")) {
    if(grepl(run.name, filename)) {
      
      if(grepl("add.info", filename)) {
        add.info <- readRDS(paste("results/", filename, sep=""))
        out.list[["preqin.basis"]] <- add.info[["preqin.basis"]]
        out.list[["party"]] <- add.info[["party"]]
      } else {
        nuname <- strsplit(strsplit(filename, ".RDS")[[1]], run.name)[[1]][2]
        cv.list[[nuname]] <- readRDS(paste("results/", filename, sep=""))
      }
      
    }
  }
  out.list[["cv.list"]] <- cv.list
  out.list[["run.name"]] <- run.name
  
  return(out.list)
}


# Analyze cross validation result ---------
analyze_single_run$get.min.err <- function(perc = 0, run.ob) {
  cv.list.in = run.ob$cv.list
  party <- run.ob$party
  
  if(length(cv.list.in) == 1) {
    min.cv.err <- NULL
  } else {
    
    df.cv <- data.frame(do.call(rbind, lapply(party, function(x) cv.list.in[[x]]$cv.error)))
    cv.error.final <- colMeans(df.cv)
    cv.error.final.perc <- cv.error.final / min(cv.error.final) - 1
    
    iterM <- cv.error.final.perc[cv.error.final.perc <= perc][1]
    min.cv.err <- as.numeric(substring(names(iterM), 2))
    
  }
  
  return(min.cv.err)
}
analyze_single_run$plot.df <- function(run.ob, Coefs = "Full", do.eps = FALSE, tag="") {
  cv.list <- run.ob$cv.list
  
  df <- cv.list[[Coefs]]$model
  r <- nrow(df)
  min.cv.row <- analyze_single_run$get.min.err(0, run.ob = run.ob)
  print(df[c(round(r/4,0), round(r/2,0), round(r/4*3,0), r, min.cv.row), ])
  
  if(do.eps) {
    dir.create("eps", showWarnings = FALSE)
    setEPS()
    eps.name <- paste("eps/", Sys.Date(), "BoostingCoefs", tag, ".eps", sep="")
    postscript(eps.name, width = 5.5, height = 3,
               family = "Helvetica", pointsize = 8)
    par(mfrow=c(1,1), cex=1.3)
  }
  
  par(mar = c(4.1, 4.1, 1, 15))
  plot(x = 0:(nrow(df)-1), y = df[, 1], type = "l", ylim = c(min(df), max(df)), 
       col = "gray50",
       # main = "Componentwise L2 Boosting Coefficients",
       xlab = "Iteration",  ylab = "Coefficients")
  abline(h=0, v=0, col="grey", lty=3)
  for(i in 2:ncol(df)) {
    lines(x = 0:(nrow(df)-1), y = df[, i], col = "gray50") 
  }
  axis(side = 4, # right side
       at = df[nrow(df), ], 
       labels = paste(round(df[nrow(df), ],2), colnames(df)), 
       las = 1) # horizontal text rotation
  
  # legend("right", bty="n", legend = colnames(df), col = 1:ncol(df), lty = 1, cex=1)
  abline(v = min.cv.row, col="grey", lty=2)
  par(mar = c(5.1, 4.1, 4.1, 2.1))
  
  if(do.eps) {
    par(mfrow=c(1,1), cex=1)
    dev.off() 
  }
}
analyze_single_run$plot.cv.error <- function(do.eps = FALSE, run.ob) {
  cv.list.in <- run.ob$cv.list
  party <- run.ob$party
  
  if(do.eps) {
    dir.create("eps", showWarnings = FALSE)
    setEPS()
    postscript(paste("eps/", Sys.Date(), "LOVO_CrossValidation.eps",sep=""), width = 5.5, height = 2.5,
               family = "Helvetica", pointsize = 8)
    par(mfrow=c(1,1),  cex=1.3, mar = c(4.5, 4.5, 1, 1))
  }
  
  df.cv <- data.frame(do.call(rbind, lapply(party, function(x) cv.list.in[[x]]$cv.error)))
  cv.error.final <- colMeans(df.cv)
  # print(cv.error.final)
  plot(cv.error.final, type = "l", 
       # main = "Leave One Vintage Out - Cross Validation", 
       xlab = "Iteration",ylab = "Mean Cross Validation Error")
  
  abline(v = analyze_single_run$get.min.err(0, run.ob = run.ob), col="grey", lty=2)
  
  if(do.eps) {
    par(mfrow=c(1,1), cex=1)
    dev.off() 
  }
  
  return(cv.error.final)
}


# Apply strategy to all vintages ---------
analyze_single_run$tex.coef <- function(run.ob, add.percs = NA) {
  
  model.list = run.ob$cv.list$Full$model
  
  min.iter <- analyze_single_run$get.min.err(0, run.ob = run.ob)
  df <- data.frame(t(model.list[min.iter, ]))
  colnames(df) <- paste("Coef", min.iter, sep = "_")
  df$Factor <- NA
  df$Predictor <- NA
  df <- df[, c("Factor", "Predictor", paste("Coef", min.iter, sep = "_"))]
  df$FacPred <- rownames(df)
  
  if(!(is.na(add.percs[1]))) {
    for(perc in add.percs) {
      iter <- analyze_single_run$get.min.err(perc, run.ob = run.ob)
      df2 <- data.frame(t(model.list[iter,]))
      colnames(df2) <- paste("Coef", iter, sep="_")
      df2$FacPred <- rownames(df2)
      
      df <- merge(df, df2,by="FacPred",all.x=TRUE)
    }
  }
  
  for(i in 1:nrow(df)) {
    df$Factor[i] <- strsplit(df$FacPred, "_")[[i]][1]
    df$Predictor[i] <- strsplit(df$FacPred, "_")[[i]][2]
  }
  df$FacPred <- NULL
  df <- df[order(df$Factor, df$Predictor), ]
  print(xtable::xtable(df), include.rownames=FALSE)
  return(df)
}
analyze_single_run$hedge.it <- function(df = df.vc.us, coefs, perc.fee, time.series=FALSE) {
  cashflows <- rep(0, nrow(df))
  # apply strategy
  for(i in 1:nrow(coefs)) {
    fac <- strsplit(rownames(coefs), "_")[[i]][1]
    pred <- strsplit(rownames(coefs), "_")[[i]][2]
    cf <- coefs[i, ] * (df[, fac] - perc.fee) * df[, pred] * df$NAV.lag1 / df$Numeraire.Index
    cashflows <- cashflows + cf
  }
  if(time.series) {
    return(cumsum(cashflows))
  } else {
    return(sum(cashflows[1:(15*12)]))
  }
}
analyze_single_run$name2fee <- function(run.name) {
  r.name <- strsplit(run.name, "_i")[[1]][1]
  perc.fee <- as.numeric(substr(r.name, nchar(r.name), nchar(r.name)))
  return(perc.fee/100/12)
}
analyze_single_run$tex.repl <- function(run.ob, percs) {
  preqin.basis <- run.ob$preqin.basis
  cv.list <- run.ob$cv.list
  
  perc.fee <- analyze_single_run$name2fee(run.ob$run.name)
  
  df.replc <- data.frame(Observed = sapply(preqin.basis$df.vc, function(x) sum(x$CashFlow / x$Numeraire.Index)))
  for(perc in percs) {
    iterM <- analyze_single_run$get.min.err(perc, run.ob = run.ob)
    df.coef <- data.frame(t(cv.list$Full$model[iterM, ]))
    df.replc[, paste("Replication", iterM, sep="_")] <- sapply(preqin.basis$df.vc, analyze_single_run$hedge.it,
                                                               coefs = df.coef, perc.fee = perc.fee)
    df.replc[, paste("Error", iterM, sep="_")] <- df.replc$Observed - df.replc[, paste("Replication", iterM, sep="_")]
  }
  df.replc <- rbind(df.replc, Sum= colSums(df.replc))
  print(xtable::xtable(df.replc))
  return(df.replc)
}


# Plot sinlge vintage year hedge -----
analyze_single_run$get.best.predictor.factor <- function(run.ob) {
  df <- analyze_single_run$tex.coef(run.ob = run.ob)
  rownames(df) <- NULL
  colnames(df)[3] <- "Coef"
  return(df)
}


# Plot LOVO cross validation error for World and NoAm --------
analyze_single_run$plot.cv.error.double <- function(do.eps = create.EPS) {
  
  cv.er.W <- analyze_single_run$plot.cv.error(run.ob = run.ob.World, do.eps = create.EPS)
  cv.er.N <- analyze_single_run$plot.cv.error(run.ob = run.ob.NoAm, do.eps = create.EPS)
  
  if(do.eps) {
    dir.create("eps", showWarnings = FALSE)
    setEPS()
    postscript(paste("eps/", Sys.Date(), "LOVO_CrossValidation_BOTH.eps",sep=""), width = 5.5, height = 2.5,
               family = "Helvetica", pointsize = 8)
    par(mfrow=c(1,1),  cex=1.3, mar = c(4.5, 4.5, 1, 1))
  }
  
  plot(cv.er.W, ylim = c(0, 0.25), type = "l", ylab = "Mean Cross Validation Error", xlab = "Iteration")
  lines(cv.er.N, lty = 2)
  abline(v = analyze_single_run$get.min.err(run.ob = run.ob.World))
  abline(v = analyze_single_run$get.min.err(run.ob = run.ob.NoAm), lty = 2)
  
  legend("topright", bty="n", legend = c("World", "NoAm"), lty = c(1,2))
  
  if(do.eps) {
    par(mfrow=c(1,1), cex=1)
    dev.off() 
  }
  
}
analyze_single_run$tex.repl.both <- function() {
  df.World <- analyze_single_run$tex.repl(run.ob = run.ob.World, percs = 0)
  df.NoAm <- analyze_single_run$tex.repl(run.ob = run.ob.NoAm, percs = 0)
  
  df.rep <- merge(df.World, df.NoAm, by = "row.names", suffixes=c(".World", ".NoAm"))
  rownames(df.rep) <- df.rep$Row.names
  df.rep$Row.names <- NULL
  
  print(xtable::xtable(df.rep))
  return(df.rep)
}


# Epilogue -------

if(sys.nframe() == 0L) {
  # list.files("results")
  
  # select run.name
  run.name <- "2019-10-01_MSCI.World_f2_i10"
  
  # extract cached run information
  run.ob <- analyze_single_run$extract.run(run.name)
  
  # analyze cross validation result
  analyze_single_run$get.min.err(run.ob = run.ob)
  analyze_single_run$plot.df(run.ob = run.ob, do.eps = create.EPS)
  analyze_single_run$plot.cv.error(run.ob = run.ob, do.eps = create.EPS)
 
  # apply strategy to all vintages
  analyze_single_run$tex.coef(run.ob = run.ob, add.percs = c(0.05))
  analyze_single_run$tex.repl(run.ob = run.ob, percs = c(0, 0.05))
  
  # plot single vintage year hedge
  source("single_vintage_example.R")
  single_vintage_example$plot.single.vin(do.eps = create.EPS, 
                  perc.fee = analyze_single_run$name2fee(run.ob$run.name),
                  PreCoef = analyze_single_run$get.best.predictor.factor(run.ob), 
                  preqin.basis = run.ob$preqin.basis)
  
  # compare World vs. NoAm run
  run.ob.World <- analyze_single_run$extract.run("2019-10-01_MSCI.World_f2_i10")
  run.ob.NoAm  <- analyze_single_run$extract.run("2019-10-01_MSCI.NoAM_f2_i10")
  
  analyze_single_run$plot.cv.error.double()
  analyze_single_run$tex.repl.both()
  
  analyze_single_run$plot.df(run.ob = run.ob.World, do.eps = create.EPS, tag = "World")
  analyze_single_run$plot.df(run.ob = run.ob.NoAm, do.eps = create.EPS, tag = "NoAm")
  
}
  


