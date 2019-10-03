#################
## base procedure
#################
# Prologue ------
base_procedure <- new.env()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Create predictor.coefs -----------
base_procedure$create.predictor.coefs <- function (use.stability = FALSE, preqin.basis) {
  
  predictor.coefs <- data.frame(expand.grid(Factor = preqin.basis$factor.set, 
                                            Predictor = preqin.basis$predictor.set), Coef=0)
  
  if(TRUE) {
    predictor.coefs <- predictor.coefs[(predictor.coefs$Factor %in% "NASDAQ.Numeraire") | predictor.coefs$Predictor == "One", ]
  }
  
  stability.path <- "results/StabilitySelection.RDS"

  if(use.stability & file.exists(stability.path)) {
    stability.selection.predictor.coefs <- function(path) {
      df.ss <- readRDS(path)
      
      selection.criterion <- mean(df.ss$Selected)
      print(paste("Stability Selection Criterion", selection.criterion))
      
      fac.preds <- df.ss$FacPred[df.ss$Selected >= selection.criterion]
      fac.preds <- strsplit(fac.preds, "_")
      pc <- data.frame(Coef = rep(0, length(fac.preds)))
      pc$Factor <- NA
      pc$Predictor <- NA
      for(i in 1:length(fac.preds)) {
        fp <- fac.preds[[i]]
        pc$Factor[i] <- fp[1]
        pc$Predictor[i] <- fp[2]
      }
      return(pc)
    }
    
    predictor.coefs <- stability.selection.predictor.coefs(stability.path)
  }
  
  rownames(predictor.coefs) <- paste(predictor.coefs$Factor, predictor.coefs$Predictor)
  return(list(df = predictor.coefs, error = NA))
}

# Gain Process ------------
Rcpp::sourceCpp('GHV.cpp')

base_procedure$gain.process1 <- function(par, 
                         df, 
                         PreCoef,
                         factor,
                         predictor,
                         perc.fee) {
  H <- cumsum(df$CashFlow / df$Numeraire.Index)
  V <- df$NAV / df$Numeraire.Index
  
  # Update Predictor x Factor Coefficient
  PreCoef[PreCoef$Predictor == predictor & 
            PreCoef$Factor == factor, "Coef"] <- PreCoef[PreCoef$Predictor == predictor & 
                                                           PreCoef$Factor == factor, "Coef"] + par
  
  PreCoef.nonzero <- PreCoef[PreCoef$Coef != 0, ]
  
  if(nrow(PreCoef.nonzero) == 0) {
    G <- df$Numeraire.Index * 0
  } else {
    G <- cumsum(apply(PreCoef.nonzero, 1, function(x){
      (df[, x["Factor"]] - perc.fee) * df[, x["Predictor"]]
    }) %*% PreCoef.nonzero[, "Coef"] * df$NAV.lag1  / df$Numeraire.Index)
  }
  
  
  return(list(H = H, V = V, G = G, PreCoef = PreCoef))
}

base_procedure$gain.process <- function(par, 
                                        df, 
                                        PreCoef,
                                        FactorPredictor,
                                        perc.fee) {
  # Update Predictor x Factor Coefficient
  PreCoef[FactorPredictor, "Coef"] <- PreCoef[FactorPredictor, "Coef"] + par
  
  return(GHV_cpp(df, PreCoef, perc.fee))
}

# Empirical Loss Function -------------
base_procedure$empirical.loss1 <- function(par, data.list, perc.fee, factor, predictor, predictor.coefs) {
  R.list <- list()
  for(vin in names(data.list)) {
    gain.list <- base_procedure$gain.process1(par, df = data.list[[vin]], 
                              PreCoef = predictor.coefs, 
                              factor = factor,
                              predictor = predictor,
                              perc.fee = perc.fee)
    
    H <- gain.list$H # observed cash flow
    V <- gain.list$V # observed value
    G <- gain.list$G # hedging gain

    pos <- min(length(H), 15*12) # 15 years after vintage
    NAV.adjustment <- 1
    R <- (H[pos] + NAV.adjustment * V[pos] - G[pos])^2
    
    R.list[[vin]] <- sum(R)/length(R)
  }
  return( sum(unlist(R.list)) )
}

base_procedure$empirical.loss <- function(par, data.list, perc.fee, FactorPredictor, predictor.coefs) {
  empirical.loss.value <- 0 
  for(vin in names(data.list)) {
    empirical.loss.value <- empirical.loss.value + base_procedure$gain.process(par, df = data.list[[vin]], 
                                                                               PreCoef = predictor.coefs, 
                                                                               FactorPredictor = FactorPredictor,
                                                                               perc.fee = perc.fee)
  }
  return(empirical.loss.value)
}


# Define base procedure ------------
base_procedure$base.procedure1 <- function(fp.coefs, perc.fee, preqin.basis, step.len = 0.3, bound = 100, p.set = NA, f.set = NA, partition = "Full") {
  
  if(!is.na(p.set)) fp.coefs <- fp.coefs[fp.coefs$Predictor %in% p.set, ]
  if(!is.na(f.set)) fp.coefs <- fp.coefs[fp.coefs$Factor %in% f.set, ]
  
  iterations <- 1
  for(i in 1:iterations) {
    # select or create data
    if(i == iterations) {
      list.of.dfs <- preqin.basis$df.vc
    } else {
      list.of.dfs <- create.df.vc.us(TRUE)$df.vc
    }
    
    # remove vintage for cross validation
    if(partition %in% names(list.of.dfs)) list.of.dfs[partition] <- NULL
    
    # restrict no of predictor-factors (hard threshold)
    rows2use <- 1:nrow(fp.coefs)
    if(sum(fp.coefs$Coef != 0) == 60) {
      rows2use <- as.numeric(rownames(fp.coefs[fp.coefs$Coef != 0, ]))
    }
    
    # try all combinations
    res.list <- list()
    for(row in rows2use) {
      factor <- fp.coefs$Factor[row]
      predictor <- fp.coefs$Predictor[row]
      
      f <- function(x) {
        y <- base_procedure$empirical.loss1(x, 
                           data.list = list.of.dfs,
                           perc.fee = perc.fee,
                           predictor.coefs = fp.coefs,
                           factor = factor,
                           predictor = predictor)
        return(y)
      }
      
      res <- optimize(f, c(-bound, bound))
      res.list[[paste(factor, predictor, sep="_")]] <- data.frame(res)
    }
    
    # average performance (over boostrap samples)
    if(i == 1) {
      df.opti <- data.frame(do.call(rbind, res.list))
    } else {
      df.o.new <- data.frame(do.call(rbind, res.list))
      df.opti$minimum <- df.opti$minimum + df.o.new$minimum
      df.opti$objective <- df.opti$objective + df.o.new$objective
    }
  }
  df.opti$minimum <- df.opti$minimum / iterations
  df.opti$objective <- df.opti$objective / iterations
  
  df.opti <- df.opti[which.min(df.opti$objective), ]
  
  # update optimal combination
  fapre <- strsplit(rownames(df.opti), "_")
  fac <- fapre[[1]][1]
  pred <- fapre[[1]][2]
  fp.coefs$Coef[fp.coefs$Factor == fac & fp.coefs$Predictor == pred] <- step.len * df.opti$minimum + fp.coefs$Coef[fp.coefs$Factor == fac & fp.coefs$Predictor == pred]
  
  return(fp.coefs)
}

base_procedure$base.procedure <- function(fp.coefs, perc.fee, preqin.basis, step.len = 0.3, bound = 100, partition = "Full") {
  
  # remove vintage for cross validation
  if(partition != "Full") preqin.basis$df.vc[partition] <- NULL
  
  # restrict no of predictor-factors (hard threshold)
  rows2use <- 1:nrow(fp.coefs)
  #if(sum(fp.coefs$Coef != 0) == 60) rows2use <- as.numeric(rownames(fp.coefs[fp.coefs$Coef != 0, ]))
  
  # try all combinations
  opti.objective <- Inf
  for(row in rows2use) {
    FacPred <- rownames(fp.coefs)[row]
    f <- function(x) {
      y <- base_procedure$empirical.loss(x, 
                                         data.list = preqin.basis$df.vc,
                                         FactorPredictor = FacPred,
                                         perc.fee = perc.fee,
                                         predictor.coefs = fp.coefs)
      return(y)
    }
    
    res <- optimize(f, c(-bound, bound))
    
    if(res$objective < opti.objective) {
      opti.FP <- FacPred
      opti.objective <- res$objective
      opti.coef.addition <- res$minimum
    }
  }

  fp.coefs[opti.FP, "Coef"] <- step.len * opti.coef.addition + fp.coefs[opti.FP, "Coef"]
  
  return(list(df = fp.coefs, error = opti.objective))
}

# Epilogue ------

if(sys.nframe() == 0L) {
  # Import preqin basis
  preqin.basis <- readRDS("preqin_basis/MSCI.World_preqin_basis.RDS")
  
  # Create predictor.coefs
  base_procedure$create.predictor.coefs(preqin.basis = preqin.basis)
  
  create.predictor.coefs <- base_procedure$create.predictor.coefs
    
  # Gain process
  base_procedure$gain.process(par = 0, 
               df = preqin.basis$df.vc$`1992`, 
               PreCoef = base_procedure$create.predictor.coefs(preqin.basis = preqin.basis)$df,
               FactorPredictor = paste("NASDAQ.Numeraire", "One"),
               perc.fee = 2/100/12)
  
  # Empirical loss
  base_procedure$empirical.loss(10, 
                 data.list = preqin.basis$df.vc, 
                 perc.fee = 2/100/12,
                 FactorPredictor = paste("NASDAQ.Numeraire", "One"),
                 predictor.coefs = base_procedure$create.predictor.coefs(preqin.basis = preqin.basis)$df)
  
  # Base procedure
  base_procedure$base.procedure(
    fp.coefs = base_procedure$create.predictor.coefs(preqin.basis = preqin.basis)$df, 
                 perc.fee = 0, 
                 preqin.basis = preqin.basis)
  
  # Test base procedure
  system.time({
      # Init
      predictor.coefs <- base_procedure$create.predictor.coefs(preqin.basis = preqin.basis)
      
      for(i in 1:100) {
        print(i)
        predictor.coefs <- base_procedure$base.procedure(fp.coefs = predictor.coefs$df, 
                                          perc.fee = 2/100/12, 
                                          preqin.basis = preqin.basis)
      }
      print(predictor.coefs$df[predictor.coefs$df$Coef != 0, ])
    })
  
}
