# Prologue ----

if(sys.nframe() == 0L) rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(mboost)
library(glmnet)
library(olsrr)
library(lars)

# Load Preqin Basis -----

weighting <- c("EW")

#RF_preqin.basis <- readRDS("preqin_basis/RF_preqin_basis_boostrap_seed1.RDS")
RF_preqin.basis <- list()
RF_preqin.basis$VW <- readRDS("preqin_basis/RF_valueweighted_preqin_basis.RDS")[["original"]]
RF_preqin.basis$EW <- readRDS("preqin_basis/RF_equalweighted_preqin_basis.RDS")[["original"]]

for(w in names(RF_preqin.basis)) {
  for(fund.type0 in names(RF_preqin.basis[[w]])) {
    RF_preqin.basis[["original"]][[paste(fund.type0, w, sep="_")]] <- RF_preqin.basis[[w]][[fund.type0]]
  }
}
#names(RF_preqin.basis$original)

trim.preqin.basis <- function(RF_preqin.basis, max.vintage) {
  for(fund.type0 in names(RF_preqin.basis$original)) {
    lod <- RF_preqin.basis$original[[fund.type0]]$df
    no.funds.total <- 0
    for(vintage in names(lod)) {
      if(as.numeric(vintage) > max.vintage) {
        lod[[vintage]] <- NULL
        no.funds.total = no.funds.total + RF_preqin.basis$original[[fund.type0]]$no.funds[[vintage]]
      }
    }
    RF_preqin.basis$original[[fund.type0]]$df <- lod
    #print(paste(fund.type0, no.funds.total))
    RF_preqin.basis$original[[fund.type0]]$no.funds.total <- no.funds.total
  }
  return(RF_preqin.basis)
}

# Prepare Pseudo X -----

type <- "BO_VW"
max.vintage <- 2010
lambda <- 2 / 100 / 12

prep.df.x <- function(type, max.vintage, lambda) {
  preqin.basis <- trim.preqin.basis(RF_preqin.basis, max.vintage)
  
  Vintages <- list()
  for(vintage in names(preqin.basis$original[[type]]$df)) {
    df <- preqin.basis$original[[type]]$df[[vintage]]
    final.age <- 15
    df <- df[df$Age < final.age, ]
    df$CashFlow[nrow(df)] <- df$CashFlow[nrow(df)] + df$NAV[nrow(df)]
    
    # new numeraire
    df$Numeraire.Return <- df$Mkt.RF + df$RF
    df$Numeraire.Index <- cumprod(1 + df$Numeraire.Return)
    
    ResponseFactorPredictor <- list()
    ResponseFactorPredictor[["Discounted.CF"]] <- sum(df$CashFlow / df$Numeraire.Index) # * df$Numeraire.Index[nrow(df)]
    
    factor.set <- c("Mkt.RF", "SMB", "HML", "CMA", "RMW")
    predictor.set <- preqin.basis$original[[type]]$predictor.set
    # predictor.set <- c("One")
    
    for(factor in factor.set) {
      for(predictor in predictor.set) {
        x <- sum(df[, predictor] * (df[, factor] - lambda) * df$NAV / df$Numeraire.Index)  # * df$Numeraire.Index[nrow(df)]
        ResponseFactorPredictor[[paste(factor,predictor,sep="_x_")]] <- x
      }
    }
    Vintages[[paste(vintage)]] <- data.frame(ResponseFactorPredictor)
  }
  df.x <- do.call(rbind, Vintages)
  return(df.x)
}
df.x <- prep.df.x(type, max.vintage, lambda)
df.x

sqrt(nrow(df.x) / log(ncol(df.x) - 1))

# Set parameters -----

start.in.sample <- 1986
end.in.sample <- 2000
start.out.of.sample <- end.in.sample + 1
end.out.of.sample <- 2010

types <- c("BO_EW", "BO_VW")
max.vintages <- 2000:end.in.sample
lambdas <- seq(0,4,1) / 100 / 12

type <- types[1]
max.vintage <- max.vintages[1]
lambda <- lambdas[1]

K.FOLD <- nrow(df.x)
CV.iter <- 5
SEED <- 1
alpha <- 1

no.combinations <- length(types) * length(max.vintages) * length(lambdas)

# Boosting -----
iterated.boosting <- function(types, max.vintages, lambdas, stable.coef.names) {
  resi <- list()
  for(type in types) {
    for(max.vintage in max.vintages) {
      for(lambda in lambdas) {
        
        df.x <- prep.df.x(type, max.vintage, lambda)
        
        if( length(stable.coef.names) > 0 ) {
          predictors <- paste(stable.coef.names, collapse= "+")
        } else {
          predictors <- "."
        }
        fmla <- as.formula(paste("Discounted.CF ~ ", predictors, "-1"))
        
        bm <- glmboost(fmla, data = df.x, 
                       center = FALSE, offset = 0, family = Gaussian(),
                       control = boost_control(risk = "inbag", mstop = 2000, stopintern = TRUE))
        
        set.seed(SEED)
        cv.list <- list()
        k.fold <- min(K.FOLD, nrow(df.x))
        for(split in k.fold) {
          x <- c()
          for(i in 1:CV.iter) {
            x <- c(x, mstop(cvm <- cvrisk(bm, folds = cv(model.weights(bm), type="kfold", B=split) ) ) )
          }
          cv.list[[paste0(split, 'fold')]] <- mean(x)
        }
        cv.list
        
        
        #folds <- diag(2, 25) ; folds[folds == 0] <- 1 ; folds[folds == 2] <- 0
        #mstop(cvm <- cvrisk(bm, folds = folds ) )
        #plot(cvm)
        
        bm <- bm[cv.list[[paste0(k.fold, "fold")]]] # 2-fold cv is most conservative
        
        df.b <- data.frame(beta = unlist(bm$coef()), fp= names(bm$coef()))
        rownames(df.b) <- paste(names(bm$coef()), sign(unlist(bm$coef())))
        
        #plot(bm)
        #cbind(df.x$Discounted.CF, predict(bm))
        run.name <- paste(type, max.vintage, lambda)
        #print(run.name)
        
        if( length(stable.coef.names) > 0 ) {
          resi[[run.name]] <- bm$coef(which = stable.coef.names)
        } else {
          resi[[run.name]] <- df.b 
        }
        
      }
    }
  }
  return(resi)
}

system.time(
  cwb.resi <- iterated.boosting(types, max.vintages, lambdas, NULL)
)

df.cwb <- as.data.frame(table(unlist(sapply(cwb.resi, rownames))))
df.cwb <- df.cwb[order(df.cwb$Freq, decreasing=TRUE), ]
df.cwb

sum(df.cwb$Freq) / no.combinations 
# 2 fold: 14.1
# 6 fold: 14.9

stability.booster <- function(types, max.vintages, lambdas) {
  resi <- iterated.boosting(types, max.vintages, lambdas, NULL)
  
  # select stablest
  max.no.of.coefs <- 3
  tbl <- sort(table(unlist(resi)),T) / length(resi)
  #stable.coef.names <- unlist(lapply(names(tbl[tbl > 2/3]), function(x) strsplit(x, " ")[[1]][1]))
  stable.coef.names <- unlist(lapply(names(tbl[1:min(length(tbl), max.no.of.coefs)]), function(x) strsplit(x, " ")[[1]][1]))
  
  resi <- iterated.boosting(types, max.vintages, lambdas, stable.coef.names)
  
  df.resi <- data.frame(do.call(rbind,lapply(resi, unlist)))
  df.resi  <- data.frame(do.call(rbind, 
                                 list(mean = colMeans(df.resi),
                                      stdv = apply(df.resi, 2, sd))))
  
  return(list(resi = resi, df.resi = df.resi, tbl = tbl))
}

#system.time(x <- stability.booster(types, max.vintages, lambdas))
#data.table::data.table(x$df.resi)

# GLM Net ----

iterated.elastic.net <- function(types, max.vintages, lambdas, alpha = 1) {
  resi <- list()
  for(type in types) {
    for(max.vintage in max.vintages) {
      for(lambda in lambdas) {
        
        df.x <- prep.df.x(type, max.vintage, lambda)
        
        x.matrix <- as.matrix(df.x[, 2:nrow(df.x)])
        # alpha = 1 --> lasso
        # alpha = 0 --> ridge
        fit <- glmnet(x = x.matrix, y = df.x$Discounted.CF, alpha = alpha, standardize = FALSE, intercept=FALSE)
        #plot(fit)
        #print(fit)
        #coef(fit, s = 0.1)
        #coef(fit, s = 0.2)
        
        cv.fit <- cv.glmnet(x = x.matrix, y = df.x$Discounted.CF, alpha = alpha, nfolds=K.FOLD, 
                            standardize = FALSE, intercept=FALSE)
        
        set.seed(SEED)
        cv.list <- list()
        for(split in K.FOLD) {
          x <- c()
          for(i in 1:CV.iter) {
            cv.fit <- cv.glmnet(x = x.matrix, y = df.x$Discounted.CF, alpha = alpha, nfolds=split, 
                                standardize = FALSE, intercept=FALSE)
            x <- c(x, cv.fit$lambda.min)
          }
          cv.list[[paste0(split, 'fold')]] <- mean(x)
        }
        cv.list
        
        lambda.min <- cv.list[[paste0(K.FOLD, 'fold')]]
        
        # plot(cv.fit)
        lambda.min
        log(lambda.min)
        x <- coef(cv.fit, s = lambda.min)
        df.res <- data.frame(as.matrix(x))
        colnames(df.res) <- "beta"
        df.res$fp <- rownames(df.res)
        df.res <- df.res[df.res$beta != 0, ]
        
        rownames(df.res) <- paste(rownames(df.res), sign(df.res$beta))
        
        resi[[paste(type, max.vintage, lambda)]] <- df.res
        
      }
    }
  }
  return(resi)
}

# LASSO
system.time(
  lasso.resi <- iterated.elastic.net(types, max.vintages, lambdas, 1)
)

lasso.resi$`VC_EW 2000 0`

df.la <- as.data.frame(table(unlist(sapply(lasso.resi, rownames))))
df.la <- df.la[order(df.la$Freq, decreasing=TRUE), ]
df.la

sum(df.la$Freq) / no.combinations # 3.5

# RIDGE
system.time(
  ridge.resi <- iterated.elastic.net(types, max.vintages, lambdas, 0)
)

ridge.resi$`VC_EW 2000 0`

df.ri <- as.data.frame(table(unlist(sapply(ridge.resi, rownames))))
df.ri <- df.ri[order(df.ri$Freq, decreasing=TRUE), ]
df.ri

sum(df.ri$Freq) / no.combinations # 19

# LARS -----

iterated.lars <- function(types, max.vintages, lambdas) {
  resi <- list()
  for(type in types) {
    for(max.vintage in max.vintages) {
      for(lambda in lambdas) {
        
        df.x <- prep.df.x(type, max.vintage, lambda)
        
        x.matrix <- as.matrix(df.x[, 2:nrow(df.x)])
        
        lar.type = "lar" # c("lasso", "lar", "forward.stagewise", "stepwise")

        fit <- lars(x = x.matrix, y = df.x$Discounted.CF, type = lar.type, normalize = FALSE, intercept=FALSE)
        #plot(fit)
        #summary(fit)
        #coef(fit, s = 0.1)
        #coef(fit, s = 0.2)
        
        cv.fit <- cv.lars(x = x.matrix, y = df.x$Discounted.CF, type = lar.type, K=nrow(df.x), 
                          normalize = FALSE, intercept=FALSE, plot.it = FALSE)
        
        set.seed(SEED)
        cv.list <- list()
        for(split in K.FOLD) {
          x <- c()
          for(i in 1:CV.iter) {
            cv.fit <- cv.lars(x = x.matrix, y = df.x$Discounted.CF, type = lar.type, K=min(split, nrow(df.x)), 
                              normalize = FALSE, intercept=FALSE, plot.it = FALSE)
            x <- c(x, which.min(cv.fit$cv))
          }
          cv.list[[paste0(K.FOLD, 'fold')]] <- mean(x)
        }
        cv.list
        
        min.cv.error <- cv.list[[paste0(K.FOLD, 'fold')]]
        
        # plot(cv.fit)
        x <- fit$beta[min.cv.error,]
        df.res <- data.frame(as.matrix(x))
        colnames(df.res) <- "beta"
        df.res$fp <- rownames(df.res)
        df.res <- df.res[df.res$beta != 0, ]
        
        rownames(df.res) <- paste(rownames(df.res), sign(df.res$beta))
        
        resi[[paste(type, max.vintage, lambda)]] <- df.res
        
      }
    }
  }
  return(resi)
}

# LARS
system.time(
  lar.resi <- iterated.lars(types, max.vintages, lambdas)
)

lar.resi$`VC_EW 2000 0`

df.lar <- as.data.frame(table(unlist(sapply(lar.resi, rownames))))
df.lar <- df.lar[order(df.lar$Freq, decreasing=TRUE), ]
df.lar

sum(df.lar$Freq) / no.combinations # 6.85


# Stepwise OLS -----

iterated.stepwise.ols <- function(types, max.vintages, lambdas) {
  resi <- list()
  for(type in types) {
    for(max.vintage in max.vintages) {
      for(lambda in lambdas) {
        
        df.x <- prep.df.x(type, max.vintage, lambda)
        
        factor_predictors <- colnames(df.x)[-1]
        formula <- paste("Discounted.CF ~", paste(factor_predictors, collapse = " + "), "- 1")
        all.fit <- lm(formula, data = df.x)
        summary(all.fit)
        
        # system.time(all <- ols_step_all_possible(fit))
        
        # problem: ols_step_forward_p always includes intercept
        # k <- ols_step_forward_p(all.fit, penter = 0.05, details = FALSE, progress = TRUE)
        # plot(k)
        
        # first componetwise boosting step to find univariate start model
        l <- list()
        for (fp in factor_predictors) {
          formula <- paste("Discounted.CF ~", fp, "- 1")
          fit <- lm(formula, data = df.x)
          fit <- summary(fit)
          l[[fp]] <- data.frame(fp = fp, adj.R2 = fit$adj.r.squared)
        }
        
        df.1step <- data.frame(do.call(rbind, l))
        first.fp <- df.1step$fp[df.1step$adj.R2 == max(df.1step$adj.R2)]
        l[first.fp]
        
        # Forward selection based on first CL2B step
        formula <- paste("Discounted.CF ~", first.fp, "- 1")
        start_lm <- lm(formula, data = df.x)
        summary(start_lm)
        k <- 2 # AIC
        k <- log(nrow(df.x)) # BIC
        res <- summary(step(start_lm, direction = "forward", scope = formula(all.fit), 
                            k = k, trace = 0, steps = 10))
        
        df.res.ols <- data.frame(res$coefficients)
        df.res.ols$fp <- rownames(df.res.ols)
        rownames(df.res.ols) <- paste(rownames(df.res.ols), sign(df.res.ols$Estimate))
        df.res.ols$beta <- df.res.ols$Estimate
        
        resi[[paste(type, max.vintage, lambda)]] <- df.res.ols
        
      }
    }
  }
  return(resi)
}

system.time(
  sf.resi <- iterated.stepwise.ols(types, max.vintages, lambdas)
)

sf.resi$`VC_EW 2000 0`

df.sf <- as.data.frame(table(unlist(sapply(sf.resi, rownames))))
df.sf <- df.sf[order(df.sf$Freq, decreasing=TRUE), ]
df.sf

sum(df.sf$Freq) / no.combinations # 3.1

# Compare: variable selection -----

colnames(df.cwb) <- c("fp", "cwb")
colnames(df.la) <- c("fp", "lasso")
colnames(df.ri) <- c("fp", "ridge")
colnames(df.lar) <- c("fp", "lars")
colnames(df.sf) <- c("fp", "stepf")

df.var.sel <- Reduce(function(x, y) merge(x, y, all=TRUE), list(df.cwb, df.la, df.ri, df.lar, df.sf))
df.var.sel$fp <- as.character(df.var.sel$fp)
df.var.sel[is.na(df.var.sel)] <- 0
df.var.sel[, -1] <- round(df.var.sel[, -1] / no.combinations, 2)
df.var.sel$Avg <- rowSums(df.var.sel[, -1]) / 5
df.var.sel <- df.var.sel[order(df.var.sel$Avg, decreasing=TRUE), ]
df.var.sel <- rbind(df.var.sel, c("Sum", colSums(df.var.sel[, -1])))

o <- strsplit(df.var.sel$fp, "_x_")
df.var.sel$Factor <- sapply(o, function(x) x[1])
df.var.sel$Predictor <- sapply(o, function(x) x[2])
o <- strsplit(df.var.sel$Predictor, " ")
df.var.sel$Predictor <- sapply(o, function(x) x[1])
df.var.sel$Sign <- sapply(o, function(x) x[2])
df.var.sel$fp <- NULL
rownames(df.var.sel) <- NULL
df.var.sel <- df.var.sel[, c("Factor", "Predictor", "Sign", "lasso", "ridge", "stepf", "lars", "cwb", "Avg")]


print(
  xtable::xtable(df.var.sel, caption = "Stability selection result.s", 
                 digits=3, label = "tab:stability_selection_results"), 
  include.rownames=FALSE
  #, format.args = list(big.mark = ",")
)

# Select best model(s) ------ 

# OLS results of "best" factor-predictor pairs

# VC 
# stable.coef.names <- c("Mkt.RF_x_VXOCLS.lag1", "Mkt.RF_x_Age", "SMB_x_Age") # lasso
stable.coef.names <- c("CMA_x_Age", "CMA_x_T10Y3M.lag1", "HML_x_USSLIND.lag1") # stewpise
# BO
stable.coef.names <- c("Mkt.RF_x_Age", "Mkt.RF_x_VXOCLS.lag1", "Mkt.RF_x_USSLIND.lag1", "RMW_x_T10Y3M.lag1")

predictors <- paste(stable.coef.names, collapse= "+")
fmla <- as.formula(paste("Discounted.CF ~ ", predictors, "-1"))
df.data <- df.x[as.character(start.in.sample:end.in.sample), ]
res <- lm(fmla, df.data)
summary(res)

pred.ols <- predict(res, df.x[as.character(start.out.of.sample:end.out.of.sample), ])
pred.ols

# CLB
res <- glmboost(fmla, data = df.data, 
               center = FALSE, offset = 0, family = Gaussian(),
               control = boost_control(risk = "inbag", mstop = 2000, stopintern = TRUE))
m.stop <- mstop(cvm <- cvrisk(res, folds = cv(model.weights(res), type="kfold", B=nrow(df.data)) ) )
mstop(cvrisk(res, folds = cv(model.weights(res), type="subsampling", B=2) ) )
mstop(cvrisk(res, folds = cv(model.weights(res), type="bootstrap", B=2) ) )
mstop(AIC(res))
m.stop

# Plot it 

do.eps <- TRUE

if(do.eps) {
  setEPS()
  postscript("optimal_stopping_cv.eps", 
             width = 5.5, height = 3.5, 
             family = "Helvetica", pointsize = 11)
}
par( mar = c(4.2, 4.2, 1, 1) )

plot(cvm, ylab = "cross validation error")

legend("topright", bty="n", legend = paste("optimal stopping iteration:", m.stop))

if(do.eps){ 
  dev.off() 
}

if(do.eps) {
  setEPS()
  postscript("boosting_path.eps", 
             width = 5.5, height = 3.5, 
             family = "Helvetica", pointsize = 11)
}
par( mar = c(4.2, 4.2, 1, 10) )

plot(res, main="")
abline(v=m.stop, col = "blue", lty = 2)
legend("topright", bty = "n", legend = c("optimal stopping"), col = "blue", lty = 2)

if(do.eps){ 
  dev.off() 
}

# select optimal m.stop model
res <- res[m.stop]
res

pred.cwb <- predict(res, df.x[as.character(start.out.of.sample:end.out.of.sample), ])
pred.cwb

# Compare: out-of-sample prediction -----

df.oos <- df.x[as.character(start.out.of.sample:end.out.of.sample), ] # out-of-sample
# df.oos <- df.x[as.character(start.in.sample:end.in.sample), ] # in-sample
old.cols <- colnames(df.oos)

mo.name <- "lasso"
mo <- sf.resi

oos.error <- function(mo, mo.name) {
  #m <- mo$`VC_EW 2000 0`
  
  l <- list()
  for(name in names(mo)) {
    m <- mo[[name]]
    if(nrow(m) == 0) next
    
    df <- df.oos[m$fp]
    df <- data.frame(as.matrix(df) %*% m$beta)
    colnames(df) <- name
    l[[name]] <- df
  }
  df <- data.frame(do.call(cbind, l))
  df <- rowMeans(df) 
  df <- data.frame(df)
  colnames(df) <- mo.name
  
  df.oos[, mo.name] <- df[, mo.name]
  
  invisible(df.oos)
}
df.oos <- oos.error(lasso.resi, "lasso")
df.oos <- oos.error(ridge.resi, "ridge")
df.oos <- oos.error(lar.resi, "lars")
df.oos <- oos.error(sf.resi, "step.forward")
df.oos <- oos.error(cwb.resi, "cwb")

df.oos$ols.stable <- pred.ols
df.oos$cwb.stable <- as.numeric(pred.cwb)
df.oos$no.hedge <- 0

new.cols <- colnames(df.oos)
df.oos <- df.oos[, c("Discounted.CF", setdiff(new.cols, old.cols))]

df.oos["RMSE", ] <- apply(df.oos, 2, function(x){
  sqrt(sum((df.oos$Discounted.CF -x)^2))
})

# df.oos <- df.oos[, c("Discounted.CF", "ols.stable", "cwb.stable", "no.hedge")]

print(
  xtable::xtable(df.oos, 
                 caption = "Hedging cash flows generated by high-dimensional models and associated root mean squared error (RMSE).", 
                 digits=3, label = "tab:hedge_results"), 
  include.rownames=TRUE
  #, format.args = list(big.mark = ",")
)
# best subset selection -----

i <- 30
sum(sapply(1:i, function(x){choose(i, x)})) / 1000 / 1000 / 1000
