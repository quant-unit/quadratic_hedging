###########################
## Component-wise L2  Boost
###########################
# Prologue -----------
if(sys.nframe() == 0L) rm(list = ls())
run_boost <- new.env()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source("global_opts.R")
source("base_procedure.R")
library(parallel)

# Make opts ----------
run_boost$make.opts <- function(numeraire, use.stability, fee, iter) {
  opts <- list(numeraire = numeraire, 
               use.stability = use.stability, 
               perc.fee = fee / 100 / 12, 
               iter = iter)
  stability.indicator <- ifelse(use.stability, "_Stability", "")
  opts["run.name"] <- paste(Sys.Date(), "_", opts[["numeraire"]], 
                            stability.indicator,
                            "_f", opts[["perc.fee"]]*100*12, 
                            "_i", opts[["iter"]], 
                            sep = "")
  opts[["preqin.basis"]] <- readRDS(paste("preqin_basis/", opts[["numeraire"]], "_preqin_basis.RDS", sep = ""))
  return(opts)
}


# Run boost ---------
run_boost$run.boost <- function(partition, preqin.basis, run.name, iter, perc.fee, use.stability, ...) {
  print(partition)
  boost.list <- list()
  cv.error <- list()
  
  # Step 1: initialize
  predictor.coefs <- base_procedure$create.predictor.coefs(use.stability = use.stability, preqin.basis = preqin.basis)
  
  # Step 2: update & iterate
  for(i in 1:iter) {
    #print(i)
    
    predictor.coefs <- base_procedure$base.procedure(predictor.coefs$df, perc.fee = perc.fee, partition = partition, preqin.basis = preqin.basis)
    boost.list[[i]] <- predictor.coefs$df[, "Coef"]
    
    if(partition %in% names(preqin.basis$df.vc)) {
      cv.error[[i]] <- predictor.coefs$error
    }
    
  }
  
  # fill output list
  df.boost <- data.frame(t(do.call(cbind, boost.list)))
  colnames(df.boost) <- paste(predictor.coefs$df$Factor, predictor.coefs$df$Predictor, sep="_")
  df.boost <- df.boost[, colSums(df.boost != 0) > 0]

  if(partition %in% names(preqin.basis$df.vc)) {
    cv.error <- unlist(cv.error)
  } else {
    cv.error <- NULL
  }
  
  output <- list(model = df.boost, cv.error = cv.error)
  dir.create("results", showWarnings = FALSE)
  saveRDS(output, paste("results/", run.name, partition, ".RDS", sep=""))
  return(output)
}


# Run boost wrapper ---------
run_boost$run.boost.wrapper <- function(opts, run.parallel = FALSE) {
  now <- Sys.time()
  set.seed(99)
  
  preqin.basis <- opts[["preqin.basis"]]
  
  party <- c("Full", names(preqin.basis$df.vc))
  
  run.boost.opts <- function(part) {
    do.call("run.boost", append(opts, c(partition = part)),
            envir = run_boost)
  }
  
  if(run.parallel) {
    
    if(.Platform$OS.type == "unix") {
      cv.list <- mclapply(party, run.boost.opts, mc.cores = parallel::detectCores())
    } else {
      cl <- makeCluster(detectCores() - 1)
      clusterExport(cl,  varlist = c("base.procedure", "create.predictor.coefs",
                                     "empirical.loss","gain.process", "opts", "run.boost"))
      cv.list <- parLapply(cl, party, run.boost.opts)
      stopCluster(cl)
    }

  } else {
    cv.list <- lapply(party, run.boost.opts)
  }
  
  add.info <- list(preqin.basis = preqin.basis, party = party)
  saveRDS(add.info, paste("results/", opts[["run.name"]], "add.info", ".RDS", sep=""))
  
  names(cv.list) <- party
  print(Sys.time() - now)
  
  invisible(cv.list)
}


# Run boost wrapper opts -----------
run_boost$run.boost.wrapper.opts <- function(numeraireZ, feeZ, iteration) {
  run.name.list <- list()
  for(n in numeraireZ) {
    for(f in feeZ) {
      print(paste(n, f))
      opts <- run_boost$make.opts(numeraire = n, 
                        use.stability = FALSE, 
                        fee = f, 
                        iter = iteration.run_boost)

      run_boost$run.boost.wrapper(opts = opts, run.parallel = run.parallel)
      run.name.list[[paste(n, f, sep = "_")]] <- opts$run.name
    }
  }
  return(run.name.list)
}


# Epilogue ----------
run_boost$main <- function() {
  run_boost$run.boost.wrapper.opts(numeraireZ = numeraireZ.run_boost, 
                                   feeZ = feez.run_boost, 
                                   iteration = iteration.run_boost)
}

if(sys.nframe() == 0L) {
  opts <- run_boost$make.opts(numeraire = "MSCI.NoAm", 
                    use.stability = FALSE, 
                    fee = 3, 
                    iter = 10)
  rm(opts)
  # test run.boost()
  # do.call("run.boost", append(opts, c(partition = "1986")), envir = run_boost)
  
  # test run.boost.wrapper()
  # run_boost$run.boost.wrapper(opts = opts, run.parallel = FALSE)
  
  # test main function: run.boost.wrapper.opts()
  system.time(run.name.list <- run_boost$main())
  for(run.name in run.name.list) print(run.name)
}
