###############################
## Stability Selection Analysis
###############################
# Prologue --------
if(sys.nframe() == 0L) rm(list = ls())
stability_selection <- new.env()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source("global_opts.R")
source("analyze_single_run.R")

# Create create.stability.selection.result ----------
stability_selection$ex.helper <- function(run.name) {
  df <- analyze_single_run$tex.coef(run.ob = analyze_single_run$extract.run(run.name))
  df$FacPred <- rownames(df)
  df <- df[, 3:4]
  old.name <- colnames(df)
  new.name <- paste(substring(run.name, 9), old.name[1], sep="_")
  colnames(df) <- c(new.name, "FacPred")
  return(df)
}
stability_selection$create.result <- function(run.names) {
  
  first <- TRUE
  for(run.name in run.names) {
    print(run.name)
    
    if(first) {
      df <- stability_selection$ex.helper(run.name)
      first <- FALSE
    } else {
      df.new <- stability_selection$ex.helper(run.name)
      df <- merge(df, df.new, by="FacPred", all = TRUE)
    }
  }
  df[df == 0] <- NA
  
  df$Selected <- apply(df, 1, function(x) sum(!is.na(x))) - 1
  
  cols <- c(1, grep("World2", colnames(df)), grep("NoAm2", colnames(df)), grep("Selected", colnames(df)))
  df1 <- df[, cols]
  
  for(i in 1:nrow(df1)) {
    df1$Factor[i] <- strsplit(df1$FacPred, "_")[[i]][1]
    df1$Predictor[i] <- strsplit(df1$FacPred, "_")[[i]][2]
  }
  df1$FacPred <- NULL
  df1 <- df1[!is.na(df1[, 1])| !is.na(df1[, 2]), ]
  
  df1 <- df1[order(- df1$Selected, df1$Factor, df1$Predictor), ]
  
  df1$Selected <- paste(df1$Selected, length(run.names), sep="/")
  
  cols1 <- cols <- c(grep("Factor", colnames(df1)), grep("Predictor", colnames(df1)), 
                     grep("World2", colnames(df1)), grep("NoAm2", colnames(df1)), 
                     grep("Selected", colnames(df1)))
  df1 <- df1[, cols1]
  
  print(xtable::xtable(df1), include.rownames=FALSE)
  
  return(df)
}
stability_selection$save.result <- function(run.names) {
  # obtain stability selection result
  df.stability <- stability_selection$create.result(run.names = run.names)
  
  # save StabilitySelection.RDS
  getwd()
  saveRDS(df.stability, paste("results/", "StabilitySelection.RDS", sep=""))
}


# Re-run with stable factor_predictor subset -------
stability_selection$rerun.subset <- function() {
  source("run_boost.R")
  
  opts <- run_boost$make.opts(numeraire = "MSCI.World", 
                              use.stability = TRUE, 
                              fee = 2, 
                              iter = iteration.stability)
  
  # do.call("run.boost", append(opts, c(partition = "Full")), envir = run_boost)
  run_boost$run.boost.wrapper(opts = opts, run.parallel = run.parallel)
  
  return(opts$run.name)
}


# analyze stable run ---------
stability_selection$analyze.result <- function(stable.name) {
  source("analyze_single_run.R")
  
  run.ob <- analyze_single_run$extract.run(stable.name)
  
  analyze_single_run$plot.df(run.ob = run.ob, do.eps = create.EPS)
  analyze_single_run$plot.cv.error(run.ob = run.ob, do.eps = create.EPS)
  
  analyze_single_run$tex.coef(run.ob = run.ob, add.percs = c(0.05))
  analyze_single_run$tex.repl(run.ob = run.ob, percs = c(0, 0.05))
}


# Epilogue ----

stability_selection$main <- function(run.names) {
  # create and save StabilitySelection.RDS
  stability_selection$save.result(run.names)
  
  # rerun boosting with stable set
  stable.name <- stability_selection$rerun.subset()
  
  # analyze rerun boosting results
  stability_selection$analyze.result(stable.name)
  
  return(stable.name)
}

if(sys.nframe() == 0L) {
  run.names <- c("2019-03-13_MSCI.World_f0_i10", "2019-03-13_MSCI.World_f1_i10", 
                 "2019-03-13_MSCI.World_f2_i10", "2019-03-13_MSCI.World_f3_i10", 
                 "2019-03-14_MSCI.World_f4_i10", "2019-03-14_MSCI.World_f5_i10",
                 "2019-03-13_MSCI.NoAm_f0_i10", "2019-03-13_MSCI.NoAm_f1_i10", 
                 "2019-03-13_MSCI.NoAm_f2_i10", "2019-03-13_MSCI.NoAm_f3_i10", 
                 "2019-03-14_MSCI.NoAM_f4_i10", "2019-03-14_MSCI.NoAM_f5_i10")
  
  # test main() function
  system.time(
    stable.name <- stability_selection$main(run.names)
  )
}