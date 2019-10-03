###########################
## main: performs all steps
###########################
# Prologue ---------
if(sys.nframe() == 0L) rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source("global_opts.R")

# 1. preqin_basis ----
# source("preqin_basis.R")
# preqin_basis$main()


# 2. run_boost ----
source("run_boost.R")
run.name.list <- run_boost$main() # takes very long time ...


# 3. stability_selection ----
source("stability_selection.R")
stable.name <- stability_selection$main(run.name.list)

