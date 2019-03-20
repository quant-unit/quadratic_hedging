######################
## create preqin basis
######################
# Prologue ---------
# library(readxl)
# library(alfred)
# library(zoo)

preqin_basis <- new.env()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Get public data ----------
preqin_basis$get.factor.predictor.data <- function(fred.vec) {
  
  df.xl <- readxl::read_excel(path = "data_in/Public_Indices_Prepared.xlsx", sheet = "indices_filled")
  df.xl <- data.frame(df.xl)
  df.xl$Date <- as.Date(df.xl$Date)
  df.xl <- df.xl[, c("Date", 
                     "MSCI.World.Net.Return.Daily", 
                     "MSCI.North.America.Net.Return.Daily",
                     "NASDAQ.Composite.Total.Return.Index", 
                     "Russell.2000.Total.Return.Index")]
  colnames(df.xl) <- c("Date", "MSCI.World", "MSCI.NoAm", "NASDAQ", "Russell.2000")
  
  # merge with fama french data
  df.ff <- readRDS("data_in/fama.5fac.monthly.index.RDS")
  df_pubin <- merge(df.xl, df.ff, by = "Date", all.x = TRUE)
  # calc return from index
  for(col in colnames(df_pubin[, -1])) {
    df_pubin[, col] <- (df_pubin[, col] + c(diff(df_pubin[, col]), NA)) / df_pubin[, col] - 1
  }
  df_pubin$Mkt <- df_pubin$`Mkt-RF` + df_pubin$RF
  
  df_pubin <- df_pubin[complete.cases(df_pubin), ]
  df_pubin <- df_pubin[df_pubin$Date > as.Date("1979-12-31"), ]
  
  # fred.. data 
  dl.fred <- function(new.var) {
    df.will <- alfred::get_fred_series(series_id = new.var)
    colnames(df.will)[colnames(df.will) == "date"] <- "Date"
    df.will$Date <- as.Date(df.will$Date)
    min.date <- min(df.will$Date)
    max.date <- max(df.will$Date)
    df.will <- merge(df.will,
                     data.frame(Date = seq.Date(from = min.date, to = max.date, by = "day")),
                     by = "Date", all = TRUE)
    df.will[, 2] <- zoo::na.locf(df.will[, 2])
    return(df.will)
  }
  
  for(fred.factor in fred.vec) {
    df_pubin <- merge(df_pubin, dl.fred(fred.factor), by = "Date", all.x = TRUE)
    df_pubin[is.na(df_pubin[, fred.factor]), fred.factor] <- mean(df_pubin[, fred.factor], na.rm = TRUE)
  }
  
  return(df_pubin)
}

# Create preqin.basis ------
preqin_basis$create.preqin.basis <- function(do.boostrap = FALSE, 
                                numeraire, 
                                fred.vec,
                                df_pubin) {
  # load preqin data
  df.pq <- readRDS("data_in/PQ.RDS")
  
  
  # utils function
  lag.it <- function(x) {
    c(0, x[1:(length(x)-1)])
  }
  
  out.list <- list()
  for(vintage in seq(1986,2000)) {
    # print(vintage)
    df.vc.us <- data.frame(df.pq)
    vc.types <- c("Balanced", "Early Stage", "Early Stage: Seed", "Early Stage: Start-up", 
                  "Expansion / Late Stage", "Venture (General)", "Venture Debt")
    df.vc.us <- df.vc.us[df.vc.us$Category.Type %in% vc.types, ]
    df.vc.us <- df.vc.us[df.vc.us$Fund.Focus == "US", ]
    df.vc.us <- df.vc.us[df.vc.us$Vintage == vintage, ]
    # df.vc.us <- df.vc.us[df.vc.us$Fund.Status == "Liquidated", ]
    no.funds <- length(unique(df.vc.us$Fund.ID))
    # print(paste("# funds:", no.funds))
    
    # bootstrap
    if(do.boostrap) {
      boostrap.list <- list()
      for (id in sample(levels(as.factor(df.vc.us$Fund.ID)), replace = TRUE)) {
        boostrap.list[[id]] <- df.vc.us[df.vc.us$Fund.ID == id, ]
      }
      df.vc.us <- data.frame(do.call(rbind, boostrap.list))
    }
    
    # Transaction Month & Quarter
    df.vc.us$Transaction.Month <- sapply(df.vc.us$Transaction.Date, function(x){
      as.character(seq(from=as.Date(paste(format(x, format="%Y-%m"),"01",sep="-")), by='months', length.out=2)[2] -1)
    })
    df.vc.us$Transaction.Month <- as.Date(df.vc.us$Transaction.Month)
    df.vc.us$Transaction.Quarter <- sapply(df.vc.us$Transaction.Date, function(x){
      year.quarter <- NA
      month.str <- substr(x, 6, 7)
      year.str <- substr(x, 1, 4)
      if(month.str %in% c("01","02","03")){
        year.quarter <- paste(year.str, "03-31",sep="-")
      }
      if(month.str %in% c("04","05","06")){
        year.quarter <- paste(year.str, "06-30",sep="-")
      }
      if(month.str %in% c("07","08","09")){
        year.quarter <- paste(year.str, "09-30",sep="-")
      }
      if(month.str %in% c("10","11","12")){
        year.quarter <- paste(year.str, "12-31",sep="-")
      }
      return(year.quarter)
    })
    df.vc.us$Transaction.Quarter <- as.Date(df.vc.us$Transaction.Quarter)
    
    df.vc.us1 <- data.frame(aggregate(Transaction.Amount ~ Transaction.Quarter, 
                                      data = df.vc.us, subset = Transaction.Category != "Value", FUN=sum))
    colnames(df.vc.us1) <- c("Date", "CashFlow")
    min.date <- min(df.vc.us1$Date)
    max.date <- max(df.vc.us1$Date)
    df.vc.us2 <- data.frame(aggregate(Transaction.Amount ~ Transaction.Quarter, 
                                      data = df.vc.us, subset = Transaction.Category == "Value", FUN=sum))
    df.vc.us2 <- df.vc.us2[df.vc.us2$Transaction.Amount > 0, ]
    colnames(df.vc.us2) <- c("Date", "NAV")
    df.vc.us <- merge(df.vc.us1, df.vc.us2, by = "Date", all = TRUE)
    rm(df.vc.us1, df.vc.us2)
    
    
    df.vc.us <- merge(df.vc.us, df_pubin, by="Date", all=TRUE)
    df.vc.us$NAV <- zoo::na.locf(df.vc.us$NAV, na.rm=FALSE)
    df.vc.us[is.na(df.vc.us)] <- 0
    df.vc.us <- df.vc.us[df.vc.us$Date >= min.date & df.vc.us$Date <= max.date, ]
    
    # scaled cash flows and NAVs 
    commitment.funds <- no.funds * 1000 * 1000 * 10
    df.vc.us$CashFlow <- df.vc.us$CashFlow / commitment.funds
    df.vc.us$NAV <- df.vc.us$NAV / commitment.funds
    
    # numeraire index
    df.vc.us$Numeraire.Return <- df.vc.us[, numeraire]
    df.vc.us$Numeraire.Index <- cumprod(1+df.vc.us[, numeraire])
    
    # factor set (excess returns)
    old.colnames <- colnames(df.vc.us)
    
    df.vc.us$NASDAQ.Numeraire <- df.vc.us$NASDAQ - df.vc.us$Numeraire.Return
    
    factor.set <- colnames(df.vc.us)[!(colnames(df.vc.us) %in% old.colnames)]
    factor.set <- c(factor.set, "Mkt-RF","SMB","RMW","HML","CMA")

    # predictor set
    old.colnames <- colnames(df.vc.us)
    df.vc.us$One <- 1
    df.vc.us$NAV.lag1 <- lag.it(df.vc.us$NAV)
    df.vc.us$Age <- as.numeric(df.vc.us$Date - df.vc.us$Date[1]) / 365.25
    for(fred.pred in fred.vec) {df.vc.us[, paste(fred.pred, "lag1", sep=".")] <- lag.it(df.vc.us[, fred.pred])}
    
    predictor.set <- colnames(df.vc.us)[!(colnames(df.vc.us) %in% old.colnames)]
    
    # calculate standard deviation
    fac.pred.sds <- data.frame(expand.grid(Factor = factor.set, Predictor = predictor.set), SD=0)
    for(factor in factor.set) {
      for(predictor in predictor.set) {
        fac.pred.sds$SD[fac.pred.sds$Factor == factor & 
                          fac.pred.sds$Predictor == predictor] <- sd(df.vc.us[, factor] * df.vc.us[, predictor])
      }
    }
    
    out.list[["SD"]][[paste(vintage)]] <- fac.pred.sds
    out.list[["df.vc"]][[paste(vintage)]] <- df.vc.us
    out.list[["factor.set"]] <- factor.set
    out.list[["predictor.set"]] <- predictor.set
  }
  return(out.list)
}


# Save preqin.basis ------------ 
preqin_basis$save.preqin.basis <- function(numeraire,
  fred.vec = c('TB3MS', 'T10Y3M', 'TEDRATE', 
              'USSLIND', 'VXOCLS', 'CFNAIDIFF', 'BAA10Y')) {
  
  preqin.basis <- preqin_basis$create.preqin.basis(do.boostrap = FALSE, 
                                      numeraire = numeraire,
                                      fred.vec = fred.vec,
                                      df_pubin = preqin_basis$get.factor.predictor.data(fred.vec))
  
  output.folder <- "preqin_basis"
  dir.create(output.folder, showWarnings = FALSE)
  file.name <- paste(file.path(output.folder, numeraire), "_preqin_basis.RDS", sep = "")
  saveRDS(preqin.basis, file.name)
}

# Epilogue ----------

preqin_basis$main <- function() {
  preqin_basis$save.preqin.basis(numeraire = "MSCI.World")
  preqin_basis$save.preqin.basis(numeraire = "MSCI.NoAm")
}

if(sys.nframe() == 0L) {
  preqin_basis$main()
}

