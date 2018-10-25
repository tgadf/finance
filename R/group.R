## Load libraries
setwd("/Users/tgadfort/Documents/finance/")

source(file.path(getwd(), "R", "lib.R"))
source(file.path(getwd(), "R", "date.R"))
source(file.path(getwd(), "R", "helper.R"))
source(file.path(getwd(), "R", "dataOps.R"))
source(file.path(getwd(), "R", "strdate.R"))
source(file.path(getwd(), "R", "sums.R"))
require(futile.logger)
require(lubridate)
require(zoo)

basedir <- file.path(getwd(), "Rda")
doDays <- FALSE

clean <- function() { rm(list=ls()) }


getAccountLevelRecords <- function(input = NULL, timeUnit = "Year") {
  flog.info("Getting Account-Level Record Information")
  if ( is.null(input) ) {
    retval <- loadData()
    input <- records
  }

  if ( !(timeUnit %in% c("Year", "Month", "Quarter", "Day")) ) {
    flog.warn(paste("TimeUnit",timeUnit,"must be Year, Month, Quarter, or Day"))
    stop()
  }
  
  retval <- getSums(df = input, "account", timeUnit)
  return( retval )
}

getSuperGroupLevelRecords <- function() {
  flog.info("Getting Account-Level Record Information")
  retval <- loadData()
  
  retval <- getSums("supergroup", "account")
  return( retval )
}

mergeChaseCredit <- function() {
  flog.info("Merging Chase and Credit Card accounts because they are really the same thing.")
  retval <- loadData()
  
  final <- records
  total <- sum(final[,"amount"])
  transfers <- NULL
  accountData <- list()
  
  ## 0) Flip Credit Amount
  creditIDs <- which(final[,"account"] == "Credit")
  final[creditIDs,"amount"] <- -1*final[creditIDs,"amount"]
  runningTotal <- sum(final[,"amount"])
  runningTransfersTotal <- 0.0
  flog.info(paste("Sum:",runningTotal,"    Original:",total,". After flipping credit amount sign."))
  assign("final", final, envir = .GlobalEnv)

  ## 1) Rename Chase and CreditCard Account to Chase
  creditIDs <- which(final[,"account"] == "Credit")
  final[creditIDs,"account"] <- as.factor("Chase")
  
  ## 2) Drop Credit Card Payments/Receipts
  flog.info(paste("Fixing Chase/Credit Account"))
  credit <- subset(final, category=="CreditCardReceipt", select = c("id", "match", "amount"))
  chase  <- subset(final, category=="CreditCardPayment", select = c("id", "match", "amount"))
  flog.info(paste("Found credit account with",nrow(credit),"entries and total $",sum(credit[,"amount"])))
  flog.info(paste("Found chase account with",nrow(chase),"entries and total $",sum(chase[,"amount"])))
  if ( nrow(credit) != nrow(chase) ) { flog.warn("The number of rows are not equal. Could be a problem.") }
  retval <- checkMatches(df1 = chase, df2 = credit)
  if ( !is.null(retval) ) {
    drop1 <- match(retval[["drop1"]], final[,"id"])
    drop2 <- match(retval[["drop2"]], final[,"id"])
    final <- final[-c(drop1,drop2),]
  }
  flog.info(paste("After dropping credit payment/receipt records there are now",nrow(final),"/",nrow(records),"entries."))
  runningTotal <- sum(final[,"amount"])
  runningTransfersTotal <- 0.0
  flog.info(paste("Sum:",runningTotal,"    Original:",total,". After dropping credit payments"))
  assign("final", final, envir = .GlobalEnv)
}

dropReceipts <- function() {
  flog.info("Merging Chase and Credit Card accounts because they are really the same thing.")
  retval <- loadData()
  
  final <- records
  total <- sum(final[,"amount"])
  transfers <- NULL
  accountData <- list()
}

plotTimeData <- function(input = NULL, datatype = "account", timeUnit = "Year") {
  if ( is.null(input) ) {
    retvals <- loadData()
    input <- records
  }
  
  pdfname <- paste("Results/",datatype,"DataBy",timeUnit,".pdf",sep = "")
  pdf(pdfname)
  
  colline   <- 'darkorange'
  colpoints <- 'navy'
  
  timeData <- getSums(df = input, datatype, timeUnit)
  colnames(timeData)[1] <- "datatype"
  for ( dataname in as.character(unique(timeData[,"datatype"])) ) {
    values <- subset(x = timeData, datatype == dataname)
    par(mfrow=c(2,1))
    y <- values[,"total"]
    x <- values[,"Year"]
    plot(y~x, type="c", col = colline, axes=F, xlab="", ylab="", main=paste(dataname,datatype))
    points(y = y, x = x, col = colpoints, pch=19)
    axis(1, at = x, labels = x, las=2, col = colpoints)
    axis(2, at=pretty(y, min.n = 0), labels=pretty(y, min.n = 0), las=2, col = colpoints)
    box()
    
    values[,"running"] <- cumsum(values$total)
    par(new=F)
    
    y <- values[,"running"]
    x <- values[,"Year"]
    plot(y~x, type="c", col = colline, axes=F, xlab="", ylab="", 
         main=paste(dataname,datatype,"   Balance:",paste("$",y[length(y)],sep="")))
    points(y = y, x = x, col = colpoints, pch=19)
    axis(1, at = x, labels = x, las=2, col = colpoints)
    axis(2, at=pretty(y), labels=pretty(y), las=2, col = colpoints)
    box() 
    
    flog.info(paste("Wrote account data for",dataname,"by",timeUnit))
  }
  dev.off()
  flog.info(paste("Wrote",pdfname))
}


plotHist <- function(mdf, name, debug) {
  if ( debug ) { funcinfo(match.call(), 2) }
  hdata <- tapply(mdf$amount, INDEX = mdf$histgroup, FUN = sum)
  
  opn <- grep(pattern = "OpeningBalance", names(hdata))
  hopn <- hdata[opn]
  inc <- grep(pattern = "Income", names(hdata))
  hinc <- hdata[inc]
  int <- grep(pattern = "Interest", names(hdata))
  hint <- hdata[int]
  exp <- grep(pattern = "Expenses", names(hdata))
  hexp <- hdata[exp]
  gexp <- grep(pattern = "Expenses - General", names(hdata))
  i <- which(exp == gexp)
  exp <- exp[-i]
  hbexp <- hdata[exp]
  genexp <- mdf[mdf$histgroup == "Expenses - General",]
  hgexp <- tapply(genexp$amount, genexp$group, sum)
  
  
  rec <- grep(pattern = "Receipt", names(hdata))
  hrec <- hdata[rec]
  els <- hdata[-c(opn,inc,int,exp,rec)]
  hels <- hdata[els]
  
  
  b <- c(hinc, hbexp, hgexp, hint)
  facs <- list()
  facs[names(hinc)] <- "Income/Savings"
  facs[names(hbexp)] <- "Recuring Exp."
  facs[names(hgexp)] <- "General Exp."
  facs[names(hint)] <- "Interest"
  facs <- unlist(facs)
  
  histcat(mdata = b, facs = facs, name = name, dosum = T)
}

plotHists <- function(fulldf, debug = F) {
  if ( missing(fulldf) ) {
    rdaname <- "Rda/MergedFullRecords.rData"
    writeLines(paste("Loading",rdaname,"to plot histograms."))
    load(file = rdaname)
    fulldf <- mdf
  }
  pdfname <- paste(paste("Results/Yearly","HistChart",sep="/"), "pdf", sep=".")
  if ( debug ) { writeLines(paste("  ---> Saving histogram charts to",pdfname)) }
  pdf(pdfname)
  
  plotHist(fulldf, "All", debug)
  years <- unique(fulldf$year)
  for ( year in years ) {
    tmp <- fulldf[fulldf$year == as.Date(year),]
    name <- format(tmp$year[1], format="%Y")
    #print(paste(name,dim(tmp)))
    plotHist(tmp, name, debug)
    next
    uquarters <- unique(tmp$quarter)
    for ( quarter in uquarters ) {
      qtmp <- tmp[tmp$quarter == as.Date(quarter),]
      name <- QtrName(qtmp$quarter[1])
      #print(paste(name,dim(qtmp)))
      plotHist(qtmp, name, debug)
    }
  }
  dev.off()
}

showData <- function(dataset, category = NULL, account = NULL, write = F) {
  tmp <- data.frame()
  if ( !is.null(category) ) {
    tmp <- dataset[which(dataset[,"category"] == category),]
    if ( nrow(tmp) > 0 ) { showSlimDF(df = tmp, dfname = paste("Required category ->",category), summary = T, write)}
  }
  if ( !is.null(account) ) {
    tmp <- dataset[which(dataset[,"account"] == account),]
    if ( nrow(tmp) > 0 ) { showSlimDF(df = tmp, dfname = paste("Required account ->",account), summary = T, write)}
  }
  
}

findPaymentsFromDate <- function(date, dT, fulldf) {
  date <- as.Date(date)
  tmp <- fulldf[which(fulldf$time < date + dT & fulldf$time > date - dT),]
  showSlimDF(tmp)
  writeLines("")
  writeLines("")
}

findDates <- function(ddf, dT, fulldf, debug = F) {
  dates <- ddf[,"time"]
  for ( i in seq_len(nrow(ddf)) ) {
    if ( debug ) { writeLines("=================================================") }
    #showSlimDF(ddf[i,])
    date <- ddf[i,"time"]
    tmp <- fulldf[which(fulldf$time < date + dT & fulldf$time > date - dT),]
    testrowname <- rownames(ddf[i,])
    tmp2 <- tmp[rownames(tmp) != testrowname,]
    if ( debug ) { writeLines(paste("  Found maching entries:",nrow(tmp2))) }
    showMatchingDF(ddf[i,],tmp2)
    if ( debug ) { writeLines("") }
    if ( debug ) { writeLines("") }
  }
  return( tmp )
}

findTransferMatchingRow <- function(comp, dT, testdf, debug) {
  date <- comp[,"time"]
  category <- comp[,"category"]
  isThomas <- length(grep(pattern = "Thomas", category)) > 0
  isRuth <- length(grep(pattern = "Ruth", category)) > 0
  if ( isRuth & isThomas ) {
    print("???")
    print(category)
    f()
  }
  amount <- comp[,"amount"]
  mindate <- date - dT
  maxdate <- date + dT
  for ( j in seq_len(nrow(testdf)) ) {
    testdate = testdf[j,"time"]
    testamnt = testdf[j,"amount"]
    testcategory <- testdf[j,"category"]
    dtime <- testdate > mindate & testdate < maxdate
    damnt <- abs(testamnt + amount) < 1
    dfound <- is.na(testdf[j,"transferrow"])
    dperson <- T
    if ( isThomas ) { 
      if ( length(grep(pattern = "Thomas", testcategory)) == 0 ) { 
        dperson <- F
      }
    }
    if ( isRuth ) { 
      if ( length(grep(pattern = "Ruth", testcategory)) == 0 ) { 
        dperson <- F
      }
    }
    if ( debug ) {
      if ( damnt | dtime ) {
        writeLines(paste("dT =",dT,"   Possible match: time =",dtime,"   amount =",damnt))
        showMatchingDF(comp, testdf[j,])
      }
    }
    if ( dtime & damnt & dfound & dperson ) { return( j ) }
  }
  return( -1 )
}

findTransferMatch <- function(expcf, reccf, debug = F) {
  if ( is.null(expcf) | is.null(reccf) ) { return(NULL) }
  if ( !(is.data.frame(expcf) & is.data.frame(reccf)) ) { return(NULL) }
  if ( nrow(expcf) == 0 | nrow(reccf) == 0 ) { return(NULL) }
  expcf["transferrow"] <- NA
  reccf["transferrow"] <- NA
  exprownames <- rownames(expcf)
  for ( i in seq_len(nrow(expcf)) ) {
    retval <- findTransferMatchingRow(comp = expcf[i,], dT = 1, debug = debug, testdf = reccf)
    if ( retval < 0 ) {
      retval <- findTransferMatchingRow(comp = expcf[i,], dT = 2, debug = debug, testdf = reccf)
    } 
    if ( retval < 0 ) {
      retval <- findTransferMatchingRow(comp = expcf[i,], dT = 4, debug = debug, testdf = reccf)
    }
    if ( retval < 0 ) {
      retval <- findTransferMatchingRow(comp = expcf[i,], dT = 8, debug = debug, testdf = reccf)
    }
    if ( retval < 0 ) {
      retval <- findTransferMatchingRow(comp = expcf[i,], dT = 10, debug = debug, testdf = reccf)
    }
    if ( retval > -1 ) {
      expcf[i,"transferrow"] = rownames(reccf[retval,])
      reccf[retval,"transferrow"] = rownames(expcf[i,])
      df1 <- expcf[i,]
      df2 <- reccf[retval,]
      if ( debug ) { showMatchingDF(df1, df2) }
    } else {
      if ( debug ) { 
        df1 <- expcf[i,]
        showSlimDF(df1, summary = F)
        findDates(df1, dT = 10, fulldf = reccf)
        stop("\nCould not find a match for the following item. Also showing items +/- 10 days")
      }
    }
  }
  return( list(expcf=expcf, reccf=reccf) )
}

showMatchingDF <- function(df1, df2, summary = F) {
  print(paste(nrow(df1), nrow(df2)))
  if ( nrow(df1) > 0 & nrow(df2) > 0 ) {
    tmp1 <- df1[c("time","category","group","supergroup","account","payee","amount","transferrow")]
    tmp2 <- df2[c("time","category","group","supergroup","account","payee","amount","transferrow")]
    tmp <- rbind(tmp1, tmp2)
    showSlimDF(tmp, summary = summary)
  } else {
    if ( nrow(df1) > 0 ) {
      tmp1 <- df1[c("time","category","group","supergroup","account","payee","amount","transferrow")]
      showSlimDF(tmp1, summary = F)
    }
    if ( nrow(df2) > 0 ) {
      tmp2 <- df2[c("time","category","group","supergroup","account","payee","amount","transferrow")]
      showSlimDF(tmp2, summary = F)
    }
    
  }
}

showSlimDF <- function(df, dfname="data.frame", summary=T, write=F) {
  if ( write ) { sink(paste(dfname, "dat", sep = ".")) }
  if ( summary ) { writeLines(paste("Data Frame:",dfname)) }
  tmp <- df[c("time","category","account","payee","amount","transferrow")] 
  tmp["sum"] <- cumsum(tmp[,"amount"])
  #print(tmp)
  print(tmp[order(tmp[,"time"]),])
  if ( summary ) {
    writeLines(paste("Entries:     ",dim(tmp)[[1]]))
    ucats <- unique(tmp[,"category"])
    writeLines(paste("Categories:  ", ucats))
    catres <- tapply(tmp[["amount"]], INDEX = tmp[["category"]], FUN = sum)
    print(catres[!is.na(catres)])
    writeLines(paste("Accounts:    ",unique(tmp[,"account"])))
    writeLines(paste("Total Amount:",sum(tmp[,"amount"])))
    writeLines(paste("Data Frame:  ",dfname))
  }
  if ( write ) { sink() }
}


getSuperGroupDirection <- function(supergroup) {
  posdir <- 0
  if ( supergroup == "Expenses" ) { posdir <- -1 }
  if ( supergroup == "CollegeFund" ) { posdir <- 1 }
  if ( supergroup == "Income" ) { posdir <- 1 }
  if ( supergroup == "Savings" ) { posdir <- 1 }
  if ( supergroup == "HomeEquity" ) { posdir <- -1 }
  if ( posdir == 0 ) {
    writeLines(paste("SuperGroup",supergroup,"not recognized"))
    pause()
  }
  return( posdir )
}






############################################################
##
##
##  Make Histogram Grouping
##
##
############################################################
makeHistogramGrouping <- function() {
  
  rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), "AllGroups", sep="/"), "rda", sep=".")
  load(file = rdaname)
  print(names(finaldf))
  
  ## Income
  finaldf[["Income"]][["histgroup"]] <- "Income - Salary"
  
  ## Extra
  finaldf[["ExtraIncome"]][["histgroup"]] <- "Income - Extra"
  rows <- which(finaldf[["ExtraIncome"]][["category"]] == "ChaseOpeningBalance")
  if ( length(rows) > 0 ) { finaldf[["ExtraIncome"]][rows,][["histgroup"]] <- "OpeningBalance - Savings" }
  
  ## Work Expense
  finaldf[["Work"]][["histgroup"]] <- "Expenses - Work"
  
  ## Expense
  finaldf[["Expenses"]][["histgroup"]] <- "Expenses - General"
  rows <- which(finaldf[["Expenses"]][["category"]] == "CreditOpeningBalance")
  if ( length(rows) > 0 ) { finaldf[["Expenses"]][rows,][["histgroup"]] <- "OpeningBalance - Savings" }
  rows <- which(finaldf[["Expenses"]][["category"]] == "CreditCardPayment")
  if ( length(rows) > 0 ) { finaldf[["Expenses"]][rows,][["histgroup"]] <- "Expenses - Credit" }
  rows <- which(finaldf[["Expenses"]][["category"]] == "CreditCardReceipt")
  if ( length(rows) > 0 ) { finaldf[["Expenses"]][rows,][["histgroup"]] <- "Receipt - Credit" }
  
  
  ## Transfers (split these up: HomeEquity, Savings, CollegeFund)
  ## Transfers
  finaldf[["Transfers"]][["histgroup"]] <- ""
  rows <- which(finaldf[["Transfers"]][["category"]] == "HSBCSavingsPayment")
  if ( length(rows) > 0 ) { finaldf[["Transfers"]][rows,][["histgroup"]] <- "Expenses - House" }
  rows <- which(finaldf[["Transfers"]][["category"]] == "VanguardThomasContributionPayment")
  if ( length(rows) > 0 ) { finaldf[["Transfers"]][rows,][["histgroup"]] <- "Expenses - Savings" }
  rows <- which(finaldf[["Transfers"]][["category"]] == "VanguardRuthContributionPayment")
  if ( length(rows) > 0 ) { finaldf[["Transfers"]][rows,][["histgroup"]] <- "Expenses - Savings" }
  rows <- which(finaldf[["Transfers"]][["category"]] == "Claire529Payment")
  if ( length(rows) > 0 ) { finaldf[["Transfers"]][rows,][["histgroup"]] <- "Expenses - CollegeFund" }
  rows <- which(finaldf[["Transfers"]][["category"]] == "Charlie529Payment")
  if ( length(rows) > 0 ) { finaldf[["Transfers"]][rows,][["histgroup"]] <- "Expenses - CollegeFund" }
  
  ## Savings (split these up: HomeEquity, Savings, CollegeFund)
  ## Savings
  finaldf[["Savings"]][["histgroup"]] <- ""
  
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "FidelityOpenDataPayrollContribution"),][["histgroup"]] <- "Income - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "FidelityOpenDataCompanyContribution"),][["histgroup"]] <- "Income - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "TIAAFNALThomasContribution"),][["histgroup"]] <- "Income - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "TIAAFNALThomasCredit"),][["histgroup"]] <- "Income - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardColumbiaContribution"),][["histgroup"]] <- "Income - Savings"
  
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "HSBCSavingsReceipt"),][["histgroup"]] <- "Receipt - House"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardThomasContributionReceipt"),][["histgroup"]] <- "Receipt - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardRuthContributionReceipt"),][["histgroup"]] <- "Receipt - Savings"
  
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "FidelityFNALInterest"),][["histgroup"]] <- "Interest - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "FidelityOpenDataInterest"),][["histgroup"]] <- "Interest - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "HSBCSavingsInterest"),][["histgroup"]] <- "Interest - HouseSavings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "TIAAFNALThomasInterest"),][["histgroup"]] <- "Interest - Savings"
  
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardRuthShortTermCapitalGains"),][["histgroup"]] <- "Interest - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardRuthLongTermCapitalGains"),][["histgroup"]] <- "Interest - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardThomasLongTermCapitalGains"),][["histgroup"]] <- "Interest - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardThomasShortTermCapitalGains"),][["histgroup"]] <- "Interest - Savings"
  
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "TIAABNLThomasDividends"),][["histgroup"]] <- "Interest - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "TIAAFNALThomasDividends"),][["histgroup"]] <- "Interest - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardRuthMarketIndexDividends"),][["histgroup"]] <- "Interest - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardRuthDividends"),][["histgroup"]] <- "Interest - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardThomasMarketIndexDividends"),][["histgroup"]] <- "Interest - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardThomasDividends"),][["histgroup"]] <- "Interest - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardColumbiaEquity"),][["histgroup"]] <- "Interest - Savings"
  
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "FidelityFNALFee"),][["histgroup"]] <- "Interest - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "MissingHSBCCorrection"),][["histgroup"]] <- "Interest - Savings"
  
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardRuthMarketIndexConversionFrom"),][["histgroup"]] <- "Conversion - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardRuthMarketIndexConversionTo"),][["histgroup"]] <- "Conversion - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardThomasMarketIndexConversionFrom"),][["histgroup"]] <- "Conversion - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardThomasMarketIndexConversionTo"),][["histgroup"]] <- "Conversion - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "FidelityFNALConversion"),][["histgroup"]] <- "Conversion - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "TIAAFNALThomasTransfer"),][["histgroup"]] <- "Conversion - Savings"
  
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "FidelityOpenDataOpeningBalance"),][["histgroup"]] <- "OpeningBalance - Savings"
  rows <- which(finaldf[["Savings"]][["category"]] == "HSBCOpeningBalance")
  if ( length(rows) > 0 ) { finaldf[["Savings"]][rows,][["histgroup"]] <- "OpeningBalance - Savings" }
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "TIAABNLThomasOpeningBalance"),][["histgroup"]] <- "OpeningBalance - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "TIAAFNALThomasOpeningBalance"),][["histgroup"]] <- "OpeningBalance - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardColumbiaOpeningBalance"),][["histgroup"]] <- "OpeningBalance - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardThomasOpeningBalance"),][["histgroup"]] <- "OpeningBalance - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "VanguardRuthOpeningBalance"),][["histgroup"]] <- "OpeningBalance - Savings"
  
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "FidelityFNALTransfer"),][["histgroup"]] <- "Transfer - Savings"
  finaldf[["Savings"]][which(finaldf[["Savings"]][["category"]] == "TIAAFNALThomasTransfer"),][["histgroup"]] <- "Transfer - Savings"
  
  
  #################################  These are "accounts" where money goes #################################
  ## Expense
  finaldf[["CollegeFund"]][["histgroup"]] <- ""
  rows <- which(finaldf[["CollegeFund"]][["category"]] == "Charlie529ILOpeningBalance")
  if ( length(rows) > 0 ) { finaldf[["CollegeFund"]][rows,][["histgroup"]] <- "OpeningBalance - Savings" }
  rows <- which(finaldf[["CollegeFund"]][["category"]] == "Claire529ILOpeningBalance")
  if ( length(rows) > 0 ) { finaldf[["CollegeFund"]][rows,][["histgroup"]] <- "OpeningBalance - Savings" }
  rows <- which(finaldf[["CollegeFund"]][["category"]] == "Claire529NYOpeningBalance")
  if ( length(rows) > 0 ) { finaldf[["CollegeFund"]][rows,][["histgroup"]] <- "OpeningBalance - Savings" }
  finaldf[["CollegeFund"]][which(finaldf[["CollegeFund"]][["category"]] == "Charlie529ILInterest"),][["histgroup"]] <- "Interest - CollegeFund"
  finaldf[["CollegeFund"]][which(finaldf[["CollegeFund"]][["category"]] == "Claire529ILInterest"),][["histgroup"]] <- "Interest - CollegeFund"
  finaldf[["CollegeFund"]][which(finaldf[["CollegeFund"]][["category"]] == "Claire529NYInterest"),][["histgroup"]] <- "Interest - CollegeFund"
  rows <- which(finaldf[["CollegeFund"]][["category"]] == "Charlie529ILCharges")
  if ( length(rows) > 0 ) { finaldf[["CollegeFund"]][rows,][["histgroup"]] <- "Interest - CollegeFund" }
  rows <- which(finaldf[["CollegeFund"]][["category"]] == "Claire529ILCharges")
  if ( length(rows) > 0 ) { finaldf[["CollegeFund"]][rows,][["histgroup"]] <- "Interest - CollegeFund" }
  rows <- which(finaldf[["CollegeFund"]][["category"]] == "Claire529NYCharges")
  if ( length(rows) > 0 ) { finaldf[["CollegeFund"]][rows,][["histgroup"]] <- "Interest - CollegeFund" }
  rows <- which(finaldf[["CollegeFund"]][["category"]] == "Charlie529ILReceipt")
  if ( length(rows) > 0 ) { finaldf[["CollegeFund"]][rows,][["histgroup"]] <- "Receipt - CollegeFund" }
  rows <- which(finaldf[["CollegeFund"]][["category"]] == "Claire529ILReceipt")
  if ( length(rows) > 0 ) { finaldf[["CollegeFund"]][rows,][["histgroup"]] <- "Receipt - CollegeFund" }
  rows <- which(finaldf[["CollegeFund"]][["category"]] == "Claire529NYReceipt")
  if ( length(rows) > 0 ) { finaldf[["CollegeFund"]][rows,][["histgroup"]] <- "Receipt - CollegeFund" }

  ## Expense
  if ( nrow(finaldf[["HomeEquity"]]) > 0 ) {
    finaldf[["HomeEquity"]][["histgroup"]] <- "Expenses - HomeEquity"
    #finaldf[["Expenses"]][which(finaldf[["Expenses"]][["category"]] == "Mortgage"),][["histgroup"]] <- "Expenses - HomeEquity"
    #finaldf[["Expenses"]][which(finaldf[["Expenses"]][["category"]] == "HomeDownPayment"),][["histgroup"]] <- "Expenses - HomeEquity"
  }
  
  
  fulldf <- finaldf[[names(finaldf)[[1]]]]
  for ( d in 2:length(finaldf) ) {
    fulldf <- rbind(fulldf, finaldf[[names(finaldf)[[d]]]])
  }
  
  histgroups <- unique(fulldf$histgroup)
  histgroupmap <- list()
  for ( histgroup in histgroups ) {
    tmp <- fulldf[which(fulldf[,"histgroup"] == histgroup),]
    groups <- unique(tmp[,"group"])
    categories <- unique(tmp[,"category"])
    for ( category in categories ) {
      histgroupmap[[category]] <- histgroup
    }
  }
  
  
  fulldf["transferrow"] <- '-'
  newnames <- c()
  newnames <- unlist(lapply(rownames(fulldf), function(x) digest(x, algo="murmur32")))
  if ( length(unique(newnames)) == length(newnames) ) {
    rownames(fulldf) <- newnames
  } else {
    newnames <- unlist(lapply(rownames(fulldf), function(x) digest(x, algo="xxhash32")))
    if ( length(unique(newnames)) == length(newnames) ) {
      rownames(fulldf) <- newnames
    } else {
      newnames <- unlist(lapply(rownames(fulldf), function(x) digest(x, algo="md5")))
      rownames(fulldf) <- newnames
    }
  }
  
  
  ########################################################################################################################
  #
  # Connect the transfers
  #
  ########################################################################################################################
  
  
  histdf <- list()
  for ( histval in unique(fulldf[["histgroup"]]) ) {
    histdf[[histval]] <- fulldf[which(fulldf[["histgroup"]] == histval),]
  }
  

  ##
  ## Credit Card
  ##
  ## "Expenses - Credit" <-> "Receipt - Credit"
  ##
  
  writeLines("Match Credit Transfers")
  retvals <- findTransferMatch(histdf[["Expenses - Credit"]], histdf[["Receipt - Credit"]])
  if ( !is.null(retvals) ) {
    histdf[["Expenses - Credit"]] <- retvals[["expcf"]]
    histdf[["Receipt - Credit"]]  <- retvals[["reccf"]]
    if ( sum(histdf[["Expenses - Credit"]][,"amount"]) + sum(histdf[["Receipt - Credit"]][,"amount"]) > 2000 ) {
      showMatchingDF(histdf[["Expenses - Credit"]], histdf[["Receipt - Credit"]], summary = T)
      stop("Amounts of credit transfers do not match")
    }
  }
  
  writeLines("Match CollegeFund Transfers")
  retvals <- findTransferMatch(histdf[["Expenses - CollegeFund"]], histdf[["Receipt - CollegeFund"]])
  histdf[["Expenses - CollegeFund"]] <- retvals[["expcf"]]
  histdf[["Receipt - CollegeFund"]] <- retvals[["reccf"]]
  if ( sum(histdf[["Expenses - CollegeFund"]][,"amount"]) + sum(histdf[["Receipt - CollegeFund"]][,"amount"]) > 2000 ) {
    showMatchingDF(histdf[["Expenses - CollegeFund"]], histdf[["Receipt - CollegeFund"]], summary = T)
    stop("Amounts of credit transfers do not match")
  }
  
  #writeLines("Match HomeEquity Transfers")
  #retvals <- findTransferMatch(histdf[["Expenses - HomeEquity"]], histdf[["Receipt - HomeEquity"]])
  #histdf[["Expenses - HomeEquity"]] <- retvals[["expcf"]]
  #histdf[["Receipt - HomeEquity"]] <- retvals[["reccf"]]
  
  writeLines("Match House Transfers")
  retvals <- findTransferMatch(histdf[["Expenses - House"]], histdf[["Receipt - House"]])
  histdf[["Expenses - House"]] <- retvals[["expcf"]]
  histdf[["Receipt - House"]] <- retvals[["reccf"]]
  if ( sum(histdf[["Expenses - House"]][,"amount"]) + sum(histdf[["Receipt - House"]][,"amount"]) > 2000 ) {
    showMatchingDF(histdf[["Expenses - House"]], histdf[["Receipt - House"]], summary = T)
    stop("Amounts of credit transfers do not match")
  }
  
  writeLines("Match Savings Transfers")
  retvals <- findTransferMatch(histdf[["Expenses - Savings"]], histdf[["Receipt - Savings"]])
  histdf[["Expenses - Savings"]] <- retvals[["expcf"]]
  histdf[["Receipt - Savings"]] <- retvals[["reccf"]]
  if ( sum(histdf[["Expenses - Savings"]][,"amount"]) + sum(histdf[["Receipt - Savings"]][,"amount"]) > 2000 ) {
    showMatchingDF(histdf[["Expenses - Savings"]], histdf[["Receipt - Savings"]], summary = T)
    stop("Amounts of credit transfers do not match")
  }
  
  
  
  
  histdata <- tapply(fulldf$amount, INDEX = fulldf$histgroup, FUN = sum)
  accdata <- tapply(fulldf$amount, INDEX = fulldf$account, FUN = sum)
  
  
  writeLines("Combining histdfs back into fulldf")
  fulldf <- histdf[[names(histdf)[[1]]]]
  for ( d in 2:length(histdf) ) {
    fulldf <- rbind(fulldf, histdf[[names(histdf)[[d]]]])
  }
  fulldf <- fulldf[order(fulldf[,"time"]),]
  save(fulldf, file = "Rda/FullRecords.rData")
  
  
  ###########################################
  #
  # Combine Credit and Chase then throw away cross payments
  #
  ###########################################
  mdf <- fulldf[which(fulldf$category != "CreditCardReceipt" & fulldf$category != "CreditCardPayment"),]
  levels(mdf$account)[which(levels(mdf$account) == "Chase")] <- "Bank"
  levels(mdf$account)[which(levels(mdf$account) == "Credit")] <- "Bank"
  save(mdf, file = "Rda/MergedFullRecords.rData")
}





############################################################
##
##
##  Make Accounts data
##
##
############################################################
makeAccounts <- function(accounts) {
  ##
  ## Group Data
  ##
  totalsize <- 0
  supergroups <- list()
  
  for ( i in seq_along(accounts) ) {
    accountname <- accounts[[i]]
    accountdata <- subset(x = pdata, pdata$account == accountname)
    rdaname <- paste(paste(paste(basedir, "Accounts", sep="/"), accountname, sep="/"), "rda", sep=".")
    size <- dim(accountdata)[[1]]
    if ( size == 0 ) { next }
    totalsize <- totalsize + size    
    save(accountdata, file=rdaname)
    sumval <- paste(paste(paste("Saving",rdaname), "w/ size ="), size)
    writeLines(sumval)
  }
  
  sumval <- paste(paste("Wrote",totalsize),"account rows")
  writeLines(sumval)
  sumval <- paste(paste("Originally",total),"rows")
  writeLines(sumval)
}



############################################################
##
##
##  Get money direction in basis of Chase account
##
##
############################################################
getDirection <- function(account, type, amount) {
  retval <- 0
  
  ## Everything is in the basis of the chase acount (purchases are negative and credits are positive)
  if ( type == "ChaseOpeningBalance" ) { retval <- 1 }
  if ( type == "ChaseWithdrawal" ) { retval <- 1 }    
  if ( type == "ChaseDeposit" ) { retval <- 1 }    
  if ( type == "ChaseCheck" ) { retval <- 1 }    
  if ( type == "ChaseCashWithdrawal" ) { retval <- 1 }    
  if ( type == "CreditRedemption" ) { retval <- 1 }
  if ( type == "CreditPurchase" ) { retval <- -1 }
  if ( type == "CreditCashAdvance" ) { retval <- -1 }
  
  if ( type == "Charlie529" ) { retval <- 1 }
  if ( type == "Claire529" ) { retval <- 1 }
  
  if ( type == "MissingCreditPayment" ) { retval <- 1 }
  if ( type == "CreditPayment" ) { retval <- 1 }
  if ( type == "CreditOpeningBalance" ) { retval <- 1 }
  if ( type == "CreditFee" ) { retval <- -1 }
  
  if ( type == "ChaseFee" ) { retval <- 1 }
  
  if ( type == "FidelityOpeningBalance" ) { retval <- 1 }
  if ( type == "FidelitySummary" ) { retval <- 1 }
  if ( type == "FidelityDeposit" ) { retval <- 1 }
  if ( type == "FidelityInterest" ) { retval <- 1 }
  
  if ( type == "HSBCDeposit" ) { retval <- 1 }
  if ( type == "HSBCPayment" ) { retval <- 1 }
  if ( type == "HSBCOpeningBalance" ) { retval <- 1 }
  if ( type == "HSBCInterest" ) { retval <- 1 }
  
  if ( type == "MissingHSBCCorrection" ) { retval <- 1 }
  
  if ( type == "TIAAOpeningBalance" ) { retval <- 1 }
  if ( type == "TIAADeposit" ) { retval <- 1 }
  if ( type == "TIAATransfer" ) { retval <- 1 }
  if ( type == "TIAAInterest" ) { retval <- 1 }
  
  if ( type == "VanguardOpeningBalance" ) { retval <- 1 }
  if ( type == "VanguardInterest" ) { retval <- 1 }
  if ( type == "VanguardDeposit" ) { retval <- 1 }
  if ( type == "VanguardConversion" ) { retval <- 1 }
  
  ##HSBC##
  ##Taxes##
  ##TIAA##
  
  if ( retval == 0 ) {
    writeLines(paste("Type",type,"not recognized"))
    print(match.call())
    pause()
  }
  
  return( retval )
}

getDirections <- function(amount, type, account) {
  retval = rep(NA, length(amount))
  for(i in seq_along(amount)){
    retval[[i]] <- getDirection(account[[i]], type[[i]], amount[[i]])
  }
  return( retval )
}

############################################################
##
##
##  Turn on some categories when plotting groups
##
##
############################################################
getKeep <- function(categoryname, type, account) {
  retval = rep(NA, length(type))
  for(i in seq_along(type)){
    retval[[i]] <- 1
    typename <- type[[i]]
    
    if ( categoryname == "Claire529Payment" ) { retval[[i]] <- 0 }
    if ( categoryname == "Charlie529Payment" ) { retval[[i]] <- 0 }
    
    if ( categoryname == "CreditCardPayment" ) { retval <- 0 }
    if ( categoryname == "MissingCreditCorrection" ) { retval <- 0 }
    
    if ( typename == "FidelitySummary" ) { retval <- 0 }
    
    if ( categoryname == "HSBCSavingsPayment" ) { retval <- 0 }
    
    if ( typename == "VanguardConversion" ) { retval <- 0 }
    
    if ( categoryname == "ThomasIRAContributionPayment" ) { retval <- 0 }
    if ( categoryname == "RuthIRAContributionPayment" ) { retval <- 0 }
    
  }
  return( retval )
}


############################################################
##
##
##  Plot Account Data
##
##
############################################################
plotAccount <- function(accountname, debug = F) {
  predict <- FALSE
  
  rdaname <- paste(paste(paste(basedir, "Accounts", sep="/"), accountname, sep="/"), "rda", sep=".")
  load(rdaname)
  pdfname <- paste(paste("Results/Accounts",accountname,sep="/"), "pdf", sep=".")
  saveText(accountdata, pdfname)
  
  groups <- unique(accountdata$group)
  ngroups <- length(groups)
  
  qtrs <- as.Date(unique(pdata$quarter))
  nqtrs <- length(qtrs)
  
  
  quarterMatrix <- matrix(0, nrow=nqtrs, ncol=ngroups)
  rownames(quarterMatrix) <- as.numeric(qtrs)
  colnames(quarterMatrix) <- groups
  
  writeLines("  ---> Making matrices")
  
  
  for ( i in seq_along(groups) ) {
    groupname <- groups[[i]]
    writeLines(paste("  ---> Making matrices --", groupname))
    
    groupdata <- subset(x = accountdata, accountdata$group == groupname)
    if ( dim(groupdata)[[1]] == 0 ) { next }
    
    ## Create quarterly data
    quarterdata <- tapply(groupdata$amount, groupdata$quarter, sum)
    ## Add to matrix
    quarterMatrix <- mergeMatrix(quarterMatrix, quarterdata, i)
    #writeLines(paste("  ---> Making matrices --", groupname, "Done."))
  }
  
  ## Fix NA
  quarterMatrix[is.na(quarterMatrix)] <- 0
  
  
  
  ## Save matrix
  pdfname <- paste(paste("Results/Accounts",accountname,sep="/"), "pdf", sep=".")
  saveMatrix(quarterMatrix, pdfname)
  
  
  
  
  pdfname <- paste(paste("Results/Quarters",accountname,sep="/"), "pdf", sep=".")
  writeLines(paste("  ---> Saving quarterly to",pdfname))
  pdf(pdfname)
  baroverlay(quarterMatrix, "AccountSummary", FALSE, "quarters")    
  baroverlay(quarterMatrix, "AccountSummary", FALSE, "quarters", "sum")    
  if ( predict && FALSE ) {
    HWforecast(quarterMatrix, accountname, debug)
  }
  for ( i in seq_along(groups) ) {
    plotoverlay(quarterMatrix[,i], groups[[i]], FALSE)
  }
  
  writeLines(paste("  ---> Saving quarterly to",pdfname,"Done."))
  
  
  dev.off()
  
  
  ## Save matrix
  rdaname <- paste(paste(paste(basedir, "QuarterMatrices", sep="/"), accountname, sep="/"), "rda", sep=".")
  save(quarterMatrix, file=rdaname)
  
}


plotAccounts <- function(accounts, debug = F) {
  predict <- FALSE
  
  qtrs <- as.Date(unique(pdata$quarter))
  nqtrs <- length(qtrs)
  
  ngroups <- length(accounts)
  
  for ( i in seq_along(accounts) ) {                
    accountname <- accounts[[i]]
    plotAccount(accountname)
  }
  
  quarterMatrix <- matrix(0, nrow=nqtrs, ncol=naccounts)
  rownames(quarterMatrix) <- as.numeric(qtrs)
  colnames(quarterMatrix) <- accounts
  
  
  for ( i in seq_along(accounts) ) {                
    
    rdaname <- paste(paste(paste(basedir, "Accounts", sep="/"), accountname, sep="/"), "rda", sep=".")
    load(rdaname)
    
    ## Create quarterly data
    quarterdata <- tapply(accountdata$amount, accountdata$quarter, sum)
    ## Add to matrix
    quarterMatrix <- mergeMatrix(quarterMatrix, quarterdata, i)
  }
  
  ## Fix NA
  quarterMatrix[is.na(quarterMatrix)] <- 0
  
  
  
  pdfname <- paste(paste("Results/Quarters","AccountSummary",sep="/"), "pdf", sep=".")
  writeLines(paste("  ---> Saving quarterly to",pdfname))
  pdf(pdfname)
  baroverlay(quarterMatrix, "AccountSummary", FALSE, "quarters", "sum")    
  if ( predict && FALSE ) {
    HWforecast(quarterMatrix, accountname, debug)
  }
  for ( i in seq_along(accounts) ) {
    plotoverlay(quarterMatrix[,i], accounts[[i]], FALSE)
  }
  
  
  
  writeLines(paste("  ---> Saving quarterly to",pdfname,"Done."))
  dev.off()
  
  
  ## Save matrix
  rdaname <- paste(paste(paste(basedir, "QuarterMatrices", sep="/"), "AccountSummary", sep="/"), "rda", sep=".")
  save(quarterMatrix, file=rdaname)
  
}



############################################################
##
##
##  Plot Group Data
##
##
############################################################
plotGroup <- function(groupname, debug = F) {
  predict <- TRUE
  
  rdaname <- paste(paste(paste(basedir, "Groups", sep="/"), groupname, sep="/"), "rda", sep=".")
  writeLines(paste("Loading",rdaname))
  load(rdaname)
  
  qtrs <- as.Date(unique(groupdata$quarter))
  nqtrs <- length(qtrs)
  categories <- unique(groupdata$category)
  ncategories <- length(categories)
  
  ## For corrected amount
  quarterMatrix <- matrix(0, nrow=nqtrs, ncol=ncategories)
  rownames(quarterMatrix) <- as.numeric(qtrs)
  colnames(quarterMatrix) <- categories    
  
  ## For true amount (for later use)
  trueQuarterMatrix <- matrix(0, nrow=nqtrs, ncol=ncategories)
  rownames(trueQuarterMatrix) <- as.numeric(qtrs)
  colnames(trueQuarterMatrix) <- categories    
  
  ## We want positive plots so artifically flip overall direction
  supergroup <- unique(groupdata$supergroup)
  posdir <- getSuperGroupDirection(supergroup)
  
  
  for ( i in seq_along(categories) ) {
    categoryname <- categories[[i]]
    ## Get subset
    catdata <- subset(x = groupdata, groupdata$category == categoryname)
    if ( dim(catdata)[[1]] == 0 ) { next }
    catdata$direction <- getDirections(catdata$amount, catdata$type, catdata$account)
    catdata$keep <- getKeep(categoryname, catdata$type, catdata$account)
    catdata$trueamount <- catdata$amount * catdata$direction * posdir
    catdata$amount <- catdata$keep * catdata$trueamount
    if ( debug ) {
      print("<< Amount >>")        
      print(catdata$amount)
      print("<< DF >>")        
      print(catdata)
      print("")
    }
    
    ## Create quarterly data
    quarterdata <- tapply(catdata$amount, catdata$quarter, sum)
    ## Add to matrix
    quarterMatrix <- mergeMatrix(quarterMatrix, quarterdata, i)
    
    ## Create quarterly data for true amount
    truequarterdata <- tapply(catdata$trueamount, catdata$quarter, sum)
    ## Add to matrix
    trueQuarterMatrix <- mergeMatrix(trueQuarterMatrix, truequarterdata, i)
  }
  
  ## Fix NA
  quarterMatrix[is.na(quarterMatrix)] <- 0
  trueQuarterMatrix[is.na(trueQuarterMatrix)] <- 0
  
  ## Save matrix
  rdaname <- paste(paste(paste(basedir, "QuarterMatrices", sep="/"), groupname, sep="/"), "rda", sep=".")
  save(quarterMatrix, file=rdaname)
  
  ## Save matrix
  rdaname <- paste(paste(paste(basedir, "TrueQuarterMatrices", sep="/"), groupname, sep="/"), "rda", sep=".")
  save(trueQuarterMatrix, file=rdaname)
  
  
  ## Make PDFs
  pdfname <- paste(paste("Results/Quarters",groupname,sep="/"), "pdf", sep=".")
  writeLines(paste("  ---> Saving quarterly to",pdfname))
  pdf(pdfname)
  baroverlay(quarterMatrix, groupname, FALSE, "quarters", "sum")    
  if ( predict && FALSE ) {
    HWforecast(quarterMatrix, groupname, debug)
  }
  
  for ( i in seq_along(categories) ) {
    plotoverlay(quarterMatrix[,i], categories[[i]], FALSE)
  }
  writeLines(paste("  ---> Saving quarterly to",pdfname,"Done."))
  dev.off()
}

plotGroups <- function(groups) {
  for ( i in seq_along(groups) ) {
    groupname <- groups[[i]]
    plotGroup(groupname)
  }
}



############################################################
##
##
##  Plot Super-Group Data
##
##
############################################################
plotSuperGroup <- function(supergroupname, supergroupvals, debug = F) {
  predict <- TRUE
  
  qtrs <- as.Date(unique(pdata$quarter))
  nqtrs <- length(qtrs)
  
  groups <- supergroupvals
  ngroups <- length(supergroupvals)
  
  superQuarterMatrix <- matrix(0, nrow=nqtrs, ncol=ngroups)
  rownames(superQuarterMatrix) <- as.numeric(qtrs)
  colnames(superQuarterMatrix) <- supergroupvals    
  
  trueSuperQuarterMatrix <- matrix(0, nrow=nqtrs, ncol=ngroups)
  rownames(trueSuperQuarterMatrix) <- as.numeric(qtrs)
  colnames(trueSuperQuarterMatrix) <- supergroupvals    
  
  for ( i in seq_along(groups) ) {
    groupname <- groups[[i]]
    rdaname <- paste(paste(paste(basedir, "QuarterMatrices", sep="/"), groupname, sep="/"), "rda", sep=".")
    load(rdaname)
    
    ## Get sum over all columns
    quarterdata <- rowSums(quarterMatrix)
    names(quarterdata) <- as.Date(as.numeric(names(quarterdata)))
    
    ## Merge matrix columns
    superQuarterMatrix <- mergeMatrix(superQuarterMatrix, quarterdata, i)
    
    
    ## For the uncorrected values
    rdaname <- paste(paste(paste(basedir, "TrueQuarterMatrices", sep="/"), groupname, sep="/"), "rda", sep=".")
    load(rdaname)
    
    ## Get sum over all columns
    truequarterdata <- rowSums(trueQuarterMatrix)
    names(truequarterdata) <- as.Date(as.numeric(names(truequarterdata)))
    
    ## Merge matrix columns
    trueSuperQuarterMatrix <- mergeMatrix(trueSuperQuarterMatrix, truequarterdata, i)
  }
  
  
  ## Fix NA
  superQuarterMatrix[is.na(superQuarterMatrix)] <- 0
  trueSuperQuarterMatrix[is.na(trueSuperQuarterMatrix)] <- 0
  
  ## Save matrix
  rdaname <- paste(paste(paste(basedir, "QuarterMatrices", sep="/"), supergroupname, sep="/"), "rda", sep=".")
  save(superQuarterMatrix, file=rdaname)
  
  ## Save matrix (true)
  rdaname <- paste(paste(paste(basedir, "TrueQuarterMatrices", sep="/"), supergroupname, sep="/"), "rda", sep=".")
  save(trueSuperQuarterMatrix, file=rdaname)
  
  
  pdfname <- paste(paste("Results/Quarters",supergroupname,sep="/"), "pdf", sep=".")
  writeLines(paste("  ---> Saving quarterly to",pdfname))
  pdf(pdfname)
  baroverlay(superQuarterMatrix, supergroupname, FALSE, "quarters", "sum")    
  if ( predict && FALSE ) {
    HWforecast(superQuarterMatrix, supergroupname, debug)
  }
  for ( i in seq_along(groups) ) {
    plotoverlay(superQuarterMatrix[,i], groups[[i]], FALSE)
  }
  writeLines(paste("  ---> Saving quarterly to",pdfname,"Done."))
  dev.off()
}

plotSuperGroups <- function() {
  rdaname <- paste(paste(paste(basedir, "Groups", sep="/"), "SuperGroups", sep="/"), "rda", sep=".")
  load(rdaname)
  
  for ( i in seq_along(supergroups) ) {
    supergroupname <- names(supergroups)[[i]]
    plotSuperGroup(supergroupname, supergroups[[i]])
  }
}



############################################################
##
##
##  Plot Everything
##
##
############################################################
makeFinalGrouping <- function(debug = F) {
  predict <- TRUE
  
  sgroups <- sort(unique(pdata$supergroup))
  accounts <- sort(unique(pdata$account))
  categories <- sort(unique(pdata$category))
  groups <- sort(unique(pdata$group))
  
  ##
  ## Payments to Savings/CollegeFund
  ##
  transfers <- c("Charlie529Payment", "Claire529Payment", "RuthIRAContributionPayment", "ThomasIRAContributionPayment", "HSBCSavingsPayment")
  #transfers <- c("Charlie529Payment", "Claire529Payment", "RuthIRAContributionPayment", "ThomasIRAContributionPayment", "HSBCSavingsPayment", "HomeDownPayment", "Mortgage")
  #transfers <- c("HomeDownPayment", "Mortgage")
  hometfs <- c("HomeDownPayment", "Mortgage")
  #transfers <- c()
  
  
  transfertotal <- list()
  for ( i in seq_along(transfers) ) { transfertotal[[i]] <- 0 }    
  hometotal <- list()
  for ( i in seq_along(hometfs) ) { hometotal[[i]] <- 0 }    
  sgrouptotal <- list()
  for ( s in seq_along(sgroups) ) { sgrouptotal[[s]] <- 0 }
  accounttotal <- list()
  for ( s in seq_along(accounts) ) { accounttotal[[s]] <- 0 }
  categorytotal <- list()
  for ( s in seq_along(categories) ) { categorytotal[[s]] <- 0 }
  grouptotal <- list()
  for ( s in seq_along(groups) ) { grouptotal[[s]] <- 0 }
  
  finaldf <- list()
  for ( s in seq_along(sgroups) ) {
    finaldf[[sgroups[[s]]]] <- data.frame()
  }
  finaldf[["Transfers"]] <- data.frame()
  #finaldf[["Home"]] <- data.frame()
  
  
  ##
  ## SuperGroup
  ##
  total <- 0
  for ( s in seq_along(sgroups) ) {
    sgroupname <- sgroups[[s]]
    rdaname <- paste(paste(paste(basedir, "SuperGroups", sep="/"), sgroupname, sep="/"), "rda", sep=".")
    load(rdaname)
    size <- dim(supergroupdata)[[1]]
    #writeLines(paste("Found",size,"from",sgroupname))
    if ( debug ) { writeLines("") }
    #writeLines(paste("========",sgroupname,"========"))
    
    if ( debug ) { print(unique(supergroupdata$year)) }
    #supergroupdataX <- subset(supergroupdata, as.Date(supergroupdata$year) == as.Date("2013-01-01"))
    supergroupdataX <- supergroupdata
    
    ##
    ## Accounts
    ##
    stotal <- 0
    accounts <- sort(unique(supergroupdataX$account))
    for ( a in seq_along(accounts) ) {
      accountname <- accounts[[a]]        
      adata <- subset(x = supergroupdataX, supergroupdataX$account == accountname)
      
      if ( debug ) { writeLines("") }
      #writeLines(paste("  ========",accountname,"========"))
      groups <- sort(unique(adata$group))
      ngroups <- length(groups)
      
      ##
      ## Groups
      ##
      atotal <- 0
      for ( g in seq_along(groups) ) {
        groupname <- groups[[g]]
        #writeLines(paste("\t========",groupname,"========"))
        
        gdata <- subset(x = adata, adata$group == groupname)
        types <- unique(gdata$type)
        ntypes <- length(types)
        
        categories <- unique(gdata$category)
        ncategories <- length(categories)
        
        ##
        ## Categories
        ##
        gtotal <- 0
        for ( c in seq_along(categories) ) {
          catname <- categories[[c]]
          cdata <- subset(x = gdata, gdata$category == catname)
          if ( dim(cdata)[[1]] == 0 ) { next }
          
          dir <- 1
          if ( accountname == "Chase" ) {
            #if ( catname == "CreditCardPayment" ) { dir = 0 }
            #if ( catname == "HSBCSavingsPayment" ) { dir = 0 }
          }
          if ( accountname == "Credit" ) {
            dir = -1
            #if ( catname == "CreditCardReceipt" ) { dir = 0 }
          }
          # Convert HSBC Savings into 1/2 HomeEq effectively...
          if ( groupname == "Home" & F ) { dir <- 0 }
          
          ## Special for HomeEquity
          if ( catname %in% hometfs & F ) {
            cdata$amount <- cdata$amount * -1
            csum <- as.integer(sum(cdata$amount))
            pos <- which(hometfs == catname)
            hometotal[[pos]] <- hometotal[[pos]] + csum
            if ( debug ) { writeLines(paste("\t\t\t\t>>>",csum,"<<<========",catname,"======== (transfer)")) }
            finaldf[["HomeEquity"]] <- rbind(finaldf[["HomeEquity"]], cdata)
            
            cdata$amount <- cdata$amount * -1
            csum <- as.integer(sum(cdata$amount))
            pos <- which(transfers == catname)
            transfertotal[[pos]] <- transfertotal[[pos]] + csum
            if ( debug ) { writeLines(paste("\t\t\t\t>>>",csum,"<<<========",catname,"======== (transfer)")) }
            cdata$supergroup <- "Transfers"
            finaldf[["Transfers"]] <- rbind(finaldf[["Transfers"]], cdata)
            next
          }
          
          
          cdata$amount <- cdata$amount * dir
          csum <- as.integer(sum(cdata$amount))
          
          if ( catname %in% transfers ) {
            pos <- which(transfers == catname)
            transfertotal[[pos]] <- transfertotal[[pos]] + csum
            if ( debug ) { writeLines(paste("\t\t\t\t>>>",csum,"<<<======== Category:",catname,"======== (transfer)")) }
            finaldf[["Transfers"]] <- rbind(finaldf[["Transfers"]], cdata)
          } else if ( catname %in% hometfs && FALSE ) {
            pos <- which(hometfs == catname)
            hometotal[[pos]] <- hometotal[[pos]] + csum
            if ( debug ) { writeLines(paste("\t\t\t\t>>>",csum,"<<<======== Category:",catname,"======== (transfer)")) }
            finaldf[["HomeEquity"]] <- rbind(finaldf[["HomeEquity"]], cdata)
          } else {
            if ( debug ) { writeLines(paste("\t\t\t\t>>>",csum,"<<<======== Category:",catname,"========")) }
            gtotal <- gtotal + csum
            categorytotal[[c]] <- categorytotal[[c]] + csum
            finaldf[[sgroupname]] <- rbind(finaldf[[sgroupname]], cdata)
          }
        }
        if ( debug ) { writeLines(paste("\t\t\t>>>",gtotal,"<<<======== Group:",groupname,"========")) }
        atotal <- atotal + gtotal
        grouptotal[[g]] <- grouptotal[[g]] + gtotal
      }
      if ( debug ) { writeLines(paste("\t\t>>>",atotal,"<<<======== Account:",accountname,"========")) }
      stotal <- stotal + atotal
      accounttotal[[a]] <- accounttotal[[a]] + atotal
    }
    if ( debug ) { writeLines(paste("\t>>>",stotal,"<<<======== SuperGroup:",sgroupname,"========")) }
    total <- total + stotal
    sgrouptotal[[s]] <- sgrouptotal[[s]] + stotal
  }
  
  if ( debug ) { writeLines("") }
  trtotal <- 0
  for ( t in seq_along(transfers) ) {
    if ( debug ) { writeLines(paste("\t\t>>>",transfertotal[[t]],"<<<========",transfers[[t]],"========")) }
    trtotal <- trtotal + transfertotal[[t]]
  }
  total <- total + trtotal
  if ( debug ) { writeLines(paste("\t>>>",trtotal,"<<<======== Transfers ========")) }
  homeeqtotal <- 0
  for ( t in seq_along(hometfs) ) {
    if ( debug ) { writeLines(paste("\t\t>>>",hometotal[[t]],"<<<========",hometfs[[t]],"========")) }
    homeeqtotal <- homeeqtotal + hometotal[[t]]
  }
  total <- total + homeeqtotal
  if ( debug ) {
    writeLines(paste("\t>>>",homeeqtotal,"<<<======== Home ========"))    
    writeLines("")
    writeLines("")
    writeLines(paste(">>>",total,"<<<======== Total ========"))
    writeLines("")
    writeLines("")
  }
  sgroupsum <- Reduce("+", sgrouptotal)
  if ( debug ) { 
    for ( s in seq_along(sgroups) ) {
      print(paste(">>>",sgrouptotal[[s]],"<<< ",sgroups[[s]]))
    }
  }
  sgroupsum <- sgroupsum + Reduce("+", transfertotal)
  sgroupsum <- sgroupsum + Reduce("+", hometotal)
  if ( debug ) { 
    print(paste(">>>",Reduce("+", transfertotal),"<<< Transfer"))
    print(paste(">>>",Reduce("+", hometotal),"<<< Home"))
    print(paste(">>>",sgroupsum,"<<< Total"))
  }
  for ( d in seq_along(finaldf) ) {
    name <- names(finaldf)[[d]]
    rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), name, sep="/"), "rda", sep=".")
    df <- finaldf[[d]]
    if ( debug ) {
      writeLines(rdaname)
      writeLines(paste("Entries:", nrow(df)))
      writeLines(paste("Sum:    ", sum(df$amount)))
    }
    if ( nrow(df) > 0 ) {
      tabledata <- tapply(df$amount, INDEX = df$category, FUN = sum)
      tabledata <- tabledata[!is.na(tabledata)]
      if ( debug ) { print(tabledata) }
    }
    if ( debug ) { writeLines("") }
    save(df, file=rdaname)
  }
  
  rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), "AllGroups", sep="/"), "rda", sep=".")
  save(finaldf, file = rdaname)
  
  ## Merge transfers and income (if needed)    
  df <- rbind(finaldf[["Transfers"]], finaldf[["Income"]])
  rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), "IncomeX", sep="/"), "rda", sep=".")
  save(df, file=rdaname)
  
  ## Merge transfers and income (if needed)    
  df <- rbind(finaldf[["Transfers"]], finaldf[["Income"]], finaldf[["Expenses"]])
  rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), "IncomeXX", sep="/"), "rda", sep=".")
  save(df, file=rdaname)
  
  ## Merge transfers and income (if needed)
  df <- finaldf[["HomeEquity"]]
  df$amount <- df$amount * -1
  df <- rbind(df, finaldf[["Transfers"]], finaldf[["Income"]], finaldf[["Expenses"]])
  rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), "IncomeXXX", sep="/"), "rda", sep=".")
  save(df, file=rdaname)
}


plotFinalGroup <- function(finalgroupname, retcmd, debug = F) {
  ## Get organized data frames
  rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), finalgroupname, sep="/"), "rda", sep=".")
  load(rdaname)
  if ( debug ) { writeLines(paste("Found",dim(df)[[1]],"entries from",finalgroupname)) }
  
  qtrs <- sort(as.Date(unique(df$quarter)))
  nqtrs <- length(qtrs)
  
  months <- sort(as.Date(unique(df$month)))
  nmonths <- length(months)
  
  weeks <- sort(as.Date(unique(df$week)))
  nweeks <- length(weeks)
  
  days <- sort(as.Date(unique(df$day)))
  ndays <- length(days)
  
  groups <- sort(unique(df$group))
  ngroups <- length(groups)
  
  quarterMatrix <- matrix(0, nrow=nqtrs, ncol=ngroups)
  rownames(quarterMatrix) <- as.numeric(qtrs)
  colnames(quarterMatrix) <- groups    
  
  monthMatrix <- matrix(0, nrow=nmonths, ncol=ngroups)
  rownames(monthMatrix) <- as.numeric(months)
  colnames(monthMatrix) <- groups    
  
  weekMatrix <- matrix(0, nrow=nweeks, ncol=ngroups)
  rownames(weekMatrix) <- as.numeric(weeks)
  colnames(weekMatrix) <- groups
  
  
  for ( g in seq_along(groups) ) {
    groupname <- groups[[g]]
    
    groupdata <- subset(x = df, df$group == groupname)
    
    
    quarterdata <- tapply(groupdata$amount, groupdata$quarter, sum)
    quarterMatrix <- mergeMatrix(quarterMatrix, quarterdata, g)
    
    monthdata <- tapply(groupdata$amount, groupdata$month, sum)
    monthMatrix <- mergeMatrix(monthMatrix, monthdata, g)
    
    weekdata <- tapply(groupdata$amount, groupdata$week, sum)
    weekMatrix <- mergeMatrix(weekMatrix, weekdata, g)
    
    if ( doDays ) {
      daydata <- tapply(groupdata$amount, groupdata$day, sum)
      dayMatrix <- mergeMatrix(dayMatrix, daydata, g)
    }
  }
  
  
  ## Fix NA
  quarterMatrix[is.na(quarterMatrix)] <- 0
  monthMatrix[is.na(monthMatrix)] <- 0
  weekMatrix[is.na(weekMatrix)] <- 0
  if ( doDays ) {
    dayMatrix[is.na(dayMatrix)] <- 0
  }
  
  
  ## Plots
  plots <- list()   
  #plots <- list(plots, ggBarPlot(quarterMatrix, "Running Sum of Quarterly Activity", "sum"))
  #plots <- list(plots, ggBarPlot(quarterMatrix, "Quarterly Activity", "norm"))
  plots <- list(plots, ggBarPlotPrep(monthMatrix, paste("Running Sum of Monthly Activity for", finalgroupname), "sum"))
  plots <- list(plots, ggDataPlotPrep(monthMatrix, paste("Monthly Activity for",finalgroupname), "sum"))
  #plots <- list(plots, ggBarPlot(monthMatrix, "Monthly Activity", "norm"))
  #plots <- list(plots, ggBarPlot(weekMatrix, "Running Sum of Weekly Activity", "sum"))   
  if ( doDays ) {
    plots <- list(plots, ggBarPlot(dayMatrix, "Daily Activity", "sum"))
    plots <- list(plots, ggBarPlot(dayMatrix, "Running Sum of Daily Activity", "norm"))
  }
  
  
  doForecast <- FALSE
  if ( doForecast ) {
    tsvals <- createTimeSeries("Month", monthMatrix)
    for ( tsval in tsvals ) {
      fit <- tbats(tsval)
      fcast <- forecast(fit)
      plots <- list(plots, plot(fcast))
    }
  }
  
  if ( retcmd == "return" ) {
    return( plots )
  }
  
  pdfname <- paste(paste("Results/Quarters/Summary",finalgroupname,sep="-"),"pdf", sep=".")
  if ( debug ) { writeLines(paste("Creating",pdfname)) }
  pdf(pdfname)
  print(plots)
  dev.off()
  if ( debug ) { writeLines(paste("Wrote",pdfname)) }
  
  
  rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), "MatrixData", sep="/"), "rda", sep=".")
  if ( debug ) { writeLines(paste("Saving monthly, quarterly, weekly, and daily matrix data to",rdaname)) }
  save(monthMatrix, quarterMatrix, weekMatrix, dayMatrix, file = rdafile)
}

plotFinalGroups <- function(retcmd) {
  list.files("")  
}


combineForecasts <- function(mdata1, mdata2) {
  sub11 <- subset(mdata1, mdata1$Var2 == "Data")
  sub12 <- subset(mdata1, mdata1$Var2 == "Fit")
  sub13 <- subset(mdata1, mdata1$Var2 == "Forecast")
  print(paste("Data",min(sub11$Var1), max(sub11$Var1)))
  print(paste("Fit",min(sub12$Var1), max(sub12$Var1)))
  print(paste("Forecast",min(sub13$Var1), max(sub13$Var1)))
  
  sub21 <- subset(mdata2, mdata2$Var2 == "Data")
  sub22 <- subset(mdata2, mdata2$Var2 == "Fit")
  sub23 <- subset(mdata2, mdata2$Var2 == "Forecast")
  print(paste("Data",min(sub21$Var1), max(sub21$Var1)))
  print(paste("Fit",min(sub22$Var1), max(sub22$Var1)))
  print(paste("Forecast",min(sub23$Var1), max(sub23$Var1)))
  
  mdata <- rbind(sub11, sub13, sub23)
  
  print(paste("Combine",min(mdata$Var1), max(mdata$Var1)))
  
  return( mdata )
}

combineForecast <- function(fcast) {
  ## Fitted data
  x <- fcast$x
  t <- as.Date(time(x))
  fitted <- fcast$fitted
  
  ## Forecast data
  central <- fcast$mean
  tcentral <- as.Date(time(central))
  centralp1 <- fcast$upper[,1]
  centralm1 <- fcast$lower[,1]
  
  matdata <- matrix(c(x, fitted), nrow=length(t), ncol=2)
  rownames(matdata) <- as.numeric(t)
  colnames(matdata) <- c("Data", "Fit")
  mdata1 <- melt(matdata)
  
  matdata2 <- matrix(c(central, centralp1, centralm1), nrow=length(tcentral), ncol=3)
  rownames(matdata2) <- as.numeric(tcentral)
  colnames(matdata2) <- c("Forecast", "+10%", "-10%")
  mdata2 <- melt(matdata2)
  mdata <- rbind(mdata1, mdata2)
  
  return( mdata )
}


############################################################
##
##
##  Write Final Grouping to CSV
##
##
############################################################
createFundingMatrices <- function(version, debug = F) {
  writeLines("Creating funding matrices")
  
  ## In case I ever need to get all files
  fileNames <- Sys.glob("Rda/FinalGrouping/*.rda")
  if ( debug ) { print(fileNames) }
  if ( version == 0 ) {
    groupings <- c("CollegeFund", "HomeEquity", "Savings", "Expenses", "Income")
    groupingnames <- c("College Fund", "Home Equity", "Savings, IRA, 401k", "Expenses", "Income")
  }
  if ( version == 1 ) {
    groupings <- c("CollegeFund", "HomeEquity", "Savings", "Expenses", "Income", "Transfers", "Work", "ExtraIncome")
    groupingnames <- c("College Fund", "Home Equity", "Savings, IRA, 401k", "Expenses", "Salary", "Transfers", "Work", "Extra Income")
  }
  if ( version == 2 ) {
    groupings <- c("CollegeFund", "HomeEquity", "Savings", "Expenses", "IncomeX")
    groupingnames <- c("College Fund", "Home Equity", "Savings, IRA, 401k", "Expenses", "Income'")
  }
  if ( version == 3 ) {
    groupings <- c("Savings", "CollegeFund", "HomeEquity", "IncomeXX")
    groupingnames <- c("Savings, IRA, 401k", "College Fund", "Home Equity", "Income - Expenses")
  }
  if ( version == 4 ) {
    groupings <- c("Savings", "IncomeXXX", "CollegeFund", "HomeEquity")
    groupingnames <- c("Savings, IRA, 401k", "Income - Expenses", "College Fund", "Home Equity")
  }
  
  
  groupname <- "IncomeXXX"
  rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), groupname, sep="/"), "rda", sep=".")
  load(rdaname)
  if ( debug ) { writeLines(paste("Found",dim(df)[[1]],"entries from",groupname,"with sum =",sum(df$amount))) }
  
  groupname <- "IncomeXX"
  rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), groupname, sep="/"), "rda", sep=".")
  load(rdaname)
  if ( debug ) { writeLines(paste("Found",dim(df)[[1]],"entries from",groupname,"with sum =",sum(df$amount))) }
  
  groupname <- "IncomeX"
  rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), groupname, sep="/"), "rda", sep=".")
  load(rdaname)
  if ( debug ) { writeLines(paste("Found",dim(df)[[1]],"entries from",groupname,"with sum =",sum(df$amount))) }
  
  groupname <- "Income"
  rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), groupname, sep="/"), "rda", sep=".")
  load(rdaname)
  if ( debug ) { writeLines(paste("Found",dim(df)[[1]],"entries from",groupname,"with sum =",sum(df$amount))) }
  
  groupname <- "HomeEquity"
  rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), groupname, sep="/"), "rda", sep=".")
  load(rdaname)
  if ( debug ) { writeLines(paste("Found",dim(df)[[1]],"entries from",groupname,"with sum =",sum(df$amount))) }
  
  fplots <- list()
  for ( g in seq_along(groupings) ) {
    fplots[[g]] <- plotFinalGroup(groupings[[g]], "return")
  }
  
  
  qtrs <- as.Date(unique(pdata$quarter))
  nqtrs <- length(qtrs)
  
  months <- as.Date(unique(pdata$month))
  nmonths <- length(months)
  
  weeks <- as.Date(unique(pdata$week))
  nweeks <- length(weeks)
  
  days <- as.Date(unique(pdata$day))
  ndays <- length(days)
  
  ngroups <- length(groupings)
  
  quarterMatrix <- matrix(0, nrow=nqtrs, ncol=ngroups)
  rownames(quarterMatrix) <- as.numeric(qtrs)
  colnames(quarterMatrix) <- groupingnames    
  
  monthMatrix <- matrix(0, nrow=nmonths, ncol=ngroups)
  rownames(monthMatrix) <- as.numeric(months)
  colnames(monthMatrix) <- groupingnames    
  
  weekMatrix <- matrix(0, nrow=nweeks, ncol=ngroups)
  rownames(weekMatrix) <- as.numeric(weeks)
  colnames(weekMatrix) <- groupingnames
  
  doDays <- FALSE
  
  if ( doDays ) {
    dayMatrix <- matrix(0, nrow=ndays, ncol=ngroups)
    rownames(dayMatrix) <- as.numeric(days)
    colnames(dayMatrix) <- groupingnames    
  }
  
  for ( g in seq_along(groupings) ) {
    groupname <- groupings[[g]]
    
    
    ## Get organized data frames
    rdaname <- paste(paste(paste(basedir, "FinalGrouping", sep="/"), groupname, sep="/"), "rda", sep=".")
    load(rdaname)
    if ( debug ) { writeLines(paste("Found",dim(df)[[1]],"entries from",groupname)) }
    saveText(df, paste(paste("Results/Data",groupname,sep="/"),"dat", sep="."))
    
    
    ## Create quarterly data
    quarterdata <- tapply(df$amount, df$quarter, sum)
    ## Add to matrix
    quarterMatrix <- mergeMatrix(quarterMatrix, quarterdata, g)
    
    
    ## Create monthly data
    monthdata <- tapply(df$amount, df$month, sum)
    ## Add to matrix
    monthMatrix <- mergeMatrix(monthMatrix, monthdata, g)
    
    
    ## Create weekly data
    weekdata <- tapply(df$amount, df$week, sum)
    ## Add to matrix
    weekMatrix <- mergeMatrix(weekMatrix, weekdata, g)
    
    
    if ( doDays ) {
      ## Create weekly data
      daydata <- tapply(df$amount, df$day, sum)
      ## Add to matrix
      dayMatrix <- mergeMatrix(dayMatrix, daydata, g)
    }
  }
  
  ## Fix NA
  quarterMatrix[is.na(quarterMatrix)] <- 0
  monthMatrix[is.na(monthMatrix)] <- 0
  weekMatrix[is.na(weekMatrix)] <- 0
  if ( doDays ) {
    dayMatrix[is.na(dayMatrix)] <- 0
  }
  
  ## Save matrix
  appendval <- paste(version, "rda", sep=".")
  save(groupings, groupingnames, quarterMatrix, fplots, file=paste("Rda/FinalGrouping/QuarterMatrix", appendval, sep="-"))
  save(groupings, groupingnames, monthMatrix, fplots, file=paste("Rda/FinalGrouping/MonthMatrix", appendval, sep="-"))
  save(groupings, groupingnames, weekMatrix, fplots, file=paste("Rda/FinalGrouping/WeekMatrix", appendval, sep="-"))
  if ( doDays ) {
    save(groupings, groupingnames, dayMatrix, fplots, file=paste("Rda/FinalGrouping/DayMatrix.rda", appendval, sep="-"))
  }
}

writeFinalGrouping <- function() {
  rdaname <- "Rda/FinalGrouping/MonthMatrix.rda"
  writeLines(paste("Loading",rdaname))
  load(file = rdaname)
  dates <- as.Date(as.numeric(rownames(monthMatrix)))
  mdf <- as.data.frame(monthMatrix)
  mdf["Dates"] <- dates
  csvname <- gsub(".rda", ".csv", rdaname)
  writeLines(paste("Writing data to",csvname))
  write.table(mdf, sep='\t', row.names = F, file = csvname)
}


############################################################
##
##
##  Plot Everything
##
##
############################################################
plotAll <- function(version, debug = F) {
  predict <- TRUE
  
  ##
  ## Create funding matrices
  ##
  createFundingMatrices(version)
  
  ##
  ## Load Quarter Matrix
  ##
  appendval <- paste(version, "rda", sep=".")
  load(paste("Rda/FinalGrouping/QuarterMatrix", appendval, sep="-"))
  if ( doDays ) { load(paste("Rda/FinalGrouping/DayMatrix", appendval, sep="-")) }
  
  
  ##  
  ## Plots for Quarter Matrix
  ## 
  plots <- list()
  plots[[length(plots)+1]] <- ggBarPlotPrep(quarterMatrix, "Running Sum of Quarterly Activity", "sum")
  plots[[length(plots)+1]] <- ggBarPlotPrep(quarterMatrix, "Quarterly Activity", "norm")
  #plots <- c(plots, ggBarPlotPrep(monthMatrix, "Running Sum of Monthly Activity", "sum"))
  #plots <- c(plots, ggDataPlotPrep(monthMatrix, "Monthly Activity", "sum"))
  #plots <- list(plots, ggBarPlot(monthMatrix, "Monthly Activity", "norm"))
  #plots <- list(plots, ggBarPlot(weekMatrix, "Running Sum of Weekly Activity", "sum"))   
  if ( doDays ) {
    plots <- list(plots, ggBarPlot(dayMatrix, "Daily Activity", "sum"))
    plots <- list(plots, ggBarPlot(dayMatrix, "Running Sum of Daily Activity", "norm"))
  }
  #print(length(plots))
  #for ( i in seq_along(plots) ) {
  #    print(i)
  #    print(plots[[i]])
  #}
  #pause()
  
  doForecast <- TRUE
  foreplots <- list()
  fdata <- list()
  if ( doForecast ) {
    unit <- "Quarter"
    aggQuarterMatrix <- quarterMatrix
    for ( i in seq_along(colnames(aggQuarterMatrix)) ) {
      aggQuarterMatrix[,i] <- cumsum(aggQuarterMatrix[,i])
    }
    tsvals <- createTimeSeries("Quarter", aggQuarterMatrix)
    for ( i in seq_along(tsvals) ) {
      #fit <- tbats(tsvals[[i]])
      name <- groupings[[i]]
      
      
      ## Determine steps and create HW
      #endyear <- year(today()) + 30
      endyear <- 2050
      if ( name == "CollegeFund" ) { endyear <- 2035 }
      if ( name == "HomeEquity" ) { endyear <- 2035 }
      if ( name == "Transfers" ) { endyear <- 2035 }
      #steps <- getSteps("Quarter", tail(t, 1), endyear)
      steps <- 4*(endyear - year(today()))
      
      tsval <- tsvals[[i]]
      tstart <- start(tsval)
      tend <- end(tsval)
      dt <- tend - tstart
      frac <- 0.9
      training <- window(tsval, start=tstart, end=tstart + frac*dt)
      
      #fit0 <- ses(tsvals[[i]], alpha=0.1)
      fit1 <- auto.arima(tsvals[[i]])
      ##fit2 <- tbats(tsvals[[i]])
      fit <- fit1
      fcast <- forecast(fit1, h=steps, level=c(10))
      
      mdata <- combineForecast(fcast)
      if ( debug ) { writeLines("") }
      if ( debug ) { writeLines(paste("MDATA ---",name)) }
      if ( name == "HomeEquity" || name == "Transfers" ) {            
        tsval <- createTSfromMelt(unit, mdata)
        endyear <- 2050
        lastyear <- as.data.frame(end(tsval))[1,]
        steps <- 4*(endyear - lastyear)
        fit1 <- naive(tsval, h=steps)
        fcast <- forecast(fit1, h=steps, level=c(10))
        
        mdata2 <- combineForecast(fcast)
        mdata <- combineForecasts(mdata, mdata2)
      }
      fdata[[i]] <- mdata
      next
      #central <- fcast$mean
      foreplots[[length(foreplots)+1]] <- ggForecastPlot(mdata, paste("ARIMA Forecast for",name))
      
    }
    names(fdata) <- colnames(quarterMatrix)
  }
  
  
  pdfname <- paste(paste("Results/Quarters/Summary", version, sep="-"), "pdf", sep=".")
  pdf(pdfname)
  if ( debug ) { writeLines(paste("Creating",pdfname)) }
  if ( length(plots) > 0 ) {
    for ( fplot in plots ) { print(fplot) }
  }
  if ( length(fdata) > 0 ) {
    fMatrix <- plotForecast(fdata)
    print( ggBarPlotPrep(fMatrix, "Forecasted Running Sum of Monthly Activity", "norm") )
  }
  if ( length(fplots) > 0 && FALSE ) {
    for ( fplot in fplots ) { print(fplot) }
  }
  if ( length(foreplots) > 0 && FALSE ) {
    for ( fplot in foreplots ) { print(fplot) }
  }
  dev.off()
  if ( debug ) { writeLines(paste("Created",pdfname)) }
  
}


plotForecast <- function(fdata) {
  ## Split the data and merge predition and data
  
  forecastdata <- list()
  qtrs <- NULL
  for ( i in seq_along(fdata) ) { qtrs <- unique(c(qtrs, fdata[[i]]$Var1)) }
  
  qtrs <- sort(qtrs)
  groups <- names(fdata)
  ngroups <- length(groups)
  
  nqtrs <- length(qtrs)
  
  print("fcast")
  quarterMatrix <- matrix(0, nrow=nqtrs, ncol=ngroups)
  rownames(quarterMatrix) <- as.numeric(qtrs)
  colnames(quarterMatrix) <- groups
  
  for ( i in seq_along(fdata) ) {
    fitteddata <- subset(fdata[[i]], fdata[[i]]$Var2 == "Data")
    fitapply <- tapply(fitteddata$value, as.Date(as.numeric(fitteddata$Var1)), mean)
    
    forecastdata <- subset(fdata[[i]], fdata[[i]]$Var2 == "Forecast")
    foreapply <- tapply(forecastdata$value, as.Date(as.numeric(forecastdata$Var1)), mean)
    
    fapply <- c(fitapply, foreapply)
    quarterMatrix <- mergeMatrix(quarterMatrix, fapply, i)
  }
  quarterMatrix[is.na(quarterMatrix)] <- 0
  return( quarterMatrix )
  
  plots <- list()
  plots <- list(plots, ggBarPlotPrep(quarterMatrix, "Forecast of Quarterly Activity", "norm"))
  plots <- list(plots, ggDataPlotPrep(quarterMatrix, "Forecast of Quarterly Activity", "norm"))
  print(plots)
  pause()
  return( plots )
}


getSteps <- function(unit, lastdate, endyear) {
  lastdate <- as.Date(lastdate)
  
  year <- as.character(endyear)
  month <- as.character(month(lastdate))
  day <- as.character(day(lastdate))
  endyear <- as.Date(paste(year, month, day, sep="/"))
  enddate=as.Date(endyear, origin="1970-01-01")    
  ndays <- as.integer(enddate - lastdate)
  
  retval <- 0
  if ( unit == "Quarter" ) { retval <- as.integer(ndays/90) }
  else if ( unit == "Month" ) { retval <- as.integer(ndays/30) }
  else if ( unit == "Year" ) { retval <- as.integer(ndays/365) }
  else {
    writeLines(paste(unit,"was not recognized."))
    pause()
  }
  
  return( retval )
}

getrmse <- function(x,h,...)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x,start=test.start)
  fit <- Arima(train,...)
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test)["RMSE"])
}

fCastTS <- function(debug = F) {
  
  ## Change names to avoid confusion
  unit <- "Quarter"    
  rdaname <- paste(paste("Rda/FinalGrouping/",unit,"Matrix",sep=""), "rda", sep=".")
  load(rdaname) # returns quarterMatrix
  
  ## Change names to avoid confusion
  mdata <- quarterMatrix
  
  
  ##       model: A list containing information about the fitted model
  ##      method: The name of the forecasting method as a character string
  ##        mean: Point forecasts as a time series
  ##       lower: Lower limits for prediction intervals
  ##       upper: Upper limits for prediction intervals
  ##       level: The confidence values associated with the prediction
  ##              intervals
  ##           x: The original time series
  ##              (either object itself or the time series used to
  ##               create the model stored as object).
  ##   residuals: Residuals from the fitted model. For models with additive
  ##          errors, the residuals will be x minus the fitted values.
  ##      fitted: Fitted values (one-step forecasts)
  
  ##data.hw$fitted     data.hw$mean       data.hw$residuals  
  ##data.hw$level      data.hw$method     data.hw$upper      
  ##data.hw$lower      data.hw$model      data.hw$x
  
  
  if ( debug ) { writeLines("Creating HW forecast") }
  
  
  ## Get time
  t <- as.Date(as.numeric(rownames(mdata)))
  
  
  ## Get groupings
  groupings <- colnames(mdata)
  
  tsvals <- createTimeSeries(unit, mdata)
  save(tsvals, file="Rda/TimeSeries/example.rda")
  pause()
  
  for ( i in seq_along(groupings) ) {
    nicegroupname <- groupings[[i]]
    
    ##
    ## Check for each case ##
    ##
    accounttype <- ""
    if ( str_count(nicegroupname, "Savings") > 0 ) { accounttype <- "Savings" }
    else if ( str_count(nicegroupname, "Home Equity") > 0 ) { accounttype <- "Mortgage" }
    else if ( str_count(nicegroupname, "College Fund") > 0 ) { accounttype <- "CollegeFund" }
    else if ( str_count(nicegroupname, "Income") > 0 ) { accounttype <- "Income" }
    else if ( str_count(nicegroupname, "Expenses") > 0 ) { accounttype <- "Expenses" }
    else if ( str_count(nicegroupname, "Transfers") > 0 ) { accounttype <- "Transfers" }
    else { writeLines(paste("Could not find account type for",nicegroupname)) }
    
    groupdata <- mdata[,i]
    if ( debug ) { print(paste(nicegroupname,sum(groupdata))) }
    
    
    
    
    ## Create zoo object from group data
    groupdata.zoo <- zoo(groupdata, order.by=t)
    
    ## Create timeseries object from zoo object
    groupdata.ts <- as.ts(groupdata.zoo)
    
    ## Remove NAs
    groupdata.ts <- na.remove(groupdata.ts)
    
    #save(groupdata.ts, file="Rda/TimeSeries/example.rda")
    
    ##
    ##
    ##
    ## Average of all data (mean)
    fcast.meanf <- meanf(groupdata.ts, h=20)
    
    ## Uses last data point
    fcast.naive <- naive(groupdata.ts, h=20)
    
    ## Seasonal naive
    fcast.snaive <- snaive(groupdata.ts, h=20)
    
    ## Drift
    fcast.drift <- rwf(groupdata.ts, drift=TRUE, h=20)
    
    ## Create training/test samples
    tstart <- start(groupdata.ts)
    tend <- end(groupdata.ts)
    dt <- tend - tstart
    frac <- 0.8
    training <- window(groupdata.ts, start=tstart, end=tstart + frac*dt)
    testing <- window(groupdata.ts, start=tstart + frac*dt, end=tend)
    
    
    ## Get fit
    fit <- stl(groupdata.ts, s.window=100)
    groupdata.ts.sa <- seasadj(fit)
    lines(groupdata.ts.sa, col="orange")
    
    
    ## More decomp
    fit <- stl(groupdata.ts, t.window=15, s.window="periodic", robust=TRUE)
    
    
    ## Exponential smoothing (alpha has usual meaning)
    fit <- ses(groupdata.ts, alpha=0.1, initial="simple", h=10)
    
    
    ## Holt
    fit <- holt(groupdata.ts)
    closure <- fitted(fit)
    
    
    ## Holt w/ exponential (must have positive data)
    fit <- holt(groupdata.ts, exponential=TRUE)
    closure <- fitted(fit)
    
    
    ## Holt w/ damping (must have positive data)
    fit <- holt(groupdata.ts, dampled=TRUE)
    closure <- fitted(fit)
    
    ## Lag plot
    lag.plot(groupdata.ts, lags=4)
    
    ## Decompose/Seasonal info
    fcast.stl <- stl(groupdata.ts)
    fcast.decompose <- decompose(groupdata.ts)
    
    ## Seasonal adjusted
    seasadj(fcast.stl)
    seasadj(fcast.decompose)
    
    
    ## ETS
    ##ets(y, model="ZZZ", damped=NULL, alpha=NULL,
    ##    beta=NULL, gamma=NULL, phi=NULL,
    ##    additive.only=FALSE,
    ##    lower=c(rep(0.0001,3),0.80),
    ##    upper=c(rep(0.9999,3),0.98),
    ##    opt.crit=c("lik","amse","mse","sigma"), nmse=3,
    ##    bounds=c("both","usual","admissible"),
    ##    ic=c("aic","aicc","bic"), restrict=TRUE)
    fit <- ets(groupdata.ts)
    fit <- ets(groupdata.ts, model="AAA", damped=FALSE)
    
    
    ## Predict/Forecast
    fcast <- forecast(fit, level=c(50, 80, 95))
    #plot(fcast)
    #plot(forecast(groupdata.ts)) #This also works although selection is automatic
    
    
    ## Transformations
    ##   yt =  { exp(w_{t}) for l = 0;
    ##         { (lw_{t} + 1)^{1/l) for l != 0.
    groupdata.ts.bc <- BoxCox(groupdata.ts, lambda=0.1)
    
    
    ## Different/Stationary data
    groupdata.ts.diff <- diff(groupdata.ts)
    groupdata.ts.diff2 <- diff(diff(groupdata.ts))
    
    
    ##
    ## ARIMA --> (Autoregressive Integrated Moving Average)
    ## ARIMA(p, d, q)
    ##  p -> order of the autoregressive part
    ##  d -> degree of first differencing involved
    ##  q -> order of the moving average part
    ##
    ## ARIMA(0,0,0): White noise model
    ## ARIMA(0,1,0): Random walk
    ## ARIMA(p,0,0): AR(p)
    ## ARIMA(0,0,q): MA(q)
    ##
    fit <- auto.arima(groupdata.ts)
    fit <- auto.arima(groupdata.ts, seasonal=FALSE)
    fcast <- forecast(fit)
    
    ## ARIMA output
    ## c=0,  d=0 -> long-term forecasts will go to zero
    ## c=0,  d=1 -> long-term forecasts will go to non-zero constant
    ## c=0,  d=2 -> long-term forecasts will follow a straight line
    ## c!=0, d=0 -> long-term forecasts will go to mean of data
    ## c!=0, d=1 -> long-term forecasts will follow a straight line
    ## c!=0, d=2 -> long-term forecasts will follow a quadratic trend
    
    
    ## ACF for white noise
    Acf(residuals(fit))
    
    
    ## Fourier terms for cyclical data
    fdata <- fourier(groupdata.ts, K=2)
    
    
    ## More ARIMA
    ## Arima(x, order=c(0,0,0), seasonal=c(0,0,0),
    ##       xreg=NULL, include.mean=TRUE, include.drift=FALSE, 
    ##       include.constant, lambda=model$lambda, transform.pars=TRUE, 
    ##       fixed=NULL, init=NULL, method=c("CSS-ML","ML","CSS"), n.cond, 
    ##       optim.control=list(), kappa=1e6, model=NULL)
    ##  x: a univariate time series of class ts.
    ##  order: A specification of the non-seasonal part of the ARIMA model:
    ##         the three components (p, d, q) are the AR order, the degree
    ##         of differencing, and the MA order.
    ##  xreg: external data (regressors)
    
    
    ## TBATS()
    ## Trigonometric terms for seasonality
    ## Box-Cox transformations for heterogeneity
    ## ARMA errors for short-term dynamics
    ## Trend (possibly damped)
    ## Seasonal (including multiple and
    fit <- tbats(groupdata.ts)
    fcast <- forecast(fit)
    plot(fcast)
    
    
    ## Determine steps and create HW
    steps <- getSteps(unit, tail(t, 1), 2028)
  }
  pause()
  
  
  
  ## Create data
  mtdata <- t(mdata)
  x <- as.Date(as.numeric(colnames(mtdata)))
  catnames <- rownames(mtdata)
  
  
  ## Merge all columns and sum (one entry per date)
  sumdata <- colSums(mtdata)
  print(sumdata)
  pause()
  
  
  ## Create Zoo
  data.zoo <- zoo(sumdata, order.by=x)
  
  ## Create ts
  data.ts <- as.ts(data.zoo)
  
  ## Remove NAs
  data.ts <- na.remove(data.ts)
  steps <- 100
  data.hw <- hw(data.ts, h=steps)
  
  ## Start/End
  data.start <- start(data.hw$fitted)
  data.end <- end(data.hw$fitted)
  
  
  setMargins()
  if ( debug ) { print("  Creating HW forecast plot.") }
  plot(data.hw, axes=FALSE, main="")
  box()
  
  if ( debug ) { print("  Filling axis info.") }
  
  ## axis information    
  xticks <- axTicks(1)
  startdate <- as.Date(min(xticks)-180)
  enddate <- as.Date(max(xticks)+180)
  atdates <- seq(from=startdate, to=enddate, by="year")
  labeldates <- format(atdates, "%Y")
  atdates <- as.numeric(atdates)
  
  
  ## y-axis (right) w/ amount
  yaxis <- prettyYaxis()
  axis(2, at=yaxis[[1]], labels=yaxis[[2]], col='dodgerblue', cex.axis=0.85)
  mtext("Running Sum ($)", 2, line=2.5, col='dodgerblue', cex=1.25)
  axis(4, at=yaxis[[1]], labels=yaxis[[2]], col='darkorange', cex.axis=0.85)
  mtext("Running Sum ($)", 4, line=2.5, col='darkorange', cex=1.25)
  
  title <- paste(name, "Data + Forecast", sep=" ")
  mtext(title, 3, cex=1.5, line=1.0, col='darkblue')
  
  
  #axis(1, at=atdates, labels=labeldates)
  axis(1, at=atdates, labels=labeldates, las=3, tck=-0.015)
  
  
  ## ETS
  doETA <- FALSE
  if ( doETA ) {
    data.etafit <- ets(data.ts, model="ZMA", damped=F)
    data.fcast <- forecast(data.etafit, h=30)
    plot(data.fcast, axes=FALSE)
    box()
    axis(2)
    axis(1, at=atdates, labels=labeldates)
  }
  
  ## ARIMA
  doARIMA <- FALSE
  if ( doARIMA ) {
    data.arima <- auto.arima(data.ts)
    data.afcast <- forecast(data.arima, h=30)
    plot(data.fcast, axes=FALSE)
    box()
    axis(2)
    axis(1, at=atdates, labels=labeldates)
  }
  
  
  if ( debug ) { print("  Done with HW forecast.") }
}



get <- function() {
  ##
  ## Create data frame/matrix
  ##
  sdata <- rbind(sdata, catdata)
  sdf[[j]] <- catdata
  
  
  ##
  ## Copy quarter data
  ##
  if ( debug ) { print(paste("  Merging matrices for", indivcatname)) }
  quarterdata[[j]] <- tapply(sdf[[j]]$amount, sdf[[j]]$quarter, sum)
  quartermatrix <- mergeMatrix(quartermatrix, quarterdata[[j]], j)
  if ( testAllNA(quartermatrix[,j]) ) {
    print("Column of matrix is all NA!")
    print(quartermatrix[,j])
    pause()
  }
  if ( debug ) { print(paste("  Created matrices for", indivcatname)) }
  
  
  if ( savetable || saveplots ) {
    indivquartermatrix <- as.matrix(quartermatrix[,j])
    indivquartermatrix[is.na(indivquartermatrix)] <- 0
    if ( !testMatrix(indivquartermatrix) ) {
      print(indivquartermatrix)
      quit(save='y')
    }
    colnames(indivquartermatrix) <- indivcatname
    pdfname <- paste("Results/Quarters/Indiv", indivcatname, sep="/")
    pdfname <- paste(pdfname, "pdf", sep=".")
    if ( debug ) { print(paste("Creating plots in", pdfname)) }
    pdf(pdfname)
    
    if ( saveplots ) {
      baroverlay(indivquartermatrix, indivcatname, debug, "quarters")
      baroverlay(indivquartermatrix, indivcatname, debug, "quarters", "sum")
      plotoverlay(indivquartermatrix, indivcatname, debug)
      if ( predict ) {
        HWforecast(indivquartermatrix, indivcatname, debug)
      }
      dotsummary(catdata, indivcatname, debug)
      
    }
    
    if ( savetable ) {
      if ( debug ) { print(paste(" Printing data frame in", pdfname)) }
      savedf(catdata, pdfname)
    }
    dev.off()
  }
}



############################################################
##
##
##  Make Group data
##
##
############################################################
makeGroups <- function(groups) {
  ##
  ## Group Data
  ##
  totalsize <- 0
  supergroups <- list()
  
  for ( i in seq_along(groups) ) {
    groupname <- groups[[i]]
    groupdata <- subset(x = pdata, pdata$group == groupname)
    rdaname <- paste(paste(paste(basedir, "Groups", sep="/"), groupname, sep="/"), "rda", sep=".")
    size <- dim(groupdata)[[1]]
    if ( size == 0 ) { next }
    totalsize <- totalsize + size    
    save(groupdata, file=rdaname)
    sumval <- paste(paste(paste("Saving",rdaname), "w/ size ="), size)
    writeLines(sumval)
    
    
    supergroup <- unique(groupdata$supergroup)
    #if ( !supergroup %in% supergroups ) {
    #    supergroups[[supergroup]] <- list()
    #}
    supergroups[[supergroup]] <- c(supergroups[[supergroup]], groupname)
  }
  
  rdaname <- paste(paste(paste(basedir, "Groups", sep="/"), "SuperGroups", sep="/"), "rda", sep=".")
  save(supergroups, file=rdaname)
  pause()
  
  sumval <- paste(paste("Wrote",totalsize),"group rows")
  writeLines(sumval)
  sumval <- paste(paste("Originally",total),"rows")
  writeLines(sumval)
}


makeSuperGroups <- function(debug = F) {
  writeLines("Making all super groups")
  
  ##
  ## Check that pdata exists
  ## 
  if ( !exists("pdata") ) {
    writeLines("pdata is not defined in makeSuperGroups()")
    return
  }
  
  
  ##
  ## Group Data
  ##
  total <- dim(pdata)[[1]]
  totalsize <- 0
  supergroups <- list()
  supergroups <- sort(unique(pdata$supergroup))
  
  for ( i in seq_along(supergroups)) {
    supergroupname <- supergroups[[i]]
    supergroupdata <- subset(x = pdata, pdata$supergroup == supergroupname)
    rdaname <- paste(paste(paste(basedir, "SuperGroups", sep="/"), supergroupname, sep="/"), "rda", sep=".")
    size <- dim(supergroupdata)[[1]]
    if ( size == 0 ) { next }
    totalsize <- totalsize + size    
    save(supergroupdata, file=rdaname)
    if ( debug ) { 
      sumval <- paste(paste(paste("Saving",rdaname), "w/ size ="), size)
      writeLines(sumval)
    }
  }
  if ( debug ) { 
    sumval <- paste(paste("Wrote",totalsize),"group rows")
    writeLines(sumval)
    sumval <- paste(paste("Originally",total),"rows")
    writeLines(sumval)
  }
}

checkMatches <- function(df1, df2) {
  flog.info(paste("Checking for matching from data frames with",nrow(df1),"and",nrow(df2),"rows."))
  transfers <- df1[,"match"]
  transfers <- transfers[transfers != 0]
  matches1  <- df2[,"id"]
  drop1     <- match(transfers, matches1)
  testdf1   <- df2[-drop1,]
  
  transfers <- df2[,"match"]
  transfers <- transfers[transfers != 0]
  matches2  <- df1[,"id"]
  drop2     <- match(transfers, matches2)
  testdf2   <- df1[-drop2,]
  
  retval <- list()
  retval[["drop1"]] <- matches1[drop1]
  retval[["drop2"]] <- matches2[drop2]
  if ( nrow(testdf1) == nrow(testdf2) ) {
    flog.info("Found equal matching transfers. Returning their hash values.")
    return( retval )
  }
  flog.info("There was a mismatch in maching transfers. Returning NULL.")
  return( NULL )
}



runAll <- function() {
  ## Step 0 is to source other R files
  source("R/intro.R")
  load(file = "Perm/HistgroupMap.rData")
  
  ## Step 1 is to make SuperGroups
  makeSuperGroups()
  
  ## Step 2 is to make FinalGrouping
  makeFinalGrouping()
  
  ## Step 3 is to make Histogram Grouping
  makeHistogramGrouping()
  
  ## Step 4 is to plot it all
  for ( i in seq(4) ) { plotAll(i) }
  plotHists()
}

#runAll()
#makeAccounts(accounts)
#plotAccounts(accounts)
#plotAccount("Credit")

#makeGroups(groups)
#plotGroups("VanguardThomas")
#plotGroups("VanguardRuth")
#plotGroups("Work")
#plotGroups(groups)

#plotSuperGroups()

