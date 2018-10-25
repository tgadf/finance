#######################################################################################################
#
# Loading Data Function. Result is common data.frame: records
#
#######################################################################################################
loadData <- function(infile = "Portfolio/records.csv", reload = F) {
  
  test <- get0("records")
  if ( !(is.null(test)) & is.data.frame(test) ) {
    if ( reload == F ) {
      flog.info("Money records already loaded. Returning.")
      return(NULL)
    }
  }
  
  filename <- file.path(getwd(), infile)
  if ( !file.exists(filename)) {
    flog.error(paste("Could not load",filename))
    stop()
  }
  
  records <- read.csv(infile, header = T)
  flog.info(paste("Loaded money records with size:",getDimStr(records)))
  flog.debug("Assigning records to variable records in the global environment")
  assign("records", records, envir = .GlobalEnv)
  setDateInformation()
  return(NULL)
}

setDateInformation <- function() {
  flog.info("Setting R Data Level Information")
  retval <- loadData()
  require(lubridate)
  require(zoo)
  
  records[,"Date"]    <- as.Date(records[,"date"], format="%m/%d/%Y")
  records[,"Day"]     <- wday(records[,"Date"], label=T)
  records[,"Week"]    <- week(records[,"Date"])
  records[,"Year"]    <- year(records[,"Date"])
  records[,"Month"]   <- as.yearmon(records[,"Date"])
  records[,"Quarter"] <- as.yearqtr(records[,"Date"])
  if ( isColumn("year", records) )  { records[,"year"]    <- NULL }
  if ( isColumn("day", records) )   { records[,"day"]     <- NULL }
  if ( isColumn("month", records) ) { records[,"month"]   <- NULL }
  
  flog.info(paste("Set date information. Records now has size:",getDimStr(records)))
  
  assign("records", records, envir = .GlobalEnv)
}

collapseTransfers <- function() {
  flog.info("Collapsing Transfers from Records")
  retval <- loadData()

  final <- records
  total <- sum(final[,"amount"])
  transfers <- NULL
  accountData <- list()
  
  ## 0) Flip Credit
  creditIDs <- which(final[,"account"] == "Credit")
  final[creditIDs,"amount"] <- -1*final[creditIDs,"amount"]
  runningTotal <- sum(final[,"amount"])
  runningTransfersTotal <- 0.0
  flog.info(paste("Sum:",runningTotal,"  Transfers:",runningTransfersTotal,"  Original:",total,". After flipping credit amount sign."))
  assign("final", final, envir = .GlobalEnv)

  
  ## 1) Credit Card Payments/Receipts
  flog.info(paste("Fixing Chase/Credit Account"))
  credit <- subset(final, account=="Credit" & category=="CreditCardReceipt", select = c("id", "match", "amount"))
  chase  <- subset(final, account=="Chase" & category=="CreditCardPayment", select = c("id", "match", "amount"))
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
  flog.info(paste("Sum:",runningTotal,"  Transfers:",runningTransfersTotal,"  Original:",total,". After Chase/Credit Fixes."))
  assign("final", final, envir = .GlobalEnv)
  assign("accountData", accountData, envir = .GlobalEnv)
  
  ##
  ## 2) Claire NY 529 Payment/Receipts
  ##
  flog.info(paste("Fixing Claire's 529 Account"))
  claireNY1 <- subset(final, account=="529ClaireNY" & category=="Claire529NYReceipt", select = c("id", "match", "amount"))
  claireNY2 <- subset(final, account=="529ClaireNY" & category=="Claire529NYGiftReceipt", select = c("id", "match", "amount"))
  claireNY  <- rbind(claireNY1,claireNY2)
  chase    <- subset(final, account=="Chase" & category=="Claire529NYPayment", select = c("id", "match", "amount"))
  flog.info(paste("Found 529 account with",nrow(claireNY),"entries and total $",sum(claireNY[,"amount"])))
  flog.info(paste("Found chase account with",nrow(chase),"entries and total $",sum(chase[,"amount"])))
  if ( nrow(claireNY) != nrow(chase) ) { flog.warn("The number of rows are not equal. Could be a problem.") }
  retval <- checkMatches(df1 = chase, df2 = claireNY)
  if ( !is.null(retval) ) {
    drop1 <- match(retval[["drop1"]], final[,"id"])
    drop2 <- match(retval[["drop2"]], final[,"id"])
    if ( is.null(transfers) ) { transfers <- final[drop2,] }
    else                      { transfers <- rbind(transfers, final[drop2,]) }
    accountData[["529ClaireNY"]] <- final[drop1,]
    final <- final[-c(drop1,drop2),]
  }
  flog.info(paste("After dropping Claire's 529 payment records there are now",nrow(final),"/",nrow(records),"entries."))
  runningTotal         <- sum(final[,"amount"])
  runningTransfersTotal <- sum(transfers[,"amount"])
  flog.info(paste("Sum:",runningTotal,"  Transfers:",runningTransfersTotal,"  Original:",total,". After Claire's 529 Fixes."))
  assign("final", final, envir = .GlobalEnv)
  assign("transfers", transfers, envir = .GlobalEnv)
  assign("accountData", accountData, envir = .GlobalEnv)
  
  
  ##
  ## 3) Claire IL 529 Payment/Receipts
  ##
  flog.info(paste("Fixing Claire's 529 Account"))
  claireIL1 <- subset(final, account=="529ClaireIL" & category=="Claire529ILReceipt", select = c("id", "match", "amount"))
  claireIL2 <- subset(final, account=="529ClaireIL" & category=="Claire529ILGiftReceipt", select = c("id", "match", "amount"))
  claireIL  <- rbind(claireIL1,claireIL2)
  chase    <- subset(final, account=="Chase" & category=="Claire529ILPayment", select = c("id", "match", "amount"))
  flog.info(paste("Found 529 account with",nrow(claireIL),"entries and total $",sum(claireIL[,"amount"])))
  flog.info(paste("Found chase account with",nrow(chase),"entries and total $",sum(chase[,"amount"])))
  if ( nrow(claireIL) != nrow(chase) ) { flog.warn("The number of rows are not equal. Could be a problem.") }
  retval <- checkMatches(df1 = chase, df2 = claireIL)
  if ( !is.null(retval) ) {
    drop1 <- match(retval[["drop1"]], final[,"id"])
    drop2 <- match(retval[["drop2"]], final[,"id"])
    if ( is.null(transfers) ) { transfers <- final[drop2,] }
    else                      { transfers <- rbind(transfers, final[drop2,]) }
    accountData[["529ClaireIL"]] <- final[drop1,]
    final <- final[-c(drop1,drop2),]
  }
  flog.info(paste("After dropping Claire's 529 payment records there are now",nrow(final),"/",nrow(records),"entries."))
  runningTotal         <- sum(final[,"amount"])
  runningTransfersTotal <- sum(transfers[,"amount"])
  flog.info(paste("Sum:",runningTotal,"  Transfers:",runningTransfersTotal,"  Original:",total,". After Claire's 529 Fixes."))
  assign("final", final, envir = .GlobalEnv)
  assign("transfers", transfers, envir = .GlobalEnv)
  assign("accountData", accountData, envir = .GlobalEnv)
  
  
  ##
  ## 4) Charlie IL 529 Payment/Receipts
  ##
  flog.info(paste("Fixing Charlie's 529 Account"))
  CharlieIL1 <- subset(final, account=="529CharlieIL" & category=="Charlie529ILReceipt", select = c("id", "match", "amount"))
  CharlieIL2 <- subset(final, account=="529CharlieIL" & category=="Charlie529ILGiftReceipt", select = c("id", "match", "amount"))
  CharlieIL  <- rbind(CharlieIL1,CharlieIL2)
  chase    <- subset(final, account=="Chase" & category=="Charlie529ILPayment", select = c("id", "match", "amount"))
  flog.info(paste("Found 529 account with",nrow(CharlieIL),"entries and total $",sum(CharlieIL[,"amount"])))
  flog.info(paste("Found chase account with",nrow(chase),"entries and total $",sum(chase[,"amount"])))
  if ( nrow(CharlieIL) != nrow(chase) ) { flog.warn("The number of rows are not equal. Could be a problem.") }
  retval <- checkMatches(df1 = chase, df2 = CharlieIL)
  if ( !is.null(retval) ) {
    drop1 <- match(retval[["drop1"]], final[,"id"])
    drop2 <- match(retval[["drop2"]], final[,"id"])
    if ( is.null(transfers) ) { transfers <- final[drop2,] }
    else                      { transfers <- rbind(transfers, final[drop2,]) }
    accountData[["529CharlieIL"]] <- final[drop1,]
    final <- final[-c(drop1,drop2),]
  }
  flog.info(paste("After dropping Charlie's 529 payment records there are now",nrow(final),"/",nrow(records),"entries."))
  runningTotal         <- sum(final[,"amount"])
  runningTransfersTotal <- sum(transfers[,"amount"])
  flog.info(paste("Sum:",runningTotal,"  Transfers:",runningTransfersTotal,"  Original:",total,". After Charlie's 529 Fixes."))
  assign("final", final, envir = .GlobalEnv)
  assign("transfers", transfers, envir = .GlobalEnv)
  assign("accountData", accountData, envir = .GlobalEnv)
  
  
  ##
  ## 5) HSBC Savings
  ##
  flog.info(paste("Fixing HSBC Account"))
  HSBC     <- subset(final, account=="HSBC" & category=="HSBCSavingsReceipt", select = c("id", "match", "amount","category","date"))
  chase    <- subset(final, account=="Chase" & category=="HSBCSavingsPayment", select = c("id", "match", "amount","category","date"))
  flog.info(paste("Found HSBC account with",nrow(HSBC),"entries and total $",sum(HSBC[,"amount"])))
  flog.info(paste("Found chase account with",nrow(chase),"entries and total $",sum(chase[,"amount"])))
  if ( nrow(HSBC) != nrow(chase) ) { flog.warn("The number of rows are not equal. Could be a problem.") }
  retval <- checkMatches(df1 = chase, df2 = HSBC)
  if ( !is.null(retval) ) {
    drop1 <- match(retval[["drop1"]], final[,"id"])
    drop2 <- match(retval[["drop2"]], final[,"id"])
    if ( is.null(transfers) ) { transfers <- final[drop2,] }
    else                      { transfers <- rbind(transfers, final[drop2,]) }
    accountData[["HSBCSavings"]] <- final[drop1,]
    final <- final[-c(drop1,drop2),]
  }
  HSBC     <- subset(final, account=="HSBC" & category=="HSBCSavingsPayment", select = c("id", "match", "amount","category","date"))
  chase    <- subset(final, account=="Chase" & category=="HSBCSavingsReceipt", select = c("id", "match", "amount","category","date"))
  flog.info(paste("Found HSBC account with",nrow(HSBC),"entries and total $",sum(HSBC[,"amount"])))
  flog.info(paste("Found chase account with",nrow(chase),"entries and total $",sum(chase[,"amount"])))
  if ( nrow(HSBC) != nrow(chase) ) { flog.warn("The number of rows are not equal. Could be a problem.") }
  retval <- checkMatches(df1 = chase, df2 = HSBC)
  if ( !is.null(retval) ) {
    drop1 <- match(retval[["drop1"]], final[,"id"])
    drop2 <- match(retval[["drop2"]], final[,"id"])
    if ( is.null(transfers) ) { transfers <- final[drop2,] }
    else                      { transfers <- rbind(transfers, final[drop2,]) }
    accountData[["HSBCSavings"]] <- rbind(accountData[["HSBCSavings"]], final[drop1,])
    final <- final[-c(drop1,drop2),]
  }
  flog.info(paste("After dropping HSBC payment records there are now",nrow(final),"/",nrow(records),"entries."))
  runningTotal          <- sum(final[,"amount"])
  runningTransfersTotal <- sum(transfers[,"amount"])
  flog.info(paste("Sum:",runningTotal,"  Transfers:",runningTransfersTotal,"  Original:",total,". After HSBC Fixes."))
  assign("final", final, envir = .GlobalEnv)
  assign("transfers", transfers, envir = .GlobalEnv)
  assign("accountData", accountData, envir = .GlobalEnv)
  
  
  ##
  ## 6) Thomas Vanguard Payment/Receipts
  ##
  flog.info(paste("Fixing Thomas' Vanguard Account"))
  Vanguard  <- subset(final, account=="VanguardRothIRAThomas" & category=="VanguardRothIRAThomasContributionReceipt", select = c("id", "match", "amount"))
  Vanguard2 <- subset(final, account=="VanguardRetirementThomas" & category=="VanguardRetirementThomasContributionReceipt", select = c("id", "match", "amount"))
  Vanguard  <- rbind(Vanguard1,Vanguard2)
  chase1    <- subset(final, account=="Chase" & category=="VanguardRothIRAThomasContributionPayment", select = c("id", "match", "amount"))
  chase2    <- subset(final, account=="Chase" & category=="VanguardRetirementThomasContributionPayment", select = c("id", "match", "amount"))
  chase     <- rbind(chase1,chase2)
  flog.info(paste("Found Vanguard account with",nrow(Vanguard),"entries and total $",sum(Vanguard[,"amount"])))
  flog.info(paste("Found chase account with",nrow(chase),"entries and total $",sum(chase[,"amount"])))
  if ( nrow(Vanguard) != nrow(chase) ) { flog.warn("The number of rows are not equal. Could be a problem.") }
  retval <- checkMatches(df1 = chase, df2 = Vanguard)
  if ( !is.null(retval) ) {
    drop1 <- match(retval[["drop1"]], final[,"id"])
    drop2 <- match(retval[["drop2"]], final[,"id"])
    if ( is.null(transfers) ) { transfers <- final[drop2,] }
    else                      { transfers <- rbind(transfers, final[drop2,]) }
    accountData[["VanguardThomas"]] <- final[drop1,]
    final <- final[-c(drop1,drop2),]
  }
  flog.info(paste("After dropping Charlie's 529 payment records there are now",nrow(final),"/",nrow(records),"entries."))
  runningTotal         <- sum(final[,"amount"])
  runningTransfersTotal <- sum(transfers[,"amount"])
  flog.info(paste("Sum:",runningTotal,"  Transfers:",runningTransfersTotal,"  Original:",total,". After Charlie's 529 Fixes."))
  assign("final", final, envir = .GlobalEnv)
  assign("transfers", transfers, envir = .GlobalEnv)
  assign("accountData", accountData, envir = .GlobalEnv)
  
  
}