
findPaymentsFromDate <- function(date, dT, fulldf) {
  date <- as.Date(date)
  tmp <- fulldf[which(fulldf$time < date + dT & fulldf$time > date - dT),]
  showSlimDF(tmp)
  writeLines("")
  writeLines("")
}

findDates <- function(ddf, dT, fulldf) {
  dates <- ddf[,"time"]
  for ( i in seq_len(nrow(ddf)) ) {
    writeLines("=================================================")
    #showSlimDF(ddf[i,])
    date <- ddf[i,"time"]
    tmp <- fulldf[which(fulldf$time < date + dT & fulldf$time > date - dT),]
    testrowname <- rownames(ddf[i,])
    tmp2 <- tmp[rownames(tmp) != testrowname,]
    writeLines(paste("  Found maching entries:",nrow(tmp2)))
    showMatchingDF(ddf[i,],tmp2)
    writeLines("")
    writeLines("")
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

findTransferMatch <- function(expcf, reccf) {
  debug <- F
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
        writeLines("\nCould not find a match for the following item. Also showing items +/- 10 days")
        df1 <- expcf[i,]
        showSlimDF(df1, summary = F)
        findDates(df1, dT = 10, fulldf = reccf)
        f()
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

showSlimDF <- function(df, dfname="data.frame", summary=T) {
  if ( summary ) { writeLines(paste("Data Frame:",dfname)) }
  tmp <- df[c("time","category","supergroup","account","payee","amount","transferrow")] 
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
}

load(file = "fulldf.rData")

########################################################################################################################
#
# Connect the transfers
#
########################################################################################################################


histdf <- list()
for ( histval in unique(fulldf[["histgroup"]]) ) {
  histdf[[histval]] <- fulldf[which(fulldf[["histgroup"]] == histval),]
}
histdata <- tapply(fulldf$amount, INDEX = fulldf$histgroup, FUN = sum)


#331a7b63 2011-07-01 VanguardThomasContributionReceipt    Savings VanguardThomas Vanguard  416.66        <NA>
#817b4a58 2011-07-08   VanguardRuthContributionReceipt    Savings   VanguardRuth Vanguard  416.66    5f275f50
#64dfaa4f 2011-08-03 VanguardThomasContributionReceipt    Savings VanguardThomas Vanguard  416.66    3b6f6547


#5f275f50 2011-07-05 VanguardThomasContributionPayment  Transfers   Chase  IRAPayment  -416.66    817b4a58
#9c82e826 2011-07-11   VanguardRuthContributionPayment  Transfers   Chase  IRAPayment  -416.66        <NA>
#3b6f6547 2011-08-04 VanguardThomasContributionPayment  Transfers   Chase  IRAPayment  -416.66    64dfaa4f

##
## Credit Card
##
## "Expenses - Credit" <-> "Receipt - Credit"
##




writeLines("Match Savings Transfers")
retvals <- findTransferMatch(histdf[["Expenses - Savings"]], histdf[["Receipt - Savings"]])
histdf[["Expenses - Savings"]] <- retvals[["expcf"]]
histdf[["Receipt - Savings"]] <- retvals[["reccf"]]
df1 <- histdf[["Expenses - Savings"]]
mdf1 <- df1[which(is.na(df1[,"transferrow"])),]
df2 <- histdf[["Receipt - Savings"]]
mdf2 <- df2[which(is.na(df2[,"transferrow"])),]
f()


writeLines("Match CollegeFund Transfers")
retvals <- findTransferMatch(histdf[["Expenses - CollegeFund"]], histdf[["Receipt - CollegeFund"]])
histdf[["Expenses - CollegeFund"]] <- retvals[["expcf"]]
histdf[["Receipt - CollegeFund"]] <- retvals[["reccf"]]
df1 <- histdf[["Expenses - CollegeFund"]]
mdf1 <- df1[which(is.na(df1[,"transferrow"])),]
df2 <- histdf[["Receipt - CollegeFund"]]
mdf2 <- df2[which(is.na(df2[,"transferrow"])),]


writeLines("Match House Transfers")
retvals <- findTransferMatch(histdf[["Expenses - House"]], histdf[["Receipt - House"]])
histdf[["Expenses - House"]] <- retvals[["expcf"]]
histdf[["Receipt - House"]] <- retvals[["reccf"]]
df1 <- histdf[["Expenses - House"]]
mdf1 <- df1[which(is.na(df1[,"transferrow"])),]
df2 <- histdf[["Receipt - House"]]
mdf2 <- df2[which(is.na(df2[,"transferrow"])),]


writeLines("Match HomeEquity Transfers")
retvals <- findTransferMatch(histdf[["Expenses - HomeEquity"]], histdf[["Receipt - HomeEquity"]])
histdf[["Expenses - HomeEquity"]] <- retvals[["expcf"]]
histdf[["Receipt - HomeEquity"]] <- retvals[["reccf"]]
df1 <- histdf[["Expenses - HomeEquity"]]
mdf1 <- df1[which(is.na(df1[,"transferrow"])),]
df2 <- histdf[["Receipt - HomeEquity"]]
mdf2 <- df2[which(is.na(df2[,"transferrow"])),]


writeLines("Match Credit Transfers")
retvals <- findTransferMatch(histdf[["Expenses - Credit"]], histdf[["Receipt - Credit"]])
histdf[["Expenses - Credit"]] <- retvals[["expcf"]]
histdf[["Receipt - Credit"]] <- retvals[["reccf"]]
df1 <- histdf[["Expenses - Credit"]]
mdf1 <- df1[which(is.na(df1[,"transferrow"])),]
df2 <- histdf[["Receipt - Credit"]]
mdf2 <- df2[which(is.na(df2[,"transferrow"])),]



writeLines("Combining histdfs back into fulldf")
fulldf <- histdf[[names(histdf)[[1]]]]
for ( d in 2:length(histdf) ) {
  fulldf <- rbind(fulldf, histdf[[names(histdf)[[d]]]])
}
fulldf <- fulldf[order(fulldf[,"time"]),]