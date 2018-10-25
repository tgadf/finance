account <- "Credit"
#account <- "Chase"
tmp <- read.csv(paste(account, "/old/", account, "_2000.01.01--2012.08.06.csv.formatcategory", sep = ""), header = F)
names(tmp) <- c("Row", "Date", "Amount", "Category", "Payee", "Entry", "ID", "Key", "END")
tmp <- tmp[c("Date", "Amount", "Payee")]

x <- tmp[1,]
x[,"Payee"] = "Opening Balance"
x[,"Amount"] = 0.0
tmp <- rbind(x, tmp)
tmp$time <- mdy(tmp[,"Date"], tz = "America/Chicago") # w/ timezone
tmp[1,"time"] <- tmp[1,"time"] - 1

## group by week,month
## group by quarter
#tmp$time <- as.Date(tmp$Date, format="%m/%d/%Y")
#tmp$day <- floor_date(tmp$time, unit = c("day"))
#tmp$week <- floor_date(tmp$time, unit = c("week"))
#tmp$month <- floor_date(tmp$time, unit = c("month"))
#tmp$year <- floor_date(tmp$time, unit = c("year"))
#tmp$quarter <- mkQtrs(tmp$time)

if ( account == "Credit" ) {
  oldtmp <- tmp[which(tmp$time < mdy("9/8/2009", tz = "America/Chicago")),]
  oldtmp$Amount <- oldtmp$Amount * -1
}
if ( account == "Chase" ) {
  oldtmp <- tmp[which(tmp$time < mdy("9/5/2008", tz = "America/Chicago")),]
  oldtmp <- oldtmp[which(oldtmp$time > mdy("6/21/2007", tz = "America/Chicago")),]
  x <- oldtmp[1,]
  x[,"Payee"] = "Opening Balance"
  x[,"Amount"] = 0.0
  oldtmp <- rbind(x, oldtmp)
  oldtmp[1,"time"] <- oldtmp[1,"time"] - 1
}
oldtmp[1,"Amount"] <- -1*sum(oldtmp[,"Amount"])
oldtmp["Date"] <- as.Date(oldtmp[,"time"])
out <- oldtmp[c("Date", "Amount", "Payee")]
out[out$Payee == "Check","Payee"] <- "UnknownCheck"
if ( account == "Chase") {
  out <- out[-which(out$Payee == "Split"),]
}

write.csv(x = out, file = paste(account, "/old/oldentries.csv", sep = ""), row.names = F)