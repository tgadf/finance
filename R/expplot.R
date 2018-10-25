load("Rda/FinalGrouping/MonthMatrix-1.rda")
load("Rda/FinalGrouping/QuarterMatrix-1.rda")

mondata <- monthMatrix[106,]
qtrdata <- quarterMatrix[44,]
#qrtdata 
#mondata <- monthMatrix[106,]

qdata <- replicate(n = length(qtrdata), expr = 0.0)
qdata[[1]] <- sum(qtrdata)
qdata[[2]] <- qdata[[1]] - qtrdata[["Extra Income"]]
qdata[[3]] <- qdata[[2]] - qtrdata[["Savings, IRA, 401k"]]
qdata[[4]] <- qdata[[3]] - qtrdata[["Work"]]
qdata[[4]] <- qdata[[3]] - qtrdata[["Work"]]

#qdata[[]]
onames <- names(qtrdata)
for ( oname in onames ) {
  print(paste(oname,qtrdata[[oname]]))
}