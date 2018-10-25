#######################################################################################################
#
# Functions To Get Sums of Various Divisions of Money
#
#######################################################################################################
getKnownDivs <- function() { return( colnames(records) ) }

isKnownDiv <- function(div) {
  return( ifelse(div %in% getKnownDivs(), T, F) )
}

getSums <- function(df, ...) {
  divs <- unlist(list(...))
  if ( !all(isKnownDiv(divs)) ) {
    flog.warn(paste("One of the divisions is not known:",divs))
    stop()
  }
  require(dplyr)

  sumdf <- as.data.frame(summarise(grouped_df(df, divs), sum(amount)))
  colnames(sumdf)[ncol(sumdf)] <- "total"
  sumdf[,"total"] <- round(sumdf[,"total"], 0)
  return( sumdf )
}
