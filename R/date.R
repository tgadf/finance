NumQtrs <- function(dftime) {
    return( length(unique(quarter(dftime, with_year=T))) )
}
NumMonths <- function(dftime) {
    return( length(unique(month(dftime, with_year=T))) )
}
NumYears <- function(dftime) {
    return( length(unique(year(dftime))) )
}

QtrName <- function(date) {
    month <- month(date)

    if ( is.na(month) ) { return( "" ) }
    
    if ( month == 2 ) { return( "Q1" ) }
    else if ( month == 5 ) { return( "Q2" ) }
    else if ( month == 8 ) { return( "Q3" ) }
    else if ( month == 11 ) { return( "Q4" ) }
    else { return( "" ) }
}

QtrNames <- function(dates) {
    ret     = rep(NA, length(dates))
    retyear = as.character(year(dates))

    retyear[is.na(retyear)] <- ""

    for(i in seq_along(dates)){
        ret[i] = QtrName(dates[i])
    }
    
    retval <- paste(ret, retyear, sep=" ")
    return(retval)
}

convQ <- function(quarter) {
    if ( quarter == 1 ) { return( "02-01" ) }
    else if ( quarter == 2 ) { return( "05-01" ) }
    else if ( quarter == 3 ) { return( "08-01" ) }
    else if ( quarter == 4 ) { return( "11-01" ) }
    else { return( "00" ) }
}

Qtr <- function(qs) {
    ret = rep(NA, length(qs))

    for(i in seq_along(qs)){
        ret[i] = convQ(qs[i])
    }
    return(ret)
}

mkQtrs <- function(dftime, debug=FALSE) {
    if ( debug ) { funcinfo(match.call(), 2) }

    yrqtrs <- quarter(dftime, with_year=TRUE)
    vals <- ldply(strsplit(as.character(yrqtrs), "\\."))
    years <- vals$V1
    qtrs <- vals$V2
    qtrdates <- Qtr(qtrs)
    ret <- as.Date(paste(years, qtrdates, sep="-"))

    return( ret )
}
