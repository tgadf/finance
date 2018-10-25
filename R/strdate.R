#######################################################################################################
#
# Format Date Functions
#
#######################################################################################################
getDate <- function(dateval) {
  require(zoo)
  require(lubridate)
  
  if ( class(dateval) == "yearqtr" ) {
    return( as.Date(dateval) )
  } else if ( class(dateval) == "yearmon" ) {
    return( as.Date(dateval) )
  } else if ( class(dateval) == "yearqtr" ) {
    return( as.Date(dateval) )
  } else if ( class(dateval) == "Date" ) {
    return( dateval )
  } else if ( is.character(dateval) ) {
    seps <- c("/", "-")
    for ( sep in seps ) {
      if ( length(grep(pattern = sep, x = dateval) > 0 ) ) {
        formats <- c(paste("%b","%d","%Y", sep=sep),
                     paste("%m","%d","%Y", sep=sep),
                     paste("%m","%d","%y", sep=sep),
                     paste("%Y","%m","%d", sep=sep))
        for ( fmt in formats ) {
          test <- as.Date(dateval, fmt)
          if ( isValidDate(test) ) {
            flog.debug(paste("Parsing date",dateval,"with format",fmt))
            return( test )
          }
        }
      }
    }

    formats <- c("%b %d, %Y", "%b %d %Y")
    for ( fmt in formats ) {
      test <- as.Date(dateval, fmt)
      if ( isValidDate(test) ) {
        flog.debug(paste("Parsing date",dateval,"with format",fmt))
        return( test )
      }
    }
    
    flog.warn(paste("Could not parse date",dateval,"with any known format"))
    stop()
  }
  
  return( dateval )
}

getQuarters <- function(minQuarter = NULL, maxQuarter = NULL, year = NULL, returnDate = F) {
  if ( is.null(minQuarter) ) { minQuarter <- getDate(min(records[,"Quarter"])) }
  else                       { minQuarter <- getDate(minQuarter) }
  
  if ( is.null(maxQuarter) ) { maxQuarter <- getDate(max(records[,"Quarter"])) }
  else                       { maxQuarter <- getDate(maxQuarter) }
  
  if ( !is.null(year) ) {
    minQuarter <- getDate(paste(1,1,year, sep = "/"))
    maxQuarter <- getDate(paste(12,getLastDayInMonth(12,year),year, sep = "/"))
  }

  dates <- seq.Date(from=minQuarter, to=maxQuarter, by = "quarter")
  if ( returnDate ) {
    return( dates )
  }
  return( as.yearqtr(dates) )
}

getMonths <- function(minMonth = NULL, maxMonth = NULL, year = NULL, quarter = NULL, returnDate = F) {
  if ( is.null(minMonth) ) { minMonth <- getDate(min(records[,"Month"])) }
  else                     { minMonth <- getDate(minMonth) }
  
  if ( is.null(maxMonth) ) { maxMonth <- getDate(max(records[,"Month"])) }
  else                     { maxMonth <- getDate(maxMonth) }
  
  if ( !is.null(year) ) {
    minMonth <- getDate(paste(1,1,year, sep = "/"))
    maxMonth <- getDate(paste(12,getLastDayInMonth(month,year),year, sep = "/"))
  }
  
  dates <- seq.Date(from=minMonth, to=maxMonth, by = "month")
  if ( returnDate ) {
    return( dates )
  }
  return( as.yearmon(dates) )
}

getDays <- function(minDay = NULL, maxDay = NULL, year = NULL, quarter = NULL, month = NULL, returnDate = F) {
  if ( !is.null(year) ) {
    if ( !is.null(month) ) {
      if ( is.character(month) ) { 
        monthname <- match(month, month.abb)
        if ( is.na(monthname)) { monthname <- match(month, month.name) }
        else { month <- monthname }
        if ( is.na(monthname) ) {
          flog.warn(paste("Could not convert month name:",monthname,"to a number"))
          stop()
        }
      }
      minDay <- getDate(paste(month,1,year, sep = "/"))
      maxDay <- getDate(paste(month,getLastDayInMonth(month,year),year, sep = "/"))
    } else {
      minDay <- getDate(paste(1,1,year, sep = "/"))
      maxDay <- getDate(paste(12,getLastDayInMonth(12,year),year, sep = "/"))
    }
  } else if ( !is.null(quarter) ) {
    require(timeDate)
    minDay <- as.Date(timeFirstDayInQuarter(as.Date(as.yearmon(quarter))))    
    maxDay <- as.Date(timeLastDayInQuarter(as.Date(as.yearmon(quarter))))    
  } else {
    if ( is.null(minDay) ) { minDay <- getDate(min(records[,"Date"])) }
    else                   { minDay <- getDate(minDay) }
    
    if ( is.null(maxDay) ) { maxDay <- getDate(max(records[,"Date"])) }
    else                   { maxDay <- getDate(maxDay) }
  }
  
  
  dates <- seq.Date(from=minDay, to=maxDay, by = "day")
  return( dates )
}


#######################################################################################################
#
# Helper Functions
#
#######################################################################################################
isValidDate <- function(test) {
  if ( is.Date(test) ) {
    if ( is.na(test) ) { return( F ) }
    if ( year(test) > year(Sys.Date())) { return( F ) }
    if ( year(test) < 1970 ) { return( F ) }
  } else {
    flog.warn("Trying to validate a date that is not a Date object")
    stop()
  }
  return( T )
}

getLastDayInMonth <- function(month, year) {
  require(timeDate)
  dateval <- paste(month, 1, year, sep="/")
  lastDay <- day(timeLastDayInMonth(getDate(dateval)))
  return( lastDay )
}