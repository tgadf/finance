#' Returns the year (fiscal or calendar) and quarter in which the date appears.
#' 
#' This function will cut the given date vector into quarters (i.e. three month
#' increments) and return an ordered factor with levels defined to be the quarters
#' between the minimum and maximum dates in the given vector. The levels, by
#' default, will be formated as \code{FY2013-Q1}, however the \code{FY} and \code{Q}
#' can be changed using the \code{fy.prefix} and \code{quarter.prefix} parameters,
#' respectively.
#' 
#' @param x vector of type \code{\link{Date}}.
#' @param firstMonth the month corresponding to the first month of the fiscal year.
#'        Setting \code{firstMonth=1} is equivalent calenadar years.
#' @param fy.prefix the character string to paste before the year.
#' @param quarter.prefix the character string to paste before the quarter.
#' @param sep the separater between the year and quarter.
#' @param level.range the range to use for defining the levels in the returned
#'        factor.
#' @export
#' @examples
#'         dates <- as.Date(c('2013-04-03','2012-03-30','2011-10-31',
#' 	                   '2011-04-14','2010-04-22','2004-10-04',
#' 	                   '2000-02-29','1997-12-05','1997-04-23',
#' 	                   '1997-04-01'))
#' 	getYearQuarter(dates)
#' 	getYearQuarter(dates, firstMonth=1)
#' 	getYearQuarter(dates, 1, '', '', '')
#' 	\dontrun{
#' 	getYearQuarter(dates, level.range=as.Date(c('2010-01-01','2013-01-01')))
#' 	}
getYearQuarter <- function(x, 
                           firstMonth=7, 
                           fy.prefix='FY', 
                           quarter.prefix='Q',
                           sep='-',
                           level.range=c(min(x), max(x)) ) {
    if(level.range[1] > min(x) | level.range[2] < max(x)) {
        warning(paste0('The range of x is greater than level.range. Values ',
                       'outside level.range will be returned as NA.'))
    }
    quarterString <- function(d) {
        year <- as.integer(format(d, format='%Y'))
        month <- as.integer(format(d, format='%m'))
        y <- ifelse(firstMonth > 1 & month >= firstMonth, year+1, year)  
        q <- cut( (month - firstMonth) %% 12, breaks=c(-Inf,2,5,8,Inf), 
                 labels=paste0(quarter.prefix, 1:4))
        return(paste0(fy.prefix, y, sep, q))
    }
    vals <- quarterString(x)
    levels <- unique(quarterString(seq(
        as.Date(format(level.range[1], '%Y-%m-01')), 
        as.Date(format(level.range[2], '%Y-%m-28')), by='month')))
    return(factor(vals, levels=levels, ordered=TRUE))
}
