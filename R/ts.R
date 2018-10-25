getTSvals <- function(unit, t) {
    start <- year(min(t))
    end <- year(max(t))
    if ( unit == "Year" ) {
        freq <- 1
    }
    else if ( unit == "Quarter" || unit == "quarter" ) {
        freq <- 4
        minQ=quarters(min(t))
        if ( minQ == "Q1" ) { start <- start + 0.0 }
        if ( minQ == "Q2" ) { start <- start + 0.25 }
        if ( minQ == "Q3" ) { start <- start + 0.50 }
        if ( minQ == "Q4" ) { start <- start + 0.75 }
        maxQ=quarters(max(t))
        if ( maxQ == "Q1" ) { end <- end + 0.0 }
        if ( maxQ == "Q2" ) { end <- end + 0.25 }
        if ( maxQ == "Q3" ) { end <- end + 0.50 }
        if ( maxQ == "Q4" ) { end <- end + 0.75 }
    }
    else if ( unit == "Month" || unit == "month" ) {
        freq <- 12
        minM=month(min(t))
        start <- start + (minM-1)/12
        maxM=month(max(t))
        end <- end + (maxM-1)/12
    }
    else {
        writeLines(paste(unit,"not recognized."))
        pause()
    }

    return( c(freq, start, end) )
}
    
createTSfromMelt <- function(unit, mdata) {
    fitteddata <- subset(mdata, mdata$Var2 == "Data")
    forecastdata <- subset(mdata, mdata$Var2 == "Forecast")

    df <- rbind(fitteddata, forecastdata)
    tsdata <- df$value
    t <- sort(unique(as.Date(as.numeric(df$Var1))))

    vals <- getTSvals(unit, t)
    freq <- vals[[1]]
    start <- vals[[2]]
    end <- vals[[3]]
    
    tsval <- ts(tsdata, frequency=freq, start=start, end=end)

    return( tsval )

}

createTimeSeries <- function(unit, mdata) {
    tsvals <- list()

    for ( i in seq_along(colnames(mdata)) ) {
        tsdata <- mdata[,i]
        tsdata <- tsdata[which(as.integer(tsdata) != 0)]
 
        t <- as.Date(as.numeric(names(tsdata)))
        start <- year(min(t))
        end <- year(max(t))
 
        if ( unit == "Year" ) {
            freq <- 1
        }
        else if ( unit == "Quarter" || unit == "quarter" ) {
            freq <- 4
            minQ=quarters(min(t))
            if ( minQ == "Q1" ) { start <- start + 0.0 }
            if ( minQ == "Q2" ) { start <- start + 0.25 }
            if ( minQ == "Q3" ) { start <- start + 0.50 }
            if ( minQ == "Q4" ) { start <- start + 0.75 }
            maxQ=quarters(max(t))
            if ( maxQ == "Q1" ) { end <- end + 0.0 }
            if ( maxQ == "Q2" ) { end <- end + 0.25 }
            if ( maxQ == "Q3" ) { end <- end + 0.50 }
            if ( maxQ == "Q4" ) { end <- end + 0.75 }
        }
        else if ( unit == "Month" || unit == "month" ) {
            freq <- 12
            minM=month(min(t))
            start <- start + (minM-1)/12
            maxM=month(max(t))
            end <- end + (maxM-1)/12
        }
        else {
            writeLines(paste(unit,"not recognized."))
            pause()
        }
    
        tsvals[[i]] <- ts(tsdata, frequency=freq, start=start, end=end)
        names(tsvals)[[i]] <- colnames(mdata)[[i]]
    }

    return(tsvals)
}


createTS <- function() {
    ## Change names to avoid confusion
    unit <- "Quarter"    
    rdaname <- paste(paste("Rda/FinalGrouping/",unit,"Matrix",sep=""), "rda", sep=".")
    load(rdaname) # returns quarterMatrix
    createTimeSeries(unit, quarterMatrix)
}
