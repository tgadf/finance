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

HWforecast <- function(mdata, name, debug) {
    if ( debug ) { print(paste(" Creating HW forecast with name =", name)) }

    ## Create data
    mtdata <- t(mdata)
    x <- as.Date(as.numeric(colnames(mtdata)))
    catnames <- rownames(mtdata)

    
    ## Merge all columns and sum (one entry per date)
    sumdata <- colSums(mtdata)


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
    axis(1, at=atdates, labels=labeldates, las=3, ,tck=-0.015)
    

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

extra <- FALSE
if ( extra ) {
    ##x.dates <- as.Date(index(x.hw$x))
    
    ## plot time series
    ts.plot(x.zoo, type='o', col='blue', lty='dashed')
    
    ## data
    zoocoredata <- coredata(sbux.z)
    ## time
    zooindex <- index(sbux.z)
    
    ## window of data
    subzoodata <- window(sbux.z, start=as.Date("2013/2/25"), end=as.Date("2014/5/1"))
    
    ## create ARIMA
    arimafit <- arima(subzoodata)
    
    ## forcast
    fcast <- forecast(arimafit, 2)
    
    ## plot(jj, type="o", col="blue", lty="dashed")
    
    ## side	an integer indicating the side of the graph to draw the axis (1=bottom, 2=left, 3=top, 4=right)
}
