prettyYaxis <- function(ydata) {
    if ( missing(ydata) ) {
        yticks <- axTicks(2)
    } else {
        yticks <- pretty(ydata)
    }
    dy = tail(yticks, n=2)
    last = dy[2]
    dy = dy[2] - dy[1]
    yticks <- c(yticks, last + dy)
    ylabels <- yticks
    if ( max(yticks) > 10000 ) {
        ylabels <- paste(yticks/1000, "k", sep='')
    }
    ylabels <- paste(yticks/1000, "k", sep='')

    yaxis <- list()
    yaxis[[1]] <- yticks
    yaxis[[2]] <- ylabels

    return( yaxis )
}

prettyAxis <- function(x, mod) {
    keep <- x[seq(1, length(x), mod)]
    ret <- rep(NA, length(x))
    isDate <- is.Date(x)
    for(i in seq_along(x)){
        if ( i %% mod == 0 ) {
            if ( isDate ) {
                ret[[i]] = as.Date(x[i])
            } else {
                ret[[i]] = x[i]
            }
        }
    }
    ret[is.na(ret)] <- ""
    if ( is.Date(x) ) {
        ret <- as.Date(ret)
    }

    return( ret )
}

timeAxisBar <- function(x) {
    startdate <- as.Date(min(x)) - 180
    enddate <- as.Date(max(x)) + 180
    atdates <- x
    ##labeldates <- format(atdates, "%b, %y")
    ##atdates <- as.numeric(atdates)
    
    taxis <- list()
    taxis[[1]] <- atdates
    taxis[[2]] <- labeldates

    return( taxis )
}


timeAxis <- function(x, mod) {
    startdate <- as.Date(min(x)) - 180
    enddate <- as.Date(max(x)) + 180
    atdates <- seq(from=startdate, to=enddate, length.out=20)
    labeldates <- format(atdates, "%b, %y")
    atdates <- as.numeric(atdates)

    xlen <- length(x)
    lenx <- xlen/mod
    d <- atdates[1:lenx*mod]
    atdates <- d[!is.na(d)]

    d <- labeldates[1:lenx*mod]
    labeldates <- d[!is.na(d)]

    taxis <- list()
    taxis[[1]] <- atdates
    taxis[[2]] <- labeldates

    return( taxis )
}

setMargins <- function() {        
    leftmar <- 0.8
    rightmar <- 0.8
    bottommar <- 1.5
    topmar <- 1.5
    par(mai=c(bottommar,leftmar,topmar,rightmar))
}



###################################################
##
## Histogram Charts
##
###################################################
dollarval <- function(val) {
  isNeg <- val < 0
  return( formatC(val, format="d", big.mark=',') )
}

histcat <- function(mdata, facs, name, dosum = T, debug = F) {
  if ( debug ) { funcinfo(match.call(), 2) }
  
  if ( dosum ) {
    odata <- mdata
    mdata <- cumsum(mdata)
  }
  
  leftmar <- 0.8
  rightmar <- 0.8
  bottommar <- 1.5
  topmar <- 0.5
  par(mai=c(bottommar,leftmar,topmar,rightmar))
  
  labels <- names(mdata)
  x <- unname(mdata)
  col <- topo.colors(length(unique(facs)))
  col <- terrain.colors(length(unique(facs)))
  #col <- heat.colors(length(unique(facs)))
  cols <- col[as.factor(facs[labels])]
  
  ucols <- col[as.factor(unique(facs))]
  labels <- gsub("Income - ", "", labels)
  labels <- gsub("Expenses - ", "", labels)
  labels <- gsub("Interest - ", "", labels)
  
  ylim <- pretty(range(x))
  ylim <- c(head(ylim, n=1), tail(ylim, n=1))
  if ( ylim[1] > 0 ) { ylim[1] <- 0 }
  
  barplot(x, axes=F, axisnames = F, space = 0, col = cols, ylim = ylim)
  
  #text(x = bp, y = dat$freqs, label = dat$freqs, pos = 3, cex = 0.8, col = "red")
  yaxis <- prettyYaxis()
  axis(2, at=yaxis[[1]], labels=yaxis[[2]], cex.axis=0.85)
  axis(4, at=yaxis[[1]], labels=yaxis[[2]], cex.axis=0.85)
  attick <- seq_len(length(labels) + 1)
  axis(side=1, at=attick - 1, labels=FALSE)
  axis(side=1, at=seq_along(labels)-0.5, tick=FALSE, labels=labels, las=3, cex.axis=0.7)
  grid()
  
  lastX = -1
  lastY = -1
  yy <- c()
  xx <- c()
  vals <- c()
  for ( i in seq_len(length(x)) ) {
    val <- as.integer(odata[i])
    if ( abs(val) > 1e5 ) { val <- signif(val, 4) }
    else if ( abs(val) > 1e4 ) { val <- signif(val, 3) }
    else if ( abs(val) > 1e3 ) { val <- signif(val, 2) }
    else if ( abs(val) > 0 ) { val <- signif(val, 1) }
    val <- dollarval(val)
    yy <- c(yy, mdata[i])
    xx <- c(xx, i - 0.5)
    vals <- c(vals, val)
    #text(i - 0.5, mdata[i] + 1e4, val, srt = 270, cex = 0.55)
    lastX = i - 0.5
    lastY = mdata[i]
  }

  text(x = xx, y = yy, label = vals, pos = 3, cex = 0.6, offset = 1, srt = 270, col = "darkblue")
  
    
  #mtext(xlab, 2, line=2.5, col='dodgerblue', cex=1.25)
  
  title <- paste(name, "Data", sep=" ")
  mtext(title, 3, cex=1.5, line=1.0, col='darkblue')
  
  x0 = lastX
  y0 = lastY + 1e3
  y1 = lastY + 1e4
  #arrows(x0, y0, x1 = x0, y1, length = 0.10, code = 1)
  val <- paste(expression(Sigma),as.character(signif(tail(mdata, n = 1), 3)), sep = " ")
  #val <- expression(Sigma)
  val2 <- dollarval(signif(tail(mdata, n = 1), 3))
  #text(x0, y1 + 5e3, val, srt = 90, cex = 0.6, col = 'darkblue')
  #text(x = x0, y = y1, label = val2, srt = 270, pos = 3, offset = 3, cex = 0.75, col = 'darkred')
  
  legend("topright", legend = unique(facs), col = ucols, fill = ucols, cex = 0.75)
}



###################################################
##
## Dot Summary Charts
##
###################################################
printdotsummary <- function(df, name, debug) {
    if ( debug ) { funcinfo(match.call(), 4) }

    
    amounts <- df$amount
    labels <- paste(df$payee, df$time, sep=": ")
    
    max <- 40
    svals <- ceiling(seq_along(amounts)/max)

    dotamounts <- split(amounts, svals)    
    dotlabels <- split(labels, svals)
    par(mfrow=c(1,1))
    colors <- terrain.colors(max)
    colors <- heat.colors(max)
    colors <- cm.colors(max)
    colors <- rainbow(max, start=0.67, end=0.9)
    title <- unique(df$category)
    pchlist <- c(19, 20, 21, 22, 23)
    pchs <- sample(pchlist, max, replace=TRUE)
    for ( j in seq_along(unique(svals)) ) {
        dotchart(dotamounts[[j]], labels=dotlabels[[j]], main=title, col=colors, pch=pchs, cex=0.7)
    }
}

dotsummary <- function(sdf, name, debug) {
    if ( debug ) { funcinfo(match.call(), 2) }

    if ( class(sdf) == "data.frame" ) {
        ##if ( sdf == NULL ) { return }
        ##if ( dim(sdf)[[1]] == 0 ) { return }
        df <- sdf[order(sdf$time),]
        printdotsummary(df, name, debug)
    }

    if ( class(sdf) == "list") {
        for ( df in sdf ) {
            if ( is.null(df) ) { next }
            df <- df[order(df$time),]
            printdotsummary(df, name, debug)
        }
    }
}


###################################################
##
## Dot Charts
##
###################################################
dotoverlay <- function(mdata) {
    if ( debug ) { funcinfo(match.call(), 2) }
    mtdata <- t(mdata)
    x <- colnames(mtdata)

    taxis <- timeAxis(x)
    atdates <- taxis[[1]]
    labeldates <- taxis[[2]]

    setMargins()
    
    if ( debug ) { info("Creating dotchart", 4) }
    dotchart(mdata, cex=0.5, pch=19, col=topo.colors(length(x)))
}



###################################################
##
## Box-Whiskers Charts
##
###################################################
boxoverlay <- function(sdata) {
    if ( debug ) { funcinfo(match.call(), 2) }

    x <- unique(sdata$quarter)

    taxis <- timeAxis(x)
    atdates <- taxis[[1]]
    labeldates <- taxis[[2]]

    setMargins()

    if ( debug ) { info("Creating boxplot", 4) }
    boxplot(amount~quarter, data=sdata, axes=FALSE, col=topo.colors(length(x)))
        
    axis(side=2, pos=-0.2)
    attick <- seq_len(length(x) + 1)
    axis(side=1, at=attick - 1, labels=FALSE)
    axis(side=1, at=seq_along(x)-0.5, tick=FALSE, labels=x, las=3)

    mtext("Payment Amount ($)", 2, line=2.5, col='dodgerblue', cex=1.25)
}



###################################################
##
## Point/Plot Charts
##
###################################################
printplotoverlay <- function(x, y, sumy, name, debug) {    
    if ( debug ) { funcinfo(match.call(), 4) }

    setMargins()

    xaxis <- timeAxis(x, 3)
    atdates <- xaxis[[1]]
    labeldates <- xaxis[[2]]

    
    if ( debug ) { info("Creating pointplot", 6) }
    plot(y ~ x, type='b', col='dodgerblue', axes=T, col.axis='dodgerblue', pch=19, yaxt='n', xaxt='n', xlab='', ylab='')


    if ( debug ) { info("Filling axis info.", 6) }

    ## x-axis w/ time
    axis(1, at=atdates, labels=labeldates, las=3, tck=-0.015, cex.axis=0.85)
    
    ## y-axis (left) w/ amount
    yaxis <- prettyYaxis()
    axis(2, at=yaxis[[1]], labels=yaxis[[2]], col='dodgerblue', cex.axis=0.85)
    mtext("Payment Amount ($)", 2, line=2.5, col='dodgerblue', cex=1.25)
    
    par(new=T)
    yaxis <- prettyYaxis(sumy)
    plot(sumy ~ x, col='darkorange', type='l', lty=2, col.axis='darkorange', xaxt='n', yaxt='n', xlab='', ylab='')
    ##box()
    
    ## y-axis (right) w/ amount
    axis(4, at=yaxis[[1]], labels=yaxis[[2]], col='darkorange', cex.axis=0.85)
    mtext("Running Sum ($)", 4, line=2.5, col='darkorange', cex=1.25)
    
    title <- paste(name, "Data", sep=" ")
    mtext(title, 3, cex=1.5, line=1.0, col='darkblue')
}


plotoverlay <- function(mdata, name, debug) {
    if ( debug ) { funcinfo(match.call(), 2) }

    ## Matrix
    if ( class(mdata) == "matrix" ) {
        mtdata <- t(mdata)
        x <- as.Date(as.numeric(colnames(mtdata)))
        y <- colSums(mtdata)
        sumy <- cumsum(rowSums(mdata))
        printplotoverlay(x, y, sumy, name, debug)
        return
    }

    ## Vector
    else if ( class(mdata) == "numeric" ) {
        x <- as.Date(as.numeric(names(mdata)))
        y <- mdata
        sumy <- cumsum(y)
        printplotoverlay(x, y, sumy, name, debug)
        return
    }

    else {
        print("Unknown class!")
        print(class(mdata))
        print(mdata)
        pause()
    }
    return
}


###################################################
##
## Bar Charts
##
###################################################
baroverlay <- function(mdata, name, debug, datetype, dataoption) {
    if ( debug ) { funcinfo(match.call(), 2) }

    setMargins()

    mtdata <- t(mdata)
    x <- as.Date(as.numeric(colnames(mtdata)))
    
    catnames <- rownames(mtdata)
    colors <- topo.colors(length(catnames))
    print(catnames)
    print(colors)
    
    isSum <- FALSE
    
    if ( missing(dataoption) ) { xlab <- "Payment Amount ($)" }
    else {
        if ( dataoption == "sum" ) {
            xlab <- "Running Sum ($)"
            isSum <- TRUE
            for ( i in seq_along(catnames) ) {
                mtdata[i,] <- cumsum(mtdata[i,])
            }
        }
    }

    if ( debug ) { info("Creating barplot", 4) }
    print(mtdata)
    barplot(mtdata, axes=F, axisnames=F, space = 0, col=colors)
    if ( debug ) { info("Creating barplot - Done", 4) }

    if ( datetype == "quarters" ) {
        labels <- QtrNames(x)
    } else {
        labels <- format(x, "%b, %Y")
    }
    if ( debug ) { info("Making labels", 4) }
    mod <- max(as.integer(length(labels)/15), 1)
    labels <- prettyAxis(labels, mod)

    if ( debug ) { info("Filling axis info.", 4) }
    
    yaxis <- prettyYaxis()
    axis(2, at=yaxis[[1]], labels=yaxis[[2]], col='dodgerblue', cex.axis=0.85)
    
    ##axis(side=2, pos=-0.2)
    attick <- seq_len(length(labels) + 1)
    axis(side=1, at=attick - 1, labels=FALSE)
    axis(side=1, at=seq_along(labels)-0.5, tick=FALSE, labels=labels, las=3)
    if ( isSum ) {
        axis(4, at=yaxis[[1]], labels=yaxis[[2]], col='dodgerblue', cex.axis=0.85)
    }
    
    mtext(xlab, 2, line=2.5, col='dodgerblue', cex=1.25)

    title <- paste(name, "Data", sep=" ")
    mtext(title, 3, cex=1.5, line=1.0, col='darkblue')

    legend("topleft", legend = catnames, fill=colors)
}




###################################################
##
## Summary Bar Charts
##
###################################################
summarybaroverlay <- function(mdata, name, debug, datetype, dataoption) {
    if ( debug ) { funcinfo(match.call(), 2) }

    setMargins()

    mtdata <- t(mdata)
    x <- as.Date(as.numeric(colnames(mtdata)))
    
    catnames <- rownames(mtdata)
    isSum <- FALSE
    
    if ( missing(dataoption) ) { xlab <- "Payment Amount ($)" }
    else {
        if ( dataoption == "sum" ) {
            xlab <- "Running Sum ($)"
            isSum <- TRUE
            for ( i in seq_along(catnames) ) {
                mtdata[i,] <- cumsum(mtdata[i,])
            }
        }
    }

    if ( debug ) { info("Creating barplot", 4) }
    barplot(mtdata, beside=FALSE, axes=F, axisnames=F, space = 0, col=topo.colors(length(rownames(mtdata))))
    if ( debug ) { info("Creating barplot - Done", 4) }

    if ( datetype == "quarters" ) {
        labels <- QtrNames(x)
    } else {
        labels <- format(x, "%b, %Y")
    }
    if ( debug ) { info("Making labels", 4) }
    mod <- as.integer(length(labels)/15)
    labels <- prettyAxis(labels, mod)

    if ( debug ) { info("Filling axis info.", 4) }
    
    yaxis <- prettyYaxis()
    axis(2, at=yaxis[[1]], labels=yaxis[[2]], col='dodgerblue', cex.axis=0.85)
    
    ##axis(side=2, pos=-0.2)
    attick <- seq_len(length(labels) + 1)
    axis(side=1, at=attick - 1, labels=FALSE)
    axis(side=1, at=seq_along(labels)-0.5, tick=FALSE, labels=labels, las=3)
    if ( isSum ) {
        axis(4, at=yaxis[[1]], labels=yaxis[[2]], col='dodgerblue', cex.axis=0.85)
    }
    
    mtext(xlab, 2, line=2.5, col='dodgerblue', cex=1.25)

    title <- paste(name, "Data", sep=" ")
    mtext(title, 3, cex=1.5, line=1.0, col='darkblue')

    legend("topleft", legend = catnames, fill=topo.colors(length(rownames(mtdata))))
}
