



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

barplot2 <- function(b) {
  plot <- ggplot() + geom_bar(data = mdata1, aes(x=as.Date(Var1, origin="1970-01-01"), y=value, fill=Var2), stat="identity") + geom_bar(data = mdata2, aes(x=as.Date(Var1, origin="1970-01-01"), y=value, fill=Var2), stat="identity") + scale_fill_brewer(name=legendname, type="seq", palette=palette)
  xmarginleft <- 0.25
  xmarginright <- 1.0
  ymargintop <- 0.75
  ymarginbottom <- 0.25
  
  #http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
  # Different legend positions
  plot <- plot + theme(legend.justification=c(0,1), legend.position=c(0,1), plot.margin = unit(c(ymargintop, xmarginright, ymarginbottom, xmarginleft), "cm"), axis.text=element_text(size=16),
                       axis.title=element_text(size=14,face="bold"), axis.text.y = element_text(face="bold", color="#993333", size=14, angle=45))
  #plot <- plot + theme(legend.position="top", plot.margin = unit(c(ymargintop, xmarginright, ymarginbottom, xmarginleft), "cm"))
  plot <- plot + ggtitle(title)
  plot <- plot + labs(x="", y="")
  ylab <- "<  Outgoing               Incoming  >"
  ylab <- ""
  plot <- plot + scale_y_continuous(name=ylab, labels = dollar)
  
  if ( addSum ) {
    #linetype="dashed", 
    plot <- plot + geom_line(data=ldata, aes(x=as.Date(Var1, origin="1970-01-01"), y=value), colour="Black", size=0.75) + geom_point(size=2, shape=21, fill="white")
  }
  
  return( plot )
  
}