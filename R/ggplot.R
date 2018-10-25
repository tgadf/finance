###################################################
##
## Data/Line Plots
##
###################################################
ggForecastPlot <- function(mdata, title) {
    xmarginleft <- 0.25
    xmarginright <- 1.0
    ymargintop <- 0.75
    ymarginbottom <- 0.25
 
    palette <- "Paired"
    legendname <- "Grouping"
  #, palette=palette
    plot <- ggplot(data=mdata, aes(x=as.Date(Var1, origin="1970-01-01"), y=value, group=Var2, colour=Var2), xlab="", ylab="Payment Amount ($)", palette=palette) + geom_line(size=1.5) + geom_point(size=1, shape=21, fill="white")
    #plot <- plot + discrete_scale(aesthetics = aesthetic, scale_name = "manual",  palette = palette, name = legendname)
    
    plot <- plot + scale_color_manual(values=brewer.pal(5, palette))
    
    #http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
    # Different legend positions
    plot <- plot + theme(legend.justification=c(0,1), legend.position=c(0,1), plot.margin = unit(c(ymargintop, xmarginright, ymarginbottom, xmarginleft), "cm"), axis.text=element_text(size=16),
                         axis.title=element_text(size=14,face="bold"), axis.text.y = element_text(face="bold", color="#993333", size=14, angle=45))
    #plot <- plot + theme(legend.position="top", plot.margin = unit(c(ymargintop, xmarginright, ymarginbottom, xmarginleft), "cm"))
    plot <- plot + ggtitle(title)
    plot <- plot + labs(x="", y="")
    ylab <- ""
    plot <- plot + scale_y_continuous(name=ylab, labels = dollar)

    return( plot )
}



###################################################
##
## Data/Line Plots
##
###################################################
ggPointPlot <- function(mdata, title, option, leg=FALSE) {

    if ( option == "sum" ) {
        mnames <- unique(mdata$Var2)
        for ( name in mnames ) {
            whichvals <- which(mdata$Var2 == name)
            vals <- cumsum(mdata[which(mdata$Var2 == name),][["value"]])
            mdata[which(mdata$Var2 == name),][["value"]] <- vals
        }
    }
    
    ngroups <- unique(mdata$Var2)
    print(ngroups)
    palette <- "Paired"
    palette <- "Set1"
    legendname <- "Grouping"
  #, palette=palette
    
    plot <- ggplot(data=mdata, aes(x=as.Date(Var1, origin="1970-01-01"), y=value, group=Var2, colour=Var2), xlab="", ylab="Payment Amount ($)", palette=palette) + geom_line(size=1.5) + geom_point(size=3, shape=21, fill="white")
    #plot <- plot + discrete_scale(aesthetics = aesthetic, scale_name = "manual",  palette = palette, name = legendname)
    #plot <- plot + scale_color_manual(values=brewer.pal(ngroups, palette))
    xmarginleft <- 0.25
    xmarginright <- 1.0
    ymargintop <- 0.75
    ymarginbottom <- 0.25
    
    #http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
    # Different legend positions
    if ( leg == TRUE ) {
        #plot <- plot + theme(legend.justification=c(0,1), legend.position=c(0,1))
        plot <- plot + theme(legend.position="top", legend.key = element_rect(size=0.1))
        plot <- plot + theme(legend.text = element_text(size = 5, hjust = 3, vjust = 3, face = 'bold'))
    }
    else {
        plot <- plot + theme(legend.position="none")
    }
    plot <- plot + theme(axis.title=element_text(size=14,face="bold"), axis.text.y = element_text(face="bold", color="#993333", size=14, angle=45))
    #plot <- plot + theme(legend.justification=c(0,1), legend.position=c(0,1), plot.margin = unit(c(ymargintop, xmarginright, ymarginbottom, xmarginleft), "cm"), axis.text=element_text(size=16),
    #axis.title=element_text(size=14,face="bold"), axis.text.y = element_text(face="bold", color="#993333", size=14, angle=45))
    #plot <- plot + theme(legend.position="top", plot.margin = unit(c(ymargintop, xmarginright, ymarginbottom, xmarginleft), "cm"))
    if ( length(title) > 0 ) {
        plot <- plot + ggtitle(title)
    }
    plot <- plot + labs(x="", y="")
    ylab <- ""
    plot <- plot + scale_y_continuous(name=ylab, labels = dollar)

    
    print(paste("option = ",option))
    return(plot)
}



###################################################
##
## Data/Line Plots
##
###################################################
ggDataPlot <- function(mdata, title) {
    ngroups <- unique(mdata$Var2)
    print(ngroups)
    palette <- "Paired"
    palette <- "Set1"
    legendname <- "Grouping"
  #, palette=palette
    
    plot <- ggplot(data=mdata, aes(x=as.Date(Var1, origin="1970-01-01"), y=value, group=Var2, colour=Var2), xlab="", ylab="Payment Amount ($)", palette=palette) + geom_line(size=1.5) + geom_point(size=3, shape=21, fill="white")
    #plot <- plot + discrete_scale(aesthetics = aesthetic, scale_name = "manual",  palette = palette, name = legendname)
    #plot <- plot + scale_color_manual(values=brewer.pal(ngroups, palette))
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
    ylab <- ""
    plot <- plot + scale_y_continuous(name=ylab, labels = dollar)

    return(plot)
}

ggDataPlotPrep <- function(matdata, title, options) {
    if ( options == "sum" ) {
        for ( i in seq_along(colnames(matdata)) ) {
            matdata[,i] <- cumsum(matdata[,i])
        }
    }
    mdata <- melt(matdata)
    
    return( ggDataPlot(mdata, title) )
}


###################################################
##
## Bar Charts
##
###################################################
ggBarPlot <- function(mdata, title) {
    mdata1 <- subset(mdata,value >= 0)
    mdata2 <- subset(mdata,value < 0)

    addSum <- FALSE


    # Blue
    palette <- 1
    # Blue w/ Purple
    palette <- 3
    # Crazy
    # Light crazy
    palette <- 2 ## Green
    palette <- 3 ## Purple
    palette <- 4 ## Blue/Green
    palette <- 5 ## DarkGreen

    palette <- "Accent"
    palette <- "Set2"
    palette <- "Paired"

    #palette <- "Spectral"
    legendname <- "Grouping"
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

ggBarPlotPrep <- function(matdata, title, options) {
    if ( options == "sum" ) {
        for ( i in seq_along(colnames(matdata)) ) {
            matdata[,i] <- cumsum(matdata[,i])
        }
    }
    
    mdata <- melt(matdata)
    return( ggBarPlot( mdata, title ) )
}


ggbaroverlay <- function(mdata, name, debug, datetype, dataoption) {
    if ( debug ) { funcinfo(match.call(), 2) }

    setMargins()
}

ggLines <- function(mdata, name, savedir, debug) {

    ## First reshape data
    mdf <- melt(mdata)

    ## Get total for that period
    groupsum <- matrix(rowSums(mdata), ncol=1)
    rownames(groupsum) <- rownames(mdata)
    colnames(groupsum) <- "Total"
    tdf <- melt(groupsum)
    
    plot <- ggplot(data=mdf, aes(x=as.Date(Var1, origin="1970-01-01"), y=value, group=Var2, colour=Var2), xlab="", ylab="Payment Amount ($)") + geom_line() + geom_point(size=2, shape=21, fill="white")
    xmarginleft <- 0.25
    xmarginright <- 1.0
    ymargintop <- 0.75
    ymarginbottom <- 0.25
    plot <- plot + theme(plot.margin = unit(c(ymargintop, xmarginright, ymarginbottom, xmarginleft), "cm"))
    plot <- plot + ggtitle(paste("Quarterly",accountname,"Activity"))
    plot <- plot + labs(x="", y="Amount [$]")

    plot <- plot + geom_line(data=tdf, aes(x=as.Date(Var1, origin="1970-01-01"), y=value), colour="Black", size=1.5)
    
    print(plot)

    pdfname <- paste(name, "pdf", sep=".")
    pdfname <- paste(savedir, pdfname, sep="/")
    ggsave(pdfname)
    writeLines(pdfname)
}
