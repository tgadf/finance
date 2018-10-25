mergeMatrix <- function(original, newdata, colval) {

    #print(rownames(original))
    #print(names(newdata))
    #print(as.Date(names(newdata)))
    
    ## Get row names
    alldates <- as.numeric(rownames(original))
    newdates <- as.numeric(as.Date(names(newdata)))

    ## Get overlap
    mval <- match(alldates, newdates)
    
    ## Enter data
    original[,colval] <- newdata[mval]

    return( original )
}

##BFSI
##TMEE
