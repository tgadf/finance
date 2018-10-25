testMatrix <- function(mat) {
    ret <- anyNA(mat)
    if ( ret ) { return( FALSE ) }
    return( TRUE )
}

testVector <- function(vec) {
    ret <- any(is.na(data))
    if ( ret ) { return( FALSE ) }
    return( TRUE )
}

testDF <- function(quartermatrix, catdata) {
            quarterdata[[j]] <- tapply(sdf[[j]]$amount, sdf[[j]]$quarter, sum)
        mval <- match(rownames(quartermatrix), as.numeric(as.Date(names(quarterdata[[1]]))))
        quartermatrix[,j] <- quarterdata[[j]][mval]

}

testAllNA <- function(vec) {
    retval <- all(is.na(vec))
    return( retval )
}
