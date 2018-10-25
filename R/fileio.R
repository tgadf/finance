mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("memory.R", "dataOps.R")
for ( ifile in files ) {
  fname <- file.path(mvadir, ifile)
  stopifnot(file.exists(fname))
  source(fname)
}


##############################################################################################################################
# Find Files
##############################################################################################################################
findFiles <- function(dtype = NULL, prefix = NULL) {
  files <- NULL
  if ( is.null(dtype) & is.null(prefix) )   { files <- files <- Sys.glob(file.path(mvadir, "output", "*.rData")) }
  if ( !is.null(dtype) & is.null(prefix) )  { files <- files <- Sys.glob(file.path(mvadir, "output", paste(dtype,".","*.rData",sep=""))) }
  if ( is.null(dtype) & !is.null(prefix) )  { files <- files <- Sys.glob(file.path(mvadir, "output", paste("*",prefix,".","*.rData",sep=""))) }
  if ( !is.null(dtype) & !is.null(prefix) ) { files <- files <- Sys.glob(file.path(mvadir, "output", paste(dtype,".",prefix,".","*.rData",sep=""))) }
  return( files )
}


##############################################################################################################################
# Get Most Recent Files
##############################################################################################################################
getRecentFile <- function(dtype = NULL, prefix = NULL) {
  flog.debug("Getting most recent file.")
  
  files <- findFiles(dtype, prefix)
  if ( is.null(files) ) { warning("No files were found"); return( NULL ) }
  times <- file.mtime(files)
  df    <- data.frame("files"=files, "times"=times)
  df    <- df[rev(order(df$times)),]
  mostRecent <- as.character(df[1, "files"])
  return( mostRecent )
}


##############################################################################################################################
# Load Items From File
##############################################################################################################################
loadFile <- function(filename) {
  flog.info(paste("Loading file:",filename))
  
  if ( file.exists(filename) ) {
    flog.debug(paste("Loading",filename,"..."))
    lvals <- load(filename)
    flog.debug(paste("Loading",filename,"... Done"))
    flog.debug(paste("Found the following items:",paste(lvals, collapse = ", ")))
    retvals <- lapply(lvals, function(x) get0(x))
    names(retvals) <- lvals
    return( retvals )
  } else {
    flog.error(paste("The file",filename,"does not exist."))
    return( NULL )
  }
}


##############################################################################################################################
# Load Test/Train/Truth Data
##############################################################################################################################
saveSplitData <- function(dataset = NULL, traindata = NULL, testdata = NULL, truthdata = NULL, targetcol = NULL) {
  if ( is.null(dataset) ) { dataset <- get0("dataset") }
  flog.info("Saving split dataset with name:",dataset)
  
  savename <- file.path(mvadir, "output", paste("data", dataset, make.names(Sys.time()), "rData", sep = "."))
  flog.info(paste("Saving train/test/truth data to", savename,"..."))
  save(traindata, testdata, truthdata, targetcol, file = savename, compress = T)
  flog.info(paste("Saving train/test/truth data to", savename,"... Done"))
  flog.info(paste("File has size:",formatMemory(file.size(savename))))
}

loadSplitData <- function(dataset, forceReLoad = F) {
  flog.info("Loading Split Dataset with name:",dataset)
  
  lvals <- c("traindata", "testdata", "truthdata", "targetcol")
  if ( forceReLoad | !all(sapply(lvals, function(x) exists(x))) ) {
    filename <- getRecentFile("data", dataset)
    retvals <- loadFile(filename)
  } else {
    flog.info(paste(paste(lvals, collapse = ", "),"all exist. Returning them."))
    retvals <- sapply(lvals, function(x) get(x))
  }
  showMem(retvals)
  return( retvals )
}


##############################################################################################################################
# Load Fits
##############################################################################################################################
saveFits <- function(dataset = NULL, fits = NULL, times = NULL) {
  if ( is.null(dataset) ) { dataset <- get0("dataset") }
  flog.info("Saving fits for dataset:",dataset)
  
  savename <- file.path(mvadir, "output", paste("fits", dataset, make.names(Sys.time()), "rData", sep = "."))
  showMem(fits)
  flog.info(paste("Saving fits/time to", savename,"..."))
  save(fits, times, file = savename, compress = T)
  flog.info(paste("Saving fits/time to", savename,"... Done"))
  flog.info(paste("File has size:",formatMemory(file.size(savename))))
}

loadFits <- function(dataset, forceReLoad = T) {
  flog.info(paste("Loading Fits for dataset:",dataset))
  
  lvals <- c("fits", "times")
  if ( forceReLoad | !all(sapply(lvals, function(x) exists(x))) ) {
    filename <- getRecentFile("fits", dataset)
    retvals <- loadFile(filename)
  } else {
    flog.info(paste(paste(lvals, collapse = ", "),"all exist. Returning them."))
    retvals <- sapply(lvals, function(x) get(x))
  }
  showMem(retvals)
  return( retvals )
}


##############################################################################################################################
# Load Tests
##############################################################################################################################
saveTests <- function(dataset = NULL, tests = NULL, times = NULL) {
  if ( is.null(dataset) ) { dataset <- get0("dataset") }
  flog.info("Saving tests for dataset:",dataset)
  
  savename <- file.path(mvadir, "output", paste("tests", dataset, make.names(Sys.time()), "rData", sep = "."))
  showMem(tests)
  flog.info(paste("Saving tests/time to", savename,"..."))
  save(tests, times, file = savename, compress = T)
  flog.info(paste("Saving tests/time to", savename,"... Done"))
  flog.info(paste("File has size:",formatMemory(file.size(savename))))
}

loadTests <- function(dataset, forceReLoad = T) {
  flog.info(paste("Loading Tests for dataset:",dataset))
  
  lvals <- c("tests", "times")
  if ( forceReLoad | !all(sapply(lvals, function(x) exists(x))) ) {
    filename <- getRecentFile("tests", dataset)
    retvals <- loadFile(filename)
  } else {
    flog.info(paste(paste(lvals, collapse = ", "),"all exist. Returning them."))
    retvals <- sapply(lvals, function(x) get(x))
  }
  if ( is.null(retvals[["testResults"]]) ) {
    retvals[["testResults"]] <- retvals[["tests"]]
    retvals[["tests"]] <- NULL
  }
  showMem(retvals)
  return( retvals )
}


##############################################################################################################################
# Sample dataset
##############################################################################################################################
sampleData <- function(fdata, sFraction) {
  flog.info(paste("Sampling data of size:",getDimStr(fdata),"with fraction:",sFraction))

  keep <- sample(nrow(fdata), size = as.integer(sFraction*nrow(fdata)), replace = T)
  retdata <- fdata[keep,]

  flog.info(paste("  New data size is:",getDimStr(retdata)))

  return( retdata )
}




############################################################################################################
# Get Sep Length (if any)
############################################################################################################
getSepLength <- function(line, sep, ...) {
  options    <- unlist(list(...))
  debug      <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  vals    <- gregexpr(pattern = sep, text = line)[[1]]
  nSep <- NULL
  if ( length(vals) > 1 ) {
    nSep <- 1
    for ( i in seq(vals) ) {
      diff <- vals[i+1] - vals[i]
      if ( diff > 1 ) { break }
      else            { nSep = nSep + 1 }
    }
  }
  return( nSep )
}


############################################################################################################
# Get Sep Lengths
############################################################################################################
getSepLengths <- function(line, ...) {
  options    <- unlist(list(...))
  debug      <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  seps <- c(",", " ", "\t", ";", ":")
  for ( sep in seps ) {
    sepL <- getSepLength(line = line, sep = sep, options)
    if ( !is.null(sepL) ) { 
      sepVal <- paste(rep(sep, length.out=sepL), collapse = "")
      return( sepVal )
      break
    }
  }
  return( NULL )
}


############################################################################################################
# Split Line by Sep
############################################################################################################
splitLineBySep <- function(line, sep, ...) {
  options    <- unlist(list(...))
  debug      <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  if ( !is.null(sep) ) {
    strVals    <- strsplit(x = line, split = sep)[[1]]
    return( strVals )
  } else {
    stop(paste("No separator in line:",line))
  }
}

splitLine <- function(line, ...) {
  options    <- unlist(list(...))
  debug      <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  sep        <- getSepLengths(line, options)
  return( splitDataBySep(line, sep, options) )
}
