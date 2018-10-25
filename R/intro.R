sourcefiles <- c("categories", "helper", "lib", "plot", "ggplot", "load", "date", "test", "matrix", "forecast", "ts")
for ( sourcefile in sourcefiles ) {
  filename <- paste("R", paste(sourcefile, "R", sep = "."), sep = "/")
  if ( file.exists(filename) ) {
    writeLines(paste("Source:",filename))
    source(filename)
  } else {
    writeLines(paste("Could not find",filename))
  }
}