#rm(list=ls())
source("R/helper.R")
source("R/lib.R")

setSuperCategories <- function(categories) {
    supercategories <- fromJSON(file="Perm/schema/supercatmap.json")
    categorytype <- fromJSON(file="Perm/schema/indivcattypes.json")
    
    accounts <- fromJSON(file="Perm/schema/accountmap.json")
    indivaccounts <- fromJSON(file="Perm/schema/indivaccountmap.json")
    
    #for ( i in 1:length(categoriesdata) ) { print(categories[[i]]) }
    
    ## Flattens list
    categorydata <- unlist(categoriesdata)
    
    
    retval     = rep(NA, length(categories))
    for(i in seq_along(categories)){
        cat <- pdata$category[[i]]
        retval[i] <- indivaccounts[cat][[1]]
     }

    return(retval)
}

invList <- function(allnames, alldata) {
  adata <- rep(NA,length(alldata))
  names(adata) <- allnames
  tdata <- rep(NA,length(alldata))
  names(tdata) <- allnames
  for ( i in seq_along(allnames) ) {
    groupname <- allnames[i]
    groupdata <- alldata[[i]][[groupname]]

    categorytypes <- try ( laply(groupdata, function(x) x[[1]]), silent = TRUE )
    if ( class(categorytypes) == "try-error" ) { categorytypes <- groupdata }
    
    categorynames <- try ( laply(groupdata, function(x) names(x)), silent = TRUE )
    if ( class(categorynames) == "try-error" ) { categorynames <- groupdata }
    
    if ( length(categorynames) == 1 ) { adata[groupname] <- categorynames }
    else { adata[groupname] <- list(categorynames) }

    if ( length(categorytypes) == 1 ) { tdata[groupname] <- categorytypes }
    else { tdata[groupname] <- list(categorytypes) }
  }
  return( list(adata,tdata) )
}


getCategoryData <- function(alldata) {
  groupnames <- laply(alldata, function(x) names(x))
  groupdata <- invList(groupnames, alldata)
  return( groupdata )
}

getGroupData <- function(alldata) {
  groupnames <- laply(alldata, function(x) names(x))
  sgnum <- which(groupnames == "SuperGroupings")
  categorydata <- invList(groupnames[-sgnum], alldata[-sgnum])
  sgdata <- alldata[[sgnum]]$SuperGroupings
  sgroupnames <- laply(sgdata, function(x) names(x))
  supergroupdata <- invList(sgroupnames, sgdata)
  return( list(categorydata, supergroupdata) )
}


##
## Invert categories yaml data
##
categoriesfile <- "Perm/schema/categories.yaml"
if ( file.exists(categoriesfile) ) { categoriesdata <- yaml.load_file(categoriesfile) }
categorydata <- getCategoryData(categoriesdata)


##
## Invert grouping yaml data
##
groupingfile <- "Perm/schema/groupings.yaml"
if ( file.exists(groupingfile) ) { allgroupingdata <- yaml.load_file(groupingfile) }
retvals <- getGroupData(allgroupingdata)
groupdata <- retvals[[1]]
supergroupdata <- retvals[[2]]


##
## Create data frame with category/group info
##
catdf <- data.frame()
for ( groupname in names(categorydata[[1]]) ) {
  groupcategories <- categorydata[[1]][[groupname]]
  groupcategorytypes <- categorydata[[2]][[groupname]]
  groupdf <- as.data.frame(matrix(NA, nrow=length(groupcategories), ncol=5))
  rownames(groupdf) <- groupcategories
  groupdf$V1 <- rep(groupname, length(groupcategories))
  groupdf$V2 <- groupcategorytypes
  colnames(groupdf)[[1]] <- "SuperCategory"
  colnames(groupdf)[[2]] <- "Type"
  if ( dim(catdf)[1] == 0 ) { catdf = groupdf }
  else { catdf <- rbind(catdf, groupdf) }
}


colnames(catdf)[3] <- "InOut"
colnames(catdf)[4] <- "SuperGroup"
colnames(catdf)[5] <- "Group"
for ( supergroupname in names(supergroupdata[[1]]) ) {
  print(supergroupname)
  sgroupdata <- supergroupdata[[1]][[supergroupname]]
  for ( groupname in sgroupdata ) {
    categories <- groupdata[[1]][[groupname]]
    categoryinouts <- groupdata[[2]][[groupname]]
    for ( j in 1:length(categoryinouts)) {
      category <- categories[[j]]
      inoutvalue <- categoryinouts[[j]]
      catdf[category,"InOut"] <- inoutvalue
    }
    for ( category in categories ) {
      catdf[category,"SuperGroup"] <- supergroupname
      catdf[category,"Group"] <- groupname
    }
  }
}


## Check for NA
nadf <- catdf[rowSums(is.na(catdf)) > 0,]
if ( dim(nadf)[1] > 0 ) {
  print(nadf)
  writeLines("Rows contain NA values")
  pause()
} else { writeLines("  --> Everything looks good with the categories.") }

writeLines("Saving categories data frame")
save(categorydata, groupdata, supergroupdata, catdf, file = "Rda/Categories.rda")