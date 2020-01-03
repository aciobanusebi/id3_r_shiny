function(pathDir,pattern="*.R") {
  e <- new.env()
  files <- list.files(pathDir,pattern=pattern,recursive = TRUE)
  for(file in files) {
    iteratedE <- e
    splitPath <- strsplit(file,"/")[[1]]
    for(i in 1:length(splitPath)) {
      if(i == length(splitPath)) {
        iteratedE[[splitPath[i]]] <- file.path(pathDir,file)
      } else {
        envName <- splitPath[i]
        if(is.null(iteratedE[[envName]])) {
          iteratedE[[envName]] <- new.env()
        }
        iteratedE <- iteratedE[[envName]]
      }
    }
  }
  e
}