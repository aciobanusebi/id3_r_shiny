function(fileWithPackages) {
  con <- file(fileWithPackages, "r")
  while(TRUE) {
    line <- readLines(con, n=1)
    if(length(line)==0) {
      break
    }
    args <- strsplit(line," ")[[1]]
    if(!grepl("Package",args[1])) {
      stop("Class should be a subclass of Package...")
    }
    clazz <- Classes[[args[1]]]
    args <- args[2:length(args)]
    # create new object
    do.call(clazz$new, as.list(args))
  }
  close(con)
}
