importIntoEnv <- function(pathToDir, fileNameGivesName = TRUE) {
  e <- new.env()
  files <- list.files(pathToDir, recursive = TRUE, full.names = TRUE)
  for(file in files) {
    if(fileNameGivesName) {
      base <- basename(file)
      base <- substr(base,1,nchar(base)-2)
      e[[base]] <- source(file,local = TRUE)[[1]]
    } else {
      source(file, local = e)
    }
  }
  e
}