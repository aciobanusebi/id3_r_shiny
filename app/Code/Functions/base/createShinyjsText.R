function(directory) {
  files <- list.files(directory,pattern="shinyjs.js",recursive = TRUE,full.names = TRUE)
  text <- ""
  for(file in files) {
    text <- paste0(text,Functions$readWholeFile(file),sep="\r\n")
  }
  text
}
