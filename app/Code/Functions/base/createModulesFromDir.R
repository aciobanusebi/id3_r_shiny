function(e, pathToDir) {
  dirs <- list.dirs(pathToDir, full.names = TRUE, recursive = FALSE)
  for(dir in dirs) {
    if(dir == pathToDir) {
      next
    }
    base <- basename(dir)
    e[[base]] <- new.env()
    
    Functions$importPackages(file.path(dir,"packages.txt"))
    
    e[[base]]$dir <- dir
    e[[base]]$Functions <- importIntoEnv(file.path(dir,"Code","Functions"))
    e[[base]]$Classes <- importIntoEnv(file.path(dir,"Code","Classes"))
    e[[base]]$CodeFiles <- Functions$createTreeEnvFromDir(file.path(dir,"Code","CodeFiles"))
    e[[base]]$Data <- Functions$createTreeEnvFromDir(file.path(dir,"Data"))
    # e[[base]]$Others <- Functions$createTreeEnvFromDir(file.path(dir,"Others"))
    e[[base]]$ui <- Functions$sourceHere(file.path(dir,"ui.R"))
    e[[base]]$server <- Functions$sourceHere(file.path(dir,"server.R"))
    e[[base]]$Variables <- importIntoEnv(file.path(dir,"Code","Variables"), fileNameGivesName = FALSE)
  }
}