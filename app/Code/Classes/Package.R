# abstract class
R6Class(
  "Package",
  private = list(
    name = NULL
  ),
  public = list(
    initialize = function(name) {
      # check if abstract class
      Functions$imposeAbstractness(self,"Package")
      
      private$name <- name
      if(!self$isInstalled()) {
        self$install()
      }
      library(name,character.only = TRUE)
      
      line <- paste0("library(",name,")")
      write(line,file="libraries.R",append=TRUE)
      
      self$writeInstallLine()
    },
    
    isInstalled = function() {
      private$name %in% rownames(installed.packages())
    }
  )
)
