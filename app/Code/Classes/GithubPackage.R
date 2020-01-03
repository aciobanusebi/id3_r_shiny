R6Class(
  "GithubPackage",
  inherit = Classes$Package,
  private = list(
    githubLocation = NULL
  ),
  public = list(
    initialize = function(name, githubLocation) {
      private$githubLocation <- githubLocation
      super$initialize(name)
    },
    
    install = function() {
      Classes$CranPackage$new("devtools") # to install + include devtools
      install_github(private$githubLocation)
    },
    
    writeInstallLine = function() {
      line <- paste0("RUN R -e \"devtools::install_github('",private$githubLocation,"')\"")
      write(line,file="RUN_install.txt",append=TRUE)
    }
  )
)