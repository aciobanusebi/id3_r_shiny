R6Class(
  "CranPackage",
  inherit = Classes$Package,
  private = list(),
  public = list(
    install = function() {
      install.packages(private$name)
    },
    
    writeInstallLine = function() {
      line <- paste0("RUN R -e \"install.packages('",private$name,"', repos='https://cloud.r-project.org/')\"")
      write(line,file="RUN_install.txt",append=TRUE)
    }
  )
)