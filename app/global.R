# mandatory when using shinyapps.io:
try(source("libraries.R"))
file.remove("libraries.R")
file.remove("RUN_install.txt")
file.remove("Dockerfile")

pathVariables <- "Code/Variables"
pathFunctions <- "Code/Functions"
pathClasses <- "Code/Classes"
pathCodeFiles <- "Code/CodeFiles"
pathData <- "Data"
pathOthers <- "Others"
pathPackages <- "packages.txt"

source("importIntoEnv.R")
Functions <- importIntoEnv(pathFunctions)

if(!Functions$isInstalled("R6")) {
  install.packages("R6")
}

library(R6)

Classes <- importIntoEnv(pathClasses)

Functions$importPackages("packages.txt")

CodeFiles <- Functions$createTreeEnvFromDir(pathCodeFiles)

Modules <- new.env()
Functions$createModulesFromDir(Modules, "shiny/Modules")

Functions$createDockerfile()

Data <- Functions$createTreeEnvFromDir(pathData)

Others <- Functions$createTreeEnvFromDir(pathOthers,"*")

Variables <- importIntoEnv(pathVariables, fileNameGivesName = FALSE)

Variables$toIncludeList <- Functions$createToIncludeList()

# options:
options(shiny.maxRequestSize = 30 * 1024 ^ 2) # 30 MB max file size

#### ADDED
insertSource(Others$visTree.R,package = "visNetwork",functions = "visTree")
source(CodeFiles$functions.R)
source(CodeFiles$functions3.R)