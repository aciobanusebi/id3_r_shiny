function() {
  if(grepl(".shinyjs",Functions$createShinyjsText("shiny"))) {
    deps <- tagList(
      div(class = "loading"),
      
      useToastr(),
      useShinyjs(),
      useSweetAlert(),
      useShinyFeedback(),
      
      extendShinyjs(text = Functions$createShinyjsText("shiny"))
    )
  } else {
    deps <- tagList(
      div(class = "loading"),
      
      useToastr(),
      useShinyjs(),
      useSweetAlert(),
      useShinyFeedback()
    )
  }
  
  path <- "src"
  if(length(list.files(path,pattern = "*(.css|.js)",recursive = TRUE)) != 0) {
    deps <- tagList(
      deps,
      htmlDependency(
        name = "global", 
        version = "1", 
        src = path,
        stylesheet = list.files(path,pattern = "*.css",recursive = TRUE),
        script = list.files(path,pattern = "*.js",recursive = TRUE)
      )
    )
  }
  
  if(length(ls(Modules)) != 0) {
    for(i in 1:length(ls(Modules))) {
      name <- ls(Modules)[i]
      path <- file.path(Modules[[name]]$dir,"src")
      if(length(list.files(path,pattern = "*(.css|.js)",recursive = TRUE)) != 0) {
        deps <- tagList(
          deps,
          htmlDependency(
            name = name, 
            version = "1", 
            src = path,
            stylesheet = list.files(path,pattern = "*.css",recursive = TRUE),
            script = list.files(path,pattern = "*.js",recursive = TRUE)
          )
        )
      }
    }
  }
  
  deps
}
