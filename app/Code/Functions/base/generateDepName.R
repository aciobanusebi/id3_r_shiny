function() {
  name <- paste0("dep",Variables$MY_HTML_DEPENDENCY_COUNT)
  Variables$MY_HTML_DEPENDENCY_COUNT <- Variables$MY_HTML_DEPENDENCY_COUNT + 1
  name
}