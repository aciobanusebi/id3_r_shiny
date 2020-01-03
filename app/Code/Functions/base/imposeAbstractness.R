function(obj, className) {
  if(class(obj)[1] == className) {
    message <- paste0(className," is Abstract class...")
    stop(message)
  }
}
