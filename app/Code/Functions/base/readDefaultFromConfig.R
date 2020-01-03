function(file) {
  config <- eval.config(file=file, rcmd.parse = TRUE)
  config <- config[config!=""]
  config
}