function() {
  text1 <- "
FROM openanalytics/r-base

# system libraries of general use
RUN apt-get update && apt-get install -y \\
  sudo \\
  pandoc \\
  pandoc-citeproc \\
  libcurl4-gnutls-dev \\
  libcairo2-dev \\
  libxt-dev \\
  libssl-dev \\
  libssh2-1-dev \\
  libssl1.0.0
  
# system library dependency for the euler app
RUN apt-get update && apt-get install -y \\
  libv8-3.14-dev # for V8
  
# basic shiny functionality
RUN R -e \"install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')\"
  
# install dependencies of the apps
RUN R -e \"install.packages('Rmpfr', repos='https://cloud.r-project.org/')\"
"
  text2 <- paste0(readLines("RUN_install.txt"),collapse = "\n")
  appName <- basename(getwd())
  text3 <- paste0("

# copy the app to the image
RUN mkdir /root/",appName,"
COPY new_app /root/",appName,"

COPY Rprofile.site /usr/lib/R/etc/


EXPOSE 3838

CMD [\"R\", \"-e\", \"shiny::runApp('/root/",appName,"')\"]
")
  text <- paste0(text1,text2,text3)
  write(text,file="Dockerfile")
}