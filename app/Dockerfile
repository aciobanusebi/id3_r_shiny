
FROM openanalytics/r-base

# system libraries of general use
RUN apt-get update && apt-get install -y \
  sudo \
  pandoc \
  pandoc-citeproc \
  libcurl4-gnutls-dev \
  libcairo2-dev \
  libxt-dev \
  libssl-dev \
  libssh2-1-dev \
  libssl1.0.0
  
# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
  libv8-3.14-dev # for V8
  
# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"
  
# install dependencies of the apps
RUN R -e "install.packages('Rmpfr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('devtools', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('htmltools', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shiny', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyBS', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('bsplus', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyjs', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('V8', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinytoastr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinydashboard', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyWidgets', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('whisker', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyAce', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinydashboard', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinythemes', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyjqui', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyFeedback', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rlang', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('tibble', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('configr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rpart', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rpart.plot', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('visNetwork', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('formatR', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('sparkline', repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/id3App-oct-2019
COPY new_app /root/id3App-oct-2019

COPY Rprofile.site /usr/lib/R/etc/


EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/id3App-oct-2019')"]

