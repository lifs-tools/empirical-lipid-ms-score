FROM rocker/r-ver:4.3
RUN apt-get update && apt-get install -y \
  --no-install-recommends \
  libssl-dev \
  libxml2-dev \
  build-essential \
  libicu-dev \
  git \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

RUN /rocker_scripts/install_shiny_server.sh
# install precompiled packages
RUN install2.r --error --skipinstalled \
  openxlsx \
  readxl \
  shiny \
  shinyjs \
  dplyr \
  tidyr \
  stringr \
  htmltools \
  devtools \
  cicerone \
  purrr \
  jsonlite \
  markdown \
  DT \
  rintrojs \
  here \
  knitr
# copy and install any other packages in install.R, e.g. BioConductor ones
COPY install.R /tmp/
RUN R -f /tmp/install.R
# copy the source code to the application directory and clean up after ourselves
COPY / /opt/eposmol
RUN rm -rf /opt/eposmol/.dev \
  /opt/eposmol/renv \
  /opt/eposmol/renv.lock \
  /opt/eposmol/.Rprofile \
  /opt/eposmol/.Rhistory \
  /opt/eposmol/.RData
RUN chown -R shiny.shiny /opt/eposmol
RUN cd /opt/
# create the shiny server directories
RUN mkdir -p /var/log/shiny-server
RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny
RUN mkdir -p /srv/shiny-server
# remove the default applications and link eposmol into the shiny server directory
RUN rm -r /srv/shiny-server/*
RUN ln -s /opt/eposmol/ /srv/shiny-server/eposmol

USER shiny
# make shiny server port 3838 available to the outside of the docker container
EXPOSE 3838
# run the shiny-server command as the default command to run when the container starts
CMD ["/usr/bin/shiny-server"]
