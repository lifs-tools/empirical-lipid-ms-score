FROM rocker/r-ver:4.2
RUN apt-get update && apt-get install -y \
  --no-install-recommends \
  libssl-dev \
  libxml2-dev \
  build-essential \
  libicu-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

RUN /rocker_scripts/install_shiny_server.sh

RUN install2.r --error --skipinstalled \
  openxlsx \
  readxl \
  shinyjs \
  dplyr \
  tidyr \
  stringr \
  htmltools \
  devtools

COPY install.R /tmp/
RUN R -f /tmp/install.R

COPY / /opt/eposmol
RUN rm -f /opt/eposmol/.dev
RUN chown -R shiny.shiny /opt/eposmol
RUN cd /opt/

RUN mkdir -p /var/log/shiny-server
RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny
RUN mkdir -p /srv/shiny-server

RUN rm -r /srv/shiny-server/*
RUN ln -s /opt/eposmol/R /srv/shiny-server/eposmol

USER shiny

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
