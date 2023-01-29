FROM rocker/shiny:4.2
RUN apt-get update && apt-get install -y libssl-dev libxml2-dev build-essential \
  libicu-dev

ADD install.R /tmp/

RUN R -f /tmp/install.R

COPY / /opt/eposmol
RUN chown -R shiny.shiny /opt/eposmol
RUN cd /opt/

RUN mkdir -p /var/log/shiny-server
RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny
RUN mkdir -p /srv/shiny-server

RUN rm -r /srv/shiny-server/*
RUN ln -s /opt/eposmol/R /srv/shiny-server/eposmol

COPY shiny-server.sh /usr/bin/shiny-server.sh

RUN chmod +x /usr/bin/shiny-server.sh

EXPOSE 3838
