FROM rhub/r-minimal:4.4
ARG TARGETARCH
# install development stack, libraries and R packages
RUN installr -d -e \
  -a "cairo font-liberation git libcurl libgit2 libxml2 fontconfig fribidi harfbuzz freetype libpng tiff libjpeg icu-libs zlib" \
  -t "curl-dev libxml2-dev libgit2-dev linux-headers gcc g++ gfortran fontconfig-dev fribidi-dev harfbuzz-dev freetype-dev libpng-dev tiff-dev zlib-dev cairo-dev automake libtool m4 autoconf linux-headers" \
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
  knitr \
  bioc::rgoslin
# add a user called shiny and create a directory for the application
RUN adduser -D shiny && mkdir -p /app && chown shiny.shiny /app && chmod 755 /app
USER shiny
# copy the source code to the application directory and clean up after ourselves
COPY . /app/
# make shiny server port 3838 available to the outside of the docker container
EXPOSE 3838
# set the working directory to the application directory
WORKDIR /app
# run the shiny-server command as the default command to run when the container starts
CMD ["R", "-e", "shiny::runApp(host = '0.0.0.0', port = 3838)"]
