FROM rocker/shiny:4.3.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev libcurl4-openssl-dev libssl-dev libfontconfig1-dev \
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r rhino box sass httr jsonlite dplyr tidyr lubridate \
    reactable googlesheets4 gargle later bslib

COPY . /srv/shiny-server/app

WORKDIR /srv/shiny-server/app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('.', port=as.integer(Sys.getenv('PORT',3838)), host='0.0.0.0')"]
