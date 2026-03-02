FROM rocker/shiny:4.3.1

RUN install2.r bs4Dash httr jsonlite dplyr tidyr lubridate DT reactable googlesheets4 gargle

COPY . /srv/shiny-server/app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app', port=as.integer(Sys.getenv('PORT',3838)), host='0.0.0.0')"]
