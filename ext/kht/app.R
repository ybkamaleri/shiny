## app.R ##
library(shiny)
library(shinyjs)

if (.Platform$OS.type == "windows"){
  makeActiveBinding('dev_invalidate_cache', function() {
    lubridate::now()
  }, .GlobalEnv)
} else {
  shinyOptions(cache = diskCache("/tmp/", max_size = 50e6, max_age = 60*60)) # 1 hour

  makeActiveBinding('dev_invalidate_cache', function() {
    paste0(lubridate::today(), "/", lubridate::hour(lubridate::now()))
  }, .GlobalEnv)
}


source("global.R")
source("covid19.R")
source("norsyss.R")
source("norsyss_overview.R")
source("norsyss_weekly.R")
source("norsyss_daily.R")
source("norsyss_information.R")

ui <- tagList(
  tags$head(
    #tags$meta(`http-equiv`="Content-Security-Policy", content="default-src 'self'; script-src 'unsafe-inline' https://www.googletagmanager.com https://sykdomspulsen2.fhi.no; connect-src wss://sykdomspulsen2.fhi.no"),
    tags$meta(`http-equiv`="Content-Security-Policy", content="default-src * 'unsafe-inline' 'unsafe-eval'; script-src * 'unsafe-inline' 'unsafe-eval'; connect-src wss://* https://* 'unsafe-inline'; img-src * data: 'unsafe-inline'"),
    includeHTML(("google_analytics.html"))
  ),
  useShinyjs(),
  tags$style("
  .container{
    width: 1200px;
    }
 "),
  tags$div(class="container",
           navbarPage(
             title = div(img(id="logo",src="fhi.svg", height="40px"), "Sykdomspulsen for kommunehelsetjenesten"),
             tabPanel("Covid-19",
                      covid19_ui("covid19", config=config)
             ),
             tabPanel("NorSySS",
                      norsyss_ui("norsyss", config=config)
             ),
             theme = "fhi.css"
           )
  )
)

server <- function(input, output) {
  # we need to update the config dates
  auto_invalidate_1h <- reactiveTimer(1000*60*60) # invalidates every hour
  observe({
    auto_invalidate_1h()
    config_update_dates(config = config)
  })


  callModule(covid19_server, "covid19", config=config)

  callModule(norsyss_server, "norsyss", config=config)
  callModule(norsyss_overview_server, "norsyss_overview", config=config)
  callModule(norsyss_weekly_server, "norsyss_weekly", config=config)
  callModule(norsyss_daily_server, "norsyss_daily", config=config)
  callModule(norsyss_purpose_server, "norsyss_purpose", config=config)
}

shinyApp(ui, server)

#  shiny::runApp('inst/shiny/corona', port = 4989, host = "0.0.0.0")


