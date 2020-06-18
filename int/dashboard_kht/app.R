## app.R ##
library(shiny)
library(shinyjs)
library(shinycssloaders)


## shinycssloaders global optios
options(list(spinner.type = 6, spinner.color = "#027357"))

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
source("no_data.R")
source("covid19.R")
source("covid19_comparison.R")
source("covid19_modelling.R")
source("norsyss.R")
source("norsyss_overview.R")
source("norsyss_weekly.R")
source("norsyss_daily.R")
source("norsyss_information.R")

ui <- function(request){
    tagList(
    tags$head(
      tags$title("Sykdomspulsen"),
      #tags$meta(`http-equiv`="Content-Security-Policy", content="default-src 'self'; script-src 'unsafe-inline' https://www.googletagmanager.com https://sykdomspulsen2.fhi.no; connect-src wss://sykdomspulsen2.fhi.no"),
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
               id = "navbar",
               title = div(img(id="logo",src="fhi.svg", height="40px"), "Sykdomspulsen for kommunehelsetjenesten"),
               tabPanel("Covid-19",
                        value="covid19",
                        covid19_ui("covid19", config=config)
               ),
               tabPanel("NorSySS",
                        value="norsyss",
                        norsyss_ui("norsyss", config=config)
               ),
               theme = "fhi.css"
             )
    )
  )
}

server <- function(input, output, session) {
  # we need to update the config dates
  auto_invalidate_1h <- reactiveTimer(1000*60*60) # invalidates every hour
  observe({
    auto_invalidate_1h()
    config_update_dates(config = config)
  })

  observe({
    query <- parseQueryString(session$clientData$url_search)
    selected <- config$choices_location[1]
    if (!is.null(query[['location_code']])) if(query[['location_code']] %in% fhidata::norway_locations_long_b2020$location_code){
      selected <- query[['location_code']]
    }
    updateSelectizeInput(
      session,
      "covid19-covid_location_code",
      choices=config$choices_location,
      selected = selected
    )
  }, priority=10000)


  # updateSelectizeInput(
  #       session,
  #       "covid19-covid_location_code",
  #       choices=config$choices_location,
  #       selected = "municip0301"
  #     )


  # observe({
  #   query <- parseQueryString(session$clientData$url_search)
  #   if (!is.null(query[['page']])) {
  #     updateNavbarPage(session, "navbar", selected = query[["page"]])
  #     #updateSliderInput(session, "bins", value = query[['bins']])
  #     if(query[["page"]]=="covid19"){
  #
  #     }
  #   }
  #
  #
  #   selected <- config$choices_location[1]
  #   if (!is.null(query[['location_code']])) if(query[['location_code']] %in% fhidata::norway_locations_long_b2020$location_code){
  #     selected <- query[['location_code']]
  #   }
  #
  #   updateSelectizeInput(
  #     session,
  #     "covid19-covid_location_code",
  #     choices=config$choices_location,
  #     selected = selected
  #   )
  #
  # })

  # observeEvent(
  #   {
  #     input$`covid19-covid_location_code`
  #     input$navbar
  #   }, {
  #     to_replace = glue::glue(
  #       "?page={input$navbar}&location_code={input$`covid19-covid_location_code`}"
  #     )
  #     updateQueryString(to_replace, mode = "replace")
  #   }
  # )

  callModule(covid19_server, "covid19", config=config)
  callModule(covid19_comparison_server, "covid19_comparison", config = config)
  callModule(covid19_modelling_server, "covid19_modelling", config=config)

  callModule(norsyss_server, "norsyss", config=config)
  callModule(norsyss_overview_server, "norsyss_overview", config=config)
  callModule(norsyss_weekly_server, "norsyss_weekly", config=config)
  callModule(norsyss_daily_server, "norsyss_daily", config=config)
  callModule(norsyss_purpose_server, "norsyss_purpose", config=config)


  if (.Platform$OS.type == "windows"){
    session$onSessionEnded(stopApp)
  }

}

shinyApp(ui, server, enableBookmarking = "url")

#  shiny::runApp('inst/shiny/corona', port = 4989, host = "0.0.0.0")


