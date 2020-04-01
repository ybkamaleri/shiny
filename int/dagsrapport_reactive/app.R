jscode <-
  '
  var dimension = [0, 0];
  $(document).on("shiny:connected", function(e) {
    var jsWidth = screen.width;
    Shiny.onInputChange("GetScreenWidth",jsWidth);
    dimension[0] = window.innerWidth;
    dimension[1] = window.innerHeight;
    Shiny.onInputChange("dimension", dimension);
  });
  $(window).resize(function(e) {
    dimension[0] = window.innerWidth;
    dimension[1] = window.innerHeight;
    Shiny.onInputChange("dimension", dimension);
  });
'

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

source("desktop.R")
source("desktop_dagsrapport.R")

source("mobile.R")
source("mobile_dagsrapport.R")

ui <- tagList(
  useShinyjs(),
  tags$script(jscode),
  tags$style("
  .container-desktop{
    width: 1200px;
    };
  .container-mobile{
    width: 500px;
    }
 "),
  uiOutput("main")
)

server <- function(input, output) {
  observe({
    cat(input$GetScreenWidth, "\n")
    cat(input$dimension, "\n")
  })
  callModule(desktop_server, "desktop", config=config)
  callModule(desktop_dagsrapport_server, "desktop_dagsrapport", config=config)
  callModule(mobile_server, "mobile", config=config)
  callModule(mobile_dagsrapport_server, "mobile_dagsrapport", config=config)

  output$main <- renderUI({
    #ns <- session$ns
    req(input$dimension)

    if(input$dimension[1] > 500){
      desktop_ui("desktop", config=config)
    } else {
      mobile_ui("mobile", config=config)
    }
  })
}


shinyApp(ui, server)

#  shiny::runApp('inst/shiny/corona', port = 4989, host = "0.0.0.0")




