covid19_interactive_ui <- function(id, config){

  ns <- NS(id)

  fluidPage(
    useShinyjs(),
    fluidRow(
      br(),
      p("Dette er interaktiv side", br(),
        "Bla... bla.. bla.."),
      br(),
      ),

    fluidRow(
      width = 12, align = "center",
      selectizeInput(ns("int_input_location"), "Geografisk områder: ",
                     choices = config$choices_location,
                     multiple = TRUE,
                     ## selected = c("norge", "municip0301")
                     selected = NULL,
                     options = list(placeholder = "Skrev områder du vil sammenligne her",
                                    maxItems = 6,
                                    onInitialize = I('function() {this.setValue("");}')

                                    ),
                     size = "600px")
    ),

    fluidRow(
      width = 12, align = "center",
      actionButton(ns("reset_btn"), "Nullstille",
                   icon = icon("redo"))
    ),

    br(),

    fluidRow(
      width = 12, align = "left",
      br(),
      shinycssloaders::withSpinner(plotOutput(ns("msis_plot"), height = "600px")),
      br()

    ),

    br(),
    br(),
    br()
  )
}


covid19_interactive_server <- function(input, output, session, config){

  int_loc <- reactiveValues()

  observe({
    if (is.null(input$int_input_location)) {
      int_loc$locations <- "norge"
    } else {
      int_loc$locations <- input$int_input_location
    }
  })

  observeEvent(input$reset_btn, {
    int_loc$locations <- "norge"
    reset("int_input_location")
  })

  output$msis_plot <- renderCachedPlot({

    covid19_int_msis(location_codes = int_loc$locations,
                     config = config)

  }, cacheKeyExpr = {list(
    input$int_input_location,
    input$reset_btn,
    dev_invalidate_cache
  )},
  res = 72
  )

}



covid19_int_msis <- function(location_codes,
                             config){


  d <- pool %>% dplyr::tbl("data_covid19_msis_by_time_location") %>%
    dplyr::filter(granularity_time == "day") %>%
    dplyr::filter(location_code %in% !!location_codes) %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(location_code, date, n) %>%
    dplyr::collect()


  setDT(d)
  setorder(d, location_code, date)
  d[,cum_n := cumsum(n), by=.(location_code)]
  d[, date := as.Date(date)]

  d_p <- fhidata::norway_population_b2020[year==2020,.(
    pop=sum(pop)
  ),keyby=.(location_code)]

  d[
    d_p,
    on="location_code",
    pop:=pop
  ]

  d[,pr1000_cum_n := 1000*cum_n/pop]

  q <- covid19_int_gen_plot(d = d)
  q

}


covid19_int_gen_plot <- function(
                                 d = NULL,
                                 legend_position = "bottom",
                                 labs_title = " ",
                                 labs_caption = " "

                                 ){


  max_y <- max(d$pr1000_cum_n)

  q <- ggplot(d, aes(x = date, y = pr1000_cum_n)) +
    geom_line(aes(color = location_code, group = location_code), size = 3)

  q <- q + scale_x_date(
      "",
      date_breaks = "2 days",
      date_labels = "%d.%m"
    )


  q <- q + fhiplot::theme_fhi_lines(
    20, panel_on_top = T,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend_position = legend_position
  )

  q <- q + fhiplot::scale_color_fhi()
  q <- q + fhiplot::set_x_axis_vertical()
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + coord_cartesian(ylim=c(0, max_y), clip="off", expand = F)
  q <- q + labs(title = labs_title)
  q <- q + labs(caption = labs_caption)
  q <- q + guides(guide_legend(nrow = 2))
  q
}




## covid19_tables <- c("data_covid19_msis_by_time_location",
##                     "data_norsyss",
##                     "data_covid19_lab_by_time")

## location_codes <- c("norge", "municip0301")
## select_var <- c("location_code", "date", "n")

## dt <- pool %>% dplyr::tbl("data_covid19_msis_by_time_location") %>%
##   dplyr::filter(granularity_time == "day") %>%
##   dplyr::filter(location_code %in% !!location_codes) %>%
##   dplyr::filter(date >= !!config$start_date) %>%
##   dplyr::select(location_code, date, n) %>%
##   dplyr::collect()
