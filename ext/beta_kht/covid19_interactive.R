library(ggrepel)

covid19_interactive_ui <- function(id, config){

  ns <- NS(id)

  fluidPage(
    includeHTML("www/topbtn.html"),
    includeScript("www/topbtn.js"),
    includeCSS("www/topbtn.css"),
    fluidRow(
      br(),
      p("Under vil du se noen figurer der du kan sammenlikne forskjellige geografiske områder.", br(),
        "Begynn å skriv navnet på ønsket kommune eller fylke så vil det automatisk komme opp alternativer."),
      br(),
      ),

    fluidRow(
      width = 12, align = "center",
      selectizeInput(ns("int_input_location"), "Geografisk område: ",
                     choices = config$choices_location,
                     multiple = TRUE,
                     selected = NULL,
                     options = list(placeholder = "Skriv områder du vil sammenligne her",
                                    maxItems = 10,
                                    onInitialize = I('function() {this.setValue("");}')

                                    ),
                     size = "600px")
    ),

    fluidRow(
      column(
        width = 6, align = "right",
        checkboxInput(ns("cumulativ_chk"), "Kumulativ", value = FALSE)
      ),
      column(
      width = 6, align = "left",
      actionButton(ns("reset_btn"), "Nullstill",
                   icon = icon("redo")))
    ),

    br(),
    br(),

    fluidRow(
      width = 12, align = "left",
      shinycssloaders::withSpinner(plotOutput(ns("msis_plot"), height = "600px"))
    ),
    br(),
    br(),

    fluidRow(
      width = 12, align = "left",
      shinycssloaders::withSpinner(plotOutput(ns("norsyss_plot"), height = "600px"))
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


  output$norsyss_plot <- renderCachedPlot({

    covid19_int_norsyss(location_codes = int_loc$locations,
                        config = config)

  }, cacheKeyExpr = {list(
    input$int_input_location,
    input$reset_btn,
    dev_invalidate_cache
  )},
  res = 72
  )


}



covid19_int_msis <- function(location_codes, config){

  d <- pool %>% dplyr::tbl("data_covid19_msis_by_time_location") %>%
    dplyr::filter(granularity_time == "week") %>%
    dplyr::filter(location_code %in% !!location_codes) %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(location_code, yrwk, n) %>%
    dplyr::collect()

  setDT(d)
  setkey(d, location_code, yrwk)
  d[,cum_n := cumsum(n), by=.(location_code)]

  ## for reordering the yrwk
  d[, rank := 1:.N, by = .(location_code)]

  d_p <- fhidata::norway_population_b2020[year==2020,.(
    pop=sum(pop)
  ),keyby=.(location_code)]

  d[
    d_p,
    on="location_code",
    pop:=pop
  ]

  d[,pr1000_cum_n := 1000*cum_n/pop]

  covid19_int_gen_plot(d = d,
                       labs_title = "Kummulativt antall tilfeller av covid-19\n Data fra MSIS",
                       labs_caption = "År-ukenummer",
                       labs_y = "pr. 1000 innbyggere")

}


covid19_int_norsyss <- function(location_codes, config){

  d <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::filter(location_code %in%!!location_codes)%>%
    dplyr::filter(granularity_time=="day")%>%
    dplyr::filter(tag_outcome %in% "covid19_vk_ote") %>%
    dplyr::filter(age=="total") %>% # yusman, remove this
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(yrwk, location_code, n, consult_with_influenza) %>%
    dplyr::group_by(location_code, yrwk) %>%
    dplyr::summarize(n=sum(n), consult_with_influenza=sum(consult_with_influenza)) %>%
    dplyr::collect()

  setDT(d)
  setkey(d, location_code, age, yrwk)

  d[,censor := ""]
  d[censor=="" & n>0 & n<5, censor := "N"]
  d[censor != "", n := 0]

  d[,cum_n := cumsum(n), by=.(location_code, age)]

  ## for reordering the yrwk
  d[, rank := 1:.N, by = .(location_code)]

  d_p <- fhidata::norway_population_b2020[year==2020,.(
    pop=sum(pop)
  ),keyby=.(location_code)]

  d[
    d_p,
    on="location_code",
    pop:=pop
  ]

  d[,pr1000_cum_n := 1000*cum_n/pop]


  covid19_int_gen_plot(d = d,
                       labs_title = "Kummulativt antall konsultasjoner med mistenkt, sannsynlig eller bekreftet covid-19 (R991 og R992)\n Data fra NorSySS",
                       labs_caption = "År-ukenummer",
                       labs_y = "pr. 1000 innbyggere")

}





covid19_int_gen_plot <- function(
                                 d = NULL,
                                 legend_position = "none",
                                 labs_title = NULL,
                                 labs_caption = NULL,
                                 labs_x = NULL,
                                 labs_y = NULL
                                 ){


  max_x_date <- max(d$rank)
  min_x_date <- min(d$rank)
  min_x_date_extra <- min_x_date * 0.8
  max_x_date_extra <- max_x_date * 1.2

  ## areas names
  sub_data <- unique(d$location_code)
  sub_location <- config$choices_location[config$choices_location %in% sub_data]
  df_location <- stack(sub_location)
  setDT(df_location)

  d[df_location,
    on = list(location_code = values),
    ind_name := ind]

  d[, loc_name := gsub("\\s*\\(.*?\\)$", "", ind_name)]


  ## plot
  max_y <- max(d$pr1000_cum_n)
  max_y_extra <- 1.05 * max_y

  q <- ggplot(d, aes(x = yrwk, y = pr1000_cum_n)) +
    geom_line(aes(color = location_code, group = location_code), size = 2)

  q <- q + fhiplot::theme_fhi_lines(
    base_size = 20,
    panel_on_top = FALSE,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

  q <- q + theme(legend.position = legend_position)

  q <- q + fhiplot::scale_color_fhi()
  q <- q + fhiplot::set_x_axis_vertical()
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + coord_cartesian(ylim=c(0, max_y_extra), xlim = c(min_x_date_extra, max_x_date_extra), clip="off", expand = F)

  q <- q + labs(title = labs_title)
  q <- q + labs(caption = labs_caption)
  q <- q + labs(x = labs_x, y = labs_y)

  q <-  q + annotate(geom = "rect",
                     xmin = max_x_date * 1.02,
                     xmax = max_x_date * 1.3,
                     ymin = -0.5, ymax = Inf, #still not able to cover the x-axis line?
           fill = "white")


  q <- q + geom_point(
    data = subset(d, yrwk == max(yrwk)),
    aes(color = location_code),
    size = 4,
    show.legend = FALSE
  )

   q <- q + geom_point(
    data = subset(d, yrwk == min(yrwk)),
    aes(color = location_code),
    size = 4,
    show.legend = FALSE
  )

  q <- q + ggrepel::geom_text_repel(
    data = subset(d, yrwk == max(yrwk)),
    mapping = aes(label = loc_name),
    hjust = 0,
    size = 7,
    direction = "y",
    nudge_x = 0.5,
    show.legend = FALSE)

  q

}
