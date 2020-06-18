covid19_modelling_ui <- function(id, config) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width=12, align="left",

        p(
          strong("Under vil du se beregninger fra FHIs spredningsmodell."), br(),
          " En modell baseres på variablene som legges inn og matematiske beregninger.",
          " Tallene som vises er forventet spredning i et område.",
          " Den faktiske spredningen i det gitte området vil kunne avvike fra det som ble beregnet i modellen.",
          br(),br(),
          " Vi benytter en matematisk modell som simulerer spredningen av",
          "covid-19 i Norge over tid og sted. Modellen tar hensyn til",
          "befolkningsstrukturen i hver kommune, informasjon om bevegelser",
          "mellom kommunene (med basis i opplysninger fire ganger i døgnet om",
          "mobiltelefoners bevegelser mellom kommunene). Modellen er en såkalt",
          "SEIR-modell uten aldersfordeling, uten hensyn til demografiske endringer",
          "og med tilfeldig miksing mellom mennesker.", br(),

          "Modellen starter med at kjente tilfeller plasseres i tid og sted og dobles.",
          "I modellen beveger mennesker seg så gjennom stadiene mottakelig (S),",
          "eksponert og smittet, men ikke smittsom (E), smittsom (I) og immun (eller død)(R).",
          br(),br(),

          "Resultatene fra modellen er beheftet med usikkerhet på grunn av tilfeldighet i",
          "smittespredningen, tilfeldighet i mobilitet (om det er smittsomme eller mottakelige",
          "som reiser for eksempel) og usikkerhet i de tre kalibrerte parameterne.",
          "I tillegg er det flere kilder til usikkerhet som modellen ikke fanger opp,",
          "og vi tar ikke høyde for usikkerhet knyttet til modellens øvrige parametre.",
          "Modellen er en forenklet representasjon av virkeligheten og bygger på en antakelse",
          "om gjennomsnittlig atferd i befolkningen på tvers av alder.",
          br(),br(),

          "Under vil du se figurer og en tabell som gir ",
          "en oversikt over det geografiske området du velger i ",
          "nedtrekksmenyen under. Du kan også begynne å skrive navnet ",
          "på ønsket fylke eller kommune så vil det automatisk komme ",
          "opp alternativer.", br(),
          "Du kan også velge tidsrom for visning, enten hele tidsperioden",
          "som det er gjort analyser for, eller en måned.",
          br(),br(),
          strong("Norge:"), " Gir en oversikt over Norge.", br(),
          strong("Fylke:"), " Gir en oversikt over det valgte fylket.", br(),
          strong("Kommune:"), " Gir en oversikt over den valgte kommunen.",
          br(),br(),br()
        )
      )
    ),

    fluidRow(
      column(
        width=12, align="center",
        selectizeInput(
          inputId = ns("covid19_modelling_location_code"),
          label = "Geografisk område",
          choices = config$choices_location,
          selected = "norge",
          multiple = FALSE,
          options = NULL,
          width = "400px"
        )
      ),
      column(
        width = 12, align = "center",
        radioButtons(
          inputId = ns("select_plot_view"),
          label = "Tidsrom for visning",
          choices = list("Hele tidsperiode" = 1, "En måned (zoom)" = 2),
          inline = TRUE,
          selected = 1
        )),
      br(),br(),br()
    ),

    fluidRow(
      column(
        width=12, align="left",

        p(

          "Resultatene fra modellen bør tolkes med varsomhet og må alltid ses i sammenheng",
          "med annen informasjon og med epidemiologiske vurderinger.",
          "Som nevnt over er det mange usikkerhetsmomenter og modellen forbedres stadig.",
          br(),br(),

          " Spredningsmodellen oppdateres foreløpig en gang i uken.",
          " Oppdateringen kan endre tallene da variablene tilpasses ny kunnskap og endringer som skjer i samfunnet.",
          "Et eksempel på dette er reproduksjonstallet (R) som blir beregnet for den nåværende perioden.",
          "Estimatene for de neste ukene og månedene vil baseres på dette reproduksjonstallet.",
          "Det vil derfor bli endringer i grafene og tabellen når reproduksjonstallet endres.",
          br(),br(),
          "Mer informasjon om variablene som brukes i modellen kan du",
          "finne ",
          tags$a(href="https://www.fhi.no/sv/smittsomme-sykdommer/corona/koronavirus-modellering/", "her."),
          br(),br(),

          strong("Den røde vertikale linjen"), "angir dagens dato", br(),
          strong("Daglig insidens"),
          "viser forventet antall smittede på den gitte datoen.", br(),
          strong("Antall smittsomme"),
          "viser forventet antall smittsomme på den gitte datoen.", br(),
          strong("Antall på sykehus (ikke intensiv)"),
          "viser antallet som er på sykehus på den gitte datoen. Disse tallene inkluderer ikke de som er på intensivavdelingen.",br(),
          strong("Antall på intensiv"),
          "viser antall på intensivavdelingen på den gitte datoen."
        )
      )
    ),

    ## Daglig insidens plot
    fluidRow(
      column(
        width=12, align="left",
        br(),
        p(strong("Figur 1."),"Daglig insidens med 95% konfidens intervall. Den røde vertikale linjen angir dagens dato. Vær oppmerksom på at y-skalaen er forskjellig for de forskjellige geografiske områdene."),
        uiOutput(ns("covid19_ui_modelling_incidence")),
        br(),br(),br()
      )
    ),

    ## Infection
    fluidRow(
      column(
        width=12, align="left",
        br(),
        p(strong("Figur 2."),"Antall smittsomme med 95% konfidens intervall.  Den røde vertikale linjen angir dagens dato. Vær oppmerksom på at y-skalaen er forskjellig for de forskjellige geografiske områdene."),
        uiOutput(ns("covid19_ui_modelling_infectious")),
        br(),br(),br()
      )
    ),


    ## Hospital
    fluidRow(
      column(
        width=12, align="left",
        br(),
        p(strong("Figur 3."),"Antall på sykehus (ikke intensiv) med 95% konfidens intervall.  Den røde vertikale linjen angir dagens dato. Vær oppmerksom på at y-skalaen er forskjellig for de forskjellige geografiske områdene."),
        uiOutput(ns("covid19_ui_modelling_hosp")),
        br(),br(),br()
      )
    ),


    ## ICU
    fluidRow(
      column(
        width=12, align="left",
        br(),
        p(strong("Figur 4."),"Antall på intensiv med 95% konfidens intervall. Den røde vertikale linjen angir dagens dato. Vær oppmerksom på at y-skalaen er forskjellig for de forskjellige geografiske områdene."),
        uiOutput(ns("covid19_ui_modelling_icu")),
        br(),br(),br()
      )
    ),

    fluidRow(
      column(
        width=12, align="left",
        #DT::dataTableOutput(ns("covid19_modelling_main"), height = "800px"),
        br(),
        p(

          strong("Tabell 1"),
          "viser beregninger fra FHIs spredningsmodell for covid-19 for hver 7. dag",
          "i det valgte geografiske området",
         " med et estimert tall og 95% konfidens intervall i parentes.",
         "Dataene du ser er for disse datoene (ikke aggregert på ukesnivå).",
         "I tabellen som du laster ned er konfidens intervall forkortet med CI.",
        br(), br()

        ),
        downloadButton(ns("download_xls"), "Last ned tabell", class = "knappe"),
        tags$head(tags$style(".knappe{background-color:#add8e6;} .knappe{color: #111;}")),
        br(),
        formattable::formattableOutput(ns("covid19_modelling_main"), height="800px"),
        br(),
        br(),
        br(),
        br()
      )
    )

  )
}

covid19_modelling_server <- function(input, output, session, config) {

  modelling_main_table <- eventReactive(input$covid19_modelling_location_code, {

    dt_covid19_modelling_main(
      location_code = input$covid19_modelling_location_code,
      config = config
    )

  })

  #output$covid19_modelling_main <- DT::renderDataTable({
  output$covid19_modelling_main <- formattable::renderFormattable({
    req(input$covid19_modelling_location_code)

    ## dt_covid19_modelling_main(
    ##   location_code = input$covid19_modelling_location_code,
    ##   config = config
    ## )
    modelling_main_table()$tab
  })

  ## Download table
  output$download_xls <- downloadHandler(

    filename = function(){ paste0("covid", lubridate::today(), ".xlsx")},
    content = function(file){
      writexl::write_xlsx(modelling_main_table()$pd_xl, file)
    }
  )

  ## Subset to a date range. Can be dynamic
  dateRange <- eventReactive(input$select_plot_view, {
    fromDate <- lubridate::today()-7
    toDate <- fromDate + 7*5
    list(fromDate, toDate)
  })


  ## get modelling data for estimates
  dataModel <- reactive({
    x_location_code <- input$covid19_modelling_location_code

    location_codes <- get_dependent_location_codes(location_code = x_location_code)

    pd <- pool %>% dplyr::tbl("results_covid19_model") %>%
      dplyr::filter(location_code %in% !! location_codes) %>%
      dplyr::collect()

    setDT(pd)
    pd[,date:=as.Date(date)]

    ## Subset for selected date interval
    if (input$select_plot_view == 2){
      pd <- subset(pd, date > dateRange()[[1]] & date < dateRange()[[2]])
    }

    ## merge in the real names
    pd[
      fhidata::norway_locations_long_b2020,
      on = "location_code",
      location_name := location_name
    ]

    ## reorder location for facet viewing
    pd[,location_code := factor(location_code, levels = location_codes)]
    setorder(pd,location_code)
    location_names <- unique(pd$location_name)
    pd[,location_name := factor(location_name, levels = location_names)]

    list(pd = pd, opts = input$select_plot_view)

  })


  ## Incidence
  ## -----------
  output$covid19_ui_modelling_incidence <- renderUI({
    ns <- session$ns
    req(input$covid19_modelling_location_code)

    location_codes <- get_dependent_location_codes(location_code = input$covid19_modelling_location_code)
    height <- round(250 + 150*ceiling(length(location_codes)/3))
    height <- max(400, height)
    height <- paste0(height,"px")

    shinycssloaders::withSpinner(
      plotOutput(ns("covid19_modelling_plot_incidence"), height = height)
    )

  })


  output$covid19_modelling_plot_incidence <- renderCachedPlot({
    req(input$covid19_modelling_location_code)

    plot_covid19_modelling_incidence(
      location_code = input$covid19_modelling_location_code,
      config = config,
      pd = dataModel()
    )
  }, cacheKeyExpr={list(
    input$covid19_modelling_location_code,
    input$select_plot_view,
    dev_invalidate_cache
  )},
  res = 72
  )

  ## Infection
  ## ----------
  output$covid19_ui_modelling_infectious <- renderUI({
    ns <- session$ns
    req(input$covid19_modelling_location_code)


    location_codes <- get_dependent_location_codes(location_code = input$covid19_modelling_location_code)
    height <- round(250 + 150*ceiling(length(location_codes)/3))
    height <- max(400, height)
    height <- paste0(height,"px")

    shinycssloaders::withSpinner(
      plotOutput(ns("covid19_modelling_plot_infectious"), height = height)
    )
  })

  output$covid19_modelling_plot_infectious <- renderCachedPlot({
    req(input$covid19_modelling_location_code)

    plot_covid19_modelling_infectious(
      location_code = input$covid19_modelling_location_code,
      config = config,
      pd = dataModel()
    )
  }, cacheKeyExpr={list(
    input$covid19_modelling_location_code,
    input$select_plot_view,
    dev_invalidate_cache
  )},
  res = 72
  )


  ## Hospital
  ## ---------
  output$covid19_ui_modelling_hosp <- renderUI({
    ns <- session$ns
    req(input$covid19_modelling_location_code)


    location_codes <- get_dependent_location_codes(location_code = input$covid19_modelling_location_code)
    height <- round(250 + 150*ceiling(length(location_codes)/3))
    height <- max(400, height)
    height <- paste0(height,"px")

    shinycssloaders::withSpinner(
      plotOutput(ns("covid19_modelling_plot_hosp"), height = height)
    )
  })


  output$covid19_modelling_plot_hosp <- renderCachedPlot({
    req(input$covid19_modelling_location_code)

    plot_covid19_modelling_hosp(
      location_code = input$covid19_modelling_location_code,
      config = config,
      pd = dataModel()
    )
  }, cacheKeyExpr={list(
    input$covid19_modelling_location_code,
    input$select_plot_view,
    dev_invalidate_cache
  )},
  res = 72
  )


  ## ICU
  ## -------
  output$covid19_ui_modelling_icu <- renderUI({
    ns <- session$ns
    req(input$covid19_modelling_location_code)


    location_codes <- get_dependent_location_codes(location_code = input$covid19_modelling_location_code)
    height <- round(250 + 150*ceiling(length(location_codes)/3))
    height <- max(400, height)
    height <- paste0(height,"px")

    shinycssloaders::withSpinner(
      plotOutput(ns("covid19_modelling_plot_icu"), height = height)
    )
  })


  output$covid19_modelling_plot_icu <- renderCachedPlot({
    req(input$covid19_modelling_location_code)

    plot_covid19_modelling_icu(
      location_code = input$covid19_modelling_location_code,
      config = config,
      pd = dataModel()
    )
  }, cacheKeyExpr={list(
    input$covid19_modelling_location_code,
    input$select_plot_view,
    dev_invalidate_cache
  )},
  res = 72
  )


}


dt_covid19_modelling_main <- function(
                                      location_code = "norge",
                                      config = config
                                      ){
  pd <- pool %>% dplyr::tbl("results_covid19_model") %>%
    dplyr::filter(location_code == !! location_code) %>%
    dplyr::collect()
  setDT(pd)
  pd[,date:=as.Date(date)]

  dates1 <- seq.Date(from=lubridate::today()-7*6, to=lubridate::today(), by=7)
  dates2 <- seq.Date(from=lubridate::today(),to = as.Date("2030-01-01"), by=7)
  dates <- unique(c(dates1, dates2))
  dates <- dates[dates %in% pd$date]

   ## Create data for excel output
  SelectedVar <- c(
    "date",
    "incidence_est", "incidence_thresholdl0", "incidence_thresholdu0",
    "infectious_prev_est", "infectious_prev_thresholdl0", "infectious_prev_thresholdu0",
    "hosp_prev_est", "hosp_prev_thresholdl0", "hosp_prev_thresholdu0",
    "icu_prev_est", "icu_prev_thresholdl0", "icu_prev_thresholdu0"
  )

  NewName <- c(
    "Dato",
    "Daglig insidens", "Daglig insidens nedre CI ", "Daglig insidens øvre CI",
    "Antall smittsomme", "Antall smittsomme nedre CI", "Antall smittsomme øvre CI",
    "Antall på sykehus", "Antall på sykehus nedre CI", "Antall på sykehus øvre CI",
    "Antall på intensiv", "Antall på intensiv nedre CI", "Antall på intensiv øvre CI"
    )

  pd_xl <- pd[date %in% dates, ..SelectedVar]

  for (j in SelectedVar[-1]){
    set(pd_xl, j = j, value = fhiplot::format_nor(pd_xl[[j]]))
  }
 data.table::setnames(pd_xl, old= SelectedVar, new = NewName)

  pd[, incidence_format := glue::glue(
    "{est} ({l95}, {u95})",
    est=fhiplot::format_nor(incidence_est),
    l95=fhiplot::format_nor(incidence_thresholdl0),
    u95=fhiplot::format_nor(incidence_thresholdu0)
  )]

  pd[, infectious_prev_format := glue::glue(
    "{est} ({l95}, {u95})",
    est=fhiplot::format_nor(infectious_prev_est),
    l95=fhiplot::format_nor(infectious_prev_thresholdl0),
    u95=fhiplot::format_nor(infectious_prev_thresholdu0)
  )]

  pd[, hosp_prev_format := glue::glue(
    "{est} ({l95}, {u95})",
    est=fhiplot::format_nor(hosp_prev_est),
    l95=fhiplot::format_nor(hosp_prev_thresholdl0),
    u95=fhiplot::format_nor(hosp_prev_thresholdu0)
  )]

  pd[, icu_prev_format := glue::glue(
    "{est} ({l95}, {u95})",
    est=fhiplot::format_nor(icu_prev_est),
    l95=fhiplot::format_nor(icu_prev_thresholdl0),
    u95=fhiplot::format_nor(icu_prev_thresholdu0)
  )]

  pd <- pd[
    date %in% dates,
    c(
      "date",
      "incidence_format",
      "infectious_prev_format",
      "hosp_prev_format",
      "icu_prev_format"
    )
  ]
  setnames(
    pd,
    c(
      "Dato",
      "Daglig insidens",
      "Antall smittsomme",
      "Antall på sykehus (ikke intensiv)",
      "Antall på intensiv"
    )
  )

  # tab <- DT::datatable(
  #   pd,
  #   rownames = F,
  #   options = list(
  #     pageLength = 20
  #   )
  # )
  tab <- formattable::formattable(
    pd,
    align = c("l",rep("c", ncol(pd) - 1))
  )

  tab
  list(tab = tab, pd_xl = pd_xl)
}



## Incidence
plot_covid19_modelling_incidence <- function(location_code,
                                             config,
                                             pd){

  location_codes <- get_dependent_location_codes(location_code = location_code)

  selectVar <- c("location_code",
                 "date",
                 "location_name",
                 "incidence_est",
                 "incidence_thresholdl0",
                 "incidence_thresholdu0")

  ## plot type
  opts <- pd[["opts"]]

  pd <- pd[["pd"]][, ..selectVar]
  for (j in selectVar[4:6]) set(pd, j = j, value = round(pd[[j]]))

  plot_covid19_modelling_generic(incidence_est,
                                 incidence_thresholdl0,
                                 incidence_thresholdu0,
                                 y_title = "Daglig insidens")

}


## Infection
plot_covid19_modelling_infectious <- function(location_code,
                                             config,
                                             pd){

  location_codes <- get_dependent_location_codes(location_code = location_code)

  selectVar <- c("location_code",
                 "date",
                 "location_name",
                 "infectious_prev_est",
                 "infectious_prev_thresholdl0",
                 "infectious_prev_thresholdu0")


  ## plot type
  opts <- pd[["opts"]]

  pd <- pd[["pd"]][, ..selectVar]
  for (j in selectVar[4:6]) set(pd, j = j, value = round(pd[[j]]))

  plot_covid19_modelling_generic(infectious_prev_est,
                                 infectious_prev_thresholdl0,
                                 infectious_prev_thresholdu0,
                                 y_title = "Antall smittsomme")

}



## Hospital
plot_covid19_modelling_hosp <- function(location_code,
                                             config,
                                             pd){

  location_codes <- get_dependent_location_codes(location_code = location_code)

  selectVar <- c("location_code",
                 "date",
                 "location_name",
                 "hosp_prev_est",
                 "hosp_prev_thresholdl0",
                 "hosp_prev_thresholdu0")


  ## plot type
  opts <- pd[["opts"]]

  pd <- pd[["pd"]][, ..selectVar]
  for (j in selectVar[4:6]) set(pd, j = j, value = round(pd[[j]]))

  plot_covid19_modelling_generic(hosp_prev_est,
                                 hosp_prev_thresholdl0,
                                 hosp_prev_thresholdu0,
                                 y_title = "Antall på sykehus (ikke intensiv)")

}




## ICU
plot_covid19_modelling_icu <- function(location_code,
                                             config,
                                             pd){

  location_codes <- get_dependent_location_codes(location_code = location_code)

  selectVar <- c("location_code",
                 "date",
                 "location_name",
                 "icu_prev_est",
                 "icu_prev_thresholdl0",
                 "icu_prev_thresholdu0")

  ## plot type
  opts <- pd[["opts"]]

  pd <- pd[["pd"]][, ..selectVar]
  for (j in selectVar[4:6]) set(pd, j = j, value = round(pd[[j]]))

  plot_covid19_modelling_generic(icu_prev_est,
                                 icu_prev_thresholdl0,
                                 icu_prev_thresholdu0,
                                 y_title = "Antall på ICU")

}

## Generic function for plotting
plot_covid19_modelling_generic <- function(est,
                                           lower,
                                           upper,
                                           y_title){

  pd <- parent.frame()$pd
  opts <- parent.frame()$opts

  est_value <- as.character(substitute(est))
  est_lower <- as.character(substitute(lower))
  est_upper <- as.character(substitute(upper))


  ## Plotting
  q <- ggplot(pd, aes(date))
  q <- q + geom_ribbon(aes_string(ymin = est_lower, ymax = est_upper),
                       fill = fhiplot::base_color, alpha = 0.5)
  q <- q + geom_line(aes_string(y = est_value), color = fhiplot::base_color, size = 1.5)
  q <- q + lemon::facet_rep_wrap(vars(location_name),
                                 repeat.tick.labels = "y",
                                 scales = "free_y",
                                 ncol = 3)

  q <- q + scale_y_continuous(
    name = y_title,
    breaks = fhiplot::pretty_breaks(5),
    labels = fhiplot::format_nor,
    expand = expand_scale(mult = c(0, 0.1))
  )

  ## Scale options for different time intervals
  if(opts == 1){
    q <- q + scale_x_date(
      NULL,
      date_breaks = "2 months",
      date_labels = "%d.%m.%Y"
    )
  } else {
    q <- q + scale_x_date(
      NULL,
      date_breaks = "3 days ",
      date_labels = "%d.%m"
    )
  }

  q <- q + labs(
    caption = glue::glue("Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}")
  )


  q <- q + fhiplot::theme_fhi_lines(
    20, panel_on_top = T,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
  q <- q + geom_vline(xintercept = lubridate::today(), color="red")
  q <- q + fhiplot::set_x_axis_vertical()

  q <- q + theme(axis.title = element_text(hjust = 0.95))
  q

}
