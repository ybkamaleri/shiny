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
          "Under vil du se en tabell som gir ",
          "en oversikt over det geografiske området du velger i ",
          "nedtrekksmenyen under. Du kan også begynne å skrive navnet ",
          "på ønsket fylke eller kommune så vil det automatisk komme ",
          "opp alternativer.", br(), br(),
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
        ),
        br(),br(),br()
      )
    ),

    fluidRow(
      column(
        width=12, align="left",

        p(

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
          br(),

          "Resultatene fra modellen er beheftet med usikkerhet på grunn av tilfeldighet i",
          "smittespredningen, tilfeldighet i mobilitet (om det er smittsomme eller mottakelige",
          "som reiser for eksempel) og usikkerhet i de tre kalibrerte parameterne.",
          "I tillegg er det flere kilder til usikkerhet som modellen ikke fanger opp,",
          "og vi tar ikke høyde for usikkerhet knyttet til modellens øvrige parametre.",
          "Modellen er en forenklet representasjon av virkeligheten og bygger på en antakelse",
          "om gjennomsnittlig atferd i befolkningen på tvers av alder.",
          br(),br(),

          "Resultatene fra modellen bør tolkes med varsomhet og må alltid ses i sammenheng",
          "med annen informasjon og med epidemiologiske vurderinger.",
          "Som nevnt over er det mange usikkerhetsmomenter og modellen forbedres stadig.",
          br(),br(),

          " Spredningsmodellen oppdateres foreløpig en gang i uken.",
          " Oppdateringen kan endre tallene da variablene tilpasses ny kunnskap og endringer som skjer i samfunnet.",
          br(),br(),
          "Mer informasjon om variablene som brukes i modellen kan du",
          "finne ",
          tags$a(href="https://www.fhi.no/sv/smittsomme-sykdommer/corona/koronavirus-modellering/", "her."),
          br(),br(),

          strong("Dato"),
          "viser datoen for hver 7. dag. Dataene du ser er for disse datoene (ikke aggregert på ukesnivå).", br(),
          strong("Daglig insidens"),
          "viser forventet antall smittede på den gitte datoen.", br(),
          strong("Antall smittsomme"),
          "viser forventet antall smittsomme på den gitte datoen.", br(),
          strong("Antall i sykehus (ikke ICU)"),
          "viser antallet som er på sykehus på den gitte datoen. Disse tallene inkluderer ikke de som er på intensivavdelingen.",br(),
          strong("Antall i ICU"),
          "viser antall på intensivavdelingen på den gitte datoen."
        )
      )
    ),

    ## Daglig insidens plot
    fluidRow(
      column(
        width=12, align="left",
        br(),
        p(strong("Figur 1."),"Daglig insidens med 95% konfidens intervall. Vær oppmerksom på at y-skalaen er forskjellig for de forskjellige geografiske områdene."),
        uiOutput(ns("covid19_ui_modelling_incidence")),
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
          br(), br()

        ),
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
  #output$covid19_modelling_main <- DT::renderDataTable({
  output$covid19_modelling_main <- formattable::renderFormattable({
    req(input$covid19_modelling_location_code)

    dt_covid19_modelling_main(
      location_code = input$covid19_modelling_location_code,
      config = config
    )
  })


  output$covid19_ui_modelling_incidence <- renderUI({
    ns <- session$ns
    req(input$covid19_modelling_location_code)

    location_codes <- get_dependent_location_codes(location_code = input$covid19_modelling_location_code)
    height <- round(250 + 150*ceiling(length(location_codes)/3))
    height <- max(400, height)
    height <- paste0(height,"px")

    plotOutput(ns("covid19_modelling_plot_incidence"), height = height)

  })


  output$covid19_modelling_plot_incidence <- renderCachedPlot({
    req(input$covid19_modelling_location_code)

    plot_covid19_modelling_incidence(
      location_code = input$covid19_modelling_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid19_modelling_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )
}


dt_covid19_modelling_main <- function(
                                      location_code = "norge",
                                      config = config
                                      ){
  pd <- pool %>% dplyr::tbl("data_covid19_model") %>%
    dplyr::filter(location_code == !! location_code) %>%
    dplyr::collect()
  setDT(pd)
  pd[,date:=as.Date(date)]

  dates1 <- seq.Date(from=lubridate::today()-7*6, to=lubridate::today(), by=7)
  dates2 <- seq.Date(from=lubridate::today(),to = as.Date("2030-01-01"), by=7)
  dates <- unique(c(dates1, dates2))
  dates <- dates[dates %in% pd$date]

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
      "Antall i sykehus (ikke ICU)",
      "Antall i ICU"
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
}


plot_covid19_modelling_incidence <- function(location_code,config){

  location_codes <- get_dependent_location_codes(location_code = location_code)

  ## Access DB
  pd <- pool %>% dplyr::tbl("data_covid19_model") %>%
    dplyr::filter(location_code %in% !! location_codes) %>%
    dplyr::select(location_code,
                  date, incidence_est,
                  incidence_thresholdl0,
                  incidence_thresholdu0) %>%
    dplyr::collect()

  ## Edit DT
  setDT(pd)
  pd[,date:=as.Date(date)]
  pd[, incidence_est := round(incidence_est)]
  pd[, incidence_thresholdl0 := round(incidence_thresholdl0)]
  pd[, incidence_thresholdu0 := round(incidence_thresholdu0)]

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


  ## Plotting
  q <- ggplot(pd, aes(date))
  q <- q + geom_ribbon(aes(ymin = incidence_thresholdl0, ymax = incidence_thresholdu0),
                       fill = fhiplot::base_color, alpha = 0.5)
  q <- q + geom_line(aes(y = incidence_est), color = fhiplot::base_color, size = 1.5)
  q <- q + lemon::facet_rep_wrap(vars(location_name),
                                 repeat.tick.labels = "y",
                                 scales = "free_y",
                                 ncol = 3)

  q <- q + scale_y_continuous(
    "Daglig insidens",
    breaks = fhiplot::pretty_breaks(5),
    labels = fhiplot::format_nor,
    expand = expand_scale(mult = c(0, 0.1))
  )
  q <- q + scale_x_date(
    NULL,
    date_breaks = "2 months",
    date_labels = "%d.%m"
  )
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

  q

}
