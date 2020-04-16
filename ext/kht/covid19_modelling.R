

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
          strong("Tabell 1"),
          "viser data for hver 7. dag i det valgte geografiske området",
          " med et estimert tall og 95% konfidens intervall i parentes.",
          "Dataene er basert på matematisk modellering og inkluderer populasjonsstruktur i hver kommune",
          " og forflytningsinformasjon mellom hver kommune (Telenor mobiltelefon data) i tillegg til andre variabler",
          " som  for eksempel det basale reproduksjonstallet (R0) og tidsintervall fra klinisk sykdom til sykehusopphold.",
          br(),br(),
          " Covid-19 er en ny sykdom og det er fortsatt mange variabler som er usikre.",
          " Resultatene fra modelleringen vil derfor ikke alltid passe med det vi ser fra overvåkning.",
          br(),br(),
          " Spredningsmodellen oppdateres foreløpig en gang i uken.",
          " Oppdateringen kan endre tallene da variablene tilpasses ny kunnskap og endringer som skjer i samfunnet.",
          br(),br(),
          "Mer informasjon om variablene som brukes i modellen kommer i fanen 'Informasjon'.",
          "Dersom du trenger informasjon før dette blir oppdatert vennligst send oss en e-post til sykdomspulsen@fhi.no.",
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

   fluidRow(
     column(
       width=12, align="left",
       #DT::dataTableOutput(ns("covid19_modelling_main"), height = "800px"),
       br(),
       p(
         strong("Tabell 1."), " beregninger fra FHIs spredningsmodell for covid-19."
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
