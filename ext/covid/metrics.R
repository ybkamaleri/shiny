if (.Platform$OS.type == "windows"){
  fc <- cache_filesystem("~/.cache/")
} else {
  fc <- cache_filesystem("/tmp/")
}

metrics_ui <- function(id, config) {
  ns <- NS(id)
  dimensionId <- ns("dimension")

  tagList(
    fluidRow(
      column(
        width=12, align="left",
        p(
          id="toptext",
          strong("Informasjonen som finnes på denne siden er anonym, men er ment for kommuneleger fordi det krever kunnskap for å tolke disse på riktig måte. Dette er ikke ment som en offisiell statistikk."),
          br(),br()
        )
      )
    ),

    tabsetPanel(
      id="metrics",

      tabPanel(
        title="Hoved",
        tagList(

          # fig 1 ----
          fluidRow(
            fluidRow(
              column(
                width=12, align="left",
                p(
                  formattable::formattableOutput(ns("metrics_tab_main"), height="800px"),
                )
              )
            )
          )
        )
      )
    )
  )
}

metrics_server <- function(input, output, session, config) {
  output$metrics_tab_main <- formattable::renderFormattable({
    metrics_table_main(location_code = "norge")
  })

}

metrics_table_main <- function(
  location_code = "norge",
  config = config
){

  yrwks <- fhi::isoyearweek(lubridate::today()-0:8*7)

  d <- pool %>% dplyr::tbl("results_covid19_metrics") %>%
    dplyr::filter(granularity_time == "week") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(yrwk %in% yrwks) %>%
    dplyr::filter(tag_outcome %in% c(
      "n_hospital_main_cause",
      "n",
      "n_lab_tested",
      "pr100_lab_pos",
      "n_norsyss",
      "pr100_norsyss",
      "pr100_sr_symptoms"
    )) %>%
    dplyr::select(yrwk, tag_outcome, value) %>%
    dplyr::collect()
  setDT(d)

  d[,yrwk := stringr::str_replace(yrwk, "-", " ")]
  d[, tag_outcome := factor(
    tag_outcome,
    levels = c(
      "n_hospital_main_cause",
      "n",
      "n_lab_tested",
      "pr100_lab_pos",
      "n_norsyss",
      "pr100_norsyss",
      "pr100_sr_symptoms"
    ),
    labels = c(
      "Nye sykehusinnleggelser for covid-19",
      "Nye tilfeller av covid-19",
      "Testede for covid-19",
      "Andel positive blant testede",
      "Legekonsultasjoner for covid-19 (1)",
      "Legekonsultasjoner for covid-19 (2)",
      "Relevante symptomer"
    )
  )]
  d <- dcast.data.table(
    d,
    tag_outcome ~ yrwk
  )
  d[, Kilde := c(
      "Beredt C19",
      "MSIS",
      "MSIS",
      "MSIS",
      "NorSySS",
      "NorSySS",
      "Symptometeret"
    )
  ]

  d[, Benevning := c(
      "Antall",
      "Antall",
      "Antall",
      "Andel (%) av testede",
      "Antall",
      "Andel (%) av alle konsultasjoner",
      "Andel av respondenter"
    )
  ]

  setnames(d,"tag_outcome", "Indikator")
  setcolorder(d, c(
    "Indikator",
    "Kilde",
    "Benevning"
  ))

  tab <- d

  font_size <- formattable::formatter(
    "span",
    style="font-size:10px;"
  )

  ft <- formattable::formattable(
    tab,
    list(~font_size),
    align = c(rep("l",3),rep("c", ncol(tab) - 3))
  )
  ft <- formattable::fontsize(ft, size = 9, part = "body")
  ft
}
