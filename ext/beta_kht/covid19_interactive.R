covid19_interactive_ui <- function(id, config){

  ns <- NS(id)

  fluidPage(
    fluidRow(
      br(),
      p("Dette er interaktiv side", br(),
        "Bla... bla.. bla.."),
      br(),
    ),

    fluidRow(
      column(
        width = 3,
        radioButtons(ns("int_select_levels"), "Valg nivå: ",
                     choices = list("Fylke" = 1, "Kommune" = 2),
                     selected = 1, inline = TRUE)
      ),

      column(
        width = 4,
        selectizeInput(ns("int_input_location"), "Sammenligne: ",
                       choices = config$choices_location[-1],
                       ## selected = list(as.list(config$choices_location[c(2, 5)])),
                       options = list(placeholder = "Du kan kun velge 2",
                                      maxItems = 2,
                                      onInitialize = I('function() {this.setValue("");}')
                                      ))
      ),

      column(
        width = 4,
        selectizeInput(ns("int_indikator"), "Valg indikator: ",
                       selected = character(0),
                       choices = config$choices_location[-1],
                       multiple = FALSE)
      ),

      column(
        width = 1, style = "margin-top: 25px;",
        actionButton(ns("int_button"), " Vis plot", icon = icon("bar-chart-o"))
      ),
    )
  )

}


covid19_interactive_server <- function(input, output, session, config){



}




covid19_int_msis <- function(granularity_geo,
                             config){

    d_left <- pool %>% dplyr::tbl("data_covid19_msis_by_time_location") %>%
    dplyr::filter(granularity_time == "day")%>%
    dplyr::filter(granularity_geo== !!granularity_geo) %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(date, n) %>%
    dplyr::collect()
  setDT(d_left)
  d_left[, date:= as.Date(date)]
  setnames(d_left, "n", "value")

}


covid19_int_norsyss <- function(granularity_geo,
                                config){

  d_right <- pool %>% dplyr::tbl("data_norsyss") %>%
    dplyr::filter(granularity_geo== !!granularity_geo) %>%
    dplyr::filter(granularity_time=="day") %>%
    dplyr::filter(tag_outcome %in% "covid19_vk_ote") %>%
    dplyr::filter(age=="total") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(date, n, consult_with_influenza) %>%
    dplyr::collect()
  setDT(d_right)
  d_right[, date:= as.Date(date)]

  d_right[,censor := ""]
  d_right[censor=="" & n>0 & n<5, censor := "N"]
  d_right[censor != "", n := 0]
  d_right[, value := 100* n / consult_with_influenza]
  d_right[is.nan(value), value := 0]
  d_right[value>60, value := 60]
  d_right[, n := NULL]
  d_right[, no_data := consult_with_influenza==0]
  d_right[,consult_with_influenza := NULL]

}
