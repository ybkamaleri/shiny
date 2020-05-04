

norsyss_overview_ui <- function(id, config) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width=12, align="left",

        p(
          strong("Under vil du se en rekke grafer og tabeller som gir en ",
          "oversikt over det symptomet/syndromet og geografiske ",
          "område du velger i under. For å velge geografisk område ",
          "kan du begynne å skrive navnet på ønsket fylke eller ",
          "kommune så vil det automatisk komme opp alternativer."), br(), br(),

          strong("Norge: "), "Gir en oversikt over Norge i tillegg ",
          "til oversikt over alle fylkene.", br(),
          strong("Fylke: "), "Gir en oversikt over det valgte ",
          "fylket i tillegg til en oversikt over alle kommunene i dette fylket.", br(),
          strong("Kommune: "), "Gir en oversikt over den valgte kommunen ",
          "i tillegg til en oversikt over nabokommunene.",
          "Vær oppmerksom på at noen kommuner har veldig få konsultasjoner,",
          "derfor vil ikke trendene kunne brukes på en god måte.",
          "Kommuner med under 500 innbyggere vil mangle aldersdelte figurer pga anonymitet.",
          br(), br(),

          "Informasjon om dataene kan du se under fanen 'Informasjon'", br(), br(),

          strong("Mage-tarminfeksjoner"), " er en samlebetegnelse for ICPC-2 ",
          "kodene Diare (D11), Tarminfeksjon (D70) og Gastroenteritt ",
          "antatt infeksiøs (D73)", br(), br(),

          strong("Luftveisinfeksjoner"), " er en samlebetegnelse for Hoste (R05), ",
          "Akutt øvre luftveisinfeksjon (R74), Akutt bronkitt/bronkiolitt (R78) ",
          "og Luftveisinfeksjon IKA (R83).",
          br(), br(), br()
        )
      )
    ),

    fluidRow(
      column(
        width=12, align="center",

        radioButtons(
          inputId = ns("norsyss_tag"),
          label = "Symptom/syndrom",
          choices = config$choices_norsyss_tag,
          selected = config$choices_norsyss_tag[[1]],
          width = "400px"
        ),

        br(),

        selectizeInput(
          inputId = ns("norsyss_location_code"),
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
         strong("Figur 1"), " gir to oversikter (bokser) med aldersgrupper ",
         "vertikalt på venstre side og dato nede på x-aksen. ",
         "Ved å kombinere disse vil hver firkant i boksen stå ",
         "for en aldersgruppe og en tidsperiode.", br(), br(),

         strong("Boksen til venstre viser ukesoversikt. "),
         "Her ser du 16 uker og 7 aldersgrupper ('totalt' ",
         "er summen av alle aldersgrupper). ",
         "Hver firkant i boksen står for en aldersgruppe og en uke.",
         "x-aksen nederst i boksen viser år-ukenummer.", br(), br(),

         strong("Boksen til høyre viser en daglig oversikt. "),
         "Her ser du 14 dager og 7 aldersgrupper ('totalt' ",
         "er summen av alle aldersgrupper). ",
         "Hver firkant i boksen står for en aldersgruppe og en dag.",
         "x-aksen nederst i boksen viser år-måned-dag.", br(), br(),

         "Fargen i boksene er laget ut fra beregninger fra de ",
         "foregående 5 årene i samme geografiske område og ",
         "samme symptom/syndrom og aldersgruppe (for årene ",
         "2006-2010 er 2006-2010 brukt).", br(), br(),

         strong("Blå "), "farge viser at antallet konsultasjoner er som ", strong("forventet"),br(),
         strong("Gul "), "farge viser at antallet konsultasjoner er ", strong("høyere enn forventet"), br(),
         strong("Rød "), "farge viser at antall konsultasjoner er ", strong("betydelig høyere enn forventet"), br(), br(),

         strong("NB!"), "Boksen til høyre (daglig oversikt) vil ikke vises for de enkelte kommunene. "
       )
     )
   ),

   fluidRow(
     column(
       width=12, align="left",
       br(),
       p(
         strong("Figur 1."), "Ukentlig og daglig oversikt fordelt på aldersgrupper"
        ),
       shinycssloaders::withSpinner(plotOutput(ns("norsyss_plot_barometer_age"), height = "600px")),
       br(), br(), br()
     )
   ),

   fluidRow(
     column(
       width=12, align="left",

       p(
         strong("Figur 2"), " gir same type grafer som ", strong("Figur 1 "),
         "men for fylker eller kommuner i det geografiske ",
         "området som er valgt.", br(), br(),

         strong("Boksen til venstre viser ukesoversikt. "),
         "Her ser du 16 uker og 7 aldersgrupper ('totalt' ",
         "er summen av alle aldersgrupper). ",
         "Hver firkant i boksen står for en aldersgruppe og en uke.",
         "x-aksen nederst i boksen viser år-ukenummer.", br(), br(),

         strong("Boksen til høyre viser en daglig oversikt. "),
         "Her ser du 14 dager og 7 aldersgrupper ('totalt' ",
         "er summen av alle aldersgrupper). ",
         "Hver firkant i boksen står for en aldersgruppe og en dag.",
         "x-aksen nederst i boksen viser år-måned-dag.", br(), br(),

         "Fargen i boksene er laget ut fra beregninger fra de ",
         "foregående 5 årene i samme geografiske område og ",
         "samme symptom/syndrom og aldersgruppe (for årene ",
         "2006-2010 er 2006-2010 brukt).", br(), br(),

         strong("Blå "), "farge viser at antallet konsultasjoner er som ", strong("forventet"),br(),
         strong("Gul "), "farge viser at antallet konsultasjoner er ", strong("høyere enn forventet"), br(),
         strong("Rød "), "farge viser at antall konsultasjoner er ", strong("betydelig høyere enn forventet"), br(), br(),

         strong("NB!"), "Boksen til høyre (daglig oversikt) vil ikke vises for de enkelte kommunene. "
        )
     )
   ),

   fluidRow(
     column(
       width=12, align="left",
       br(),
       p(
         strong("Figur 2."), "Ukentlig og daglig oversikt fordelt på geografisk område per aldersgruppe"
       ),
       uiOutput(ns("norsyss_ui_barometer_location")),
       br(), br(), br()
       #shinycssloaders::withSpinnerplotOutput(ns("norsyss_plot_barometer_location"), height = "5000px")
     )
   ),

   fluidRow(
     column(
       width=12, align="left",

       p(
         strong("Figur 3"), " gir en oversikt over antall og andel ",
         "ukentlige konsultasjoner og daglig og ukentlig ekcess for ",
         "valgt geografisk område for forskjellige aldersgrupper. ",
         strong("Eksess"), " betyr antallet som ligger over grensen mellom ",
         "forventet antall konsultasjoner og høyere enn forventet antall konsultasjoner, ",
         "og gir indikasjon på hvor stort avvik det er fra det vi ville forventet ",
         "for samme tid, aldersgruppe og geografisk område.", br(), br(),

         "- Den ", strong("svarte streken"), " viser antallet faktiske konsultasjoner det ",
         "har vært på legekontor og legevakt. Dersom denne streken er ",
         "i det ", strong("blå feltet"), " er antallet konsultasjoner er som forventet, ",
         "om den er i det ", strong("gule feltet"), " er antallet konsultasjoner høyere ",
         "enn forventet og om den er i det ", strong("røde feltet"), " er antallet konsultasjoner ",
         "betydelig høyere enn forventet for gitt tidsrom, alder og geografisk område.", br(),
         "- Bakgrunnsfargen er laget ut fra beregninger fra de foregående 5 ",
         "årene i samme geografiske område og samme symptom/syndrom og aldersgruppe ",
         "(for årene 2006-2010 er 5 fremtidige år brukt)."
       )
     )
   ),
   fluidRow(
     column(
       width=12, align="left",
       br(),
       p(
         strong("Figur 3."),
         "Ukentlig andel, antall og eksess og daglig eksess fordelt på aldersgrupper"
       ),
       uiOutput(ns("norsyss_ui_trends")),
       br(),br(),br()
     )
   )
 )
}

norsyss_overview_server <- function(input, output, session, config) {
  output$norsyss_plot_barometer_age <- renderCachedPlot({
    req(input$norsyss_tag)
    req(input$norsyss_location_code)

    plot_barometer_age(
      tag_outcome = input$norsyss_tag,
      location_code = input$norsyss_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$norsyss_tag,
    input$norsyss_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  output$norsyss_plot_barometer_location <- renderCachedPlot({
    req(input$norsyss_tag)
    req(input$norsyss_location_code)

    plot_barameter_location(
      tag_outcome = input$norsyss_tag,
      location_code = input$norsyss_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$norsyss_tag,
    input$norsyss_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  output$norsyss_ui_barometer_location <- renderUI({
    ns <- session$ns
    req(input$norsyss_tag)
    req(input$norsyss_location_code)

    location_codes <- get_dependent_location_codes(location_code = input$norsyss_location_code)
    height <- round(200*length(location_codes))
    height <- max(1000, height)
    height <- paste0(height,"px")
    shinycssloaders::withSpinner(plotOutput(ns("norsyss_plot_barometer_location"), height = height))
  })

  #shinycssloaders::withSpinnerplotOutput(ns("norsyss_plot_barometer_location"), height = "5000px")

  output$norsyss_ui_trends <- renderUI({
    ns <- session$ns
    req(input$norsyss_tag)
    req(input$norsyss_location_code)

    if(input$norsyss_location_code %in% config$small_location_codes){
      height <- "600px"
    } else {
      height <- "2000px"
    }
    shinycssloaders::withSpinner(plotOutput(ns("norsyss_plot_trends"), height = height))
  })

  output$norsyss_plot_trends <- renderCachedPlot({
    req(input$norsyss_tag)
    req(input$norsyss_location_code)

    plot_trends_multiple(
      tag_outcome = input$norsyss_tag,
      location_code = input$norsyss_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$norsyss_tag,
    input$norsyss_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  outputOptions(output, "norsyss_plot_barometer_age", priority = 10)
  outputOptions(output, "norsyss_plot_barometer_location", priority = 9)
  outputOptions(output, "norsyss_plot_trends", priority = 8)

}






### PLOTS

plot_barometer_age <- function(
  tag_outcome = "respiratoryexternal_vk_ot",
  location_code = "norge",
  config
  ){

  granularity_geo <- get_granularity_geo(location_code = location_code)

  min_date1 <- lubridate::today()-7*15
  pd1 <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
    dplyr::filter(date >= !!min_date1) %>%
    dplyr::filter(tag_outcome %in% !!tag_outcome) %>%
    dplyr::filter(granularity_time == "week") %>%
    dplyr::filter(location_code %in% !!location_code) %>%
    dplyr::select(granularity_time, date, yrwk, age, location_code, n_status) %>%
    dplyr::collect()

  min_date2 <- lubridate::today()-14
  pd2 <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
    dplyr::filter(tag_outcome %in% !!tag_outcome) %>%
    dplyr::filter(date >= !!min_date2) %>%
    dplyr::filter(granularity_time == "day") %>%
    dplyr::filter(location_code %in% !!location_code) %>%
    dplyr::select(granularity_time, date, yrwk, age, location_code, n_status) %>%
    dplyr::collect()

  pd <- rbind(pd1,pd2)
  setDT(pd)
  pd[, date := as.Date(date)]

  pd[, age := factor(
    age,
    levels = c(
      "65+",
      "30-64",
      "20-29",
      "15-19",
      "5-14",
      "0-4",
      "total"
    ), labels = c(
      "65+",
      "30-64",
      "20-29",
      "15-19",
      "5-14",
      "0-4",
      "Totalt"
    )
  )]

  pd[, n_status := factor(
    n_status,
    levels = c("normal", "medium", "high"),
    labels = c(
      "Forventet",
      "Høyere enn\nforventet",
      "Betydelig høyere\nenn forventet"
    )
  )]

  q <- ggplot(pd[granularity_time=="week"], aes(x=yrwk,y=age,fill=n_status))
  q <- q + geom_tile(color="black")
  q <- q + scale_y_discrete("Alder")
  q <- q + scale_x_discrete("Uke")
  q <- q + fhiplot::scale_fill_fhi(
    NULL,
    palette = "warning",
    drop = F
  )
  q <- q + fhiplot::theme_fhi_basic(20)
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + theme(legend.position="bottom")
  q <- q + fhiplot::set_x_axis_vertical()
  q <- q + labs(title=glue::glue(
    "Ukentlig status etter aldersgruppe"
  ))
  q <- q + labs(caption = glue::glue(
    "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
  ))
  q1 <- q

  q <- ggplot(pd[granularity_time=="day"], aes(x=date,y=age,fill=n_status))
  q <- q + geom_tile(color="black")
  q <- q + scale_y_discrete("Alder")
  q <- q + scale_x_date(
    "Dag",
    date_breaks = "1 day",
    expand = expand_scale(mult = c(0, 0))
  )
  q <- q + fhiplot::scale_fill_fhi(
    NULL,
    palette = "warning",
    drop = F
  )
  q <- q + fhiplot::theme_fhi_basic(20)
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + theme(legend.position="bottom")
  q <- q + fhiplot::set_x_axis_vertical()
  q <- q + labs(title=glue::glue(
    "Daglig status etter aldersgruppe"
  ))
  q <- q + labs(caption = glue::glue(
    "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
  ))
  q2 <- q

  title_text <- glue::glue(
    "{names(config$choices_location)[config$choices_location==location_code]}\n",
    "{names(config$choices_norsyss_tag)[config$choices_norsyss_tag==tag_outcome]}\n",
  )
  title <- cowplot::ggdraw() +
    cowplot::draw_label(
      title_text,
      fontface = 'bold',
      x = 0,
      hjust = 0,
      size=30
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 0)
    )
  if(granularity_geo=="municip"){
    retval <- q1

    retval <- cowplot::plot_grid(
      title,
      q1,
      ncol=1,
      rel_heights = c(0.2, 1)
    )
  } else {

    retval <- cowplot::plot_grid(
      title,
      NULL,
      q1,
      q2,
      ncol=2,
      rel_heights = c(0.2, 1)
    )
  }

  return(retval)
}

plot_barameter_location <- function(
  tag_outcome = "respiratoryexternal_lt",
  location_code = "norge",
  config
){

  granularity_geo <- get_granularity_geo(location_code = location_code)
  location_codes <- get_dependent_location_codes(location_code = location_code)

  min_date1 <- lubridate::today()-7*15
  pd_week <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
    dplyr::filter(tag_outcome %in% !!tag_outcome) %>%
    dplyr::filter(date >= !!min_date1) %>%
    dplyr::filter(granularity_time == "week") %>%
    dplyr::filter(location_code %in% !!location_codes) %>%
    dplyr::select(granularity_time, date, yrwk, age, location_code, n_status) %>%
    dplyr::collect()

  min_date2 <- lubridate::today()-14
  pd_day <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
    dplyr::filter(tag_outcome %in% !!tag_outcome) %>%
    dplyr::filter(date >= !!min_date2) %>%
    dplyr::filter(granularity_time == "day") %>%
    dplyr::filter(location_code %in% !!location_codes) %>%
    dplyr::select(granularity_time, date, yrwk, age, location_code, n_status) %>%
    dplyr::collect()

  pd <- rbind(pd_week,pd_day)
  setDT(pd)
  pd[,date:=as.Date(date)]

  pd[,location_code := factor(location_code, levels=rev(location_codes))]

  pd[, age := factor(
    age,
    levels = c(
      "total",
      "0-4",
      "5-14",
      "15-19",
      "20-29",
      "30-64",
      "65+"
    ),
    labels = c(
      "Totalt",
      "0-4",
      "5-14",
      "15-19",
      "20-29",
      "30-64",
      "65+"
    )
  )]

  pd[
    fhidata::norway_locations_long_b2020,
    on="location_code",
    location_name := location_name
    ]
  setorder(pd,location_code)
  locs <- unique(pd$location_name)
  pd[,location_name := factor(location_name, levels=locs)]

  pd[, n_status := factor(
    n_status,
    levels = c(
      "normal",
      "medium",
      "high"
    ),
    labels = c(
      "Forventet",
      "Høyere enn\nforventet",
      "Betydelig høyere\nenn forventet"
    )
  )]

  q <- ggplot(pd[granularity_time=="week"], aes(x=yrwk,y=location_name,fill=n_status))
  q <- q + geom_tile(color="black")
  q <- q + scale_y_discrete(NULL)
  q <- q + scale_x_discrete(NULL)
  q <- q + lemon::facet_rep_wrap(~age, repeat.tick.labels = "y", scales="free", ncol=1)
  q <- q + fhiplot::scale_fill_fhi(
    NULL,
    palette = "warning",
    drop = F
  )
  q <- q + fhiplot::theme_fhi_basic(16)
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + theme(legend.position="bottom")
  q <- q + fhiplot::set_x_axis_vertical()
  q <- q + labs(title=glue::glue(
    "Ukentlig status etter område per aldersgruppe"
  ))
  q <- q + labs(caption = glue::glue(
    "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
  ))
  q_week <- q

  q <- ggplot(pd[granularity_time=="day"], aes(x=date,y=location_name,fill=n_status))
  q <- q + geom_tile(color="black")
  q <- q + scale_y_discrete(NULL)
  q <- q + scale_x_date(
    NULL,
    date_breaks = "1 day",
    expand = expand_scale(mult = c(0, 0))
  )
  q <- q + lemon::facet_rep_wrap(~age, repeat.tick.labels = "y", scales="free", ncol=1)
  q <- q + fhiplot::scale_fill_fhi(
    NULL,
    palette = "warning",
    drop = F
  )
  q <- q + fhiplot::theme_fhi_basic(16)
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + theme(legend.position="bottom")
  q <- q + fhiplot::set_x_axis_vertical()
  q <- q + labs(title=glue::glue(
    "Daglig status etter område per aldersgruppe"
  ))
  q <- q + labs(caption = glue::glue(
    "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
  ))
  q_day <- q

  title_text <- glue::glue("{names(config$choices_norsyss_tag)[config$choices_norsyss_tag==tag_outcome]}\n")
  title <- cowplot::ggdraw() +
    cowplot::draw_label(
      title_text,
      fontface = 'bold',
      x = 0,
      hjust = 0,
      size=30
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 0)
    )

  rel_heights <- c(0.3/length(location_codes),1)
  if(granularity_geo!="nation"){
    retval <- q_week

    retval <- cowplot::plot_grid(
      title,
      q_week,
      ncol=1,
      rel_heights = rel_heights
    )
  } else {

    retval <- cowplot::plot_grid(
      title,
      NULL,
      q_week,
      q_day,
      ncol=2,
      rel_heights = rel_heights
    )
  }

  return(retval)

  #
  # pd[, name_outcome := factor(
  #   tag_outcome,
  #   levels = c("gastro_lt", "respiratoryexternal_lt"),
  #   labels = c(
  #     "Magetarm (D11, D70, D73)",
  #     "Luftveisinfeksjoner (R05, R74, R78, R83)"
  #   )
  # )]
}


plot_trends_single <- function(pd,x_age){
  pd <- pd[age==x_age]

  pd_med <- pd[plot_n>plot_u1 & plot_n<=plot_u2]
  pd_hig <- pd[plot_n>plot_u2]

  q <- ggplot(pd, aes(x=date,y=plot_n))
  q <- q + geom_ribbon(mapping=aes(ymin=-Inf,ymax=plot_u1),fill=fhiplot::warning_color[["low"]])
  q <- q + geom_ribbon(mapping=aes(ymin=plot_u1,ymax=plot_u2),fill=fhiplot::warning_color[["med"]])
  q <- q + geom_ribbon(mapping=aes(ymin=plot_u2,ymax=Inf),fill=fhiplot::warning_color[["hig"]])
  q <- q + geom_line(color="black")
  if(nrow(pd_med)>0){
    q <- q + geom_point(data=pd_med,size=4)
    q <- q + geom_point(data=pd_med, color=fhiplot::warning_color[["med"]],size=3)
  }
  if(nrow(pd_hig)>0){
    q <- q + geom_point(data=pd_hig,size=4)
    q <- q + geom_point(data=pd_hig, color=fhiplot::warning_color[["hig"]],size=3)
  }
  q <- q + lemon::facet_rep_wrap(~type, repeat.tick.labels = "y", scales="free", nrow=1)
  q <- q + scale_y_continuous(NULL)
  q <- q + scale_x_date(
    NULL,
    date_labels = "%d.%m"
  )
  q <- q + fhiplot::scale_fill_fhi(NULL)
  q <- q + fhiplot::theme_fhi_lines(16)
  q <- q + theme(legend.key.size = unit(1, "cm"))
  #q <- q + fhiplot::set_x_axis_vertical()
  #q <- q + labs(title=x_age)
  q
}

plot_trends_multiple <- function(tag_outcome, location_code, config){

  granularity_geo <- get_granularity_geo(location_code = location_code)

  min_date_daily <- lubridate::today()-28
  min_date_weekly <- lubridate::today()-365
  pdday <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
    dplyr::filter(tag_outcome == !!tag_outcome) %>%
    dplyr::filter(date >= !!min_date_daily) %>%
    dplyr::filter(granularity_time == "day") %>%
    dplyr::filter(location_code %in% !!location_code) %>%
    dplyr::collect()
  setDT(pdday)
  pdday[,date:=as.Date(date)]

  pd <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
    dplyr::filter(tag_outcome == !!tag_outcome) %>%
    dplyr::filter(date >= !!min_date_weekly) %>%
    dplyr::filter(granularity_time == "week") %>%
    dplyr::filter(location_code %in% !!location_code) %>%
    dplyr::collect()
  setDT(pd)
  pd[,date:=as.Date(date)]

  pd1 <- copy(pd)
  pd1[,type:="Ukentlig andel (%)"]
  pd1[, plot_n := 100 * n / n_denominator]
  pd1[, plot_u1 := 100 * n_baseline_thresholdu0 / n_denominator]
  pd1[, plot_u2 := 100 * n_baseline_thresholdu1 / n_denominator]

  pd2 <- copy(pd)
  pd2[,type:="Ukentlig konsultasjoner"]
  pd2[, plot_n := n]
  pd2[, plot_u1 := n_baseline_thresholdu0]
  pd2[, plot_u2 := n_baseline_thresholdu1]

  pd3 <- copy(pd)
  pd3[,type:="Ukentlig eksess"]
  pd3[,plot_n := pmax(0, n - n_baseline_thresholdu0)]
  pd3[, plot_u1 := 0]
  pd3[, plot_u2 := n_baseline_thresholdu1 - n_baseline_thresholdu0]

  if(granularity_geo != "municip"){
    pd4 <- copy(pdday)
    pd4[,type:="Daglig eksess"]
    pd4[,plot_n := pmax(0, n - n_baseline_thresholdu0)]
    pd4[, plot_u1 := 0]
    pd4[, plot_u2 := n_baseline_thresholdu1 - n_baseline_thresholdu0]

    pd <- rbind(pd1,pd2,pd3,pd4)
    pd[,type:=factor(
      type,
      levels=c(
        "Ukentlig andel (%)",
        "Ukentlig konsultasjoner",
        "Ukentlig eksess",
        "Daglig eksess"
      )
    )]
  } else {
    pd <- rbind(pd1,pd2,pd3)
    pd[,type:=factor(
      type,
      levels=c(
        "Ukentlig andel (%)",
        "Ukentlig konsultasjoner",
        "Ukentlig eksess"
      )
    )]
  }

  if(location_code %in% config$small_location_codes){
    title <- cowplot::ggdraw() +
      cowplot::draw_label(
        glue::glue(
          "{names(config$choices_location)[config$choices_location==location_code]}\n",
          "{names(config$choices_norsyss_tag)[config$choices_norsyss_tag==tag_outcome]}"
        ),
        fontface = 'bold',
        x = 0,
        hjust = 0,
        size=30
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 0)
      )

    cowplot::plot_grid(
      title,
      plot_trends_single(pd, "total"),
      ncol=1,
      rel_heights = c(0.2, 1),
      label_x = 0,
      hjust = 0,
      vjust = 1.4,
      label_size=26
    )
  } else {
    title <- cowplot::ggdraw() +
      cowplot::draw_label(
        glue::glue(
          "{names(config$choices_location)[config$choices_location==location_code]}\n",
          "{names(config$choices_norsyss_tag)[config$choices_norsyss_tag==tag_outcome]}"
        ),
        fontface = 'bold',
        x = 0,
        hjust = 0,
        size=30
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 0)
      )

    cowplot::plot_grid(
      title,
      plot_trends_single(pd, "total"),
      plot_trends_single(pd, "0-4"),
      plot_trends_single(pd, "5-14"),
      plot_trends_single(pd, "15-19"),
      plot_trends_single(pd, "20-29"),
      plot_trends_single(pd, "30-64"),
      plot_trends_single(pd, "65+"),
      ncol=1,
      rel_heights = c(0.3, rep(1,7)),
      labels=c(
        "",
        "Totalt",
        "0-4",
        "5-14",
        "15-19",
        "20-29",
        "30-64",
        "65+"
      ),
      label_x = 0,
      hjust = 0,
      vjust = 1.4,
      label_size=26
    )
  }
}
