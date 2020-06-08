library(ggrepel)

covid19_comparison_ui <- function(id, config){

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
                     selected = "norge",
                     options = list(
                       placeholder = "Skriv områder du vil sammenligne her",
                       maxItems = 10
                     ),
                     size = "600px")
    ),

    fluidRow(
      column(
        width = 6, align = "right",
        checkboxInput(ns("cumulative_chk"), "Kumulativ", value = T)
      )
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
      shinycssloaders::withSpinner(plotOutput(ns("norsyss_plot"), height = "800px"))
    ),
    br(),
    br(),
    br()
  )
}


covid19_comparison_server <- function(input, output, session, config){
  ns <- session$ns

  output$msis_plot <- renderCachedPlot({
    req(input$int_input_location)

    covid19_int_msis(
      location_codes = input$int_input_location,
      cumulative = input$cumulative_chk,
      config = config
    )

  }, cacheKeyExpr = {list(
    input$int_input_location,
    input$cumulative_chk,
    input$reset_btn,
    dev_invalidate_cache
  )},
  res = 72
  )


  output$norsyss_plot <- renderCachedPlot({
    req(input$int_input_location)

    covid19_int_norsyss(
      location_codes = input$int_input_location,
      cumulative = input$cumulative_chk,
      config = config
    )

  }, cacheKeyExpr = {list(
    input$int_input_location,
    input$cumulative_chk,
    input$reset_btn,
    dev_invalidate_cache
  )},
  res = 72
  )


}



covid19_int_msis <- function(location_codes, cumulative, config){

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

  d <- use_int_cumulative(d = d, cumulative = cumulative)

  plotTitle <- ifelse(cumulative,
                      "Kummulativt antall tilfeller av covid-19\nData fra MSIS",
                      "Antall tilfeller av covid-19\nData fra MSIS"
  )

  covid19_int_gen_plot(d = d,
                       legend_position = ifelse(cumulative,"none","bottom"),
                       labs_title = plotTitle,
                       labs_x = glue::glue("{fhi::nb$AA}r-ukenummer"),
                       labs_y = "pr. 1 000 innbyggere",
                       cumulative = cumulative)

}


covid19_int_norsyss <- function(location_codes, cumulative, config){

  d <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::filter(location_code %in%!!location_codes)%>%
    dplyr::filter(granularity_time=="day")%>%
    dplyr::filter(tag_outcome %in% "covid19_vk_ote") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(yrwk, location_code, age, n, consult_with_influenza) %>%
    dplyr::group_by(location_code, age, yrwk) %>%
    dplyr::summarize(n=sum(n), consult_with_influenza=sum(consult_with_influenza)) %>%
    dplyr::collect()

  setDT(d)
  setkey(d, location_code, age, yrwk)

  d[, age:=factor(
    age,
    levels = c("total","0-4","5-14","15-19","20-29","30-64","65+"),
    labels = c("Totalt","0-4","5-14","15-19","20-29","30-64","65+")
    )]
  d[,censor := ""]
  d[censor=="" & n>0 & n<5, censor := "N"]
  d[censor != "", n := 0]

  d[,cum_n := cumsum(n), by=.(location_code, age)]

  ## for reordering the yrwk
  d[, rank := 1:.N, by = .(location_code)]

  d_p <- fhidata::norway_population_b2020[year==2020]
  d_p[, age := fancycut::fancycut(
    age,
    "0-4"="[0,4]",
    "5-14"="[5,14]",
    "15-19"="[15,19]",
    "20-29"="[20,29]",
    "30-64"="[30,64]",
    "65+"="[65,999]",
  )]
  d_p_x <- copy(d_p)
  d_p_x[,age:="Totalt"]
  d_p <- rbind(d_p, d_p_x)
  d_p <- d_p[
  ,.(
    pop=sum(pop)
  ),keyby=.(age,location_code)]

  d[
    d_p,
    on=c("age","location_code"),
    pop:=pop
  ]

  d[,pr1000_cum_n := 1000*cum_n/pop]

  d <- use_int_cumulative(d = d, cumulative = cumulative)


  plotTitle <- ifelse(cumulative,
                      "Kummulativt antall konsultasjoner med mistenkt, sannsynlig eller bekreftet covid-19 (R991 og R992)\nData fra NorSySS",
                      "Antall konsultasjoner med mistenkt, sannsynlig eller bekreftet covid-19 (R991 og R992)\nData fra NorSySS"
  )

  covid19_int_gen_plot(d = d,
                       labs_title = plotTitle,
                       legend_position = "bottom",
                       labs_x = glue::glue("{fhi::nb$AA}r-ukenummer"),
                       labs_y = "pr. 1 000 innbyggere",
                       cumulative = cumulative,
                       facet = TRUE)

}


use_int_cumulative <- function(d = NULL, cumulative = FALSE){

  if (cumulative) {
    d[, y_var := 1000 * cum_n / pop][]
  } else {
    d[, y_var := 1000 * n / pop][]
  }

  return(d)

}


covid19_int_gen_plot <- function(
                                 d = NULL,
                                 legend_position = "none",
                                 labs_title = NULL,
                                 labs_caption = NULL,
                                 labs_x = NULL,
                                 labs_y = NULL,
                                 cumulative = FALSE,
                                 facet = FALSE
                                 ){


  ## To make space and reordering
  if (facet){
    d[, rank := 1:.N, by = .(location_code, age)]
  }else{
    d[, rank := 1:.N, by = .(location_code)]
  }
  x_right <- max(d$rank)
  x_left <- min(d$rank)

  if (cumulative) {
    x_left_extra <- x_left * 0.8
    x_right_extra <- x_right * 1.2
    x_min_ann <- x_right * 1.02
    x_max_ann <- x_right * 1.3
  }else{
    x_left_extra <- x_left * 0.8
    x_right_extra <- x_right * 1.02
  }

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
  max_y <- max(d$y_var)
  max_y_extra <- 1.05 * max_y

  q <- ggplot(d, aes(x = yrwk, y = y_var))

  if(facet){
    q <- q + geom_line(aes(color = loc_name, group = loc_name), size = 2)
    q <- q + lemon::facet_rep_wrap( ~ age, repeat.tick.labels = "y")

    q <- q + fhiplot::theme_fhi_lines(
      base_size = 20,
      panel_on_top = FALSE,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )

  }else{
    q <- q + geom_line(aes(color = loc_name, group = loc_name), size = 2)
    q <- q + coord_cartesian(ylim=c(0, max_y_extra),
                             xlim = c(x_left_extra, x_right_extra),
                             clip="off", expand = F)

    if (cumulative){
      q <-  q + annotate(geom = "rect",
                         xmin = x_min_ann,
                         xmax = x_max_ann,
                         ymin = -0.5, ymax = Inf, #still not able to cover the x-axis line?
                         fill = "white"
                         )

      q <- q + ggrepel::geom_text_repel(
        data = subset(d, yrwk == max(yrwk)),
        mapping = aes(label = loc_name),
        hjust = 0,
        size = 6,
        direction = "y",
        nudge_x = 0.5,
        segment.size = 0.3,
        show.legend = FALSE
        )
    }

    q <- q + fhiplot::theme_fhi_lines(
      base_size = 20,
      panel_on_top = FALSE,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  }


  q <- q + fhiplot::scale_color_fhi()
  q <- q + fhiplot::set_x_axis_vertical()
  q <- q + labs(title = labs_title)
  q <- q + labs(caption = labs_caption)
  q <- q + labs(x = labs_x, y = labs_y)
  formatter <- function(x) fhiplot::format_nor(x, digits = 1)
  q <- q + scale_y_continuous(
    labels=formatter,
    expand = expand_scale(mult = c(0, 0.1))
  )
  q <- q + theme(legend.position = legend_position,
                 legend.key.size = unit(1, "cm"),
                 legend.title = element_blank())


  point_size <- ifelse(facet, 1.5, 4)

  q <- q + geom_point(
    data = subset(d, yrwk == max(yrwk)),
    aes(color = loc_name),
    size = point_size,
    show.legend = FALSE
  )

  q <- q + geom_point(
    data = subset(d, yrwk == min(yrwk)),
    aes(color = loc_name),
    size = point_size,
    show.legend = FALSE
  )

  q

}
