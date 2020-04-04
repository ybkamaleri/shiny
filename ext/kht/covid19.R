covid19_ui <- function(id, config) {
  ns <- NS(id)
  dimensionId <- ns("dimension")

  tagList(
    fluidRow(
      column(
        width=12, align="left",
        p(
          id="toptext",
          strong("NorSySS er forkortelsen for Norwegian Syndromic Surveillance System som er en del av Sykdomspulsen."), br(),
          "Dette er et overvåkningssystem basert på diagnosekoder (ICPC-2 koder) satt på legekontor og legevakt i hele Norge.", br(), br(),

          strong("MSIS er det nasjonale overvåkingssystemet for smittsomme sykdommer. "),
          "Koronavirus med utbruddspotensiale ble definert som ny meldepliktig sykdom ",
          "i MSIS fra 31.01.2020. Både leger og laboratorier som påviser sykdommen skal ",
          "melde tilfellet til MSIS. Tallene gir en indikasjon på aktiviteten av covid-19, ",
          "men angir ikke nøyaktig antall covid-19 smittede i befolkningen.", br(), br(),

          "Formålet med denne siden er å gi en ", strong("oversikt over covid-19 epidemien "),
          "til bruk i kommuneoverlegens daglige arbeid. ",br(), br(),

          "Gi gjerne tilbakemeldinger og ønskede endringer via sykdomspulsen@fhi.no", br(), br(),

          "Under kan du velge blant to faner som gir deg forskjellig informasjon:", br(),
          "- ",strong("Oversikt")," fanen vil gi deg en rekke grafer, tabeller og kart hvor du kan velge det geografiske området du er interessert i", br(),
          "- ",strong("Informasjon")," fanen gir deg litt mer informasjon om dataene vi bruker", br()
        )
      )
    ),

    tabsetPanel(
      tabPanel(
        title="Oversikt",
        tagList(
          fluidRow(
            column(
              width=12, align="left",
              p(
                "For å overvåke covid-19 epidemien har vi valgt å følge ",
                "ekstra nøye med på to ICPC-2 diagnosekoder i primærhelsetjenesten:", br(), br(),

                strong("R991: Covid-19 (mistenkt eller bekreftet)"), " ble opprettet 06.03.2020. ",
                "Ikke alle legekontor og legevakt kunne benytte seg av denne koden ",
                "med en gang etter at den ble opprettet da de først ",
                "måtte implementere denne i deres journalsystem.",
                "De kliniske tegnene på covid-19 er akutt luftveisinfeksjon med ",
                "symptomer som feber, hoste og kortpustethet. Det er sesong ",
                "for vanlig forkjølelse og influensa som også kan gi slike ",
                "symptomer. Vi ønsker derfor å påpeke at covid-19 diagnosen ",
                "i denne sammenheng ikke nødvendigvis er koronavirus, ",
                "men overvåkningen kan gi en oversikt over utbredelse og ",
                "hvor stort press det er på primærhelsetjenesten.",br(),
                strong("R27: Engstelig for sykdom i luftveiene IKA"), " ble anbefalt ",
                "brukt av referansegruppen for primærmedisinsk kodeverk i ",
                "Direktoratet for e-helse og Legeforeningen 13. mars. ",
                "Denne koden skal brukes ved sykmelding/konsultasjon/kontakt ",
                "vedrørende covid-19, med unntak av bekreftet/mistenkt ",
                "koronavirus-sykdom (https://fastlegen.no/artikkel/diagnosekoder-ved-Covid-19). ",
                "Derfor følger vi også denne diagnosekoden i vår overvåkning av Covid-19.", br(), br(),

                strong("Under vil du se en rekke grafer, kart og tabeller som gir ",
                "en oversikt over det geografiske området du velger i ",
                "nedtrekksmenyen under. Du kan også begynne å skrive navnet ",
                "på ønsket fylke eller kommune så vil det automatisk komme ",
                "opp alternativer."), br(), br(),

                strong("Norge:"), " Gir en oversikt over Norge i tillegg til oversikt ",
                "over alle fylkene.", br(),
                strong("Fylke:"), " Gir en oversikt over det valgte fylket i tillegg ",
                "til en oversikt over alle kommunene i dette fylket.", br(),
                strong("Kommune:"), " Gir en oversikt over den valgte kommunen i ",
                "tillegg til en oversikt over nabokommunene.", br(), br(),

                "Informasjon om dataene kan du se under fanen 'Informasjon'",
                br(),br(),br()
              )
            )
          ),

          fluidRow(
            column(
              width=12, align="center",
              selectizeInput(
                inputId = ns("covid_location_code"),
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
            fluidRow(
              column(
                width=12, align="left",
                p(
                  strong("Figur 1")," viser antall covid-19 meldinger ",
                  "til MSIS sammenstilt med andel konsultasjoner ",
                  "for covid-19 (mistenkt eller bekreftet) på ",
                  "legekontor og legevakt (NorSySS). Denne grafen kan gi en ",
                  "oversikt over trendene i MSIS og NorSySS i ",
                  "forhold til hverandre. Det er noe forsinkelse ",
                  "både i MSIS dataene og NorSySS, derfor ",
                  "kan grafen endre seg etter hvert.")
              )
            ),

            fluidRow(
              column(
                width=12, align="left",
                br(),
                p(
                  strong("Figur 1.")," Antall covid-19 meldinger til MSIS og andel konsultasjoner for ",
                  "covid-19 (mistenkte eller bekreftet) på legekontor og legevakt"
                ),
                plotOutput(ns("overview_norsyss_vs_msis"), height = "700px"),
                br(),br(),br()
              )
            ),

            column(
              width=12, align="left",

              p(
                strong("Figur 2")," gir en oversikt over trendene for forskjellige luftveisagens. ",
                "Vi har inkludert andel konsultasjoner av fire forskjellige enkeltstående ",
                "ICPC-2 diagnosekoder og en kode der vi har samlet en rekke luftveis ",
                "diagnoser (R01, R02, R03, R04, R05, R07, R08, R09, R21, R24, R25, ",
                "R27, R29, R74, R75, R76, R77, R79, R80, R81, R82, R83, R99). ",
                "Se i fanen «informasjon» for å få navn på diagnosekodene som er med.", br(), br(),

                "Nevneren i andelen er totalt antall konsultasjoner per dag i det ",
                "samme geografisk område."
              )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              p(strong("Figur 2.")," Andel konsultasjoner med forskjellig luftveisagens."),
              plotOutput(ns("overview_plot_national_syndromes_proportion"), height = "700px"),
              br(),br(),br()
            )
          ),

          fluidRow(
            column(
              width=12, align="left",

              p(
                strong("Figur 3")," viser fordelingen av konsultasjoner på lege/legevakt ",
                "og telefon/e-konsultasjon/oppmøte ved hjelp av søyler som skal ",
                "leses av på x-aksen til venstre på grafen. Den røde linjen viser ",
                "andel konsultasjoner per dag og skal leses av på høyre side av grafen.", br(), br(),

                "Nevneren er totalt antall konsultasjoner per dag i valgt geografiske område.", br(), br(),

                "De røde pilene på x-aksen under grafen indikerer helger og helligdager. ",
                "Det er som regel færre konsultasjoner hos lege og legevakt i helger og ",
                "helligdager enn på hverdager."
              )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              p(
                strong("Figur 3."), " Antall konsultasjoner for covid-19 fordelt på type ",
                "konsultasjon samt andel konsultasjoner for covid-19."
              ),
              plotOutput(ns("overview_plot_national_source_proportion"), height = "700px"),
              br(),br(),br()
            )
          ),

          fluidRow(
            column(
              width=12, align="left",

              p(
                strong("Figur 4")," viser andel konsultasjoner med covid-19 ",
                "(mistenkt eller bekreftet) diagnose per aldersgruppe. Vær ",
                "oppmerksom på at aldersgruppene ikke er like store og ",
                strong("nevneren er totalt antall konsultasjoner per dag og per geografisk område. "),
                "Dette gir en oversikt over hvilke aldersgrupper som i hovedsak ",
                "kontakter lege og legevakt for covid-19 (mistenkt eller bekreftet). ",
                "For å se mer på spesifikke aldersgrupper se ", strong("Figur 5"), "."
              )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              p(
                strong("Figur 4."), "Andel konsultasjoner i Norge med covid-19 (mistenkt eller bekreftet) fordelt på aldersgruppe"
              ),
              plotOutput(ns("overview_plot_national_age_burden"), height = "700px"),
              br(),br(),br()
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              p(
                strong("Figur 5")," viser andel konsultasjoner med covid-19 ",
                "(mistenkt eller bekreftet) diagnose per aldersgruppe. ",
                "Vær oppmerksom på at ",
                strong("nevneren er totalt antall konsultasjoner per aldersgruppe, per dag og per geografisk område. "),
                "Dette gir en oversikt over hvordan trendene er for covid-19 ",
                "(mistenkt eller bekreftet) per aldersgruppe."
              )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              p(
                strong("Figur 5."), " Andel konsultasjoner med covid-19 (mistenkt eller bekreftet) ",
                "fordelt på aldersgrupper"
              ),
              plotOutput(ns("overview_plot_national_age_trends"), height = "700px"),
              br(),br(),br()
            )
          ),

          fluidRow(
            column(
              width=12, align="left",

              p(
                strong("Figur 6")," viser andel konsultasjoner med R991: Covid-19 ",
                "(mistenkt eller bekreftet) og R27: Engstelig luftveissykdom ",
                "IKA diagnose per geografisk område.",
                "Den blå og grønne boksen ",
                "viser prosentandel for den siste dagen i grafen.",
                "Nevneren er totalt antall konsultasjoner per dag i det viste ",
                "geografiske området."
              )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              p(
                strong("Figur 6."), " Andel konsultasjoner av R991: Covid-19 (mistenkt eller bekreftet) ",
                "og R27: Engstelig luftveissykdom IKA diagnose per geografisk område."
              ),
              uiOutput(ns("overview_ui_county_proportion")),
              br(),br(),br()
              #plotOutput(ns("overview_plot_county_proportion"), height = "900px")
            )
          ),

          fluidRow(
            column(
              width=12, align="left",

              p(
                strong("Figur 7")," viser det geografiske området du har valgt ",
                "øverst på siden med kumulativt antall konsultasjoner per ",
                "geografisk område.",
                "De kommunene som ikke har legevakt eller legekontor vil ",
                "bli hvite med røde 'X' i disse kartene da vi ikke har noe data over ",
                "disse kommunene. De som bor i disse kommunene drar til ",
                "legekontor i andre kommuner."
                )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              p(strong("Figure 7.")),
              plotOutput(ns("overview_map_county_proportion"), height = "600px"),
              br(),br(),br()
            )
          ),

          fluidRow(
            column(
              width=12, align="left",

              p(
                strong("Figur 8")," viser det geografiske området du har valgt ",
                "øverst på siden med andel konsultasjoner for R991 og R27.",
                "De kommunene som ikke har legevakt eller legekontor vil ",
                "bli hvite med røde 'X' i disse kartene da vi ikke har noe data over ",
                "disse kommunene. De som bor i disse kommunene drar til ",
                "legekontor i andre kommuner."
              )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              p(strong("Figure 8.")),
              plotOutput(ns("overview_map_county_proportion_2"), height = "600px"),
              br(),br(),br()
            )
          )
        )
      ),
      tabPanel(
        title="Informasjon",
        tagList(
          fluidRow(
            column(
              width=1,
              p("")
            ),
            column(
              width=12, align="left",

              p(
                strong("Vi får data til covid-19 overvåkingen via NorSySS "),
                "Diagnosekoder som registreres hos lege eller legevakt sendes ",
                "til Helsedirektoratet som en del av legenes refusjonskrav ",
                "(KUHR-systemet). Folkehelseinstituttet mottar daglig ",
                "oppdatert KUHR-data til NorSyss Dataene er ",
                "anonyme når vi mottar dem, uten pasientidentifikasjon, ",
                "men med informasjon om kjønn, aldersgruppe, konsultasjonsdato ",
                "og sted for konsultasjon.", br(), br(),

                strong("Informasjon om dataene i covid-19 overvåkingen:"), br(),
                "- Telefon, legekontakt og e-konsultasjon er inkludert", br(),
                "- Legekontor og legevakt er inkludert", br(),
                "- Geografisk område basert på stedet for legekonsultasjon, ikke pasientens bosted", br(),
                "- De kommunene som ikke har legevakt eller legekontor har vi ikke noe data for. De som bor i disse kommunene bruker legevakt og legekontor i andre kommuner. Dersom du har valgt en av kommunene uten legevakt eller legekontor vil det stå «error» for noen av grafene fordi vi ikke har data.", br(),
                "- Personene som bor i kommuner uten lege og legevakt benytter legekontor og legevakt i andre kommuner", br(),
                "- Antallet konsultasjoner er vanligvis lavere i ferier og på helligdager. Dette er spesielt tydelig rundt jul/nyttår og påske, men også i sommerferieukene.", br(),
                "- Det kan være mer enn 14 dager forsinkelse i dataene da de kommer fra KUHR systemet. Dersom det for noen datoer ikke er registrert noen konsultasjoner fra et geografisk område vil dette vises som røde stiplede linjer i grafene.", br(), br(),

                strong("Luftvei diagnosekoder (samlet) inneholder:"), br(),
                "- R01: Smerte luftveier", br(),
                "- R02: Kortpustethet/dyspne", br(),
                "- R03: Piping i brystet", br(),
                "- R04: Pusteproblem IKA", br(),
                "- R05: Hoste", br(),
                "- R07: Nysing/tett nese", br(),
                "- R08: Nese symptomer/plager", br(),
                "- R09: Bihule symptomer/plager IKA", br(),
                "- R21: Hals symptomer /plager", br(),
                "- R24: Blodig oppspytt/hemoptyse", br(),
                "- R25: Unormalt oppspytt/plager IKA", br(),
                "- R27: Engstelig for sykdom luftveier IKA", br(),
                "- R29: Luftveier symptomer/plager IKA", br(),
                "- R74: Akutt øvere luftveisinfeksjon", br(),
                "- R75: Bihulebetennelse", br(),
                "- R76: Akutt tonsilitt", br(),
                "- R77: Akutt laryngitt/bronkitt", br(),
                "- R79: Kronisk bronkitt", br(),
                "- R80: Influensa", br(),
                "- R81: Lungebetennelse", br(),
                "- R82: Pleuritt IKA", br(),
                "- R83: Luftveisinfeksjon IKA", br(),
                "- R99: Luftveissykdom IKA", br(),
                "- R991: Covid-19 (mistenkt eller bekreftet)", br(), br(),

                "Overvåkingen av covid-19 er noe annerledes enn NorSySS ",
                "overvåkingen i Sykdomspulsen. Siden diagnosekoden covid-19 ",
                "(mistenkt eller bekreftet) ble implementert 06. mars har ",
                "vi ikke mulighet til å gjøre regresjonsanalyser som for ",
                "de andre diagnosekodene da vi ikke har data bakover i tid ",
                "(regresjonsanalysene i NorSySS inkluderer 5 år bakover i tid).", br(), br(),

                "Antallet konsultasjoner er lavere i ferier og på helligdager. ",
                "Dette er spesielt tydelig rundt jul/nyttår og påske, men også i ",
                "sommerferieukene.", br(), br(),

                strong("Kommunereformen:"), "Kommuner som har blitt slått sammen og fått ",
                "et nytt navn vil ikke finnes i oversiktene. Kommuner som har ",
                "blitt slått sammen med en annen kommune men beholdt navnet ",
                "vil vises i oversiktene, og beregningene tar hensyn til ",
                "sammenslåingen. Det samme gjelder sammenslåtte kommuner ",
                "som får nytt kommunenavn.", br(), br(),

                strong("Interkommunalt samarbeid om legekontor/legevakt: "),
                "I NorSyss er geografisk område basert på stedet for ",
                "legekonsultasjon, ikke pasientens bosted. Derfor vil ",
                "legekontorets/legevaktens postadresse si hvilken kommune ",
                "som vises i NorSyss De andre kommunene som er med på ",
                "det interkommunale samarbeidet vil ikke har noe data.", br(), br(),

                strong("Type konsultasjon:"),br(),
                strong("Oppmøte"), " inkluderer takstkodene: 2ad, 2ak, 2fk, 11ak, 11ad", br(),
                strong("Telefonkonsultasjon"), " inkluderer takstkodenene: 1ad, 1ak, 1bd, 1bk, 1h, 1g, 2ae", br(),
                strong("e-konsultasjon"), " inkluderer takstkodene: 1be", br(), br(),

                strong("Ved tekniske feil, spørsmål eller tilbakemeldinger "),
                "vennligst send en mail til sykdomspulsen@fhi.no."


              )
            ),
            column(
              width=1,
              p("")
            )
          )
        )
      )
    )
  )
}

covid19_server <- function(input, output, session, config) {
  #width <-  as.numeric(input$dimension[1])

  output$overview_plot_national_syndromes_proportion <- renderCachedPlot({
    req(input$covid_location_code)

    covid19_overview_plot_national_syndromes_proportion(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  output$overview_plot_national_source_proportion <- renderCachedPlot({
    req(input$covid_location_code)

    covid19_overview_plot_national_source_proportion(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  output$overview_plot_national_age_burden <- renderCachedPlot({
    req(input$covid_location_code)

    covid19_overview_plot_national_age_burden(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  output$overview_plot_national_age_trends <- renderCachedPlot({
    req(input$covid_location_code)

    covid19_overview_plot_national_age_trends(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  output$overview_plot_county_proportion <- renderCachedPlot({
    req(input$covid_location_code)

    covid19_overview_plot_county_proportion(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  output$overview_ui_county_proportion <- renderUI({
    ns <- session$ns
    req(input$covid_location_code)

    location_codes <- get_dependent_location_codes(location_code = input$covid_location_code)
    height <- round(250*ceiling(length(location_codes)/4))
    height <- max(400, height)
    height <- paste0(height,"px")
    plotOutput(ns("overview_plot_county_proportion"), height = height)
  })

  output$overview_map_county_proportion <- renderCachedPlot({

    covid19_overview_map_county_proportion(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )


  output$overview_map_county_proportion_2 <- renderCachedPlot({

    covid19_overview_map_county_proportion_2(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  output$overview_norsyss_vs_msis <- renderCachedPlot({

    covid19_norsyss_vs_msis_one_graph(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )


  outputOptions(output, "overview_plot_national_syndromes_proportion", priority = 10)
  outputOptions(output, "overview_plot_national_age_burden", priority = 9)
  outputOptions(output, "overview_plot_county_proportion", priority = 8)
  outputOptions(output, "overview_map_county_proportion", priority = 7)
  outputOptions(output, "overview_map_county_proportion_2", priority = 7)

}


covid19_overview_plot_national_syndromes_proportion <- function(
  location_code,
  config
){

    pd <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_vk_ote",
        "engstelig_luftveissykdom_ika_vk_ote",
        "influensa_vk_ote",
        "rxx_for_covid19_vk_ote",
        "akkut_ovre_luftveisinfeksjon_vk_ote"
      )) %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(age == "totalt") %>%
      dplyr::filter(location_code == !!location_code) %>%
      dplyr::select(tag_outcome, date, n, consult_with_influenza) %>%
      dplyr::collect()
    setDT(pd)
    pd[, date:= as.Date(date)]

    pd[, andel := 100*n/consult_with_influenza]
    pd[, no_data := consult_with_influenza==0]
    pd[is.nan(andel), andel := 0]

    pd[, name_outcome := factor(
      tag_outcome,
      levels = c(
        "covid19_vk_ote",
        "engstelig_luftveissykdom_ika_vk_ote",
        "influensa_vk_ote",
        "akkut_ovre_luftveisinfeksjon_vk_ote",
        "rxx_for_covid19_vk_ote"
      ),
      labels = c(
        "Covid-19 (mistenkt\neller bekreftet) (R991)",
        "Engstelig luftveissykdom\nIKA (R27)",
        "Influensa (R80)",
        "Akutt øvre\nluftveisinfeksjon (R74)",
        "Luftvei diagnosekoder\n(samlet*)"
      )
    )]

    labels <- pd[date == max(date)]
    #labels[,]

    q <- ggplot(pd, aes(x=date, y=andel, color=name_outcome))
    q <- q + geom_line(size=4)
    if(sum(pd$no_data)>0){
      q <- q + geom_vline(data=pd[no_data==TRUE], mapping=aes(xintercept = date),color= "red", lty=3, lwd=1.5)
    }
    # q <- q + ggrepel::geom_label_repel(
    #   data = labels,
    #   mapping = aes(label = name_outcome),
    #   nudge_y = 1,
    #   nudge_x = 0.5,
    #   direction = "y",
    #   angle = 0,
    #   vjust = 0,
    #   hjust = 0,
    #   label.r=0,
    #   segment.size = 1,
    #   label.size = 1,
    #   label.padding = 1,
    #   box.padding = 1,
    #   size=8
    # )
    q <- q + scale_y_continuous(
      "Andel",
      breaks = fhiplot::pretty_breaks(6),
      expand = expand_scale(mult = c(0, 0.1)),
      labels = fhiplot::format_nor_perc_0
    )
    q <- q + expand_limits(y = 0)
    q <- q + scale_x_date(
      NULL,
      date_breaks = "2 days",
      date_labels = "%d.%m",
      expand = expand_scale(mult = c(0.02, 0.02))
    )
    q <- q + fhiplot::scale_color_fhi(NULL)
    q <- q + fhiplot::theme_fhi_lines(
      20,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel_on_top = F
    )
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + theme(legend.position="bottom")
    q <- q + labs(title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "Andel konsultasjoner med forskjellig luftveisagens\n",
      "Data fra NorSySS"
    ))
    q <- q + labs(caption=glue::glue(
      "Nevneren er totalt antall konsultasjoner\n",
      "*R- 01, 02, 03, 04, 05, 06, 07, 08, 09, 21, 24, 25, 27, 29, 72, 74, 75, 76, 77, 78, 79, 81, 82, 83, 99, 991\n",
      "Røde stiplede vertikale linjer på grafen betyr at ingen konsultasjoner er rapportert på disse dagene\n",
      "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
    ))
    q
}

covid19_overview_plot_national_source_proportion <- function(
  location_code,
  config
){

    pd <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_k_o",
        "covid19_k_t",
        "covid19_k_e",
        "covid19_v_o",
        "covid19_v_t",
        "covid19_v_e"
      )) %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(age == "totalt") %>%
      dplyr::filter(location_code == !!location_code) %>%
      dplyr::select(tag_outcome, date, n, consult_with_influenza) %>%
      dplyr::collect()
    setDT(pd)
    pd[, date:= as.Date(date)]

    pd[,
       contact_type := dplyr::case_when(
         stringr::str_detect(tag_outcome, "_e$") ~ "e-konsultasjon",
         stringr::str_detect(tag_outcome, "_t$") ~ "Telefon",
         stringr::str_detect(tag_outcome, "_o$") ~ "Oppmøte"
       )]

    pd[,
       practice_type := dplyr::case_when(
         stringr::str_detect(tag_outcome, "_k_") ~ "Legekontor",
         stringr::str_detect(tag_outcome, "_v_") ~ "Legevakt"
       )]
    pd[, cat:= paste0(contact_type,"/",practice_type)]
    pd[,cat := factor(
      cat,
      levels = c(
        "e-konsultasjon/Legekontor",
        "e-konsultasjon/Legevakt",
        "Telefon/Legekontor",
        "Telefon/Legevakt",
        "Oppmøte/Legekontor",
        "Oppmøte/Legevakt"
      )
    )]
    pd[,no_data := sum(consult_with_influenza)==0, by=date]

    weekends <- unique(pd$date)
    weekends <- weekends[lubridate::wday(weekends, week_start = 1) %in% c(6,7)]
    weekends <- data.frame(date = weekends)

    max_y <- max(pd[,.(n=sum(n)),by=.(date)]$n, na.rm=T)
    min_y_start <- -0.085*max_y*1.01
    min_y_end <- -0.05*max_y*1.01

    pd_line <- pd[,.(
      n=sum(n),
      consult_with_influenza=sum(consult_with_influenza)
    ),
    by=.(date)
    ]
    pd_line[, andel := n/consult_with_influenza]
    pd_line[is.nan(andel), andel:=0]
    max_andel <- max(pd_line$andel, na.rm=T)
    pd_line[, andel := max_y * andel / max_andel]

    q <- ggplot(pd, aes(x=date, y=n))
    q <- q + geom_col(mapping=aes(fill=cat))
    q <- q + geom_line(data=pd_line, mapping=aes(y=andel),color="red", size = 3)
    if(sum(pd$no_data)>0){
      q <- q + geom_vline(data=pd[no_data==TRUE], mapping=aes(xintercept = date),color= "red", lty=3, lwd=1.5)
    }
    q <- q + geom_segment(
      data = weekends,
      mapping = aes(
        x = date, xend=date,
        y = min_y_start, yend = min_y_end
      ),
      color = "red",
      size = 1,
      arrow = arrow(length = unit(0.1, "inches"))
    )
    q <- q + fhiplot::scale_fill_fhi(NULL)
    q <- q + scale_y_continuous(
      "Antall",
      breaks = fhiplot::pretty_breaks(6),
      labels = fhiplot::format_nor,
      sec.axis = sec_axis(
        ~ . * 100 / max_y * max_andel,
        breaks = fhiplot::pretty_breaks(6),
        labels = fhiplot::format_nor_perc_0,
        name = "Andel"
      )
    )
    q <- q + scale_x_date(
      "",
      date_breaks = "2 days",
      date_labels = "%d.%m"
    )
    q <- q + fhiplot::scale_color_fhi("Syndrome", guide = "none")
    q <- q + fhiplot::theme_fhi_lines(
      20,
      panel_on_top = T,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + coord_cartesian(ylim=c(0, max_y), clip="off", expand = F)
    q <- q + labs(title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "Antall konsultasjoner for covid-19 fordelt på type konsultasjon samt andel konsultasjoner for covid-19\n",
      "Data fra NorSySS"
    ))
    q <- q + labs(caption=glue::glue(
      "Røde piler på x-aksen viser helger og helligdager\n",
      "Søylene skal leses av på venstre side, den røde linjen skal leses av på høyre side\n",
      "Nevneren på andelen er totalt antall konsultasjoner per dag i valgt geografisk område\n",
      "Røde stiplede vertikale linjer på grafen betyr at ingen konsultasjoner er rapportert på disse dagene\n",
      "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
    ))
    q
}

covid19_overview_plot_national_age_burden <- function(
  location_code,
  config
){

  pd <- pool %>% dplyr::tbl("data_norsyss") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::filter(tag_outcome == "covid19_vk_ote") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(age != "totalt") %>%
    dplyr::select(date, age, n, consult_with_influenza) %>%
    dplyr::collect()
  setDT(pd)
  pd[, date:= as.Date(date)]

  pd[,age:=factor(
    age,
    levels = c(
      "0-4",
      "5-14",
      "15-19",
      "20-29",
      "30-64",
      "65+"
    )
  )]
  pd[, consult_with_influenza_totalt := sum(consult_with_influenza), by=.(date)]

  pd[, andel := 100*n/consult_with_influenza_totalt]
  pd[, no_data := consult_with_influenza_totalt==0]
  pd[is.nan(andel), andel := 0]

  labels <- pd[date == max(date)]

  max_date_uncertain <- max(pd$date)
  min_date_uncertain <- max_date_uncertain-6
  q <- ggplot(pd, aes(x=date,y=andel))
  q <- q + geom_col(mapping=aes(fill=age))
  if(sum(pd$no_data)>0){
    q <- q + geom_vline(data=pd[no_data==TRUE], mapping=aes(xintercept = date),color= "red", lty=3, lwd=1.5)
  }
  q <- q + scale_y_continuous(
    "Andel",
    breaks = fhiplot::pretty_breaks(6),
    expand = expand_scale(mult = c(0, 0.1)),
    labels = fhiplot::format_nor_perc_0
  )
  q <- q + expand_limits(y = 0)
  q <- q + scale_x_date(
    NULL,
    date_breaks = "2 days",
    date_labels = "%d.%m"
  )
  q <- q + fhiplot::scale_fill_fhi("Aldersgruppe")
  q <- q + fhiplot::theme_fhi_lines(20, panel_on_top = T,
                                    panel.grid.major.x = element_blank(),
                                    panel.grid.minor.x = element_blank()
  )
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + labs(title = glue::glue(
    "{names(config$choices_location)[config$choices_location==location_code]}\n",
    "Andel konsultasjoner i Norge med covid-19 (mistenkt eller bekreftet) fordelt på aldersgruppe\n",
    "Data fra NorSySS"
  ))
  q <- q + labs(caption=glue::glue(
    "For alle aldersgruppene er nevneren totalt antall konsultasjoner (alle aldersgrupper summert)\n",
    "Røde stiplede vertikale linjer på grafen betyr at ingen konsultasjoner er rapportert på disse dagene\n",
    "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
  ))
  q
}

covid19_overview_plot_national_age_trends <- function(
  location_code,
  config
){

  pd <- pool %>% dplyr::tbl("data_norsyss") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::filter(tag_outcome == "covid19_vk_ote") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::select(date, age, n, consult_with_influenza) %>%
    dplyr::collect()
  setDT(pd)
  pd[, date:= as.Date(date)]

  pd[,age:=factor(
    age,
    levels = c(
      "totalt",
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

  pd[, andel := 100*n/consult_with_influenza]

  pd[, consult_with_influenza_totalt := sum(consult_with_influenza), by=.(date)]
  pd[, no_data := consult_with_influenza_totalt==0]
  pd[is.nan(andel), andel := 0]

  labels <- pd[date == max(date)]

  max_date_uncertain <- max(pd$date)
  min_date_uncertain <- max_date_uncertain-6
  q <- ggplot(pd, aes(x=date,y=andel))
  q <- q + geom_col(fill = fhiplot::base_color, width=0.8)
  if(sum(pd$no_data)>0){
    q <- q + geom_vline(data=pd[no_data==TRUE], mapping=aes(xintercept = date),color= "red", lty=3, lwd=1.5)
  }
  q <- q + scale_y_continuous(
    "Andel",
    breaks = fhiplot::pretty_breaks(5),
    expand = expand_scale(mult = c(0, 0.1)),
    labels = fhiplot::format_nor_perc_0
  )
  q <- q + expand_limits(y = 0)
  q <- q + scale_x_date(
    NULL,
    date_breaks = "4 days",
    date_labels = "%d.%m"
  )
  q <- q + lemon::facet_rep_wrap(~age, repeat.tick.labels = "all", ncol=3)
  q <- q + fhiplot::theme_fhi_lines(20, panel_on_top = T,
                                    panel.grid.major.x = element_blank(),
                                    panel.grid.minor.x = element_blank()
  )
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + labs(title = glue::glue(
    "{names(config$choices_location)[config$choices_location==location_code]}\n",
    "Andel konsultasjoner med covid-19 (mistenkt eller bekreftet) fordelt på aldersgrupper\n",
    "Data fra NorSySS"
  ))
  q <- q + labs(caption=glue::glue(
    "Nevneren er totalt antall konsultasjoner per dato, geografisk område og aldersgruppe\n",
    "Røde stiplede vertikale linjer på grafen betyr at ingen konsultasjoner er rapportert på disse dagene\n",
    "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
  ))
  q
}

covid19_overview_plot_county_proportion <- function(
  location_code,
  config
){

    location_codes <- get_dependent_location_codes(location_code = location_code)

    pd <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_vk_ote",
        "engstelig_luftveissykdom_ika_vk_ote"
      )) %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(age == "totalt") %>%
      dplyr::filter(location_code %in% !!location_codes) %>%
      dplyr::select(tag_outcome, location_code, date, n, consult_with_influenza) %>%
      dplyr::collect()
    setDT(pd)
    pd[, date:= as.Date(date)]

    pd[
      fhidata::norway_locations_long_b2020,
      on="location_code",
      location_name:=location_name
      ]

    pd[, andel := 100*n/consult_with_influenza]
    pd[, name_outcome := factor(
      tag_outcome,
      levels = c(
        "covid19_vk_ote",
        "engstelig_luftveissykdom_ika_vk_ote"
      ),
      labels = c(
        "Covid-19 (mistenkt eller bekreftet) (R991)",
        "Engstelig luftveissykdom IKA (R27)"
      )
    )]

    pd[,location_code := factor(location_code, levels = location_codes)]
    setorder(pd,location_code)
    location_names <- unique(pd$location_name)
    pd[,location_name := factor(location_name, levels = location_names)]

    pd[,no_data := sum(consult_with_influenza)==0, by=.(location_code,date)]

    max_val <- max(pd$andel,na.rm=T)
    labels <- pd[date == max(date)]
    labels[tag_outcome=="covid19_vk_ote",lab := paste0("R991: ",format_nor_perc(andel))]
    labels[tag_outcome=="engstelig_luftveissykdom_ika_vk_ote",lab := paste0("R27: ",format_nor_perc(andel))]

    labels[tag_outcome=="covid19_vk_ote",lab := paste0("R991: ",format_nor_perc(andel))]


    labels[tag_outcome=="covid19_vk_ote",andel := max_val]
    labels[tag_outcome=="engstelig_luftveissykdom_ika_vk_ote",andel := max_val-5]
    labels[, date:= min(pd$date)]

    q <- ggplot(pd, aes(x=date, y=andel))
    q <- q + geom_col(mapping = aes(fill=name_outcome), position = "dodge", width=1)
    if(sum(pd$no_data)>0){
      q <- q + geom_vline(data=pd[no_data==TRUE], mapping=aes(xintercept = date),color= "red", lty=3, lwd=1.5)
    }
    if(location_code=="norge") q <- q + ggrepel::geom_label_repel(
      data = labels,
      mapping = aes(label = lab, y=andel, color=name_outcome),
      nudge_y = 0.0,
      nudge_x = 0.0,
      direction = "y",
      angle = 0,
      vjust = 0,
      hjust = 0,
      label.r=0,
      segment.size = 0,
      segment.alpha = 0,
      label.size = 0.5,
      label.padding = 0.5,
      box.padding = 0.25,
      size=4
    )
    q <- q + lemon::facet_rep_wrap(~location_name, repeat.tick.labels = "y", ncol=3)
    q <- q + scale_y_continuous(
      "Andel",
      breaks = fhiplot::pretty_breaks(5),
      expand = expand_scale(mult = c(0, 0.1)),
      labels = fhiplot::format_nor_perc_0
    )
    q <- q + expand_limits(y = 0)
    q <- q + scale_x_date(
      NULL,
      date_breaks = "4 days",
      date_labels = "%d.%m",
      expand = expand_scale(mult = c(0.02, 0.02))
    )
    q <- q + fhiplot::scale_fill_fhi(NULL)
    q <- q + fhiplot::scale_color_fhi(NULL, guide="none")
    q <- q + fhiplot::theme_fhi_lines(20, panel_on_top = F,
                                      panel.grid.major.x = element_blank(),
                                      panel.grid.minor.x = element_blank()
    )
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + theme(legend.position="bottom")
    q <- q + labs(title=glue::glue(
      "Andel konsultasjoner av\n",
      "R991: Covid-19 (mistenkt eller bekreftet) og\n",
      "R27: Engstelig luftveissykdom IKA diagnose per geografisk område\n",
      "Data fra NorSySS"
    ))
    q <- q + labs(caption=glue::glue(
      "Nevneren er totalt antall konsultasjoner per dag i det viste geografiske området.\n",
      "Røde stiplede vertikale linjer på grafen betyr at ingen konsultasjoner er rapportert på disse dagene\n",
      "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
    ))
    q
}

covid19_overview_map_county_proportion <- function(
  location_code,
  config
){

    granularity_geo <- get_granularity_geo(location_code = location_code)
    location_codes <- get_dependent_location_codes(location_code = location_code)

    if(granularity_geo == "nation"){
      d <- pool %>% dplyr::tbl("data_norsyss") %>%
        dplyr::filter(tag_outcome %in% c(
          "covid19_vk_ote",
          "engstelig_luftveissykdom_ika_vk_ote"
        )) %>%
        dplyr::filter(date >= !!config$start_date) %>%
        dplyr::filter(granularity_geo == "county") %>%
        dplyr::filter(age == "totalt") %>%
        dplyr::collect()
    } else {
      d <- pool %>% dplyr::tbl("data_norsyss") %>%
        dplyr::filter(tag_outcome %in% c(
          "covid19_vk_ote",
          "engstelig_luftveissykdom_ika_vk_ote"
        )) %>%
        dplyr::filter(date >= !!config$start_date) %>%
        dplyr::filter(granularity_geo == "municip") %>%
        dplyr::filter(location_code %in% !!location_codes) %>%
        dplyr::filter(age == "totalt") %>%
        dplyr::collect()
    }
    setDT(d)
    d[, no_data := sum(consult_with_influenza) == 0, by=.(location_code)]
    setorder(d,tag_outcome, location_code, date)
    d[,cum_n := cumsum(n), by=.(tag_outcome, location_code)]
    d <- d[date==max(date)]

    cut_points <- unique(round(c(0, quantile(d$cum_n, probs = c(0.25, 0.5, 0.75, 1)))))
    breaks <- c(-1, cut_points)
    labels <- paste0(breaks[-length(breaks)]+1, "-", breaks[-1])
    labels[labels=="0-0"] <- "0"

    # summary(d$cum_n)
    d[, category := cut(cum_n, breaks = breaks, labels = labels)]
    # xtabs(~d$category, addNA=T)

    d[, name_outcome := factor(
      tag_outcome,
      levels = c(
        "covid19_vk_ote",
        "engstelig_luftveissykdom_ika_vk_ote"
      ),
      labels = c(
        "Covid-19 (mistenkt eller bekreftet) (R991)",
        "Engstelig luftveissykdom IKA (R27)"
      )
    )]

    if(granularity_geo == "nation"){
      pd <- merge(
        fhidata::norway_map_counties_with_insert_b2020,
        d,
        on="location_code",
        allow.cartesian = TRUE
      )
    } else {
      pd <- merge(
        fhidata::norway_map_municips_b2020,
        d,
        on="location_code",
        allow.cartesian = TRUE
      )
    }

    q <- ggplot()
    q <- q + geom_polygon(
      data = pd,
      aes( x = long, y = lat, group = group, fill=category),
      color="black",
      size=0.2
    )
    if(sum(pd$no_data)>0){
      q <- q + geom_polygon(
        data = pd[no_data==TRUE],
        aes( x = long, y = lat, group = group),
        color="black",
        fill = "white",
        size=0.2
      )
      pd_mid <- pd[no_data==T,.(
        long = mean(long),
        lat = mean(lat)
      ),
      keyby=.(location_code)
      ]
      q <- q + geom_text(
        data = pd_mid,
        mapping = aes(x = long, y= lat, color = "Ingen data"),
        label = "x",
        size = 12
      )
    }
    if(granularity_geo == "nation"){
      q <- q + annotate(
        "text",
        x = fhidata::norway_map_insert_title_position_b2020$long,
        y = fhidata::norway_map_insert_title_position_b2020$lat,
        label = "Oslo",
        size = 8
      )
    }
    q <- q + lemon::facet_rep_wrap(~name_outcome, repeat.tick.labels = "y", ncol=4)
    q <- q + theme_void(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + coord_quickmap()
    q <- q + fhiplot::scale_fill_fhi("Kumulativt\nantall",palette = "map_seq_missing_x2", direction = -1, drop=F)
    q <- q + scale_color_manual(NULL, values="red")
    q <- q + guides(fill = guide_legend(order=1), color = guide_legend(order=2))
    q <- q + labs(title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "Kumulativt antall konsultasjoner f.o.m {format(config$start_date,'%d.%m.%Y')} t.o.m {format(config$max_date_uncertain,'%d.%m.%Y')}\n",
      "Data fra NorSySS\n\n"
    ))
    q <- q + labs(caption = glue::glue(
      "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
      ))
    q
}



covid19_overview_map_county_proportion_2 <- function(
  location_code,
  config
){

  granularity_geo <- get_granularity_geo(location_code = location_code)
  location_codes <- get_dependent_location_codes(location_code = location_code)

  if(granularity_geo == "nation"){
    d <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_vk_ote",
        "engstelig_luftveissykdom_ika_vk_ote"
      )) %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(granularity_geo == "county") %>%
      dplyr::filter(age == "totalt") %>%
      dplyr::collect()
  } else {
    d <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_vk_ote",
        "engstelig_luftveissykdom_ika_vk_ote"
      )) %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(granularity_geo == "municip") %>%
      dplyr::filter(location_code %in% !!location_codes) %>%
      dplyr::filter(age == "totalt") %>%
      dplyr::collect()
  }
  setDT(d)
  d[, no_data := sum(consult_with_influenza) == 0, by=.(location_code)]
  setorder(d,tag_outcome, location_code, date)
  d[,cum_n := cumsum(n), by=.(tag_outcome, location_code)] # summing the two outcomes
  d[,cum_w_flu := cumsum(consult_with_influenza), by=.(tag_outcome, location_code)] # summing the consults with influenza
  d <- d[date==max(date)]
  d[, andel := 100*cum_n/cum_w_flu]
  d[is.nan(andel), andel := 0]

  # summary(d$cum_n)
  d[, category := fancycut::wafflecut(
    x = andel,
    intervals = c(
      0,
      "(0,2.5]",
      "(2.5,5]",
      "(5,10]",
      "(10,15]",
      "(15,100]"
    ),
    buckets = c(
      "0.0%",
      "0.1-2.5%",
      "2.6-5.0%",
      "5.1-10.0%",
      "10.1-15.0%",
      "15.1%+"
    )
  )]

  # xtabs(~d$category, addNA=T)

  d[, name_outcome := factor(
    tag_outcome,
    levels = c(
      "covid19_vk_ote",
      "engstelig_luftveissykdom_ika_vk_ote"
    ),
    labels = c(
      "Covid-19 (mistenkt eller bekreftet) (R991)",
      "Engstelig luftveissykdom IKA (R27)"
    )
  )]

  if(granularity_geo == "nation"){
    pd <- merge(
      fhidata::norway_map_counties_with_insert_b2020,
      d,
      on="location_code",
      allow.cartesian = TRUE
    )
  } else {
    pd <- merge(
      fhidata::norway_map_municips_b2020,
      d,
      on="location_code",
      allow.cartesian = TRUE
    )
  }

  q <- ggplot()
  q <- q + geom_polygon(
    data = pd,
    aes( x = long, y = lat, group = group, fill=category),
    color="black",
    size=0.2
  )
  if(sum(pd$no_data)>0){
    q <- q + geom_polygon(
      data = pd[no_data==TRUE],
      aes( x = long, y = lat, group = group),
      color="black",
      fill = "white",
      size=0.2
    )
    pd_mid <- pd[no_data==T,.(
      long = mean(long),
      lat = mean(lat)
      ),
      keyby=.(location_code)
      ]
    q <- q + geom_text(
      data = pd_mid,
      mapping = aes(x = long, y= lat, color = "Ingen data"),
      label = "x",
      size = 12
    )
  }
  if(granularity_geo == "nation"){
    q <- q + annotate(
      "text",
      x = fhidata::norway_map_insert_title_position_b2020$long,
      y = fhidata::norway_map_insert_title_position_b2020$lat,
      label = "Oslo",
      size = 8
    )
  }
  q <- q + lemon::facet_rep_wrap(~name_outcome, repeat.tick.labels = "y", ncol=4)
  q <- q + theme_void(20)
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + coord_quickmap()
  q <- q + fhiplot::scale_fill_fhi("Andel konsultasjoner",palette = "map_seq_missing_x2", direction = -1, drop=F)
  q <- q + scale_color_manual(NULL, values="red")
  q <- q + guides(fill = guide_legend(order=1), color = guide_legend(order=2))
  q <- q + labs(title = glue::glue(
    "{names(config$choices_location)[config$choices_location==location_code]}\n",
    "Andel konsultasjoner f.o.m {format(config$start_date,'%d.%m.%Y')} t.o.m {format(config$max_date_uncertain,'%d.%m.%Y')}\n",
    "Data fra NorSySS\n\n"
  ))
  q <- q + labs(caption = glue::glue(
    "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
  ))
  q
}

covid19_norsyss_vs_msis <- function(
  location_code,
  config
){

  d_norsyss <- pool %>% dplyr::tbl("data_norsyss") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(granularity_time=="day") %>%
    dplyr::filter(tag_outcome %in% "covid19_vk_ote") %>%
    dplyr::filter(age=="totalt") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(date, n, consult_with_influenza) %>%
    dplyr::collect()
  setDT(d_norsyss)
  d_norsyss[, date:= as.Date(date)]
  d_norsyss[, perc_norsyss := 100* n / consult_with_influenza]
  d_norsyss[, n := NULL]
  d_norsyss[, consult_with_influenza := NULL]

  d_msis <- pool %>% dplyr::tbl("data_covid19_msis") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(date, n) %>%
    dplyr::collect()
  setDT(d_msis)
  d_msis[, date:= as.Date(date)]

  x_location_code <- location_code
  x_pop <- fhidata::norway_population_b2020[
    year==2020 & location_code==x_location_code,
    .(
      pop = sum(pop)
    ),
    keyby=.(location_code)
    ]

  d_msis[, pop := x_pop$pop]
  d_msis[, per_100000_msis := 100000 * n / pop]
  d_msis[, n:=NULL]
  d_msis[, pop:=NULL]

  q <- ggplot(d_norsyss, aes(x=date, y = perc_norsyss))
  q <- q + geom_col(fill = fhiplot::base_color)
  q <- q + scale_y_continuous(
    "Andel konsultasjoner",
    breaks = fhiplot::pretty_breaks(6),
    labels = fhiplot::format_nor_perc_0,
    expand = expand_scale(mult = c(0, 0.1))
  )
  q <- q + expand_limits(y = 0)
  q <- q + scale_x_date(
    NULL,
    date_breaks = "2 days",
    date_labels = "%d.%m"
  )
  q <- q + fhiplot::theme_fhi_lines(
    20, panel_on_top = T,
    legend_position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + labs(title = glue::glue(
    "{names(config$choices_location)[config$choices_location==location_code]}\n",
    "Andel konsultasjoner som tilhører covid-19 (mistenkt eller bekreftet) (R991)\n"
  ))
  q <- q + labs(caption=glue::glue(
    "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
  ))
  q1 <- q


  q <- ggplot(d_msis, aes(x=date, y = per_100000_msis))
  q <- q + geom_col(fill = fhiplot::base_color)
  q <- q + scale_y_continuous(
    "Antall per 100.000 befolkning",
    breaks = fhiplot::pretty_breaks(6),
    expand = expand_scale(mult = c(0, 0.1))
  )
  q <- q + expand_limits(y = 0)
  q <- q + scale_x_date(
    NULL,
    date_breaks = "2 days",
    date_labels = "%d.%m"
  )
  q <- q + fhiplot::scale_color_fhi(NULL)
  q <- q + fhiplot::theme_fhi_lines(
    20, panel_on_top = T,
    legend_position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + labs(title = glue::glue(
    "{names(config$choices_location)[config$choices_location==location_code]}\n",
    "Antall bekreftet covid-19 tilfeller fra MSIS per 100.000 befolkning"
  ))
  q <- q + labs(caption=glue::glue(
    "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
  ))
  q2 <- q

  cowplot::plot_grid(
    q1,
    q2,
    ncol=1,
    align = "v",
    axis = "l"
  )
}


covid19_norsyss_vs_msis_one_graph <- function(
  location_code,
  config
){

  d_norsyss <- pool %>% dplyr::tbl("data_norsyss") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(granularity_time=="day") %>%
    dplyr::filter(tag_outcome %in% "covid19_vk_ote") %>%
    dplyr::filter(age=="totalt") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(date, n, consult_with_influenza) %>%
    dplyr::collect()
  setDT(d_norsyss)
  d_norsyss[, date:= as.Date(date)]
  d_norsyss[, perc_norsyss := 100* n / consult_with_influenza]
  d_norsyss[is.nan(perc_norsyss), perc_norsyss := 0]
  d_norsyss[, n := NULL]
  d_norsyss[, no_data := consult_with_influenza==0]
  d_norsyss[,consult_with_influenza := NULL]

  d_msis <- pool %>% dplyr::tbl("data_covid19_msis") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(date, n) %>%
    dplyr::collect()
  setDT(d_msis)
  d_msis[, date:= as.Date(date)]
  setnames(d_msis, "n", "n_msis")

  pd <- merge(
    d_msis,
    d_norsyss,
    by="date",
    all = T
  )

  pd <- melt.data.table(
    pd,
    id.vars = c(
      "date",
      "no_data"
    )
  )
  pd <- pd[!is.na(value)]
  pd[, scaled_value := value]
  max_left <- max(pd[variable != "perc_norsyss"]$value)+1
  max_right <- max(pd[variable == "perc_norsyss"]$value)
  pd[variable == "perc_norsyss", scaled_value := value / max_right * max_left]

  pd[, variable_pretty := factor(
    variable,
    levels = c(
      "perc_norsyss",
      "n_msis"
      ),
    labels = c(
      "Andel konsultasjoner",
      "Antall lab-bekreftet tilfeller"
    )
  )]

  weekends <- unique(pd$date)
  weekends <- weekends[lubridate::wday(weekends, week_start = 1) %in% c(6,7)]
  weekends <- data.frame(date = weekends)

  max_y <- max(pd$scaled_value, na.rm=T)+3
  min_y_start <- -0.085*max_y*1.01
  min_y_end <- -0.05*max_y*1.01

  q <- ggplot(pd, aes(x=date, y = scaled_value))
  q <- q + geom_col(data=pd[variable=="n_msis"], fill=fhiplot::base_color, width=0.8)
  q <- q + geom_line(data=pd[variable=="perc_norsyss"],lwd = 4, color="red")
  if(sum(pd$no_data)>0){
    q <- q + geom_vline(data=pd[no_data==TRUE], mapping=aes(xintercept = date),color= "red", lty=3, lwd=1.5)
  }
  q <- q + geom_segment(
    data = weekends,
    mapping = aes(
      x = date, xend=date,
      y = min_y_start, yend = min_y_end
    ),
    color = "red",
    size = 1,
    arrow = arrow(length = unit(0.1, "inches"))
  )
  q <- q + scale_y_continuous(
    "Antall tilfeller meldt til MSIS",
    breaks = fhiplot::pretty_breaks(6),
    expand = expand_scale(mult = c(0, 0.1)),
    sec.axis = sec_axis(
      ~ . * max_right / max_left,
      breaks = fhiplot::pretty_breaks(6),
      labels = fhiplot::format_nor_perc_0,
      name = "Andel NorSySS konsultasjoner\n"
    )
  )
  q <- q + expand_limits(y = 0)
  q <- q + scale_x_date(
    "",
    date_breaks = "2 days",
    date_labels = "%d.%m"
  )
  q <- q + fhiplot::scale_color_fhi(NULL)
  q <- q + fhiplot::theme_fhi_lines(
    20, panel_on_top = T,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend_position = "bottom"
  )
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + coord_cartesian(ylim=c(0, max_y), clip="off", expand = F)
  q <- q + labs(title = glue::glue(
    "{names(config$choices_location)[config$choices_location==location_code]}\n",
    "Antall covid-19 meldinger til MSIS og andel konsultasjoner for covid-19 (mistenkte eller bekreftet)\n",
    "på legekontor og legevakt\n",
    "Data fra NorSySS og MSIS"
  ))
  q <- q + labs(caption=glue::glue(

    "Røde piler på x-aksen viser helger og helligdager\n",
    "Søylene skal leses av på venstre side, den røde linjen skal leses av på høyre side\n",
    "Nevneren på andelen er totalt antall konsultasjoner per dag i valgt geografisk område\n",
    "Røde stiplede vertikale linjer på grafen betyr at ingen konsultasjoner er rapportert på disse dagene\n",
    "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
  ))
  q
}
