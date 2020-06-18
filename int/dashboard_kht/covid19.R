covid19_ui <- function(id, config) {
  ns <- NS(id)
  dimensionId <- ns("dimension")

  tagList(
    fluidRow(
      column(
        width=12, align="left",
        p(
          id="toptext",
          strong("Informasjonen som finnes på denne siden er anonym, men er ment for kommuneleger fordi det krever kunnskap for å tolke disse på riktig måte. Dette er ikke ment som en offisiell statistikk."),br(),br(),

          "Formålet med denne siden er å gi en ",
          "oversikt over covid-19 epidemien ",
          "til bruk i kommuneoverlegens daglige arbeid. ",br(),

          "Gi gjerne tilbakemeldinger og ønskede endringer via",
          strong("sykdomspulsen@fhi.no"), br(), br(),

          "I covid-19 overvåkningen bruker vi NorSySS, MSIS og laboratoriedatabasen.",
          "NorSySS dataene blir oppdatert hver morgen.",
          "MSIS og laboratorie dataene blir oppdatert ca kl 13 hver ukedag, i helger og på helligdager blir de foreløpig ikke oppdatert.",
          "Det er noe forsinkelse i alle dataene, derfor ",
          "kan figurene endre seg etter hvert.",
          "Mer informasjon om dataene finner du i 'Informasjon' fanen under.", br(),br(),

          "Under kan du velge blant to faner som gir deg forskjellig informasjon:", br(),
          "- ",strong("Oversikt")," fanen vil gi deg en rekke figurer, tabeller og kart hvor du kan velge det geografiske området du er interessert i", br(),
          "- ",strong("Modellering")," fanen vil gi deg beregninger fra FHIs spredningsmodell hvor du kan velge det geografiske området du er interessert i", br(),
          "- ",strong("Informasjon")," fanen gir deg litt mer informasjon om dataene vi bruker", br()
        )
      )
    ),

    tabsetPanel(
      id="covid19",

      tabPanel(
        title="Oversikt",
        tagList(
          fluidRow(
            column(
              width=12, align="left",
              p(

                strong("Under vil du se en rekke figurer, kart og tabeller som gir ",
                "en oversikt over det geografiske området du velger i ",
                "nedtrekksmenyen under. Du kan begynne å skrive navnet ",
                "på ønsket fylke eller kommune så vil det automatisk komme ",
                "opp alternativer."),
                 br(), br(),

                "De røde pilene på x-aksen under figuren indikerer helger og helligdager. ",
                "Det er som regel færre konsultasjoner hos lege og legevakt i helger og ",
                "helligdager enn på hverdager.",
                "En rød stjerne (*) på x-aksen under figuren indikerer at det er",
                " mellom 1 og 4 konsultasjoner i nevneren for NorSySS.",
                "Røde stiplede vertikale linjer i figuren indikerer at det",
                "ikke er rapportert noen konsultasjoner på denne datoen for dette geografiske området.",
                br(), br(),

                strong("Norge:"), " Gir en oversikt over Norge i tillegg til oversikt ",
                "over alle fylkene. De nasjonale dataene er aggregert på dagsnivå.", br(),
                strong("Fylke:"), " Gir en oversikt over det valgte fylket i tillegg ",
                "til en oversikt over alle kommunene i dette fylket. Fylkesdataene er aggregert på dagsnivå.", br(),
                strong("Kommune:"), " Gir en oversikt over den valgte kommunen i ",
                "tillegg til en oversikt over resten av kommunene i fylket. Kommunedataene er aggregert på ukesnivå.",
                "Vær oppmerksom på at noen kommuner har veldig få konsultasjoner,",
                "derfor vil ikke trendene kunne brukes på en god måte.",
                "Kommuner med under 500 innbyggere vil mangle aldersdelte figurer pga anonymitet.", br(), br(),
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
                #choices = config$choices_location,
                #selected = "norge",
                choices = NULL,
                selected = NULL,
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
                  "til MSIS (blå søyler) sammenstilt med andel konsultasjoner ",
                  "for covid-19 (mistenkt, sannsynlig eller bekreftet) på ",
                  "legekontor og legevakt gjennom NorSySS (rød linje) og andel positive laboratorietester (kun på Norgesnivå).",
                  "Denne figuren kan gi en ",
                  "oversikt over trendene i forhold til hverandre.",
                  br(),
                  "Du kan laste ned en tabell for denne figuren.",
                  " I tabellen vil det kunne være noen celler uten tall. Dette er sensurerte data.",
                  " Se mer informasjon om sensurerte data i 'Informasjon' fanen.",
                  br(),
                  downloadButton(ns("download_xls"), "Last ned tabell", class = "knappe"),
                  tags$head(tags$style(".knappe{background-color:#add8e6;} .knappe{color: #111;}")),
                )
              )
            ),

            fluidRow(
              column(
                width=12, align="left",
                br(),
                p(
                  ),
                shinycssloaders::withSpinner(plotOutput(ns("overview_norsyss_vs_msis"), height = "700px")),
                br()
              )
            ),

            column(
              width=12, align="left",

              p(
                strong("Figur 2")," gir en oversikt over trendene for forskjellige",
                "luftveisagens. ",
                "Vi har inkludert andel konsultasjoner av fire forskjellige enkeltstående",
                " ICPC-2 diagnosekoder og en kode der vi har samlet en rekke luftveis ",
                "diagnoser (R01, R02, R03, R04, R05, R07, R08, R09, R21, R24, R25, ",
                "R27, R29, R74, R75, R76, R77, R79, R80, R81, R82, R83, R991, R992). ",
                "Se i fanen «informasjon» for å få navn på diagnosekodene som er med.",
                br(),

              )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              shinycssloaders::withSpinner(plotOutput(ns("overview_plot_national_syndromes_proportion"), height = "700px")),
              br()
            )
          ),

          fluidRow(
            column(
              width=12, align="left",

              p(
                strong("Figur 3")," viser fordelingen av konsultasjoner på lege/legevakt ",
                "og telefon/e-konsultasjon/oppmøte ved hjelp av søyler som skal ",
                "leses av på x-aksen til venstre på figuren. Den røde linjen viser ",
                "andel konsultasjoner per dag og skal leses av på høyre side av figuren.",
                br(),

              )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              p(

              ),
              shinycssloaders::withSpinner(plotOutput(ns("overview_plot_national_source_proportion"), height = "700px")),
              br()
            )
          ),

          fluidRow(
            column(
              width=12, align="left",

              p(
                strong("Figur 4")," viser andel konsultasjoner med covid-19 ",
                "(mistenkt, sannsynlig eller bekreftet) diagnose per aldersgruppe. Vær ",
                "oppmerksom på at aldersgruppene ikke er like store og ",
                strong("nevneren er totalt antall konsultasjoner per dag og per geografisk område. "),
                "Dette gir en oversikt over hvilke aldersgrupper som i hovedsak ",
                "kontakter lege og legevakt for covid-19. ",
                "For å se mer på spesifikke aldersgrupper se ", strong("Figur 5"),
                br(),
              )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              p(
               ),
              shinycssloaders::withSpinner(plotOutput(ns("overview_plot_national_age_burden"), height = "700px")),
              br()
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              p(
                strong("Figur 5")," viser andel konsultasjoner med covid-19 ",
                "(mistenkt, sannsynlig eller bekreftet) diagnose per aldersgruppe. ",
                "Vær oppmerksom på at ",
                strong("nevneren er totalt antall konsultasjoner per aldersgruppe, per dag og per geografisk område. "),
                "Dette gir en oversikt over hvordan trendene er for covid-19 ",
                "(mistenkt eller bekreftet) per aldersgruppe.",
                br(),
              )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              p(

              ),
              shinycssloaders::withSpinner(plotOutput(ns("overview_plot_national_age_trends"), height = "700px")),
              br()
            )
          ),

          fluidRow(
            column(
              width=12, align="left",

              p(
                strong("Figur 6")," viser andel konsultasjoner med R991 og R992 sammen: Covid-19 ",
                "(mistenkt, sannsynlig eller bekreftet) og R27: Engstelig luftveissykdom ",
                "IKA diagnose per geografisk område.",br(),

              )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              p(
                 ),
              uiOutput(ns("overview_ui_county_proportion")),
              br()
              #shinycssloaders::withSpinnerplotOutput(ns("overview_plot_county_proportion"), height = "900px")
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
                "bli hvite med rødt 'X' i kartet da vi ikke har noe data over ",
                "disse kommunene. De som bor i disse kommunene drar til ",
                "legekontor og legevakt i andre kommuner."
                )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              shinycssloaders::withSpinner(plotOutput(ns("overview_map_county_proportion"), height = "600px")),
              br()
            )
          ),

          fluidRow(
            column(
              width=12, align="left",

              p(
                strong("Figur 8")," viser det geografiske området du har valgt ",
                "øverst på siden med andel konsultasjoner for R991 og R27.",
                "De kommunene som ikke har legevakt eller legekontor vil ",
                "bli hvite med rødt 'X' i kartet da vi ikke har noe data over ",
                "disse kommunene. De som bor i disse kommunene drar til ",
                "legekontor og legevakt i andre kommuner."
              )
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              shinycssloaders::withSpinner(plotOutput(ns("overview_map_county_proportion_2"), height = "600px")),
              br()
            )
          )
        )
      ),

      tabPanel("Sammenlikning",
               covid19_comparison_ui("covid19_comparison", config)),

      tabPanel(
        title="Modellering",
        covid19_modelling_ui("covid19_modelling", config=config)
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
                strong("Vi får data til covid-19 overvåkingen via NorSySS, MSIS og laboratoriedatabasen"),br(), br(),

                strong("NorSySS"),
                "er forkortelsen for Norwegian Syndromic Surveillance System som er en del av Sykdomspulsen.", br(),
                "Dette er et overvåkningssystem basert på diagnosekoder (ICPC-2 koder) satt på legekontor og legevakt i hele Norge.",
                "Diagnosekodene sendes ",
                "til Helsedirektoratet som en del av legenes refusjonskrav ",
                "(KUHR-systemet). Folkehelseinstituttet mottar daglig ",
                "oppdatert KUHR-data til NorSySS. Dataene er ",
                "anonyme når vi mottar dem, uten pasientidentifikasjon, ",
                "men med informasjon om kjønn, aldersgruppe, konsultasjonsdato ",
                "og sted for konsultasjon.", br(),
                "For å overvåke covid-19 epidemien har vi valgt å følge ",
                "ekstra nøye med på tre ICPC-2 diagnosekoder i primærhelsetjenesten:",br(),

                strong("- R991: Covid-19 (mistenkt eller bekreftet)"), " ble opprettet 06.03.2020, men endret til ",
                br(),
                strong("- R991: Covid-19 (mistenkt/sannsynlig) og R992: Covid-19 (bekreftet)"), " 04.05.2020. ",
                "For å få mest mulig enhetlig data for hele tidsperioden viser vi R991 og R992 samlet for tiden",
                "etter 04.05.2020. Vi vurderer å endre dette etterhvert.",br(),
                "De kliniske tegnene på covid-19 er akutt luftveisinfeksjon med ",
                "symptomer som feber, hoste og kortpustethet. Det er sesong ",
                "for vanlig forkjølelse og influensa som også kan gi slike ",
                "symptomer. Vi ønsker derfor å påpeke at covid-19 diagnosen ",
                "i denne sammenheng ikke nødvendigvis er koronavirus, ",
                "men overvåkningen kan gi en oversikt over utbredelse og ",
                "hvor stort press det er på primærhelsetjenesten.",br(),
                strong("- R27: Engstelig for sykdom i luftveiene IKA"), " ble anbefalt ",
                "brukt av referansegruppen for primærmedisinsk kodeverk i ",
                "Direktoratet for e-helse og Legeforeningen 13.03.2020. ",
                "Denne koden skal brukes ved sykmelding/konsultasjon/kontakt ",
                "vedrørende covid-19, med unntak av bekreftet/mistenkt ",
                "koronavirus-sykdom (https://fastlegen.no/artikkel/diagnosekoder-ved-Covid-19). ",
                 br(), br(),

                strong("MSIS"),
                "er det nasjonale overvåkingssystemet for smittsomme sykdommer. ",
                "Koronavirus med utbruddspotensiale ble definert som ny meldepliktig sykdom ",
                "i MSIS fra 31.01.2020. Både leger og laboratorier som påviser sykdommen skal ",
                "melde tilfellet til MSIS. Tallene gir en indikasjon på aktiviteten av covid-19, ",
                "men angir ikke nøyaktig antall covid-19 smittede i befolkningen.", br(), br(),

                strong("MSIS laboratoriedatabasen"),
                "brukes for laboratoriedata.",
                "Elektroniske kopisvar går direkte fra laboratoriene inn til MSIS laboratoriedatabase.",
                "Tallene oppgjøres på antall personer som testes og ikke antall analyser.",
                "En person kan ha fått utført mer enn en analyse for covid-19.",
                "Antall testet og andel positive funn blant de testede påvirkes av endringer i testkriterier.",
                br(), br(),

                strong("Informasjon om dataene i covid-19 overvåkingen:"), br(),
                "- På nasjonalt og fylkesnivå",
                "vil figuren vise data per dag (dag.måned).",
                "På kommunenivå vil figuren vise data per uke (år-ukenummer).",br(),
                 "- Geografisk område basert på stedet for legekonsultasjon, ikke pasientens bosted.", br(),
                 "- Dersom du har valgt en av kommunene uten legevakt eller legekontor vil det vises",
                 " figurer med røde stiplede vertikale linjer fordi vi ikke har data.",
                "Personene som bor i kommuner uten lege og legevakt benytter legekontor",
                "og legevakt i andre kommuner.", br(),
                 "- Kommuner med under 500 innbyggere vil mangle aldersdelte figurer",
                 " og det vil stå «ikke noe data å vise på dette geografiske nivået» isteden pga anonymitetshensyn.", br(),
                 "- Vær oppmerksom på at noen kommuner har veldig få konsultasjoner,",
                "derfor vil ikke trendene kunne brukes på en god måte.",  br(),
                "- Konsultasjoner med telefon, legekontakt og e-konsultasjon er samlet i alle figurene",
                "bortsett fra figur 3 der de vises hver for seg.", br(),
                "- Konsultasjoner på både legekontor og legevakt er samlet i alle figurene",
                "bortsett fra figur 3 der de vises hver for seg.", br(),
                "- Røde piler på x-aksen under figuren indikerer helger og helligdager.",
                " Det er som regel færre konsultasjoner hos lege og legevakt i helger og helligdager enn på hverdager.", br(),
                  "- Det kan være mer enn 14 dager forsinkelse i dataene da de kommer fra KUHR systemet.",
                  "Dersom det for noen datoer ikke er registrert noen konsultasjoner fra et geografisk område",
                  "vil dette vises som røde stiplede linjer i figurene.", br(),
                br(),

                strong("Sensurerte data:"), br(),
                 "- Ved 1-4 konsultasjoner i nevneren vil dataene bli sensurert,",
                 " det vil si at de ikke bli vist,",
                 " men merket med rød * i eller under figurene.",br(),
                "- Ved 1-4 konsultasjoner i telleren for figur 3, 4 eller 5 vil dataene bli sensurert,",
                " det vil si at de ikke bli vist",
                " men merket med rød * i eller under figurene. For figur 3 gjelder dette kun for oppmøte.",br(),
                "- Den høyeste verdien som vi viser for andel er 60+.",br(),
                "- Nedlastbare tabeller vil vise åpne celler (ingen tall) dersom dataene er sensurert.",
                " Disse dataene følger samme regler som beskrevet over når det gjelder sensur.",
                br(), br(),

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
                "- R991: Covid-19 (mistenkt/sannsynlig)", br(),
                "- R992: Covid-19 (bekreftet)", br(), br(),

                "Overvåkingen av covid-19 er noe annerledes enn NorSySS ",
                "overvåkingen i Sykdomspulsen. Siden diagnosekoden covid-19 ",
                "(mistenkt eller bekreftet) ble implementert 06. mars har ",
                "vi ikke mulighet til å gjøre regresjonsanalyser som for ",
                "de andre diagnosekodene da vi ikke har data bakover i tid ",
                "(regresjonsanalysene i NorSySS inkluderer 5 år bakover i tid).", br(), br(),

                "Antallet konsultasjoner er lavere i helger, ferier og på helligdager. ",
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
                strong("Telefonkonsultasjon"), " inkluderer takstkodenene: 1ad, 1ak, 1bd, 1bk, 1h, 1g", br(),
                strong("e-konsultasjon"), " inkluderer takstkodene: 1be, 2ae", br(), br(),br(),


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

  norsyss_vs_msis_list <- reactive({

      covid19_norsyss_vs_msis(
      location_code = input$covid_location_code,
      config = config
    )
  })

  output$overview_norsyss_vs_msis <- renderCachedPlot({

    norsyss_vs_msis_list()$pd_plot

  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  ## Download table
  output$download_xls <- downloadHandler(

    filename = function(){ paste0("covid19_overview_", lubridate::today(), ".xlsx")},
    content = function(file){
      writexl::write_xlsx(norsyss_vs_msis_list()$pd_xl, file)
    }
  )
  ## ----


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
    height <- round(250 + 150*ceiling(length(location_codes)/3))
    height <- max(600, height)
    height <- paste0(height,"px")
    shinycssloaders::withSpinner(plotOutput(ns("overview_plot_county_proportion"), height = height))
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


  outputOptions(output, "overview_norsyss_vs_msis", priority = 100)
}

covid19_plot_single <- function(
  granularity_time = "day",
  d_left,
  d_right = NULL,
  d_third = NULL,
  censored,
  no_data,
  type_left="col",
  group_left=FALSE,
  labs_left = NULL,
  labs_right = NULL,
  labs_title = NULL,
  labs_caption = NULL,
  labs_legend = NULL,
  legend_position = "bottom",
  right_legend_labs = NULL,
  right_legend_direction = 1,
  multiplier_min_y_censor = -0.13,
  multiplier_min_y_end = -0.14,
  multiplier_min_y_start = -0.175,
  left_labels = fhiplot::format_nor_perc_0
){

  stopifnot(type_left %in% c("col","line"))

  d_left <- copy(d_left)
  d_right <- copy(d_right)
  if(granularity_time == "day"){
    setnames(d_left,"date","time")
    if(!is.null(d_right)) setnames(d_right,"date","time")
    if(!is.null(d_third)) setnames(d_third, "date", "time")
  } else {
    setnames(d_left,"yrwk","time")
    if(!is.null(d_right)) setnames(d_right,"yrwk","time")
  }

  if(type_left=="col"){
    max_left <- max(d_left[,.(value=sum(value)),by=.(time)]$value)
  } else {
    max_left <- max(d_left$value)
  }
  max_left <- max(c(max_left, 5))

  max_right <- max(d_right$value)
  max_right <- max(c(max_right, 5))

  if(!is.null(d_third)){
    max_right <- max(c(max_right, max(d_third$value)))
    d_third[, scaled_value := value / max_right * max_left]
  }

  if(!is.null(d_right)){
    d_right[, scaled_value := value]
    d_right[, scaled_value := value / max_right * max_left]
  }

  if(granularity_time=="day"){
    weekends <- get_free_days(
      date_start = min(c(d_left$time,d_right$time)),
      date_end = max(c(d_left$time,d_right$time))
    )
    weekends <- data.frame(time = weekends)
  }

  censored <- data.frame(time = censored)
  no_data <- data.frame(time = no_data)

  max_y <- max(c(max_left, d_right$scaled_value, na.rm=T))
  max_y <- max(c(max_y, 5))

  min_y_censor <- multiplier_min_y_censor*max_y
  min_y_end <- multiplier_min_y_end*max_y
  min_y_start <- multiplier_min_y_start*max_y

  q <- ggplot(mapping=aes(x=time))
  if(type_left=="col" & group_left==FALSE){
    q <- q + geom_col(
      data=d_left,
      mapping = aes(y=value),
      fill=fhiplot::base_color,
      width=0.8
    )
  } else if(type_left=="col" & group_left==TRUE){
    q <- q + geom_col(
      data=d_left,
      mapping = aes(y=value, fill=group),
      width=0.8
    )
  } else if(type_left=="line" & group_left==FALSE){
    q <- q + geom_line(
      data=d_left,
      mapping = aes(y=value, color=group, group=1),
      lwd = 3
    )
  } else if(type_left=="line" & group_left==TRUE){
    q <- q + geom_line(
      data=d_left,
      mapping = aes(y=value, color=group, group=group),
      lwd = 3
    )
  }


  if(is.null(d_third) && !is.null(d_right)){
    q <- q + geom_line(
      data=d_right,
      mapping = aes(y=scaled_value, group=1),
      lwd = 3,
      color="red")
  }

  if(!is.null(d_third) && !is.null(d_right)){

    q <- q + geom_col(
      data = d_left,
      mapping = aes(y = value,
                    fill = right_legend_labs[3]),
      width = 0.8
      )

    q <- q + geom_line(
      data=d_right,
      mapping = aes(y=scaled_value,
                    group=1,
                    color = right_legend_labs[1]
                    ),
      lwd = 3
    )

    q <- q + geom_line(
      data = d_third,
      mapping = aes(y = scaled_value,
                    group = 1,
                    color = right_legend_labs[2]
                    ),
      size = 3

    )

  }



  if(nrow(no_data)>0) q <- q + geom_vline(data=no_data, mapping=aes(xintercept = time),color= "red", lty=3, lwd=1.5)
  if(nrow(censored)>0) q <- q + geom_label(
    data=censored,
    label="*",
    y = min_y_censor,
    size=12,
    label.r = grid::unit(0, "lines"),
    label.size = NA,
    color="red"
  )
  if(granularity_time=="day") if(nrow(weekends)>0) q <- q + geom_segment(
    data = weekends,
    mapping = aes(
      x = time,
      xend=time
    ),
    y = min_y_start,
    yend = min_y_end,
    color = "red",
    size = 1,
    arrow = arrow(length = unit(0.1, "inches"))
  )
  if(is.null(d_right)){
    q <- q + scale_y_continuous(
      labs_left,
      breaks = fhiplot::pretty_breaks(5),
      labels = left_labels,
      expand = expand_scale(mult = c(0, 0.1))
    )
  } else {
    q <- q + scale_y_continuous(
      labs_left,
      breaks = fhiplot::pretty_breaks(5),
      labels = left_labels,
      expand = expand_scale(mult = c(0, 0.1)),
      sec.axis = sec_axis(
        ~ . * max_right / max_left,
        breaks = fhiplot::pretty_breaks(5),
        labels = fhiplot::format_nor_perc_0,
        name = labs_right
      )
    )
  }


  q <- q + expand_limits(y = 0)
  if(granularity_time=="day"){
    q <- q + scale_x_date(
      "",
      date_breaks = "2 days",
      date_labels = "%d.%m"
    )
  } else {
    q <- q + scale_x_discrete(
      ""
    )
  }
  q <- q + fhiplot::scale_color_fhi(labs_legend)
  q <- q + fhiplot::scale_fill_fhi(labs_legend)

  if(!is.null(d_third) && !is.null(d_right)){
    q <- q + fhiplot::scale_fill_fhi(palette = "primary")
    q <- q + fhiplot::scale_color_fhi(palette = "posneg", direction = right_legend_direction)
    q <- q + guides(color = guide_legend(title = NULL), fill = guide_legend(title = NULL))
  }

  q <- q + fhiplot::theme_fhi_lines(
    20, panel_on_top = T,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend_position = legend_position
  )
  q <- q + fhiplot::set_x_axis_vertical()
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + coord_cartesian(ylim=c(0, max_y), clip="off", expand = F)
  q <- q + labs(title = labs_title)
  q <- q + labs(caption = labs_caption)
  q
}


covid19_norsyss_vs_msis <- function(
  location_code,
  config
){
  if(get_granularity_geo(location_code) == "nation"){
    covid19_norsyss_vs_msis_lab_daily(
      location_code = location_code,
      config = config
    )
  } else if(get_granularity_geo(location_code) == "county") {
    covid19_norsyss_vs_msis_daily(
      location_code = location_code,
      config = config
    )
  } else {
    covid19_norsyss_vs_msis_weekly(
      location_code = location_code,
      config = config
    )
  }
}



## Create table
make_table_generic <- function(...) {
  ## the order MUST be d_left, d_right, d_third
  dd <- list(...)

  varKey <- names(dd[[1]])[1] #either date or yrwk
  invisible(lapply(dd, function(x) data.table::setkeyv(x, varKey)))
  pd_xl <- Reduce(function(...) merge(..., all = TRUE), dd)

  delColName <- c("censor", "no_data", "n")
  oldColName <- setdiff(names(pd_xl), delColName)
  newColName <- c("dato", "antall_MSIS", "andel_NorSySS", "andel_positive_laboratorietester")

  if (length(dd) == 3) {
    data.table::setnames(pd_xl, oldColName, newColName)
  } else {
    data.table::setnames(pd_xl, oldColName, newColName[-4])
  }

  ## rounding and change real censored to 99
  roundCol <- newColName[3]
  pd_xl[censor != "", (roundCol) := NA] %>%
    .[, (roundCol) := round(get(roundCol), digits = 1)]

  pd_xl[, (delColName) := NULL]

  pd_xl

}


covid19_norsyss_vs_msis_lab_daily <- function(
  location_code,
  config
){

  d_left <- pool %>% dplyr::tbl("data_covid19_msis_by_time_location") %>%
    dplyr::filter(granularity_time == "day") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(date, n) %>%
    dplyr::collect()
  setDT(d_left)
  d_left[, date:= as.Date(date)]
  setnames(d_left, "n", "value")

  d_right <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(granularity_time=="day") %>%
    dplyr::filter(tag_outcome %in% "covid19_vk_ote") %>%
    dplyr::filter(age=="total") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(date, n, consult_with_influenza) %>%
    dplyr::collect()
  setDT(d_right)
  d_right[, date:= as.Date(date)]


  d_third <- pool %>%
    dplyr::tbl("data_covid19_lab_by_time") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(granularity_time=="day") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(date, pr100_pos) %>%
    dplyr::collect()
  setDT(d_third)
  d_third[, date := as.Date(date)]
  setnames(d_third, "pr100_pos", "value")


  d_right[,censor := ""]
  d_right[censor=="" & n>0 & n<5, censor := "N"]
  d_right[censor != "", n := 0]
  d_right[, value := 100* n / consult_with_influenza]
  d_right[is.nan(value), value := 0]
  d_right[value>60, value := 60]
  d_right[, n := NULL]
  d_right[, no_data := consult_with_influenza==0]
  d_right[,consult_with_influenza := NULL]

  ## Create table
  pd_xl <- make_table_generic(d_left, d_right, d_third)


  ## Create plot
  censored <- d_right[censor!=""]$date
  no_data <- d_right[no_data==TRUE]$date

  pd_plot <- covid19_plot_single(
    d_left = d_left,
    d_right = d_right,
    d_third = d_third,
    censored = censored,
    no_data = no_data,
    type_left="col",
    labs_left = "Antall tilfeller meldt til MSIS",
    labs_right = "Andel NorSySS konsultasjoner\n og andel positive laboratorietester\n",
    labs_title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "Antall covid-19 meldinger til MSIS og andel konsultasjoner for \n",
      "covid-19 (mistenkt, sannsynlig eller bekreftet) på legekontor og legevakt\n",
      "Data fra NorSySS og MSIS"
    ),
    labs_caption = glue::glue(
      "Røde piler på x-aksen viser helger og helligdager. Røde * på x-aksen viser sensurerte data\n",
      "Søylene skal leses av på venstre side, den røde linjen skal leses av på høyre side\n",
      "Nevneren på andelen er totalt antall konsultasjoner per dato i valgt geografisk område\n",
      "R{fhi::nb$oe}de stiplede vertikale linjer på figuren betyr at ingen konsultasjoner er rapportert på disse datoene\n",
      "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
    ),
    right_legend_labs = c(
      "Andel NorSySS konsultasjoner",
      "Andel positive laboratorietester",
      "Antall tilfeller meldt til MSIS"
    ),
    right_legend_direction = -1,
    multiplier_min_y_censor = -0.13,
    multiplier_min_y_end = -0.14,
    multiplier_min_y_start = -0.175,
    left_labels = fhiplot::format_nor
  )

  list(pd_plot = pd_plot, pd_xl = pd_xl)
}


covid19_norsyss_vs_msis_daily <- function(
  location_code,
  config
){

  d_left <- pool %>% dplyr::tbl("data_covid19_msis_by_time_location") %>%
    dplyr::filter(granularity_time == "day") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(date, n) %>%
    dplyr::collect()
  setDT(d_left)
  d_left[, date:= as.Date(date)]
  setnames(d_left, "n", "value")

  d_right <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::filter(location_code== !!location_code) %>%
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


  ## Create table
  pd_xl <- make_table_generic(d_left, d_right)

  ## Create plot
  censored <- d_right[censor!=""]$date
  no_data <- d_right[no_data==TRUE]$date

  pd_plot <- covid19_plot_single(
    d_left = d_left,
    d_right = d_right,
    censored = censored,
    no_data = no_data,
    type_left="col",
    labs_left = "Antall tilfeller meldt til MSIS",
    labs_right = "Andel NorSySS konsultasjoner\n",
    labs_title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "Antall covid-19 meldinger til MSIS og andel konsultasjoner for\n",
      "covid-19 (mistenkt, sannsynlig eller bekreftet) på legekontor og legevakt\n",
      "Data fra NorSySS og MSIS"
    ),
    labs_caption = glue::glue(
      "Røde piler på x-aksen viser helger og helligdager. Røde * på x-aksen viser sensurerte data\n",
      "Søylene skal leses av på venstre side, den røde linjen skal leses av på høyre side\n",
      "Nevneren på andelen er totalt antall konsultasjoner per dato i valgt geografisk område\n",
      "R{fhi::nb$oe}de stiplede vertikale linjer på figuren betyr at ingen konsultasjoner er rapportert på disse datoene\n",
      "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
    ),
    multiplier_min_y_censor = -0.13,
    multiplier_min_y_end = -0.14,
    multiplier_min_y_start = -0.175,
    left_labels = fhiplot::format_nor
  )

  list(pd_plot = pd_plot, pd_xl = pd_xl)
}


covid19_norsyss_vs_msis_weekly <- function(
  location_code,
  config
){

  d_left <- pool %>% dplyr::tbl("data_covid19_msis_by_time_location") %>%
    dplyr::filter(granularity_time == "week") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(yrwk, n) %>%
    dplyr::collect()
  setDT(d_left)
  setnames(d_left, "n", "value")

  d_right <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(granularity_time=="day") %>%
    dplyr::filter(tag_outcome %in% "covid19_vk_ote") %>%
    dplyr::filter(age=="total") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::select(yrwk, n, consult_with_influenza) %>%
    dplyr::group_by(yrwk) %>%
    dplyr::summarize(n=sum(n), consult_with_influenza=sum(consult_with_influenza)) %>%
    dplyr::collect()
  setDT(d_right)

  d_right[,censor := ""]
  d_right[censor=="" & n>0 & n<5, censor := "N"]
  d_right[censor != "", n := 0]

  d_right[, value := 100* n / consult_with_influenza]
  d_right[is.nan(value), value := 0]
  d_right[value>60, value := 60]
  d_right[, no_data := consult_with_influenza==0]
  d_right[,consult_with_influenza := NULL]


  ## Create table
  pd_xl <- make_table_generic(d_left, d_right)

  ## Create plot
  censored <- d_right[censor!=""]$yrwk
  no_data <- d_right[no_data==T]$yrwk

  pd_plot <- covid19_plot_single(
    granularity_time = "week",
    d_left = d_left,
    d_right = d_right,
    censored = censored,
    no_data = no_data,
    type_left="col",
    labs_left = "Antall tilfeller meldt til MSIS",
    labs_right = "Andel NorSySS konsultasjoner\n",
    labs_title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "Antall covid-19 meldinger til MSIS og andel konsultasjoner for\n",
      "covid-19 (mistenkt, sannsynlig eller bekreftet) på legekontor og legevakt\n",
      "Data fra NorSySS og MSIS"
    ),
    labs_caption = glue::glue(
      "\nRøde * på x-aksen viser sensurerte data\n",
      "Søylene skal leses av på venstre side, den røde linjen skal leses av på høyre side\n",
      "Nevneren på andelen er totalt antall konsultasjoner per dato i valgt geografisk område\n",
      "Røde stiplede vertikale linjer på figuren betyr at ingen konsultasjoner er rapportert på disse datoene\n",
      "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
    ),
    multiplier_min_y_censor = -0.2,
    multiplier_min_y_end = -0.14,
    multiplier_min_y_start = -0.175,
    left_labels = fhiplot::format_nor
  )

  list(pd_plot = pd_plot, pd_xl = pd_xl)
}


covid19_overview_plot_national_syndromes_proportion <- function(
  location_code,
  config
){
  if(get_granularity_geo(location_code) %in% c("nation", "county")){
    covid19_overview_plot_national_syndromes_proportion_daily(
      location_code = location_code,
      config = config
    )
  } else {
    covid19_overview_plot_national_syndromes_proportion_weekly(
      location_code = location_code,
      config = config
    )
  }
}

covid19_overview_plot_national_syndromes_proportion_daily <- function(
  location_code,
  config
){

  pd <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::filter(tag_outcome %in% c(
      "covid19_vk_ote",
      "engstelig_luftveissykdom_ika_vk_ote",
      "influensa_vk_ote",
      "rxx_for_covid19_vk_ote",
      "akkut_ovre_luftveisinfeksjon_vk_ote"
    )) %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::filter(age == "total") %>%
    dplyr::filter(location_code == !!location_code) %>%
    dplyr::select(tag_outcome, date, n, consult_with_influenza) %>%
    dplyr::collect()
  setDT(pd)
  pd[, date:= as.Date(date)]

  pd[,censor := ""]
  pd[censor=="" & n>0 & n<5, censor := "N"]
  pd[censor != "", n := 0]

  pd[, andel := 100*n/consult_with_influenza]
  pd[, no_data := consult_with_influenza==0]
  pd[is.nan(andel), andel := 0]
  pd[andel > 60, andel := 60]

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
      "Covid-19 (mistenkt\nsannsynlig\neller bekreftet) (R991, R992)",
      "Engstelig luftveissykdom\nIKA (R27)",
      "Influensa (R80)",
      "Akutt øvre\nluftveisinfeksjon (R74)",
      "Luftvei diagnosekoder\n(samlet*)"
    )
  )]

  setnames(pd,"andel","value")
  setnames(pd, "name_outcome","group")

  d_left <- pd

  censored <- d_left[censor!=""]$date
  no_data <- d_left[no_data==TRUE]$date

  covid19_plot_single(
    d_left = d_left,
    d_right = NULL,
    censored = censored,
    no_data = no_data,
    type_left="line",
    group_left = TRUE,
    labs_left = "Andel",
    labs_right = NULL,
    labs_title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "Andel konsultasjoner med forskjellig luftveisagens\n",
      "Data fra NorSySS"
    ),
    labs_caption = glue::glue(
      "Røde piler på x-aksen viser helger og helligdager. Røde * på x-aksen viser sensurerte data\n",      "Nevneren er totalt antall konsultasjoner\n",
      "*R- 01, 02, 03, 04, 05, 06, 07, 08, 09, 21, 24, 25, 27, 29, 72, 74, 75, 76, 77, 78, 79, 81, 82, 83, 99, 991, 992\n",
      "Røde stiplede vertikale linjer på figuren betyr at ingen konsultasjoner er rapportert på disse datoene\n",
      "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
    ),
    multiplier_min_y_censor = -0.145,
    multiplier_min_y_end = -0.155,
    multiplier_min_y_start = -0.19,
    left_labels = fhiplot::format_nor_perc_0
  )
}

covid19_overview_plot_national_syndromes_proportion_weekly <- function(
  location_code,
  config
){

  pd <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::filter(tag_outcome %in% c(
      "covid19_vk_ote",
      "engstelig_luftveissykdom_ika_vk_ote",
      "influensa_vk_ote",
      "rxx_for_covid19_vk_ote",
      "akkut_ovre_luftveisinfeksjon_vk_ote"
    )) %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::filter(age == "total") %>%
    dplyr::filter(location_code == !!location_code) %>%
    dplyr::select(tag_outcome, yrwk, n, consult_with_influenza) %>%
    dplyr::group_by(tag_outcome, yrwk) %>%
    dplyr::summarize(n=sum(n), consult_with_influenza=sum(consult_with_influenza)) %>%
    dplyr::collect()
  setDT(pd)

  pd[,censor := ""]
  pd[censor=="" & n>0 & n<5, censor := "N"]
  pd[censor != "", n := 0]

  pd[, andel := 100*n/consult_with_influenza]
  pd[, no_data := consult_with_influenza==0]
  pd[is.nan(andel), andel := 0]
  pd[andel > 60, andel := 60]

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
      "Covid-19 (mistenkt\n sannsynlig\neller bekreftet) (R991,R992)",
      "Engstelig luftveissykdom\nIKA (R27)",
      "Influensa (R80)",
      "Akutt øvre\nluftveisinfeksjon (R74)",
      "Luftvei diagnosekoder\n(samlet*)"
    )
  )]

  setnames(pd,"andel","value")
  setnames(pd, "name_outcome","group")

  d_left <- pd

  censored <- d_left[censor!=""]$yrwk
  no_data <- d_left[no_data==TRUE]$yrwk

  covid19_plot_single(
    granularity_time = "week",
    d_left = d_left,
    d_right = NULL,
    censored = censored,
    no_data = no_data,
    type_left="line",
    group_left = TRUE,
    labs_left = "Andel",
    labs_right = NULL,
    labs_title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "Andel konsultasjoner med forskjellig luftveisagens\n",
      "Data fra NorSySS"
    ),
    labs_caption = glue::glue(
      "\nRøde * på x-aksen viser sensurerte data\n",
      "Nevneren er totalt antall konsultasjoner\n",
      "*R- 01, 02, 03, 04, 05, 06, 07, 08, 09, 21, 24, 25, 27, 29, 72, 74, 75, 76, 77, 78, 79, 81, 82, 83, 99, 991, 992\n",
      "Røde stiplede vertikale linjer på figuren betyr at ingen konsultasjoner er rapportert på disse datoene\n",
      "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
    ),
    multiplier_min_y_censor = -0.2,
    multiplier_min_y_end = -0.14,
    multiplier_min_y_start = -0.175,
    left_labels = fhiplot::format_nor_perc_0
  )
}

covid19_overview_plot_national_source_proportion <- function(
  location_code,
  config
){
  if(get_granularity_geo(location_code) %in% c("nation", "county")){
    covid19_overview_plot_national_source_proportion_daily(
      location_code = location_code,
      config = config
    )
  } else {
    covid19_overview_plot_national_source_proportion_weekly(
      location_code = location_code,
      config = config
    )
  }
}

covid19_overview_plot_national_source_proportion_daily <- function(
  location_code,
  config
){

  pd <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::filter(tag_outcome %in% c(
      "covid19_k_o",
      "covid19_k_t",
      "covid19_k_e",
      "covid19_v_o",
      "covid19_v_t",
      "covid19_v_e"
    )) %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::filter(age == "total") %>%
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

  ### line creation

  d_right <- pd[,.(
    n=sum(n),
    consult_with_influenza=sum(consult_with_influenza)
  ),
  by=.(date)
  ]

  # sensoring

  d_right[,censor := ""]
  d_right[censor=="" & n>0 & n<5, censor := "N"]
  d_right[censor != "", n := 0]

  d_right[, andel := 100*n/consult_with_influenza]
  d_right[, no_data := consult_with_influenza==0]
  d_right[is.nan(andel), andel := 0]
  d_right[andel > 60, andel := 60]

  setnames(d_right, "andel", "value")

  d_left <- pd
  d_left[,censor := ""]
  d_left[censor=="" & n>0 & n<5, censor := "N"]
  d_left[,total_n:=sum(n),by=.(date)]
  d_left[censor=="" & total_n>0 & total_n<5, censor := "N"]
  d_left[,total_n:=NULL]
  d_left[censor=="" & consult_with_influenza>0 & consult_with_influenza<5 & stringr::str_detect(tag_outcome, "_o$"), censor := "T"]
  d_left[censor != "", n := 0]

  setnames(d_left, "n", "value")
  setnames(d_left, "cat", "group")

  censored <- unique(rbind(
    d_left[,c("date","censor")],
    d_right[,c("date","censor")]
  ))[censor!=""]$date

  no_data <- d_right[no_data==TRUE]$date

  covid19_plot_single(
    d_left = d_left,
    d_right = d_right,
    censored = censored,
    no_data = no_data,
    type_left="col",
    group_left=TRUE,
    labs_left = "Antall",
    labs_right = "Andel",
    labs_title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "Antall konsultasjoner for covid-19 fordelt på type konsultasjon\n",
      "samt andel konsultasjoner for covid-19\n",
      "Data fra NorSySS"
    ),
    labs_caption = glue::glue(
      "Røde piler på x-aksen viser helger og helligdager. Røde * på x-aksen viser sensurerte data\n",
      "Søylene skal leses av på venstre side, den røde linjen skal leses av på høyre side\n",
      "Nevneren på andelen er totalt antall konsultasjoner per dato i valgt geografisk område\n",
      "Røde stiplede vertikale linjer på figuren betyr at ingen konsultasjoner er rapportert på disse datoene\n",
      "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
    ),
    legend_position = "right",
    multiplier_min_y_censor = -0.13,
    multiplier_min_y_end = -0.14,
    multiplier_min_y_start = -0.175,
    left_labels = fhiplot::format_nor
  )
}

covid19_overview_plot_national_source_proportion_weekly <- function(
  location_code,
  config
){

  pd <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::filter(tag_outcome %in% c(
      "covid19_k_o",
      "covid19_k_t",
      "covid19_k_e",
      "covid19_v_o",
      "covid19_v_t",
      "covid19_v_e"
    )) %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::filter(age == "total") %>%
    dplyr::filter(location_code == !!location_code) %>%
    dplyr::select(tag_outcome, yrwk, n, consult_with_influenza) %>%
    dplyr::group_by(tag_outcome, yrwk) %>%
    dplyr::summarize(n = sum(n), consult_with_influenza=sum(consult_with_influenza)) %>%
    dplyr::collect()
  setDT(pd)

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

  ### line creation
  d_right <- pd[,.(
    n=sum(n),
    consult_with_influenza=sum(consult_with_influenza)
  ),
  by=.(yrwk)
  ]

  # sensoring

  d_right[,censor := ""]
  d_right[censor=="" & n>0 & n<5, censor := "N"]
  d_right[censor != "", n := 0]

  d_right[, andel := 100*n/consult_with_influenza]
  d_right[, no_data := consult_with_influenza==0]
  d_right[is.nan(andel), andel := 0]
  d_right[andel > 60, andel := 60]

  setnames(d_right, "andel", "value")

  d_left <- pd
  d_left[,censor := ""]
  d_left[censor=="" & n>0 & n<5, censor := "N"]
  d_left[,total_n:=sum(n),by=.(yrwk)]
  d_left[censor=="" & total_n>0 & total_n<5, censor := "N"]
  d_left[,total_n:=NULL]
  d_left[censor=="" & consult_with_influenza>0 & consult_with_influenza<5 & stringr::str_detect(tag_outcome, "_o$"), censor := "T"]
  d_left[censor != "", n := 0]

  setnames(d_left, "n", "value")
  setnames(d_left, "cat", "group")

  censored <- unique(rbind(
    d_left[,c("yrwk","censor")],
    d_right[,c("yrwk","censor")]
  ))[censor!=""]$yrwk

  no_data <- d_right[no_data==TRUE]$yrwk

  covid19_plot_single(
    granularity_time = "week",
    d_left = d_left,
    d_right = d_right,
    censored = censored,
    no_data = no_data,
    type_left="col",
    group_left=TRUE,
    labs_left = "Antall",
    labs_right = "Andel",
    labs_title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "Antall konsultasjoner for covid-19 fordelt på type konsultasjon\n",
      "samt andel konsultasjoner for covid-19\n",
      "Data fra NorSySS"
    ),
    labs_caption = glue::glue(
      "Røde * på x-aksen viser sensurerte data\n",
      "Søylene skal leses av på venstre side, den røde linjen skal leses av på høyre side\n",
      "Nevneren på andelen er totalt antall konsultasjoner per dato i valgt geografisk område\n",
      "Røde stiplede vertikale linjer på figuren betyr at ingen konsultasjoner er rapportert på disse datoene\n",
      "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
    ),
    legend_position = "right",
    multiplier_min_y_censor = -0.2,
    multiplier_min_y_end = -0.14,
    multiplier_min_y_start = -0.175,
    left_labels = fhiplot::format_nor
  )
}

covid19_overview_plot_national_age_burden <- function(
  location_code,
  config
){
  if(location_code %in% config$small_location_codes){
    no_data()
  } else if(get_granularity_geo(location_code) %in% c("nation", "county")){
    covid19_overview_plot_national_age_burden_daily(
      location_code = location_code,
      config = config
    )
  } else {
    covid19_overview_plot_national_age_burden_weekly(
      location_code = location_code,
      config = config
    )
  }
}

covid19_overview_plot_national_age_burden_daily <- function(
  location_code,
  config
){

  pd <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::filter(tag_outcome == "covid19_vk_ote") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(age != "total") %>%
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


  # sensoring
  pd[,censor := ""]
  pd[censor=="" & n>0 & n<5, censor := "N"]
  pd[censor != "", n := 0]

  pd[, andel := 100*n/consult_with_influenza_totalt]
  pd[, no_data := consult_with_influenza_totalt==0]
  pd[is.nan(andel), andel := 0]
  pd[andel > 60, andel := 60]

  d_left <- pd

  setnames(d_left, "andel", "value")
  setnames(d_left, "age", "group")

  censored <- unique(d_left[censor!=""]$date)
  no_data <- unique(d_left[no_data==TRUE]$date)

  covid19_plot_single(
    d_left = d_left,
    d_right = NULL,
    censored = censored,
    no_data = no_data,
    type_left="col",
    group_left=TRUE,
    labs_left = "Andel",
    labs_right = NULL,
    labs_title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "Andel konsultasjoner med covid-19 (mistenkt, sannsynlig eller bekreftet) fordelt på aldersgruppe\n",
      "Data fra NorSySS"
    ),
    labs_caption = glue::glue(
      "Røde * på x-aksen viser sensurerte data\n",
      "For alle aldersgruppene er nevneren totalt antall konsultasjoner (alle aldersgrupper summert)\n",
      "Røde stiplede vertikale linjer på figuren betyr at ingen konsultasjoner er rapportert på disse datoene\n",
      "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
    ),
    labs_legend = "Aldersgruppe",
    legend_position = "right",
    multiplier_min_y_censor = -0.13,
    multiplier_min_y_end = -0.14,
    multiplier_min_y_start = -0.175,
    left_labels = fhiplot::format_nor_perc_0
  )
}

covid19_overview_plot_national_age_burden_weekly <- function(
  location_code,
  config
){
  pd <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::filter(tag_outcome == "covid19_vk_ote") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(age != "total") %>%
    dplyr::select(yrwk, age, n, consult_with_influenza) %>%
    dplyr::group_by(yrwk, age) %>%
    dplyr::summarize(n=sum(n), consult_with_influenza=sum(consult_with_influenza)) %>%
    dplyr::collect()
  setDT(pd)

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
  pd[, consult_with_influenza_totalt := sum(consult_with_influenza), by=.(yrwk)]

  # sensoring
  pd[,censor := ""]
  pd[censor=="" & n>0 & n<5, censor := "N"]
  pd[censor != "", n := 0]

  pd[, andel := 100*n/consult_with_influenza_totalt]
  pd[, no_data := consult_with_influenza_totalt==0]
  pd[is.nan(andel), andel := 0]
  pd[andel > 60, andel := 60]

  d_left <- pd

  setnames(d_left, "andel", "value")
  setnames(d_left, "age", "group")

  censored <- unique(d_left[censor!=""]$yrwk)
  no_data <- unique(d_left[no_data==TRUE]$yrwk)

  covid19_plot_single(
    granularity_time = "week",
    d_left = d_left,
    d_right = NULL,
    censored = censored,
    no_data = no_data,
    type_left="col",
    group_left=TRUE,
    labs_left = "Andel",
    labs_right = NULL,
    labs_title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "Andel konsultasjoner med covid-19 (mistenkt, sannsynlig eller bekreftet) fordelt på aldersgruppe\n",
      "Data fra NorSySS"
    ),
    labs_caption = glue::glue(
      "Røde * på x-aksen viser sensurerte data\n",
      "For alle aldersgruppene er nevneren totalt antall konsultasjoner (alle aldersgrupper summert)\n",
      "Røde stiplede vertikale linjer på figuren betyr at ingen konsultasjoner er rapportert på disse datoene\n",
      "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
    ),
    labs_legend = "Aldersgruppe",
    legend_position = "right",
    multiplier_min_y_censor = -0.2,
    multiplier_min_y_end = -0.14,
    multiplier_min_y_start = -0.175,
    left_labels = fhiplot::format_nor_perc_0
  )
}

covid19_overview_plot_national_age_trends <- function(
  location_code,
  config
){
  if(location_code %in% config$small_location_codes){
    no_data()
  } else if(get_granularity_geo(location_code) %in% c("nation", "county")){
    covid19_overview_plot_national_age_trends_daily(
      location_code = location_code,
      config = config
    )
  } else {
    covid19_overview_plot_national_age_trends_weekly(
      location_code = location_code,
      config = config
    )
  }
}

covid19_overview_plot_national_age_trends_daily <- function(
  location_code,
  config
){
  pd <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
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

  # sensoring
  pd[, consult_with_influenza_totalt := sum(consult_with_influenza), by=.(date)]

  pd[,censor := ""]
  pd[censor=="" & n>0 & n<5, censor := "N"]
  pd[censor != "", n := 0]

  pd[, andel := 100*n/consult_with_influenza]
  pd[, no_data := consult_with_influenza_totalt==0]
  pd[is.nan(andel), andel := 0]
  pd[andel > 60, andel := 60]

  censored <- unique(pd[,c("date","age","censor")])[censor!=""]

  max_y <- max(pd$andel, na.rm=T)
  max_y <- max(c(max_y,5))
  min_y_censor <- 0.01*max_y

  q <- ggplot(pd, aes(x=date,y=andel))
  q <- q + geom_col(fill = fhiplot::base_color, width=0.8)
  if(sum(pd$no_data)>0){
    q <- q + geom_vline(data=pd[no_data==TRUE], mapping=aes(xintercept = date),color= "red", lty=3, lwd=1.5)
  }
  if(nrow(censored)>0){
    q <- q + geom_text(
      data=censored,
      label="*",
      y = min_y_censor,
      size=10,
      label.r = grid::unit(0, "lines"),
      label.size = NA,
      color="red"
    )
  }
  q <- q + scale_y_continuous(
    "Andel",
    breaks = fhiplot::pretty_breaks(4),
    expand = expand_scale(mult = c(0, 0.1)),
    labels = fhiplot::format_nor_perc_0,
    limits=c(0,max_y)
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
  q <- q + fhiplot::set_x_axis_vertical()
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + labs(title = glue::glue(
    "{names(config$choices_location)[config$choices_location==location_code]}\n",
    "Andel konsultasjoner med covid-19 (mistenkt, sannsynlig eller bekreftet) fordelt på aldersgrupper\n",
    "Data fra NorSySS"
  ))
  q <- q + labs(caption=glue::glue(
    "Røde * på x-aksen viser sensurerte data\n",
    "Nevneren er totalt antall konsultasjoner per dato, geografisk område og aldersgruppe\n",
    "Røde stiplede vertikale linjer på figuren betyr at ingen konsultasjoner er rapportert på disse datoene\n",
    "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
  ))
  q
}

covid19_overview_plot_national_age_trends_weekly <- function(
  location_code,
  config
){
  pd <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::filter(tag_outcome == "covid19_vk_ote") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::select(yrwk, age, n, consult_with_influenza) %>%
    dplyr::group_by(yrwk, age) %>%
    dplyr::summarize(n = sum(n), consult_with_influenza = sum(consult_with_influenza)) %>%
    dplyr::collect()
  setDT(pd)

  pd[,age:=factor(
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

  # sensoring
  pd[, consult_with_influenza_totalt := sum(consult_with_influenza), by=.(yrwk)]

  pd[,censor := ""]
  pd[censor=="" & n>0 & n<5, censor := "N"]
  pd[censor != "", n := 0]

  pd[, andel := 100*n/consult_with_influenza]
  pd[, no_data := consult_with_influenza_totalt==0]
  pd[is.nan(andel), andel := 0]
  pd[andel > 60, andel := 60]

  censored <- unique(pd[,c("yrwk","age","censor")])[censor!=""]

  max_y <- max(pd$andel, na.rm=T)
  max_y <- max(c(max_y,5))
  min_y_censor <- 0.01*max_y

  q <- ggplot(pd, aes(x=yrwk,y=andel))
  q <- q + geom_col(fill = fhiplot::base_color, width=0.8)
  if(sum(pd$no_data)>0){
    q <- q + geom_vline(data=pd[no_data==TRUE], mapping=aes(xintercept = yrwk),color= "red", lty=3, lwd=1.5)
  }
  if(nrow(censored)>0){
    q <- q + geom_text(
      data=censored,
      label="*",
      y = min_y_censor,
      size=10,
      label.r = grid::unit(0, "lines"),
      label.size = NA,
      color="red"
    )
  }
  q <- q + scale_y_continuous(
    "Andel",
    breaks = fhiplot::pretty_breaks(4),
    expand = expand_scale(mult = c(0, 0.1)),
    labels = fhiplot::format_nor_perc_0
  )
  q <- q + expand_limits(y = 0)
  q <- q + scale_x_discrete(NULL)
  q <- q + lemon::facet_rep_wrap(~age, repeat.tick.labels = "all", ncol=3)
  q <- q + fhiplot::theme_fhi_lines(20, panel_on_top = T,
                                    panel.grid.major.x = element_blank(),
                                    panel.grid.minor.x = element_blank()
  )
  q <- q + fhiplot::set_x_axis_vertical()
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + labs(title = glue::glue(
    "{names(config$choices_location)[config$choices_location==location_code]}\n",
    "Andel konsultasjoner med covid-19 (mistenkt, sannsynlig eller bekreftet) fordelt på aldersgrupper\n",
    "Data fra NorSySS"
  ))
  q <- q + labs(caption=glue::glue(
    "Røde * på x-aksen viser sensurerte data\n",
    "Nevneren er totalt antall konsultasjoner per dato, geografisk område og aldersgruppe\n",
    "Røde stiplede vertikale linjer på figuren betyr at ingen konsultasjoner er rapportert på disse datoene\n",
    "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
  ))
  q
}

covid19_overview_plot_county_proportion <- function(
  location_code,
  config
){
  if(get_granularity_geo(location_code) %in% c("nation", "county")){
    covid19_overview_plot_county_proportion_weekly(
      location_code = location_code,
      config = config
    )
  } else {
    covid19_overview_plot_county_proportion_weekly(
      location_code = location_code,
      config = config
    )
  }
}

covid19_overview_plot_county_proportion_weekly <- function(
  location_code,
  config
){

  location_codes <- get_dependent_location_codes(location_code = location_code)

  granularity_geo <- get_granularity_geo(location_code)

  pd <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::filter(tag_outcome %in% c(
      "covid19_vk_ote",
      "engstelig_luftveissykdom_ika_vk_ote"
    )) %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::filter(age == "total") %>%
    dplyr::filter(location_code %in% !!location_codes) %>%
    dplyr::select(tag_outcome, location_code, yrwk, n, consult_with_influenza) %>%
    dplyr::group_by(tag_outcome, location_code, yrwk) %>%
    dplyr::summarize(n = sum(n), consult_with_influenza = sum(consult_with_influenza)) %>%
    dplyr::collect()
  setDT(pd)

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
      "Covid-19 (mistenkt, sannsynlig eller bekreftet) (R991, R992)",
      "Engstelig luftveissykdom IKA (R27)"
    )
  )]

  pd[,location_code := factor(location_code, levels = location_codes)]
  setorder(pd,location_code)
  location_names <- unique(pd$location_name)
  pd[,location_name := factor(location_name, levels = location_names)]

  pd[,no_data := sum(consult_with_influenza)==0, by=.(location_code, yrwk)]

  pd[,censor := ""]
  pd[censor=="" & n>0 & n<5, censor := "N"]
  pd[censor != "", n := 0]

  pd[, andel := 100*n/consult_with_influenza]
  pd[, no_data := consult_with_influenza==0]
  pd[is.nan(andel), andel := 0]
  pd[andel > 60, andel := 60]

  censored <- unique(pd[,c("yrwk","location_name","censor")])[censor!=""]

  max_y <- max(pd$andel, na.rm=T)
  max_y <- max(c(max_y,5))
  min_y_censor <- 0.01*max_y

  q <- ggplot(pd, aes(x=yrwk, y=andel))
  #q <- q + geom_col(mapping = aes(fill=name_outcome), position = "dodge", width=0.8)
  q <- q + geom_line(mapping = aes(color=name_outcome, group=name_outcome), lwd=2)
  if(sum(pd$no_data)>0){
    q <- q + geom_vline(data=pd[no_data==TRUE], mapping=aes(xintercept = yrwk),color= "red", lty=3, lwd=1.5)
  }
  if(nrow(censored)>0){
    q <- q + geom_text(
      data=censored,
      label="*",
      y = min_y_censor,
      size=10,
      label.r = grid::unit(0, "lines"),
      label.size = NA,
      color="red"
    )
  }
  if(granularity_geo=="nation"){
    q <- q + lemon::facet_rep_wrap(~location_name, repeat.tick.labels = "y", ncol=3)
  } else {
    q <- q + lemon::facet_rep_wrap(~location_name, repeat.tick.labels = "y", ncol=3, scales="free_y")
  }
  q <- q + scale_y_continuous(
    "Andel",
    breaks = fhiplot::pretty_breaks(4),
    expand = expand_scale(mult = c(0, 0.1)),
    labels = fhiplot::format_nor_perc_0
  )
  q <- q + expand_limits(y = 0)
  q <- q + scale_x_discrete(NULL)
  q <- q + fhiplot::scale_fill_fhi(NULL, guide="none")
  q <- q + fhiplot::scale_color_fhi(NULL)
  if(granularity_geo=="nation"){
    q <- q + fhiplot::theme_fhi_lines(20, panel_on_top = F,
                                      panel.grid.major.x = element_blank(),
                                      panel.grid.minor.x = element_blank()
    )
  } else {
    q <- q + fhiplot::theme_fhi_lines(20, panel_on_top = F,
                                      panel.grid.major.x = element_blank(),
                                      panel.grid.minor.x = element_blank(),
                                      panel.grid.minor.y = element_blank()
    )
  }
  q <- q + fhiplot::set_x_axis_vertical()
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + theme(legend.position="bottom")
  q <- q + labs(title=glue::glue(
    "Andel konsultasjoner av\n",
    "R991 og R992 samlet: Covid-19 (mistenkt, sannsynlig eller bekreftet)\n",
    "R27: Engstelig luftveissykdom IKA diagnose per geografisk område\n",
    "Data fra NorSySS"
  ))
  q <- q + labs(caption=glue::glue(
    "Røde * på x-aksen viser sensurerte data\n",
    "Nevneren er totalt antall konsultasjoner per dato i det viste geografiske området.\n",
    "Røde stiplede vertikale linjer på figuren betyr at ingen konsultasjoner er rapportert på disse datoene\n",
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
      d <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
        dplyr::filter(tag_outcome %in% c(
          "covid19_vk_ote",
          "engstelig_luftveissykdom_ika_vk_ote"
        )) %>%
        dplyr::filter(date >= !!config$start_date) %>%
        dplyr::filter(granularity_geo == "county") %>%
        dplyr::filter(age == "total") %>%
        dplyr::collect()
    } else {
      d <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
        dplyr::filter(tag_outcome %in% c(
          "covid19_vk_ote",
          "engstelig_luftveissykdom_ika_vk_ote"
        )) %>%
        dplyr::filter(date >= !!config$start_date) %>%
        dplyr::filter(granularity_geo == "municip") %>%
        dplyr::filter(location_code %in% !!location_codes) %>%
        dplyr::filter(age == "total") %>%
        dplyr::collect()
    }
    setDT(d)
    d[, no_data := sum(consult_with_influenza) == 0, by=.(location_code)]
    setorder(d,tag_outcome, location_code, date)
    d[,cum_n := cumsum(n), by=.(tag_outcome, location_code)]
    d <- d[date==max(date)]

    cut_points <- unique(round(c(0, quantile(d$cum_n, probs = c(0.25, 0.5, 0.75, 1)))))
    breaks <- c(-1, cut_points)
    breaks[breaks>0 & breaks<=5] <- 5
    breaks <- unique(breaks)
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
        "Covid-19 (mistenkt, sannsynlig eller bekreftet) (R991, R992)",
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
    d <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_vk_ote",
        "engstelig_luftveissykdom_ika_vk_ote"
      )) %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(granularity_geo == "county") %>%
      dplyr::filter(age == "total") %>%
      dplyr::collect()
  } else {
    d <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_vk_ote",
        "engstelig_luftveissykdom_ika_vk_ote"
      )) %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(granularity_geo == "municip") %>%
      dplyr::filter(location_code %in% !!location_codes) %>%
      dplyr::filter(age == "total") %>%
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
      "Covid-19 (mistenkt, sannsynlig eller bekreftet) (R991, R992)",
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

