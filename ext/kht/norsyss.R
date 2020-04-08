

norsyss_ui <- function(id, config) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width=12, align="left",
        p(
          id="toptext",
          strong("Informasjonen som finnes på denne siden er anonym, men er ment for kommuneleger fordi det krever kunnskap for å tolke disse på riktig måte. Dette er ikke ment som en offisiell statistikk."),br(),br(),

          strong("NorSySS er forkortelsen for Norwegian Syndromic Surveillance System som er en del av Sykdomspulsen."), br(),
          "Dette er et overvåkningssystem basert på diagnosekoder (ICPC-2 koder) satt på legekontor og legevakt i hele Norge.", br(), br(),

          "Formålet med NorSySS er å se trender og utbredelse av smittsomme sykdommer slik at utbrudd oppdages så tidlig som mulig. ",
          "I tillegg kan overvåkningen brukes til å vurdere effekt av folkehelsetiltak.", br(), br(),

          "Under kan du velge blant tre faner som gir deg forskjellig informasjon:", br(),
          "- ", strong("Oversikt"), " fanen vil gi deg en rekke grafer or tabeller hvor du kan velge det geografiske området du er interessert i", br(),
          "- ", strong("Interaktiv graf"), " fanen vil gi deg en interaktiv graf der du kan velge geografisk område og der du kan zoome inn på spesifikke områder av tidslinjen.", br(),
          "- ", strong("Informasjon"), " fanen gir deg litt mer informasjon om dataene vi bruker", br()
          )
      )
    )
    ,
    tabsetPanel(
      id="norsyss",
      tabPanel(
        title="Oversikt",
        norsyss_overview_ui("norsyss_overview", config=config)
      ),
      tabPanel(
        title="Interaktiv graf",
        norsyss_weekly_ui("norsyss_weekly", config=config)
      ),
      tabPanel(
        title="Informasjon",
        norsyss_purpose_ui("norsyss_purpose", config = config)
      )
    )
  )
}

norsyss_server <- function(input, output, session, config) {

}

