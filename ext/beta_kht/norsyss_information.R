

norsyss_purpose_ui <- function(id, config) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width=1,
        p("")
      ),
      column(
        width=10, align="left",
        p(
          "Vi får data til denne overvåkingen via NorSySS ",
          "Diagnosekoder som registreres hos lege eller legevakt sendes ",
          "til Helsedirektoratet som en del av legenes refusjonskrav (KUHR-systemet). ",
          "Folkehelseinstituttet mottar daglig oppdatert KUHR-data til NorSySS ",
          "Dataene er anonyme når vi mottar dem, uten pasientidentifikasjon, men med ",
          "informasjon om kjønn, aldersgruppe, konsultasjonsdato og sted for konsultasjon.", br(), br(),

          strong("Informasjon om dataene vi bruker i NorSySS:"), br(),
          "- Både telefon og legekontakt er inkludert", br(),
          "- Legekontor og legevakt er inkludert", br(),
          "- Geografisk område basert på stedet for legekonsultasjon, ikke pasientens bosted", br(),
          "- De kommunene som ikke har legevakt eller legekontor har vi ikke noe data for. De som bor i disse kommunene bruker legevakt og legekontor i andre kommuner. Dersom du har valgt en av kommunene uten legevakt eller legekontor vil det stå «error» for noen av grafene fordi vi ikke har data.", br(),
          "- Personene som bor i kommuner uten lege og legevakt benytter legekontor ",
          "og legevakt i andre kommuner", br(),
          "- Antallet konsultasjoner er vanligvis lavere i ferier og på helligdager. ",
          "Dette er spesielt tydelig rundt jul/nyttår og påske, men også i ",
          "sommerferieukene.", br(),
          "- Det kan være 14 dager forsinkelse i dataene da de kommer fra KUHR ",
          "systemet. Dersom det for noen datoer ikke er registrert noen ",
          "konsultasjoner fra et geografisk område vil dette vises som røde ",
          "stiplede linjer i grafene.", br(), br(),

          strong("Fargekoder i grafene:"), br(),
          "- Bakgrunnsfargen er laget ut fra beregninger fra de foregående 5 ",
          "årene i samme geografiske område og samme symptom/syndrom og ",
          "aldersgruppe (for årene 2006-2010 er 5 fremtidige år brukt).", br(),
          "- ", strong("Blå bakgrunnsfarge"), " viser at antallet konsultasjoner er som ", strong("forventet"), br(),
          "- ", strong("Gul bakgrunnsfarge"), " viser at antallet konsultasjoner er ", strong("høyere enn forventet"), br(),
          "- ", strong("Rød bakgrunnsfarge"), " viser at antall konsultasjoner er ", strong("betydelig høyere enn forventet"), br(),
          "- I grafer der det er en ", strong("svart strek"), " viser det antallet faktiske konsultasjoner. ",
          "Dersom denne streken er i det blå feltet er antallet konsultasjoner er som forventet, ",
          "om den er i det gule feltet er antallet konsultasjoner høyere enn forventet og om ",
          "den er i det røde feltet er antallet konsultasjoner betydelig høyere enn forventet ",
          "for gitt tidsrom, alder og geografisk område.", br(), br(),

          strong("Kommunereformen: "), "Kommuner som har blitt slått sammen og fått ",
          "et nytt navn vil ikke finnes i oversiktene. Kommuner som har blitt slått ",
          "sammen med en annen kommune men beholdt navnet vil vises i oversiktene, ",
          "og beregningene tar hensyn til sammenslåingen. Det samme gjelder ",
          "sammenslåtte kommuner som får nytt kommunenavn.", br(), br(),

          strong("Små kommuner: "), "Kommuner med under 500 innbyggere vil ikke ",
          "kunne se grafer for aldersgrupperinger, men bare 'totalt antall'. ",
          "Dette er av hensyn til personvern.", br(), br(),

          strong("Interkommunalt samarbeid om legekontor/legevakt: "),
          "I NorSySS er geografisk område basert på stedet for legekonsultasjon, ",
          "ikke pasientens bosted. Derfor vil legekontorets/legevaktens postadresse ",
          "si hvilken kommune som vises i NorSySS. De andre kommunene som er ",
          "med på det interkommunale samarbeidet vil ikke vises i NorSySS", br(), br(),

          strong("Type konsultasjon:"),br(),
          strong("Oppmøte"), " inkluderer takstkodene: 11ad, 11ak, 2ad, 2ak, 2fk", br(),
          strong("Telefonkonsultasjon"), " inkluderer takstkodenene: 1ad, 1ak, 1bd, 1bk, 1g, 1h", br(),
          strong("e-konsultasjon"), " inkluderer takstkodene: 1be, 2ae", br(), br(),

          strong("Ved tekniske feil, spørsmål eller tilbakemeldinger "),
          "vennligst send en mail til sykdomspulsen@fhi.no"
        ),
        br()
      ),
      column(
        width=1,
        p("")
      )
    )
  )
}

norsyss_purpose_server <- function(input, output, session, config) {

}

