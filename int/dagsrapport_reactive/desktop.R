desktop_ui <- function(id, config) {
  ns <- NS(id)
  tags$div(class="container-desktop",
           navbarPage(
             title = div(img(id="logo",src="fhi.svg", height="40px"), "Sykdomspulsen"),
             tabPanel("Dagsrapport",
                      desktop_dagsrapport_ui("desktop_dagsrapport", config=config)
             ),
             theme = "fhi.css"
           )
  )
}

desktop_server <- function(input, output, session, config) {

}

