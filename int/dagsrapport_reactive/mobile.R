mobile_ui <- function(id, config) {
  ns <- NS(id)
  tags$div(class="container-mobile",
           navbarPage(
             title = div(img(id="logo",src="fhi.svg", height="40px"), "Sykdomspulsen"),
             tabPanel("Dagsrapport mobil",
                      mobile_dagsrapport_ui("mobile_dagsrapport", config=config)
             ),
             theme = "fhi.css"
           )
  )
}

mobile_server <- function(input, output, session, config) {

}

