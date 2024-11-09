


# # # Pack 001 - Intro
module_pack001_intro_ui <- function(id){
  ns <- shiny::NS(id)



  div(shinyjs::useShinyjs(), id = ns("input-panel"),

      shiny::h1("Steps..."),
      "1) Select a database...",br(),
      "2) Click on LOAD",br(),
      "3) Select Summary, MLR or GLM Gamma",br(),
      "4) Click Render!"

  ) # End div
}




module_pack001_intro_server <- function(id){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns



    }) # Fin Module
}



