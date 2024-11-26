

# # # 01) UI - Selection for 'database'
module_fm999_s999_ui <- function(id){

  ns <- shiny::NS(id)



  div(
    "Móludo no encontrado!"


  )


}




# Var selection - Render - Show Results
module_fm999_s999_server <- function(id, vector_all_colnames_database, database){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      output$text_no_module <- renderText({
        "No se encontró el módulo!"
      })



    }
  )
}



