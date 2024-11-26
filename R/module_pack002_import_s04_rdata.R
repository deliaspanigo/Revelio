
module_pack002_import_s04_rdata_ui <- function(id){
  ns <- shiny::NS(id)



  div(uiOutput(ns("iu_base_selector")))



}

module_pack002_import_s04_rdata_server <- function(id, sui_data_source){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_rdata"
      })


      output$iu_base_selector <- renderUI({
        req(check_ok(), sui_data_source())


        shiny::selectInput(
          inputId = ns("r_database"),
          label = "Bases de R",
          choices = c("01 - mtcars"     = "mtcars",
                      "02 - iris"       = "iris",
                      "03 - airquality" = "airquality")
        )
      })


      database <- reactive({
        req(check_ok(), sui_data_source())
        req(input$r_database)
        switch(input$r_database,
               "mtcars" = mtcars,
               "iris" = iris,
               "airquality" = airquality)
      })


      return(database)
    }
  )
}
