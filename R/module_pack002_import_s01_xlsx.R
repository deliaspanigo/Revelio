
module_pack002_import_s01_xlsx_ui <- function(id){
  ns <- shiny::NS(id)



  div(uiOutput(ns("iu_base_selector")))



}

module_pack002_import_s01_xlsx_server <- function(id, sui_data_source){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_xlsx"
      })

      output$iu_base_selector <- renderUI({
        req(check_ok())
        div(
          fileInput(ns("file1"), "Elige un archivo xlsx (Solo primer hoja)",
                    accept = c(
                      ".xlsx")
          )
        )




      })



      database <- reactive({
        req(input$file1)
        req(check_ok())


        # Lee el archivo CSV
        aver <- openxlsx::read.xlsx(xlsxFile = input$file1$datapath,
                                    sheet = 1)

        # Muestra el contenido del archivo xlsx
        aver
      })



      return(database)
    }
  )
}

