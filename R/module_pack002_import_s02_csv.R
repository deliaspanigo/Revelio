
module_pack002_import_s02_csv_ui <- function(id){
  ns <- shiny::NS(id)



  div(uiOutput(ns("iu_base_selector")))



}

module_pack002_import_s02_csv_server <- function(id, sui_data_source){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns


      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_csv"
      })

      output$iu_base_selector <- renderUI({
        req(check_ok())
        div(
          fileInput(ns("file1"), "Elige un archivo CSV",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          tags$hr(),
          fluidRow(
            column(2, radioButtons(ns("header"), "Encabezado", choices = c(Yes = 1, No = 0))),
            column(2, radioButtons(ns("sep"), "Separador",
                                   choices = c(PuntoYComa = ";",
                                               Coma = ",",
                                               Tab = "\t"),
                                   selected = ";")),
            column(2, radioButtons(ns("dec"), "Decimal",
                                   choices = c(Dot = ".",
                                               Comma = ","),
                                   selected = ".")),
            column(2, radioButtons(ns("quote"), "Comillas",
                                   choices = c(Ninguna = "",
                                               Doble = '"',
                                               Simple = "'"),
                                   selected = ""))
          )
        )
      })

      database <- reactive({
        req(input$file1)
        req(check_ok())

        # Lee el archivo CSV
        database <- read.csv(file = input$file1$datapath,
                             header = as.logical(as.numeric(input$header)),
                             sep = input$sep,
                             dec = input$dec,
                             quote = input$quote,
                             na.strings = c("", NA),
                             stringsAsFactors = F)

        # Muestra el contenido del archivo CSV
        database
      })


      return(database)
    }
  )
}
