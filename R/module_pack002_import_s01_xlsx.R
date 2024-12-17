
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


      #
      # import_code <- reactive({
      #
      #   my_code <- "# Lee el archivo CSV
      #   aver <- openxlsx::read.xlsx(xlsxFile = '_my_path_xlsx_',
      #                               sheet = 1)"
      #
      #   my_code
      # })
      #

      str_import_general_sentence <- reactive({
        my_text <- "openxlsx::read.xlsx(xlsxFile = '_my_path_', sheet = 1)"

        my_text
      })

      full_import_info <- reactive({
        req(input$file1)
        print(input$file1)
        input$file1
      })


      temporal_file_path <- reactive({
        req(input$file1$datapath)
        input$file1$datapath
      })



      original_file_name <- reactive({
        req(input$file1$name)
        input$file1$name
      })

      str_import_local <- reactive({
        req(str_import_general_sentence(), original_file_name())

        my_text <- str_import_general_sentence()
        my_text <- sub(pattern = "_my_path_", replacement = original_file_name(), x = my_text)
        my_text

      })

      database <- reactive({
        req(input$file1)
        req(check_ok())


        # Lee el archivo CSV
        print(input$file1$datapath)
        my_text <- str_import_general_sentence()
        my_text <- sub(pattern = "'_my_path_'", "input$file1$datapath", x = my_text)
        aver <- eval(parse(text = my_text))

        # Muestra el contenido del archivo xlsx
        aver
      })



      # observe({
      #
      #   print(str_import_general_sentence())
      #   print(full_import_info())
      #   print(temporal_file_path())
      #   print(original_file_name())
      #   print(str_import_local())
      #
      # })

      import_pack <- reactive({
        req(str_import_general_sentence(), full_import_info(), temporal_file_path(),
            original_file_name(),  str_import_local(), database())


        armado <- list("str_import_general_sentence" = str_import_general_sentence(),
             "full_import_info" = full_import_info(),
             "temporal_file_path" = temporal_file_path(),
             "original_file_name" = original_file_name(),
             "str_import_local" = str_import_local(),
             "database" = database()

        )

        #$print(armado)

        armado
      })


      return(import_pack)
    }
  )
}

