

module_pack002_import_s03_revelio_ui <- function(id){
  ns <- shiny::NS(id)



  div(uiOutput(ns("iu_base_selector")))



}

module_pack002_import_s03_revelio_server <- function(id, sui_data_source){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_revelio"
      })

      all_names_database <- reactive({
        req(check_ok())

        # list.files(input_folder_master)
        datasets_info <- data(package = "Revelio")
        dataset_names <- datasets_info$results[, "Item"]
        dataset_names

      })

      select_opt_database <- reactive({
        req(check_ok())
        req(all_names_database())

        # # # Nombre de las bases de datos
        vector_obj <- all_names_database()

        # # # Numero de orden, desed 01 hasta la ultima
        vector_numbers <- 1:length(vector_obj)
        amount_digits <- max(nchar(vector_numbers))
        if(amount_digits < 2) amount_digits <- 2

        vector_formatted_numbers <- stringr::str_pad(string = vector_numbers,
                                                     width = amount_digits,
                                                     pad = "0")

        # Vector con la visualizacion que tiene el usuario
        vector_visual <- paste0(vector_formatted_numbers, " - ", vector_obj)

        # Asignamos la visual a al vector
        names(vector_obj) <- vector_visual

        # Salida!
        vector_obj

      })

      output$iu_base_selector <- renderUI({
        req(check_ok())

        shiny::selectInput(
          inputId = ns("sui_file_revelio"),
          label = "Revelio Examples",
          choices = select_opt_database()
        )
      })

      database <- reactive({
        req(input$sui_file_revelio)

        selected_dataset <- input$sui_file_revelio
        #selected_dataset <- paste0("Revelio::", selected_dataset)

        get(selected_dataset, "package:Revelio")
        #eval(parse(text=selected_dataset))

        # switch(input$sui_file_revelio,
        #        "mtcars" = mtcars,
        #        "iris" = iris,
        #        "airquality" = airquality,
        #        stop("Base de datos no encontrada"))
      })

      return(database)
    }
  )
}
