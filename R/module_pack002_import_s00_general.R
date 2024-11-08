


# # # 01) UI - Selection for 'database'
module_pack002_import_s00_general_ui <- function(id){
  ns <- shiny::NS(id)



  div(
    tags$head(
      #tags$link(rel = "stylesheet", type = "text/css", href = "inst/estilos.css"),
      tags$style(HTML("
      .shiny-output-error-AVISO {
        color: #0000ff;
        font-weight: bold;
      }
    ")),
      tags$style(HTML("
      .shiny-output-error-ERROR {
        color: #ff0000;
        font-weight: bold;
      }
    ")),
      tags$style(HTML("
        .content-wrapper, .right-side {
          overflow-y: hidden !important;
        }
      "))
    ),



    id = ns("input-panel"),
    shiny::h1("Selección de base de datos"),
    shiny::fluidRow(
      shiny::column(12,

                    uiOutput(ns("box01_database")),
                    shiny::br(),
                    shiny::br()

      )
    )
  ) # End div
}




module_pack002_import_s00_general_server <- function(id){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns


    output$box01_database <- renderUI({


             shiny::selectInput(
               inputId = ns("sui_data_source"),
               label = "Fuente de datos",
               choices = c("01 - xlsx files" = "source_xlsx",
                           "02 - CSV files"  = "source_csv",
                           "03 - Revelio examples"  = "source_revelio",
                           "04 - R examples" = "source_r"),
               selected = "source_r"

               )





      })

    sui_data_source <- reactive({
      req(input$sui_data_source)
      input$sui_data_source

    })

    return(sui_data_source)
    }
  )
}




################################################################


module_pack002_import_s01_xlsx_ui <- function(id){
  ns <- shiny::NS(id)



  div(uiOutput(ns("iu_base_selector")),
      DTOutput(ns("df_database")))



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

      output$df_database <- renderDT({
        req(database())
        mi_tabla <- database()
        new_col_names <- colnames(mi_tabla)

        DT::datatable(
          mi_tabla, colnames = new_col_names,
          filter = 'top',
          extensions = 'Buttons',
          options = list(
            autowidth = TRUE,
            order = list(list(0, 'asc')),
            columnDefs = list(list(className = 'dt-left', targets = c(0,1,2))),
            pageLength = 10,
            lengthMenu = c(10, 50, 75, 100, 150),
            dom = 'Bfrtip',  # Elementos de la tabla (botones, filtro, etc.)
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Botones para exportar la tabla
            initComplete = JS("
      function(settings, json) {
        $('body').css({
          'font-family': 'Century Gothic', 'font-size': '150%'
        });
        $(this.api().table().header()).css({
          'font-family': 'Century Gothic',
          'font-size':'125%',
          'background-color': '#008000',
          'color': '#fff'
        });
      }
    "),
            rowCallback = JS("
      function(row, data, index) {
        if(index % 2 === 0) {
          $(row).css('background-color', 'lightblue');
        } else {
          $(row).css('background-color', 'lightgreen');
        }
      }
    ")
          ),
          rownames = FALSE
        )

      })



      return(database)
    }
  )
}


################################################################

module_pack002_import_s02_csv_ui <- function(id){
  ns <- shiny::NS(id)



  div(uiOutput(ns("iu_base_selector")),
      DTOutput(ns("df_database")))



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

      output$df_database <- renderDT({
        req(database())
        mi_tabla <- database()
        new_col_names <- colnames(mi_tabla)

        DT::datatable(
          mi_tabla, colnames = new_col_names,
          filter = 'top',
          extensions = 'Buttons',
          options = list(
            autowidth = TRUE,
            order = list(list(0, 'asc')),
            columnDefs = list(list(className = 'dt-left', targets = c(0,1,2))),
            pageLength = 10,
            lengthMenu = c(10, 50, 75, 100, 150),
            dom = 'Bfrtip',  # Elementos de la tabla (botones, filtro, etc.)
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Botones para exportar la tabla
            initComplete = JS("
      function(settings, json) {
        $('body').css({
          'font-family': 'Century Gothic', 'font-size': '150%'
        });
        $(this.api().table().header()).css({
          'font-family': 'Century Gothic',
          'font-size':'125%',
          'background-color': '#008000',
          'color': '#fff'
        });
      }
    "),
            rowCallback = JS("
      function(row, data, index) {
        if(index % 2 === 0) {
          $(row).css('background-color', 'lightblue');
        } else {
          $(row).css('background-color', 'lightgreen');
        }
      }
    ")
          ),
          rownames = FALSE
        )

      })

      return(database)
    }
  )
}


################################################################

module_pack002_import_s03_revelio_ui <- function(id){
  ns <- shiny::NS(id)



      div(uiOutput(ns("iu_base_selector")),
          DTOutput(ns("df_database")))



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

      output$df_database <- renderDT({
        req(database())
        mi_tabla <- database()
        new_col_names <- colnames(mi_tabla)

        DT::datatable(
          mi_tabla, colnames = new_col_names,
          filter = 'top',
          extensions = 'Buttons',
          options = list(
            autowidth = TRUE,
            order = list(list(0, 'asc')),
            columnDefs = list(list(className = 'dt-left', targets = c(0,1,2))),
            pageLength = 10,
            lengthMenu = c(10, 50, 75, 100, 150),
            dom = 'Bfrtip',  # Elementos de la tabla (botones, filtro, etc.)
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Botones para exportar la tabla
            initComplete = JS("
      function(settings, json) {
        $('body').css({
          'font-family': 'Century Gothic', 'font-size': '150%'
        });
        $(this.api().table().header()).css({
          'font-family': 'Century Gothic',
          'font-size':'125%',
          'background-color': '#008000',
          'color': '#fff'
        });
      }
    "),
            rowCallback = JS("
      function(row, data, index) {
        if(index % 2 === 0) {
          $(row).css('background-color', 'lightblue');
        } else {
          $(row).css('background-color', 'lightgreen');
        }
      }
    ")
          ),
          rownames = FALSE
        )

      })

      return(database)
    }
  )
}

################################################################


module_pack002_import_s04_r_ui <- function(id){
  ns <- shiny::NS(id)



  div(uiOutput(ns("iu_base_selector")),
      DTOutput(ns("df_database")))



}

module_pack002_import_s04_r_server <- function(id, sui_data_source){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_r"
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

     output$df_database <- renderDT({
       req(database())
       mi_tabla <- database()
       new_col_names <- colnames(mi_tabla)

       DT::datatable(
         mi_tabla, colnames = new_col_names,
         filter = 'top',
         extensions = 'Buttons',
         options = list(
           autowidth = TRUE,
           order = list(list(0, 'asc')),
           columnDefs = list(list(className = 'dt-left', targets = c(0,1,2))),
           pageLength = 10,
           lengthMenu = c(10, 50, 75, 100, 150),
           dom = 'Bfrtip',  # Elementos de la tabla (botones, filtro, etc.)
           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Botones para exportar la tabla
           initComplete = JS("
      function(settings, json) {
        $('body').css({
          'font-family': 'Century Gothic', 'font-size': '150%'
        });
        $(this.api().table().header()).css({
          'font-family': 'Century Gothic',
          'font-size':'125%',
          'background-color': '#008000',
          'color': '#fff'
        });
      }
    "),
           rowCallback = JS("
      function(row, data, index) {
        if(index % 2 === 0) {
          $(row).css('background-color', 'lightblue');
        } else {
          $(row).css('background-color', 'lightgreen');
        }
      }
    ")
         ),
         rownames = FALSE
       )

     })

     return(database)
    }
  )
}

################################################################
