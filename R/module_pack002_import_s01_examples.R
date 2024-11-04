


# # # 01) UI - Selection for 'database'
module_pack002_import_s01_examples_ui <- function(id){
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




module_pack002_import_s01_examples_serverA <- function(id){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      ######################
      ##################################
      # # # Sheet selection for xlsx file
      # # Default - 'database' object


      #####################################################################
      all_names_database_MLG <- reactive({

        # the_package_name <- "Revelio"
        # special_folder <- file.path("data")
        #
        # input_path_package   <- base::system.file(package = the_package_name)
        # input_folder_package <- file.path(input_path_package, special_folder)
        # input_folder_local   <- file.path(getwd(), special_folder)
        # input_folder_master <-  ifelse(input_folder_package != "",
        #                                input_folder_package, input_folder_local)
        #
        # list.files(input_folder_master)
        datasets_info <- data(package = "Revelio")
        dataset_names <- datasets_info$results[, "Item"]
        dataset_names

      })

      select_opt_database_MLG <- reactive({
        req(all_names_database_MLG())

        # # # Nombre de las bases de datos
        vector_obj <- all_names_database_MLG()

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



      output$box01_database <- renderUI({

        shinydashboard::box(
          title = "01 - Seleccionar base de datos",
          status = "primary",
          id = ns("my_box01"),
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          width = 12,
          fluidRow(
            # Parte 1 de 3
            column(3,
                   shiny::selectInput(
                     inputId = ns("data_source"),
                     label = "Fuente de datos",
                     choices = c("01 - CSV files" = "csv_source",
                                 "02 - Diplo UNC" = "diplo_source",
                                 "03 - R examples" = "r_source"#,
                                 #"04 - xlsx files" = "xlsx_source"
                     )
                   )
            ),

            # Parte 2 de 3
            column(7,
                   div(shinyjs::useShinyjs(), id = ns("input-var-02-A"),
                       shiny::conditionalPanel(
                         condition = "input.data_source == 'csv_source'",
                         ns = ns,
                         fluidRow(
                           column(4,
                                  fileInput(
                                    inputId = ns("csv_file_path"),
                                    label = "Elija un archivo CSV",
                                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                                    #accept = "*/*"
                                  )
                           )
                         ),
                         fluidRow(
                           column(2, radioButtons(
                             inputId = ns("csv_header"),
                             label = "header",
                             choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                             selected = TRUE
                           )),
                           column(4, radioButtons(
                             inputId = ns("csv_sep"),
                             label = "Separador de columnas",
                             choices = c("semicolon (;)" = ";", "comma (,)" = ","),
                             selected = ";"
                           )),
                           column(3, radioButtons(
                             inputId = ns("csv_dec"),
                             label = "Decimal",
                             choices = c("Period (.)" = ".", "Comma (,)" = ","),
                             selected = "."
                           )),
                           column(3, radioButtons(
                             inputId = ns("csv_quote"),
                             label = "Comillas",
                             choices = c("Double quotes (\")" = "\"", "Simple quotes (')" = "'"),
                             selected = "\""
                           ))
                         )#,
                         #tags$hr()
                       )
                   ),
                   div(shinyjs::useShinyjs(), id = ns("input-var-02-B"),
                       shiny::conditionalPanel(
                         condition = "input.data_source == 'diplo_source'",
                         ns = ns,
                         fluidRow(
                           column(7,
                                  shiny::selectInput(
                                    inputId = ns("diplo_database"),
                                    label = "Bases de la Diplomatura",
                                    choices = select_opt_database_MLG()
                                  )
                           )
                         )
                       )
                   ),
                   shiny::conditionalPanel(
                     condition = "input.data_source == 'r_source'",
                     ns = ns,
                     fluidRow(
                       column(3,
                              shiny::selectInput(
                                inputId = ns("r_database"),
                                label = "Bases de R",
                                choices = c("01 - mtcars" = "mtcars", "02 - iris" = "iris")
                              )
                       )
                     )
                   ),
                   shiny::conditionalPanel(
                     condition = "input.data_source == 'xlsx_source'",
                     ns = ns,
                     fluidRow(
                       column(3,
                              fileInput(
                                inputId = ns("xlsx_file_path"),
                                label = "Elija un archivo xlsx (Solo 1ra hoja!)",
                                accept = c(".xlsx")
                                #accept = "*/*"
                              )
                       )#,
                       #column(2, uiOutput(ns("sheet_selection"))),
                     )
                   )
            ),

            # Parte 3 de 3
            column(2, actionButton(ns("action_load"),
                                   label = "LOAD"),
            )
          ),
          shinycssloaders::withSpinner(DT::DTOutput(ns("table_database"))),

        )

      })



    }
  )
}




module_pack002_import_s01_examples_serverB <- function(id){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      ##################################
      selected_xlsx_database <- reactiveVal(NULL)
      sheets_xlsx <- reactiveVal(NULL)

      observeEvent(input$xlsx_database, {

        #req(control_user_01())
        # # # Reset for action buttons
        #action_button_load(FALSE)
        #action_button_show(FALSE)
        #color_button_load(hardcorded_initial_color)

        selected_xlsx_database(input$xlsx_database)
        #shinyjs::reset("sheet_xlsx")
        # # # Reset database
        database(NULL)

      })

      # output$sheet_selection <- renderUI({
      #
      #   ns <- shiny::NS(id)
      #
      #   # Previuos controls
      #   req(sheets_xlsx())
      #
      #   pos_selected <- 1
      #
      #   # # # SelectInput for sheet selection
      #   selectInput(inputId = ns("sheet_xlsx"),
      #               label = "Sheet selector:",
      #               choices = sheets_xlsx(),
      #               selected = sheets_xlsx()[pos_selected])
      #
      # })
      #
      # # selected_sheet <- reactive({
      #   #req(control_user_03())
      #
      #   output_value <- input$sheet_xlsx
      #   output_value <- as.integer(as.numeric(as.character(output_value)))
      #   output_value
      # })

      # observeEvent(selected_xlsx_database(), {
      #
      #   #req(control_user_02())
      #   database(NULL)
      #   sheets_xlsx(NULL)
      #
      #   # # # # Reset for action buttons
      #   # action_button_load(FALSE)
      #   # action_button_show(FALSE)
      #   # color_button_load(hardcorded_initial_color)
      #   #
      #   # shinyjs::reset("input-xlsx")
      #   # shinyjs::reset("sheet_xlsx")
      #   # # # # Reset database
      #
      #   name_sheets <- openxlsx::getSheetNames(selected_xlsx_database()$datapath)
      #   set_options <- super_sheet_info(name_sheets = name_sheets)
      #   set_options <- c("Select one... " = "", set_options)
      #   sheets_xlsx(set_options)
      # })
      #


      observeEvent(selected_xlsx_database(), {

        #req(control_user_02())
        database(NULL)
        #sheets_xlsx(NULL)

        # # # # Reset for action buttons
        # action_button_load(FALSE)
        # action_button_show(FALSE)
        # color_button_load(hardcorded_initial_color)
        #
        # shinyjs::reset("input-xlsx")
        # shinyjs::reset("sheet_xlsx")
        # # # # Reset database

        # name_sheets <- openxlsx::getSheetNames(selected_xlsx_database()$datapath)
        # set_options <- super_sheet_info(name_sheets = name_sheets)
        # set_options <- c("Select one... " = "", set_options)
        # sheets_xlsx(set_options)
      })
      #########################################################

      ##############################################
      # Objetos iniciales importantes
      database <- shiny::reactiveVal()
      load_button_status  <- reactiveVal()
      load_button_counter <- reactiveVal()
      code_database <- reactiveVal()


      code_exec_database_csv <- "
      utils::read.csv(file = input$csv_file_path$datapath,
                               header = as.logical(as.character(input$csv_header)),
                               sep = input$csv_sep,
                               dec = input$csv_dec,
                               stringsAsFactors = FALSE)
      "
      code_exec_database_diplo <- "input$diplo_database"

      code_exec_database_r <-  "input$r_database"

      code_exec_database_xlsx <- "
      openxlsx::read.xlsx(xlsxFile = input$xlsx_file_path$datapath,
                               sheet = 1)
      "
      # 1) Control de lo detallado como "data_source"
      # y de lo elegido en el subtipo de fuente de datos. (control01).
      # 2) Obtener database() y control de database() (control02).
      # .Desde la fuente de datos hasta tener el database()
      # 2)


      ###---###---###---###---###---###---###---###---###---###---###---###---###---###

      #--- 1) control_01 - Control del inputfile
      #--- 6) Enable/Disable - Render Report Button




      control_01 <- reactive({

        database(NULL)
        load_button_status(FALSE)
        load_button_counter <- reactiveVal(0)
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))

        ns <- session$ns

        # # # Control sobre input$data_source
        req(input$"data_source")

        validate(
          need(!is.null(input$"data_source"), 'Error 001: Problemas en input$data_source.'),
          errorClass = "ERROR"
        )

        # Si el archivo es CSV...
        if(input$"data_source" == "csv_source"){

          req(input$csv_file_path$name)

          csv_file_name <- input$csv_file_path$name
          final_ext <- tools::file_ext(csv_file_name)

          validate(
            need(final_ext == "csv", 'Error 002: Solo es posible cargar archivos CSV.\n    Prueba con otro archivo!'),
            errorClass = "ERROR"
          )
        } else

          # Si el archivo es una base de R...
          if(input$"data_source" == "diplo_source"){

            req(input$diplo_database)

            validate(
              need(exists(input$diplo_database), 'Error 002: La base seleccionada de la Diplomatura no se encuentra en el entorno de R.\n'),
              errorClass = "ERROR"
            )
          } else
            # Si el archivo es una base de R...

            if(input$"data_source" == "r_source"){

              req(input$r_database)

              validate(
                need(exists(input$r_database), 'Error 002: La base de R seleccionada no se encuentra en el entorno de R.\n'),
                errorClass = "ERROR"
              )
            }


        shinyjs::enable("action_load")
        load_button_status(TRUE)
        return(TRUE)
      })
      ###---###---###---###---###---###---###---###---###---###---###---###---###---###



      observeEvent(input$action_load, {

        # Todo lo anterior tiene que estar OK.
        req(control_01())

        load_button_counter(load_button_counter() + 1)
      })


      observeEvent(load_button_counter(), {

        # Todo lo anterior tiene que estar OK.
        req(control_01())

        #load_button_counter(load_button_counter() + 1)
        if(load_button_counter() >= 1){
          runjs(sprintf('$("#%s").css({"background-color": "green",  "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))

        }

        if(load_button_counter() == 0){
          database(NULL)
          runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))
        }


      })



      shiny::observeEvent(input$data_source,{
        database(NULL)
        load_button_counter(0)
      })

      shiny::observeEvent(input$diplo_database,{
        database(NULL)
        load_button_counter(0)
      })

      shiny::observeEvent(input$csv_database,{

        database(NULL)
        load_button_counter(0)



      })

      shiny::observeEvent(input$csv_header,{
        database(NULL)
        load_button_counter(0)
      })

      shiny::observeEvent(input$csv_sep,{
        database(NULL)
        load_button_counter(0)
      })

      shiny::observeEvent(input$csv_dec,{
        database(NULL)
        load_button_counter(0)
      })

      shiny::observeEvent(input$csv_quote,{
        database(NULL)
        load_button_counter(0)
      })

      shiny::observeEvent(input$r_database,{


        database(NULL)
        load_button_counter(0)
      })

      ###---###---###---###---###---###---###---###---###---###---###---###---###---###


      #--- 2.2) control_02 - load button
      control_02 <- reactive({

        # # # Control sobre database()
        req(control_01(),  load_button_status(), load_button_counter()>=1)

        return(TRUE)

      })


      #--- 2) Fuente de datos y creacion de database
      #--- 2.1) Database!!!
      observe({

        #print(control_02())
        # Todo lo anterior tiene que estar OK.
        # Solo ejecuta si el status del load es TRUE y si tiene
        # un contador de load >= 1, lo que implica que el usuario hizo
        # clic en "LOAD".
        req(control_02())

        if(input$data_source == "csv_source"){

          code_mode_database_csv <- code_exec_database_csv
          code_mode_database_csv <- sub(pattern = "input$csv_file_path$datapath", replacement = input$csv_file_path$datapath, x = code_mode_database_csv)
          code_mode_database_csv <- sub(pattern = "input$csv_header",             replacement = input$csv_header,             x = code_mode_database_csv)
          code_mode_database_csv <- sub(pattern = "input$csv_sep",                replacement = input$csv_sep, x = code_mode_database_csv)
          code_mode_database_csv <- sub(pattern = "input$csv_dec",                replacement = input$csv_dec, x = code_mode_database_csv)

          code_database(NULL)
          code_database(code_mode_database_csv)
          # database(
          #   eval(parse(text = code_mode_database_csv))
          #   )


        } else

          if(input$data_source == "diplo_source"){

            req(input$diplo_database)
            code_database(NULL)
            code_database(input$diplo_database)
          } else

            if(input$data_source == "r_source"){

              req(input$diplo_database)
              code_database(NULL)
              code_database(input$r_database)



            } else

              if(input$data_source == "xlsx_source"){
                print("AVER1")
                print(input$xlsx_file_path$datapath)
                print("AVER2")

                #req(input$xlsx_database)
                code_mode_database_xlsx <- code_exec_database_xlsx
                code_mode_database_xlsx <- sub(pattern = "input$xlsx_file_path$datapath", replacement = input$xlsx_file_path$datapath, x = code_mode_database_xlsx)
                #code_mode_database_csv <- sub(pattern = "input$csv_header",             replacement = input$csv_header,             x = code_mode_database_csv)
                #code_mode_database_csv <- sub(pattern = "input$csv_sep",                replacement = input$csv_sep, x = code_mode_database_csv)
                #code_mode_database_csv <- sub(pattern = "input$csv_dec",                replacement = input$csv_dec, x = code_mode_database_csv)
                print( code_mode_database_xlsx)

                code_database(NULL)
                code_database(code_mode_database_xlsx)



              }

        # Levantamos la base de datos
        database(
          eval(parse(text = code_database()))
        )

      })




      #--- 2.2) control_02 - Control del database()
      control_03 <- reactive({

        # # # Control sobre database()
        req(control_02(), database())

        validate(
          need(!is.null(database()), 'Error 003: Problemas en la base de datos. Vuelva a cargar el archivo'),
          errorClass = "ERROR"
        )

        validate(
          need(is.data.frame(database()), 'Error 004: El objeto debe ser un data.frame.'),
          errorClass = "ERROR"
        )

        validate(
          need((ncol(database())>= 1), 'Error 005: La base de datos debe contener al menos una columna.'),
          need((nrow(database())>= 1), 'Error 006: La base de datos debe contener al menos una fila.'),
          errorClass = "ERROR"
        )



        return(TRUE)

      })


      #--- 2.3) Salida en tabla de la database()
      output$table_database <- DT::renderDT({


        req(control_03())

        # Usar lapply para mostrar los elementos deseados

        mi_tabla <- database()
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]

        datatable(
          mi_tabla,
          rownames = FALSE,
          options = list(

            headerCallback = DT::JS(
              "function(thead) {",
              "  $(thead).css('font-size', '2em');",
              "}"
            ),
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            pageLength = 5,
            lengthMenu = c(5, 10, 15, 20),
            #dom = "t",
            scrollX = TRUE,
            searching = FALSE,
            scrollCollapse = TRUE,  # Permitir colapsar el scroll
            fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
            #lengthMenu = list(c(-1), c("All")), # Todas las filas
            style = list(
              'font-size' = '15px'  # Tamaño de letra para el nombre de las columnas
            )
          )

        ) %>%formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "15px"
        )
      })
      ###---###---###---###---###---###---###---###---###---###---###---###---###---###


      ###################################################



      output_list <- Hmisc::llist(database, code_database)
      return(output_list)











    }) # Fin Module
}

