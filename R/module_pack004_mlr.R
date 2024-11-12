

# # # 01) UI - Selection for 'database'
module_pack004_mlr_ui <- function(id){

  ns <- shiny::NS(id)



  div(
    tags$head(
      tags$style(HTML("
      .shiny-output-error-WARNING {
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
    shiny::h1("Multiple Linear Regresion"),
    shiny::fluidRow(uiOutput(ns("box02_var_selector"))),
    br(),
    fluidRow(
      column(6),
      column(6, shiny::fluidRow(uiOutput(ns("box03_control_de_mision"))))
    ),

    shiny::fluidRow(
      shiny::column(12,
                    shiny::textOutput(ns("text_control_general")))),
    shiny::fluidRow(
      shiny::column(12,
                    shinycssloaders::withSpinner(uiOutput(ns("box04_report"))),
                    br(), br(), br()
      )
    )
  )


}




# Var selection - Render - Show Results
module_pack004_mlr_server <- function(id, vector_all_colnames_database, database){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      standard_info_var_selection <- reactive({
        req(vector_all_colnames_database())



        vector_colnames <- vector_all_colnames_database()
        vector_pos <- 1:length(vector_all_colnames_database())
        vector_letters <- openxlsx::int2col(vector_pos)



        # Determinar la cantidad máxima de dígitos
        max_digits <- max(nchar(vector_pos))
        max_digits <- max(max_digits, 2)
        vector_order <- sprintf(paste0("%0", max_digits, "d"), vector_pos)


        # Para el usuario
        vector_names <- paste0(vector_order, " - ", vector_letters, " - ", vector_colnames)

        # Vector de opcion interno (nombre de columnas)
        vector_options <- vector_colnames
        names(vector_options) <- vector_names
        vector_options <- c("Selecciona una..." = "", vector_options)


        output_list <- list()
        output_list$"vector_pos"      <- vector_pos
        output_list$"vector_letters"  <- vector_letters
        output_list$"vector_letters"  <- vector_letters
        output_list$"vector_colnames" <- vector_colnames
        output_list$"vector_order"    <- vector_order
        output_list$"vector_options"  <- vector_options

        return(output_list)
      })


      output$var_selector01 <- shiny::renderUI({

        req(standard_info_var_selection())


        ns <- shiny::NS(id)



        vector_options <- standard_info_var_selection()$"vector_options"
        vector_options <- vector_options[-1]

        div(
          shiny::radioButtons(inputId = ns("selected_vr_name"), label = "Response Variable (Y)",
                              choices = vector_options,
                              selected = character(0))
        )

      })

      output$var_selector02 <- shiny::renderUI({



        req(standard_info_var_selection())


        ns <- shiny::NS(id)



        vector_options <- standard_info_var_selection()$"vector_options"
        vector_options <- vector_options[-1]

        div(
          shiny::checkboxGroupInput(inputId = ns("selected_x_name"), label = "Regresor vars (X)",
                                    choices = vector_options)


        )

      })



      output$box02_var_selector <- renderUI({

        shinydashboard::box(
          title = "01 - Variables Selection",
          status = "primary",
          id = ns("my_box02"),
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          width = 12,
          fluidRow(
            column(6, shiny::uiOutput(ns("var_selector01"))),
            column(6, shiny::uiOutput(ns("var_selector02")))
          )




        )
      })

      ############################



      ############################


      output$box03_control_de_mision <- renderUI({

        ns <- shiny::NS(id)

        div(shinydashboard::box(
          title = "02 - Mission Control",
          status = "primary",
          id = ns("my_box03"),
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          width = 12,
          div(
            #h2("Generacion de Reportes"), br(),
            #h3("- Base de datos - OK!"),
            #h3("- Variable cuantitativa seleccionada - OK!"),
            #h3("- Reporte y script - OK!"),
            actionButton(ns("render_report_button"), "Render Report", width = "100%"),
            downloadButton(outputId = ns('download_button_html'),  label = "HTML", width = "100%", disabled = TRUE),
            #downloadButton(outputId = ns('download_button_pdf'),   label = "PDF", width = "100%", disabled = TRUE),
            #downloadButton(outputId = ns('download_button_word'),  label = "WORD", width = "100%", disabled = TRUE),
            #downloadButton(outputId = ns('download_button_Rcode'), label = "Rcode", width = "100%", disabled = TRUE),
            downloadButton(outputId = ns('download_button_zip'),   label = "All (ZIP)", width = "100%", disabled = TRUE)
          )
        )
        )
      })


      output$htmlviewer_temporal <- renderText({

        req(control_01(), special_path_output_folder(), special_path_output_html_code())

        # # # Definimos como un "alias" o un "bindeo".
        # A la carpeta temporal le damos como un "alias".
        # Esto es por que los HTML no pueden ser tomados de cualquier lado.
        my_path <- special_path_output_folder()
        addResourcePath(prefix = "output_temp_folder", directoryPath = my_path)

        # Armamos ahora un path con el "alias" como folder.
        my_file <- basename(special_path_output_html_code())
        my_local_file <- file.path("output_temp_folder", my_file)

        # Levantamos el html
        armado_v <- paste('<div style="height: 100%; width: 100%; overflow: hidden;"><iframe style="height: 1000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")

        return(armado_v)
      })

      output$htmlviewer_temporal2 <- renderText({

        req(control_01(), special_path_output_folder(), special_path_output_html_report())

        # # # Definimos como un "alias" o un "bindeo".
        # A la carpeta temporal le damos como un "alias".
        # Esto es por que los HTML no pueden ser tomados de cualquier lado.
        my_path <- special_path_output_folder()
        addResourcePath(prefix = "output_temp_folder", directoryPath = my_path)

        # Armamos ahora un path con el "alias" como folder.
        my_file <- basename(special_path_output_html_report())
        my_local_file <- file.path("output_temp_folder", my_file)

        # Levantamos el html
        armado_v <- paste('<div style="height: 100%; width: 100%; overflow: hidden;"><iframe style="height: 1000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")

        return(armado_v)
      })



      output$mega_switch <- renderUI({
        req(input$"mega_panel")

        switch(input$"mega_panel",
               "1" = shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal"))),
               "2" = shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal2"))),
               "3" = uiOutput(ns("selected_pack")))

      })


      output$box04_report <- renderUI({
        req(render_button_status(), render_button_counter() > 0)

        ns <- NS(id)


        div(
          tabsetPanel(id = ns("mega_panel"), selected = 2,
            tabPanel(title = "R Code", value = 1),
            tabPanel(title = "Summary MLR", value = 2),
            tabPanel(title = "Model View", value = 3)),
                     uiOutput(ns("mega_switch")),
                     br(),br(),br(),br(),br(),br(),br(),br(),br(),
          br(),br(),br(),br(),br(),br(),br(),br(),br(),
          br(),br(),br(),br(),br(),br(),br(),br(),br(),
          br(),br(),br(),br(),br(),br(),br(),br(),br()
            )


#
#         div(
#           tabsetPanel(
#             selected = 2,
#             tabPanel(title = "R Code", value = 1,
#                      fluidRow(
#                        column(12,
#                               shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal"))))
#                      )
#             ),
#             tabPanel(title = "Summary MLR", value = 2,
#                      fluidRow(
#                        column(12,
#                               shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal2"))))
#                      )
#             ),
#             tabPanel(title = "Model View", value = 3,
#                      uiOutput(ns("selected_pack")),
#                      br(),br(),br(),br(),br(),br(),br(),br(),br()
#             )
#           )
#         )

      })


      output$text_control_general <- renderText({
        req(control_01())
        ""
      })

      ####################################################

      render_button_status  <- shiny::reactiveVal()
      render_button_counter <- shiny::reactiveVal()

      observeEvent(input$selected_vr_name, {
        render_button_status(FALSE)
        render_button_counter(0)
        #shinyjs::disable("render_report_button")
        #runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))
      })

      observeEvent(input$selected_x_name, {
        render_button_status(FALSE)
        render_button_counter(0)
        #shinyjs::disable("render_report_button")
        #runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))
      })

      observeEvent(input$render_report_button, {

        # Todo lo anterior tiene que estar OK.
        req(control_01())

        render_button_counter(render_button_counter() + 1)
      })



      control_01 <- reactive({

        #req(vector_all_colnames_database(), input$selected_vr_name, input$selected_x_name)

        #req(input$selected_vr_name, input$selected_x_name)

        validate(
          need(!is.null(input$selected_vr_name),  'Select a single variable Y.'),
          need(!is.null(input$selected_x_name),   'Select two or more X variables.'),
          errorClass = "WARNING"
        )

        validate(
          need(input$selected_vr_name != "", 'Select a single variable Y.'),
          need(input$selected_x_name != "", 'Select two or more X variables.'),
          errorClass = "WARNING"
        )

        validate(
          need(length(input$selected_vr_name) == 1, 'Select only a single variable Y.'),
          need(length(input$selected_x_name) >= 2, 'Select two or more X variables.'),
          errorClass = "WARNING"
        )

        check_vr <- sum(vector_all_colnames_database() == input$selected_vr_name) == 1
        check_x <-  sum(vector_all_colnames_database() %in% input$selected_x_name) == length(input$selected_x_name)

        validate(
          need(check_vr, 'Error 006: Problems with selected names. Rename the columns or try uploading the file again..'),
          need(check_x,  'Error 007: Problems with selected names. Rename the columns or try uploading the file again..'),
          errorClass = "ERROR"
        )

        validate(
          need(sum(input$selected_x_name %in% input$selected_vr_name) == 0, 'Error 007: The same variable cannot be in both groups.'),
          errorClass = "ERROR"
        )
        #shinyjs::enable("render_report_button")
        if(!render_button_status()) render_button_status(TRUE)
        #runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))

        if(render_button_counter() == 0 && !render_button_status()){
          shinyjs::disable("render_report_button")
          runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))
        }


        # Todo lo anterior tiene que estar OK.
        if(render_button_counter() == 0 && render_button_status()){
          shinyjs::enable("render_report_button")
          runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))
        }

        #load_button_counter(load_button_counter() + 1)
        if(render_button_counter() >= 1 && render_button_status()){
          runjs(sprintf('$("#%s").css({"background-color": "green",  "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))

        }

        return(TRUE)

      })

      ##############################################################

      # Selected class


      # # # Special folder
      input_folder_master <- reactive({

        # # # Folder package
        # Depende de si lo toma como local o como parte del package
        # En Desarrollo lo toma local.
        # Para el usuario final lo toma como package.
        # De esta forma corre bien para cualquiera de los dos.

        # # # Detalles para este caso en particular
        the_package_name <- "Revelio"
        selected_folder <- "pack04_mlr"   ### CAMBIO!!!

        # # # Sigue el resto...
        special_folder_package <- file.path("extdata", selected_folder)
        special_folder_local <- file.path("inst", "extdata", selected_folder)

        input_path_package   <- base::system.file(package = the_package_name)
        input_folder_package <- file.path(input_path_package, special_folder_package)
        input_folder_local   <- file.path(getwd(), special_folder_local)


        check_cantidad_files_local <- length(list.files(input_folder_local)) > 0
        check_cantidad_files_package <- length(list.files(input_folder_package)) > 0

        input_folder_master <-  ifelse(check_cantidad_files_package,
                                       input_folder_package, input_folder_local)

        input_folder_master <- as.character(input_folder_master)
        input_folder_master
      })

      # Nombre de los archivos originales
      all_files_master <- reactive({
        req(input_folder_master())

        vector_files <- list.files(input_folder_master())
        #print(vector_files)
        vector_files
      })

      # Nombre correspondiente para los archivos modificados
      all_files_mod <- reactive({
        vector_all_master <- all_files_master()
        vector_all_mod <- gsub("_master", "_mod", vector_all_master)
        vector_all_mod

      })

      # Nombre de los nuevos objetos
      all_files_new <- reactive({

        vector_rmd_mod <- grep("\\.Rmd$", all_files_mod(), value = TRUE)

        vector_html_new <- gsub("\\.Rmd$", ".html", vector_rmd_mod)

        vector_R_new <- gsub("\\.Rmd$", ".R", vector_rmd_mod)
        vector_R_new <- grep("_CODE", vector_R_new, value = TRUE)

        vector_RData_new <- "R_objects_new.RData"

        vector_all_new <- c(vector_html_new, vector_R_new, vector_R_new, vector_RData_new)
        vector_all_new

      })

      special_path_output_folder <- reactiveVal()

      special_path_output_rmd_code <- reactiveVal()
      special_path_output_html_code <- reactiveVal()

      special_path_output_rmd_report <- reactiveVal()
      special_path_output_html_report <- reactiveVal()


      observeEvent(input$render_report_button, {

        # Todo lo anterior tiene que estar OK.
        req(control_01(), render_button_status())

        #print("AAA")
        # # # Execution time...
        original_time  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        execution_time <- gsub("[[:punct:]]", "_", original_time)
        execution_time <- gsub(" ", "_", execution_time)


        # Carpeta temporal
        # # # New temporal file
        new_temp_folder <- tempdir()
        new_sub_folder <- paste0("Revelio_FOLDER_", execution_time)
        output_folder_temp <- file.path(new_temp_folder, new_sub_folder)
        special_path_output_folder(output_folder_temp)
        dir.create(output_folder_temp)

        #print("BBB")

        # Obtener la lista de archivos en el directorio de origen
        original_files <- list.files(input_folder_master(), full.names = TRUE)

        #print("CCC")
        # Copiar los archivos al directorio de destino
        file.copy(original_files, output_folder_temp, overwrite = TRUE)

        #print("DDD")

        #print(all_files_master())
        original_name_path <- file.path(output_folder_temp, all_files_master())
        chance_name_path   <- file.path(output_folder_temp, all_files_mod())
        file.rename(original_name_path, chance_name_path)

        #print("ZZZ")

        # # # Objetos de entorno
        # Estos objetos seran usados como si estuvieran
        # detallados dentro del archivo .Rmd.
        # Por ejemplo, tomamos la base de datos.
        render_env <- new.env()
        render_env$"database" <- database()
        render_env$"selected_vr_name" <- input$selected_vr_name
        render_env$"selected_x_name" <- input$selected_x_name

        #render_env$"the_time" <- original_time
        #render_env$"data_source" <- input$data_source

        # # # Render All
        #rmarkdown::render(output_path_rmd, rmarkdown::pdf_document(),  output_file = output_path_pdf, envir = render_env)
        #vector_R_new <- gsub("\\.Rmd$", ".R", vector_rmd_mod)
        #vector_R_new <- grep("_CODE", vector_R_new, value = TRUE)
        output_path_rmd  <- chance_name_path
        output_path_rmd  <- grep("_CODE", output_path_rmd, value = TRUE)
        output_path_html <-gsub("\\.Rmd$", ".html", output_path_rmd)

        special_path_output_html_code(output_path_html)

        rmarkdown::render(output_path_rmd, rmarkdown::html_document(), output_file = output_path_html, envir = render_env)


        output_path_rmd2  <- chance_name_path
        output_path_rmd2  <- grep("_REPORT", output_path_rmd2, value = TRUE)
        output_path_html2 <- gsub("\\.Rmd$", ".html", output_path_rmd2)
        special_path_output_html_report(output_path_html2)

        rmarkdown::render(output_path_rmd2, rmarkdown::html_document(), output_file = output_path_html2, envir = render_env)


      })

      ######################################################

      download_counter_html <- reactiveVal(0)

      observeEvent(special_path_output_html_report(),{

        if(file.exists(special_path_output_html_report())){
          shinyjs::enable("download_button_html")
          runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_html")))
        }

      })

      observeEvent(download_counter_html(),{

        if(download_counter_html() > 0){
          runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_html")))

        }

      })

      # control_02 <- reactive({
      #
      #   #req(control_01(), special_path_output_html_report())
      #
      #   #req(vector_all_colnames_database(), input$selected_vr_name, input$selected_x_name)
      #
      #   #req(input$selected_vr_name, input$selected_x_name)
      #   download_counter_html(0)
      #   shinyjs::disable("download_button_html")
      #   runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_html")))
      #
      #
      #   # validate(
      #   #   need(!is.null(special_path_output_html_report()),   ''),
      #   #   errorClass = "ERROR"
      #   # )
      #
      #   check_file <- file.exists(special_path_output_html_report())
      #   print(check_file)
      #
      #   validate(
      #     need(check_file, 'HTML file does not exist!'),
      #     errorClass = "ERROR"
      #   )
      #
      #
      #   shinyjs::enable("download_button_html")
      #   runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_html")))
      #
      #
      #
      #
      #
      #   # #load_button_counter(load_button_counter() + 1)
      #   # if(download_counter_html() >= 1 && render_button_status()){
      #   #   runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_pdf")))
      #   #
      #   # }
      #
      #   return(TRUE)
      #
      # })

      output$download_button_html <- downloadHandler(
        filename = function() {
          basename(special_path_output_html_report())
        },
        content = function(file) {
          #file.copy(output_path_html(), file, overwrite = TRUE)
          file.copy(special_path_output_html_report(), file, overwrite = TRUE)
          download_counter_html(download_counter_html() + 1)
        }
      )

      #######################################################







      special_path_output_rmd_nuevo <- reactiveVal()
      special_path_output_html_nuevo <- reactiveVal()

      render_new_button_status  <- shiny::reactiveVal()
      render_new_button_counter <- shiny::reactiveVal()


      vector_models_up <- shiny::reactiveVal()

      observeEvent(input$action_up, {
        render_new_button_counter(render_new_button_counter()+1)
        #shinyjs::disable("render_report_button")
        #runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))
      })


      observeEvent(input$selected_model, {
        render_new_button_status(FALSE)
        render_new_button_counter(0)
        #shinyjs::disable("render_report_button")
        #runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))
      })
      output$htmlviewer_temporal3 <- renderText({

        req(control_01(), special_path_output_folder(), special_path_output_html_nuevo())
        req(render_new_button_counter(), render_new_button_counter() >= 1)

        # # # Definimos como un "alias" o un "bindeo".
        # A la carpeta temporal le damos como un "alias".
        # Esto es por que los HTML no pueden ser tomados de cualquier lado.
        my_path <- special_path_output_folder()
        addResourcePath(prefix = "output_temp_folder", directoryPath = my_path)

        # Armamos ahora un path con el "alias" como folder.
        my_file <- basename(special_path_output_html_nuevo())
        my_local_file <- file.path("output_temp_folder", my_file)

        # Levantamos el html
        armado_v <- paste('<div style="height: 100%; width: 100%; overflow: hidden;"><iframe style="height: 1000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")

        return(armado_v)
      })





      observeEvent(render_new_button_counter(), {

        # Todo lo anterior tiene que estar OK.
        req(control_01(), special_path_output_folder(), all_files_mod())
        req(render_new_button_counter(), render_new_button_counter() >= 1)

        req(special_path_output_folder())
        the_folder <- special_path_output_folder()
        the_paths <- list.files(the_folder, full.names = T)
        the_paths_rds <- grep("\\.rds$", the_paths, value = TRUE, ignore.case = TRUE)
        the_files_rds <- basename(the_paths_rds)
        the_names_rds <- tools::file_path_sans_ext(the_files_rds)

        for(k1 in 1:length(the_files_rds)){

          decime_ok <- paste0(the_names_rds[k1], " <- readRDS('", the_paths_rds[k1], "')", collapse = "")
          print(decime_ok)
          eval(parse(text = decime_ok))

        }

        # # # SPECIAL UP!
        vector_models_up(df_mlr_general$"text_order")

        ###
        aver <- list.files(special_path_output_folder())
        ###
        #print(paste0("my_files(): ", my_files()))

        # str_general <- "_the_name_rds_ <- readRDS(_the_path_rds_)"
        #       for(k1 in 1:length(the_files_rds)){
        #         str_new <- str_general
        #         str_new <- gsub("_the_name_rds_",the_names_rds[k1], str_new)
        #         str_new <- gsub("_the_path_rds_",the_paths_rds[k1], str_new)
        #         eval(parse(text = str_new))
        #
        #       }

        #print(the_files)
        #if(is.null(my_files)) return(NULL)
        #if(!exists(my_files[1])) return(NULL)
        #if(sum(my_files == "") > 0) return(NULL)
        #the_files
        # the_folder <- special_path_output_folder()
        # the_paths <- list.files(the_folder, full.names = T)
        # the_paths_rds <- grep("\\.rds$", the_paths, value = TRUE, ignore.case = TRUE)
        # the_files_rds <- basename(the_paths_rds)
        # the_names_rds <- tools::file_path_sans_ext(the_files_rds)
        # str_general <- "_the_name_rds_ <- readRDS(_the_path_rds_)"

        #print(the_paths_rds)
        #
        #       for(k1 in 1:length(the_files_rds)){
        #         str_new <- str_general
        #         str_new <- gsub("_the_name_rds_",the_names_rds[k1], str_new)
        #         str_new <- gsub("_the_path_rds_",the_paths_rds[k1], str_new)
        #         eval(parse(text = str_new))
        #
        #       }



        #print("AAA")
        # # # Execution time...

        #print("ZZZ")

        # # # Objetos de entorno
        # Estos objetos seran usados como si estuvieran
        # detallados dentro del archivo .Rmd.
        # Por ejemplo, tomamos la base de datos.
        render_env <- new.env()
        render_env$"selected_model" <- input$selected_model
        #render_env$"df_mlr_general" <- df_mlr_general
        str_general02 <- 'render_env$"_name_rds_" <- _name_rds_'

        for(k1 in 1:length(the_files_rds)){
          str_new02 <- str_general02
          str_new02 <- gsub("_name_rds_",the_names_rds[k1], str_new02)
          eval(parse(text = str_new02))

        }

        chance_name_path   <- file.path(special_path_output_folder(), all_files_mod())
        output_path_rmd3  <- chance_name_path
        output_path_rmd3  <- grep("_NUEVO", output_path_rmd3, value = TRUE)
        output_path_html3 <-gsub("\\.Rmd$", ".html", output_path_rmd3)

        special_path_output_html_nuevo(output_path_html3)

        rmarkdown::render(output_path_rmd3, rmarkdown::html_document(), output_file = output_path_html3, envir = render_env)



      })



      output$selected_pack <- renderUI({

        ns <- NS(id)
        # Theoretical number of pairs of different combinations: (k*(k-1))/2

        #amount_models <- vector_stock01["x_all_comb"]
        #
        cantidad_vars <- length(input$selected_x_name)
        cantidad_modelos <- 2^cantidad_vars
        cantidad_digitos <- nchar(cantidad_modelos)
        vector_opt <- 1:cantidad_modelos
        vector_opt <- formatC(x = vector_opt, width = cantidad_digitos, format = "d", flag = "0")
        vector_opt <- paste0("model_", vector_opt)


        div(
          fluidRow(
            column(3,
                   selectInput(inputId = ns("selected_model"), label = "Select a model",
                               choices = vector_opt)),

            column(1, actionButton(inputId = ns("action_up"), label = "LOAD!"))
          ),
          br(),br(),
          shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal3")))
        )


      })



      # aqui esta el archivo
      #

      #download_counter_html(0)
      #shinyjs::disable("download_button_html")
    }
  )
}



