

module_sp001_s001_ui <- function(id){

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
      ")),
    tags$style(HTML("
        .nav-tabs {
          width: 100%;
          display: flex;
          justify-content: space-around;
        }
        .nav-tabs > li {
          flex: 1;
          text-align: center;
        }
        .nav-tabs > li > a {
          display: block;
          padding: 15px;
          font-size: 18px;
          font-weight: bold;
          color: white;
          background-color: #007bff;
          border-radius: 5px;
          margin: 5px;
          transition: background-color 0.3s;
        }
        .nav-tabs > li > a:hover {
          background-color: #0056b3;
        }
        .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
          color: white;
          background-color: #0056b3;
        }
        .tab-content {
          min-height: 1000px; /* Ajusta esta altura según sea necesario */
        }
      "))
    ),
    shiny::h1("Summary - 1Q - Standard"),
    tabsetPanel(
      tabPanel("Variable selection",
               fluidRow(br()),
               fluidRow(
                 # column(3,
                 #        selectInput(inputId = ns("selected_name_var01"),
                 #                    label = "Response Variable",
                 #                    choices = NULL)),
                 column(3,
                        selectInput(inputId = ns("selected_name_var01"),
                                    label = "Factor",
                                    choices = NULL)),
                 column(3,
                        selectInput(inputId = ns("alpha_value"),
                                    label = "Alpha value",
                                    choices = NULL)
                 )
               ),
               fluidRow(
                 column(9, p(shiny::textOutput(ns("text_control_01"))))
               )
      ),
      tabPanel("Basic Details",
               fluidRow(br()),
               p("By default, the categories are presented in alphanumeric order
               and are assigned a color equidistant from the color wheel in sexagecimal format."),
               p("You can change the order of the categories and the assigned color."),
               br(),
               uiOutput(ns("order_and_color_pickers")),
               #uiOutput(ns("selected_colors")),
               tableOutput(ns("df_color_info")),
               p(shiny::textOutput(ns("text_control_02")))

      ),
      tabPanel("Report",
               h3("Clic to Render Report!"),
               br(),
               #p("Este es el contenido del tercer tab."),
               uiOutput(ns("control_de_mision")),
               uiOutput(ns("section04_download")),
               p(shiny::textOutput(ns("text_control_03"))),
               shinycssloaders::withSpinner(uiOutput(ns("report_view")))
      ),
      tabPanel("Downloads",
               h3("Contenido del Tab 4"),
               p("Este es el contenido del cuartto tab."),
               p("Acá ban los render y descargas, o solo descargas.")

      )
    )
  )


}


# Var selection - Render - Show Results
module_sp001_s001_server <- function(id, vector_all_colnames_database, database){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      # # # Step 01 - Variable selection ---------------------------------------
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
        vector_options <- c("Select one..." = "", vector_options)


        output_list <- list()
        output_list$"vector_pos"      <- vector_pos
        output_list$"vector_letters"  <- vector_letters
        output_list$"vector_letters"  <- vector_letters
        output_list$"vector_colnames" <- vector_colnames
        output_list$"vector_order"    <- vector_order
        output_list$"vector_options"  <- vector_options

        return(output_list)
      }) %>% bindCache(vector_all_colnames_database())

      # observe({
      #   req(standard_info_var_selection())
      #   vector_opt <- standard_info_var_selection()$vector_options
      #
      #   updateSelectInput(session = session,
      #                     inputId = "selected_name_var01",
      #                     choices = vector_opt,
      #                     selected = vector_opt[1])
      # })

      observe({
        req(standard_info_var_selection())
        vector_opt <- standard_info_var_selection()$vector_options

        updateSelectInput(session = session,
                          inputId = "selected_name_var01",
                          choices = vector_opt,
                          selected = vector_opt[1])
      })

      observe({
        vector_alpha_external <- c("0.10 (10%)", "0.05 (5%)", "0.01 (1%)")
        vector_alpha_internal <- c("0.10",       "0.05",      "0.01")
        vector_alpha_opt <- setNames(vector_alpha_internal, vector_alpha_external)

        updateSelectInput(session = session,
                          inputId = "alpha_value",
                          choices = vector_alpha_opt,
                          selected = vector_alpha_opt[2])
      })

      control_01 <- reactive({

        validate(
          #need(!is.null(input$selected_name_var01),   ''),
          need(!is.null(input$selected_name_var01),   ''),
          errorClass = "ERROR"
        )

        validate(
          #need(input$selected_name_var01 != "", 'Select a response variable from your dataset.'),
          need(input$selected_name_var01 != "", 'Select a factor from your dataset.'),
          errorClass = "WARNING"
        )



        #check_vr <- sum(vector_all_colnames_database() == input$selected_name_var01) == 1
        check_factor <- sum(vector_all_colnames_database() == input$selected_name_var01) == 1

        validate(
          #need(check_vr,     'Error 001: The selected response variable does not belong to the database.'),
          need(check_factor, 'Error 002: The selected factor does not belong to the database.'),
          errorClass = "ERROR"
        )


        # validate(
        #   need(input$selected_name_var01 != input$selected_name_var01, 'The response variable and the factor must be different variables. Change your choice of variables.'),
        #   errorClass = "ERROR"
        # )
        return(TRUE)

      }) %>% bindCache(input$"selected_name_var01")


      output$text_control_01 <- renderText({
        req(control_01())
        ""
      })



      # # # Step 02 - Basic Details ---------------------------------------

      # Expresión reactiva para obtener información original
      df_original_info <- reactive({
        req(control_01())

        my_level <- levels(as.factor(as.character(database()[,input$"selected_name_var01"])))
        my_number <- 1:length(my_level)
        my_color <- rainbow(max(my_number))

        df_output <- data.frame(
          "vector_orig_level" = my_level,
          "vector_orig_number" = my_number,
          "vector_orig_color" = my_color
        )
        return(df_output)
      }) %>% bindCache(database(), input$"selected_name_var01")

      # Renderizar UI para selectores de orden y color
      output$order_and_color_pickers <- renderUI({
        req(control_01(), df_original_info())

        vec_opt_original_colors <- df_original_info()$"vector_orig_color"
        vec_opt_original_categories <- df_original_info()$"vector_orig_number"
        names(vec_opt_original_categories) <-  df_original_info()$"vector_orig_level"
        value_amount_categories <- length(vec_opt_original_categories)

        color_inputs <- lapply(1:value_amount_categories, function(i) {
          fluidRow(
            column(4, shiny::selectInput(inputId = ns(paste0("pos", i)),
                                         label = paste0("Category order ", i, " - "),
                                         choices = vec_opt_original_categories,
                                         selected = vec_opt_original_categories[i])),
            column(4, colourpicker::colourInput(inputId = ns(paste0("color", i)),
                                                label = paste("Color", i), value = vec_opt_original_colors[i]))
          )
        })
        do.call(tagList, color_inputs)
      }) %>% bindCache(df_original_info())

      # Renderizar UI para mostrar colores seleccionados
      output$selected_colors <- renderUI({
        req(control_01(), df_original_info())

        num_colors <- nrow(df_original_info())

        selected_colors <- lapply(1:num_colors, function(i) {
          color <- input[[paste0("color", i)]]
          tags$p(paste("Color", i, "seleccionado:"), tags$span(style = paste("color:", color), color))
        })
        do.call(tagList, selected_colors)
      }) %>% bindCache(df_original_info())

      # Expresión reactiva para obtener nueva información
      df_new_info <- reactive({
        req(control_01(), df_original_info())

        # Original info
        vector_orig_level  <- df_original_info()$"vector_orig_level"
        vector_orig_number <- df_original_info()$"vector_orig_number"
        vector_orig_color  <- df_original_info()$"vector_orig_color"

        num_colors <- nrow(df_original_info())

        vector_new_level <- sapply(1:num_colors, function(i) {
          req(input[[paste0("pos", i)]])
          input[[paste0("pos", i)]]
        })
        vector_new_level <- as.numeric(as.character(vector_new_level))
        vector_new_level <- vector_orig_level[vector_new_level]

        vector_new_number <- match(vector_orig_level, vector_new_level)

        vector_new_color <- sapply(1:num_colors, function(i) {
          input[[paste0("color", i)]]
        })

        df_output <- data.frame(
          "vector_new_level" =  vector_new_level,
          "vector_new_number" = vector_new_number,
          "vector_new_color" =  vector_new_color
        )
        return(df_output)
      })

      # Renderizar tabla para mostrar nueva información
      output$df_color_info <- renderTable({
        req(control_01(), df_new_info())
        df_new_info()
      }) %>% bindCache(df_new_info())



      control_02 <- reactive({

        req(control_01())

        amount_ok_01 <- max(table(df_new_info()$"vector_new_level")) == 1
        validate(
          need(amount_ok_01,   'ERROR 003: Each category must be detailed only once.'),
          errorClass = "ERROR"
        )

        amount_ok_02 <- all(df_new_info()$"vector_new_level" %in% df_original_info()$"vector_orig_level")
        validate(
          need(amount_ok_02,   'ERROR 004: Some of the categories do not belong to the database.'),
          errorClass = "ERROR"
        )

        return(TRUE)
      }) %>% bindCache(df_new_info())


      output$text_control_02 <- renderText({
        req(control_01())
        req(control_02())
        ""
      })


      # # # Step 03 - Report button ---------------------------------------

      output$control_de_mision <- renderUI({
        #req(control_01(), control_02())
        ns <- shiny::NS(id)


        div(
          fluidRow(
            column(3, actionButton(ns("render_report_button"),
                                   "Render Report", width = "100%", disabled = TRUE)))


        )


      })

      # Default values
      render_button_status  <- shiny::reactiveVal(FALSE)
      render_button_counter <- shiny::reactiveVal(0)
      check_reset_render_opts <- shiny::reactiveVal(TRUE)


      # Reset values if something change from previous inputs
      observeEvent(list(input$selected_name_var01, input$selected_name_var01,
                        df_original_info(), df_new_info()), {

                          check_reset_render_opts(TRUE)


                        })


      observeEvent(check_reset_render_opts(), {

        if(check_reset_render_opts()){
          render_button_status(FALSE)
          render_button_counter(0)
          check_reset_render_opts(FALSE)
        }

        if(!check_reset_render_opts())  if(control_02()){
          render_button_status(TRUE)
        }

      })


      observeEvent(input$render_report_button, {

        # Todo lo anterior tiene que estar OK.
        req(render_button_status())

        render_button_counter(render_button_counter() + 1)
      })





      observeEvent(list(input$render_report_button, render_button_counter(), render_button_status()),{

        the_way01 <- render_button_counter() == 0 && !render_button_status()
        the_way02 <- render_button_counter() == 0 && render_button_status()
        the_way03 <- render_button_counter() >= 1 && render_button_status()


        if(the_way01){
          runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))
          shinyjs::disable("render_report_button")

        } else


          # Todo lo anterior tiene que estar OK.
          if(the_way02){
            runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))
            shinyjs::enable("render_report_button")

          } else

            #load_button_counter(load_button_counter() + 1)
            if(the_way03){
              runjs(sprintf('$("#%s").css({"background-color": "green",  "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))
              #shinyjs::enable("render_report_button")
            }
      })

      control_03 <- reactive({

        req(render_button_status(), render_button_counter() > 0)

        return(TRUE)
      })


      output$text_control_03 <- renderText({
        req(control_01())
        req(control_02())
        req(control_03())
        ""
      })
      # # # Step 04 - Download buttons ---------------------------------------

      output$section04_download <- renderUI({

        fluidRow(
          column(12,
                 downloadButton(outputId = ns('download_button_output_file01'),
                                label = "R Code and Outputs", width = "100%", disabled = TRUE)#,
                 # downloadButton(outputId = ns('download_button_output_file02_code_HTML'),
                 #                label = "HTML - R Code", width = "100%", disabled = TRUE),
                 # downloadButton(outputId = ns('download_button_output_file03_outputs_HTML'),
                 #                label = "HTML - R Outputs", width = "100%", disabled = TRUE),
                 # downloadButton(outputId = ns('download_button_output_file04_report_HTML'),
                 #                label = "HTML - Report", width = "100%", disabled = TRUE)



                 ##                 downloadButton(outputId = ns('download_button_zip'),
                 #                                label = "All (ZIP)", width = "100%", disabled = TRUE)
          )
        )
      })






      # # # Step 04 - Reports ---------------------------------------

      # Special folder
      input_folder_master <- reactive({
        the_package_name <- "Revelio"
        selected_folder <- "sp001_s001"

        special_folder_package <- file.path("extdata", selected_folder)
        special_folder_local <- file.path("inst", "extdata", selected_folder)

        input_path_package <- base::system.file(package = the_package_name)
        input_folder_package <- file.path(input_path_package, special_folder_package)
        input_folder_local <- file.path(getwd(), special_folder_local)

        check_cantidad_files_local <- length(list.files(input_folder_local)) > 0
        check_cantidad_files_package <- length(list.files(input_folder_package)) > 0

        input_folder_master <- ifelse(check_cantidad_files_package, input_folder_package, input_folder_local)
        input_folder_master <- as.character(input_folder_master)
        input_folder_master
      })

      # Nombre de los archivos originales
      all_files_master <- reactive({
        req(input_folder_master())
        list.files(input_folder_master())
      })  %>% bindCache(df_new_info())


      # A partir del clic en "render button"
      step_chain <- reactiveVal(0)
      # 1) Hora de ejecucion
      # 2) Carpeta temporal nueva
      # 3) Copiamos los archivos originales a la carpeta temporal nueva
      # 4) Entorno de trabajo con objetos de R
      # 5) Path and files01
      # 6) Renderizar archivo file01

      # Crear objetos reactivos para render_env y change_name_path


      # Primera parte: Manejar la creación del directorio temporal y la copia de archivos
      observeEvent(input$render_report_button, {
        req(control_03(), df_new_info())
        step_chain(1)
        #print(step_chain())

      })

      # 1) Hora de ejecucion
      the_time <- reactiveVal()
      observeEvent(step_chain(), {

        req(step_chain() == 1)
        print(step_chain())

        # The time
        original_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        execution_time <- gsub("[[:punct:]]", "_", original_time)
        execution_time <- gsub(" ", "_", execution_time)
        the_time(execution_time)

        step_chain(step_chain() + 1)
      })


      # 2) Carpeta temporal nueva
      special_path_output_folder <- reactiveVal()
      observeEvent(step_chain(), {
        req(step_chain() == 2,  the_time())
        print(step_chain())

        # Usar el directorio temporal directamente
        my_temp_folder <- tempdir()

        # Generar 3 números y 2 letras al azar
        random_numbers <- paste0(sample(0:9, 3, replace = TRUE), collapse = "")
        random_letters <- paste0(sample(LETTERS, 2, replace = TRUE), collapse = "")

        new_sub_folder <- paste0("Revelio_FOLDER_",  the_time(), random_numbers, random_letters)
        output_folder_temp <- file.path(my_temp_folder, new_sub_folder)

        # Si ya existe la carpeta, verificar y eliminar archivos existentes.
        # Si no existe la creamos.
        if (dir.exists(output_folder_temp)) {
          files <- list.files(output_folder_temp, full.names = TRUE)
          if (length(files) > 0) {
            file.remove(files)
          }
        } else {
          dir.create(output_folder_temp, showWarnings = FALSE)
        }

        # The new folder
        special_path_output_folder(output_folder_temp)

        step_chain(step_chain() + 1)


      })


      # 3) Copiamos los archivos originales a la carpeta temporal nueva
      observeEvent(step_chain(), {
        req(step_chain() == 3, special_path_output_folder())
        print(step_chain())



        original_files <- list.files(input_folder_master(), full.names = TRUE)
        file.copy(original_files, special_path_output_folder(), overwrite = TRUE)

        step_chain(step_chain() + 1)

      })


      # 4) Entorno de trabajo con objetos de R
      render_env <- reactiveVal()
      observeEvent(step_chain(), {
        req(step_chain() == 4)
        print(step_chain())


        # Crear el entorno de renderizado
        env <- new.env()
        env$database <- database()
        #env$selected_name_var01 <- "mpg"
        env$selected_name_var01 <- input$selected_name_var01
        env$mis_colores <- df_new_info()$vector_new_color
        env$mis_categorias <- df_new_info()$vector_new_level

        # Guardar el entorno de renderizado y change_name_path en los objetos reactivos
        render_env(env)

        step_chain(step_chain() + 1)

      })





      # Segunda parte: Manejar la generación de los informes usando render_env y change_name_path
      # 5) Path and files
      input_file01_rmd_code   <- reactiveVal("file01_master_CODE.qmd")
      output_file01_html_code <- reactiveVal("file01_master_CODE.html")
      input_path01_rmd_code   <- reactiveVal()
      output_path01_html_code <- reactiveVal()
      observeEvent(step_chain(), {
        req(step_chain() == 5, special_path_output_folder())
        print(step_chain())


        input_path01_rmd_code(file.path(special_path_output_folder(), input_file01_rmd_code()))
        output_path01_html_code(file.path(special_path_output_folder(), output_file01_html_code()))


        step_chain(step_chain() + 1)

      })

      # 6) Renderizar archivo file01
      count_general <- reactiveVal(0)
      status_file01 <- reactiveVal(FALSE)
      observeEvent(step_chain(), {
        req(step_chain() == 6, special_path_output_folder())

        print(step_chain())

        # Directorios
        count_general(0)
        old_wd <- getwd()
        new_wd <- special_path_output_folder()

        # Crear el directorio si no existe
        if (!dir.exists(new_wd)) dir.create(new_wd, recursive = TRUE)

        # Cambiar al nuevo directorio y asegurarse de restaurar al salir
        setwd(new_wd)
        on.exit(setwd(old_wd))

        # Renderizar y manejar errores
        tryCatch({
          quarto::quarto_render(
            input = input_file01_rmd_code(),
            output_format = "html",
            output_file = output_file01_html_code()
          )
          count_general(count_general() + 1)  # Incrementar solo si tiene éxito
        }, error = function(e) {
          message("Error en el renderizado file01: ", e$message)
        })

        count_general(1)
      })




      # # # Input Paths - Master Files



      download_counter_output_file01_code_HTML <- reactiveVal(0)



      output$download_button_output_file01 <- downloadHandler(
        filename = function() {
          basename(output_path01_html_code())
        },
        content = function(file) {
          #file.copy(output_path_html(), file, overwrite = TRUE)
          file.copy(output_path01_html_code(), file, overwrite = TRUE)
          download_counter_output_file01_code_HTML(download_counter_output_file01_code_HTML() + 1)
        }
      )







      observeEvent(list(count_general(), download_counter_output_file01_code_HTML(), step_chain()),{
        #req(download_counter_file01_rmd_s04_rcode())

        the_cons01 <- download_counter_output_file01_code_HTML()==0 && count_general()==0
        the_cons02 <- download_counter_output_file01_code_HTML()>=1 && count_general()==0
        the_cons03 <- download_counter_output_file01_code_HTML()==0 && count_general()==1
        the_cons04 <- download_counter_output_file01_code_HTML()>=1 && count_general()==1

        #print(the_cons)
        if(the_cons01 | the_cons02) {
          shinyjs::disable("download_button_output_file01")
          runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_output_file01")))
          print("HOLA1 o 2")
        } else
          if(the_cons03){
            shinyjs::enable("download_button_output_file01")
            runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_output_file01")))
            print("HOLA3")
          } else
            if(the_cons04){
              shinyjs::enable("download_button_output_file01")
              runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_output_file01")))
              print("HOLA4")
            }
      })







      ##################################################################
      observeEvent(check_reset_render_opts(), {

        if(check_reset_render_opts()){


          count_general(0)

        }


      })

      ##################################################################

      output$htmlviewer_temporal02 <- renderText({

        req(control_03(), special_path_output_folder(), output_file01_html_code(), render_button_status())

        # # # Definimos como un "alias" o un "bindeo".
        # A la carpeta temporal le damos como un "alias".
        # Esto es por que los HTML no pueden ser tomados de cualquier lado.
        #print(special_path_output_folder())
        my_path <- special_path_output_folder()
        addResourcePath(prefix = "output_temp_folder", directoryPath = my_path)

        #print(spo_file01_rmd_s02_html())

        # Armamos ahora un path con el "alias" como folder.
        my_file <- basename(output_file01_html_code())
        my_local_file <- file.path("output_temp_folder", my_file)

        # Levantamos el html
        armado_v <- paste('<div style="height: 100%; width: 100%; overflow: auto;"><iframe style="height: 2000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")
        #spo_file01_rmd_s02_html(NULL)
        return(armado_v)
      })




      output$report_view <- renderUI({
        req(control_01(), control_02(), control_03())

        ns <- NS(id)


        div(
          tabsetPanel(
            selected = 1,
            tabPanel(title = "R Code and Outputs", value = 1,
                     fluidRow(
                       column(12,
                              shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal02"))))
                     )
            )
          )
        )

      })




    }
  )
}



