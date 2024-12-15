

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
    tabsetPanel(id = ns("tabset_step"),
      tabPanel(title = "Variable selection",
               value = 1,
               module_sp001_s001_ui_p01(id = ns(id))
      ),
      tabPanel(title = "Basic Details",
               value = 2,
               module_sp001_s001_ui_p02(id = ns(id)),
      ),
      tabPanel(title = "Report",
               value = 3,
               module_sp001_s001_ui_p03(id = ns(id)),
               module_sp001_s001_ui_p04(id = ns(id))
      )

    )
      # tabPanel(title = "Downloads",
      #          value = 4,
      #          h3("Contenido del Tab 4"),
      #          p("Este es el contenido del cuartto tab."),
      #          p("Acá ban los render y descargas, o solo descargas.")
      #
      # )

  )


}


# Var selection - Render - Show Results
module_sp001_s001_server <- function(id, vector_all_colnames_database, database){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      output_list_01 <- module_sp001_s001_server_p01(id, database)
      output_list_02 <- module_sp001_s001_server_p02(id, database, output_list_01)
      output_list_03 <- module_sp001_s001_server_p03(id, database, output_list_01, output_list_02)
      module_sp001_s001_server_p04(id, database, output_list_01, output_list_02, output_list_03)


     }
  )
}




#######################################################################################


module_sp001_s001_ui_p01 <- function(id){

  ns <- shiny::NS(id)


    div(
        uiOutput(ns("gate01_variable_selection")),
        p(textOutput(ns("text_control_01")))
      )



}



module_sp001_s001_server_p01 <- function(id, database){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns



      # # # Step 01 - Variable selection ---------------------------------------
      # Standard info
      standard_info_var_selection <- reactive({
        req(database())



        vector_colnames <- colnames(database())
        vector_pos <- 1:length(vector_colnames)
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
      }) %>% bindCache(database())



      output$gate01_variable_selection <- renderUI({

        req(standard_info_var_selection())
        vector_opt <- standard_info_var_selection()$vector_options

        vector_alpha_external <- c("0.10 (10%)", "0.05 (5%)", "0.01 (1%)")
        vector_alpha_internal <- c("0.10",       "0.05",      "0.01")
        vector_alpha_opt <- setNames(vector_alpha_internal, vector_alpha_external)

        div(
          fluidRow(br()),
          fluidRow(
            column(3,
                   selectInput(inputId = ns("selected_name_var01"),
                               label = "Factor",
                               choices = vector_opt,
                               selected = vector_opt[1])),
            column(3,
                   selectInput(inputId = ns("alpha_value"),
                               label = "Alpha value",
                               choices = vector_alpha_opt,
                               selected = vector_alpha_opt[1])
            )
          )
        )

      })


      control_01 <- reactive({


        validate(
          need(!is.null(input$selected_name_var01),   ''),
          errorClass = "ERROR"
        )

        validate(
          need(input$selected_name_var01 != "", 'Select a variable from your dataset.'),
          errorClass = "WARNING"
        )



        check_factor <- sum(colnames(database()) == input$selected_name_var01) == 1

        validate(
          need(check_factor, 'Error 002: The selected factor does not belong to the database.'),
          errorClass = "ERROR"
        )



        return(TRUE)

      }) %>% bindCache(input$"selected_name_var01")


      output$text_control_01 <- renderText({
        req(control_01())
        ""
      })


      output_list_01 <- reactive({
        req(control_01(), input$"selected_name_var01", input$"alpha_value")

        list("control_01" = control_01(),
             "selected_name_var01" = input$"selected_name_var01",
             "alpha_value" = input$"alpha_value")
    }
  )


      return(output_list_01)
    }
  )

}


#######################################################################################



module_sp001_s001_ui_p02 <- function(id){

  ns <- shiny::NS(id)


  div(
    uiOutput(ns("gate02_basic_details")),
    p(textOutput(ns("text_control_02")))
  )



}



module_sp001_s001_server_p02 <- function(id, database, output_list_01){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns


          control_01 <- reactive({output_list_01()$"control_01"})

          selected_var <- reactive({output_list_01()$"selected_name_var01"})

          # # # Step 02 - Basic Details ---------------------------------------

          # Expresión reactiva para obtener información original
          df_original_info <- reactive({
            req(control_01())

            my_level <- levels(as.factor(as.character(database()[,selected_var()])))
            my_number <- 1:length(my_level)
            my_color <- rainbow(max(my_number))

            df_output <- data.frame(
              "vector_orig_level" = my_level,
              "vector_orig_number" = my_number,
              "vector_orig_color" = my_color
            )

            #mega_step(2)
            return(df_output)
          }) %>% bindCache(database(), selected_var())



          output$"gate02_basic_details" <- renderUI({
                req(control_01())

            div(
              fluidRow(br()),
              p("By default, the categories are presented in alphanumeric order
                   and are assigned a color equidistant from the color wheel in sexagecimal format."),
              p("You can change the order of the categories and the assigned color."),
              br(),
              uiOutput(ns("order_and_color_pickers")),
              tableOutput(ns("df_color_info"))
            )

          })

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
              req(input[[paste0("color", i)]])

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


          # Control sobre las categorias elegidas y los colores
          control_02 <- reactive({

            #print(mega_step())
            req(control_01())

            # validate(
            #   need(mega_step() >= 2,   'Categories and colors must be selected 01.'),
            #   errorClass = "ERROR"
            # )

            validate(
              need(!is.null(df_original_info()),   'Categories and colors must be selected 01.'),
              errorClass = "WARNING"
            )



            validate(
              need(!is.null(df_new_info()),   'Categories and colors must be selected 02.'),
              errorClass = "WARNING"
            )
            # print(df_new_info())

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
          })



          output$text_control_02 <- renderText({
            req(control_01())
            req(control_02())
            ""
          })



          output_list_02 <- reactive({
            req(control_02(), df_new_info())

            list("control_02" = control_02(),
                 "df_new_info" = df_new_info())
          })

          return(output_list_02)

    }
  )

}


#######################################################################################





module_sp001_s001_ui_p03 <- function(id){

  ns <- shiny::NS(id)


  div(
    uiOutput(ns("gate03_report_button")),
    p(textOutput(ns("text_control_03")))
  )



}



module_sp001_s001_server_p03 <- function(id, database, output_list_01, output_list_02){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      # Output list 01
      control_01 <- reactive({output_list_01()$"control_01"})
      selected_var <- reactive({output_list_01()$"selected_name_var01"})

      # Output list 02
      control_02 <- reactive({output_list_02()$"control_02"})
      df_new_info <- reactive({output_list_02()$"df_new_info"})


      the_bag_info <- reactive({
        req(control_01(),control_02(), selected_var(), df_new_info())
        list(control_01(),control_02(), selected_var(), df_new_info())
      })



          # # # Step 03 - Report button ---------------------------------------


          output$"gate03_report_button" <- renderUI({
            req(control_02())

            div(
              h3("Clic to Render Report!"),
              br(),
              fluidRow(
                column(3, actionButton(ns("render_report_button"),
                                       "Render Report", width = "100%", disabled = TRUE)))
            )

          })






          # Default values
          render_button_status    <- shiny::reactiveVal(FALSE)
          render_button_counter   <- shiny::reactiveVal(0)
          check_reset_render_opts <- shiny::reactiveVal(FALSE)


          # Reset values if something change from previous inputs
          observeEvent(the_bag_info(), {
              req(the_bag_info())

              check_reset_render_opts(TRUE)

                            })


          observeEvent(check_reset_render_opts(), {

            if(check_reset_render_opts()){
              render_button_status(FALSE)
              render_button_counter(0)
              check_reset_render_opts(FALSE)
            }

            req(control_02())
            if(!check_reset_render_opts()){
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
              shinyjs::disable("render_report_button")
              runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))


            } else


              # Todo lo anterior tiene que estar OK.
              if(the_way02){
                shinyjs::enable("render_report_button")
                runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))

              } else

                #load_button_counter(load_button_counter() + 1)
                if(the_way03){
                  shinyjs::disable("render_report_button")
                  runjs(sprintf('$("#%s").css({"background-color": "green",  "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))
                }
          })


          control_03 <- reactive({

            req(control_01(), control_02())

            req(render_button_status(), render_button_counter()>0)

            #print(control_02())
            # validate(
            #   need(!check_reset_render_opts() && render_button_counter()==0,   'Categories and colors must be selected.'),
            #   errorClass = "WARNING"
            # )

            # validate(
            #   need(!is.null(control_02()), 'Falta elegir.'),
            #   errorClass = "WARNING"
            # )

            return(TRUE)
          })


          output$text_control_03 <- renderText({
            req(control_01())
            req(control_02())
            req(control_03())
            ""
          })





          output_list_03 <- reactive({
            req(control_03())

            list("control_03" = control_03())
          })

          return(output_list_03)

    }
  )

}


#######################################################################################







module_sp001_s001_ui_p04 <- function(id){

  ns <- shiny::NS(id)


  div(
    uiOutput(ns("section04_download")),
    tabsetPanel(id = ns("averaver"),
                selected = 1,
                tabPanel(title = "R Code and Outputs",
                         value = 1,
                         fluidRow(
                           column(12, shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal01"))))
                           )),
                tabPanel(title = "Report",
                         value = 2,
                         fluidRow(
                           column(12, shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal02"))))
                         )),
                tabPanel(title = "R Code",
                         value = 3,
                         fluidRow(
                           column(12, shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal03"))))
                         ))
                )
    )







}



module_sp001_s001_server_p04 <- function(id, database, output_list_01, output_list_02, output_list_03){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      # Output list 01
      control_01 <- reactive({output_list_01()$"control_01"})
      selected_var <- reactive({output_list_01()$"selected_name_var01"})

      # Output list 02
      control_02 <- reactive({output_list_02()$"control_02"})
      df_new_info <- reactive({output_list_02()$"df_new_info"})

      # Output list 03
      control_03 <- reactive({output_list_03()$"control_03"})
      render_button_status <- reactive({output_list_03()$"render_button_status"})






      # # # Step 04 - Download buttons ---------------------------------------

      output$section04_download <- renderUI({
        # req(control_01())
        # req(control_02())
        # req(control_03())

        div(
          fluidRow(
            column(12,
                   downloadButton(outputId = ns('download_button_output_file01'),
                                  label = "R Code and Outputs", width = "100%", disabled = TRUE),
                   actionButton(ns("open_html01"), "Abrir HTML en Nueva Ventana"),            # Botón para abrir el HTML
                   # Manejador de mensajes en el lado del cliente
                   tags$script(HTML(paste0("
              Shiny.addCustomMessageHandler('openHtml', function(htmlPath) {
                window.open(htmlPath, '_blank');
              });
            ")))

            )
          ),
          fluidRow(
            column(12,
                   downloadButton(outputId = ns('download_button_output_file02'),
                                  label = "Special Report", width = "100%", disabled = TRUE),
                   actionButton(ns("open_html02"), "Abrir HTML en Nueva Ventana"),            # Botón para abrir el HTML
                   # Manejador de mensajes en el lado del cliente
                   tags$script(HTML(paste0("
              Shiny.addCustomMessageHandler('openHtml', function(htmlPath) {
                window.open(htmlPath, '_blank');
              });
            ")))

            )
          ),
          fluidRow(
            column(12,
                   downloadButton(outputId = ns('download_button_output_file03'),
                                  label = "R Code", width = "100%", disabled = TRUE),
                   actionButton(ns("open_html03"), "Abrir HTML en Nueva Ventana"),            # Botón para abrir el HTML
                   # Manejador de mensajes en el lado del cliente
                   tags$script(HTML(paste0("
              Shiny.addCustomMessageHandler('openHtml', function(htmlPath) {
                window.open(htmlPath, '_blank');
              });
            ")))

            )
          )
        )
      })




      #     # # # Step 04 - Reports ---------------------------------------

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
      })


      # A partir del clic en "render button"
      step_chain <- reactiveVal(0)
      # 1) Hora de ejecucion
      # 2) Carpeta temporal nueva
      # 3) Copiamos los archivos originales a la carpeta temporal nueva
      # 4) Entorno de trabajo con objetos de R
      # 5) Path and files01
      # 6) Substituir elementos de los archivos qmd file01
      # 7) Renderizar archivo qmd file01
      # 8) Path and files 02
      # 9) Renderizar archivo qmd file02

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

        my_path <- special_path_output_folder()
        addResourcePath(prefix = "output_temp_folder", directoryPath = my_path)

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
        my_env <- list()
        my_env$the_time <- the_time()
        my_env$database <- database()
        #env$selected_name_var01 <- "mpg"
        my_env$str_selected_name_var01 <- paste0( "\"", selected_var(), "\"")
        my_env$str_vector_order_categories <-  paste0("c(", paste(shQuote(df_new_info()$vector_new_level), collapse = ", "), ")")
        my_env$str_vector_colors <- paste0("c(", paste(shQuote(df_new_info()$vector_new_color), collapse = ", "), ")")

        #my_env$mis_categorias <- df_new_info()$vector_new_level


        #print(my_env$str_selected_name_var01)
        #my_env$selected_name_var01 <- input$selected_name_var01
        #my_env$mis_colores <- df_new_info()$vector_new_color

        # Guardar el entorno de renderizado y change_name_path en los objetos reactivos
        render_env(my_env)

        step_chain(step_chain() + 1)

      })



      ###############################################################################################


      # Segunda parte: Manejar la generación de los informes usando render_env y change_name_path
      # 5) Path and files01
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



      # 6) Substituir elementos de los archivos qmd file01
      observeEvent(step_chain(), {
        req(step_chain() == 6, special_path_output_folder(),  render_env())

        print(step_chain())


        # Define la ruta del archivo
        file_path <- input_path01_rmd_code()

        # Lee el contenido del archivo
        file_content <- readLines(file_path)


        # Realiza el reemplazo (por ejemplo, reemplazar "antiguo" por "nuevo")
        file_content <- gsub("_str_selected_name_var01_",  render_env()$str_selected_name_var01, file_content)
        file_content <- gsub("_str_vector_order_categories_",  render_env()$str_vector_order_categories, file_content)
        file_content <- gsub("_str_vector_colors_",  render_env()$str_vector_colors, file_content)

        # Guarda el contenido modificado en el mismo archivo
        writeLines(file_content, file_path)

        step_chain(step_chain() + 1)


      })



      # 7) Renderizar archivo file01
      count_general01 <- reactiveVal(0)
      observeEvent(step_chain(), {
        req(step_chain() == 7, special_path_output_folder(),  render_env())

        print(step_chain())

        # Directorios
        count_general01(0)
        old_wd <- getwd()
        new_wd <- special_path_output_folder()

        # Crear el directorio si no existe
        if (!dir.exists(new_wd)) dir.create(new_wd, recursive = TRUE)

        # Cambiar al nuevo directorio y asegurarse de restaurar al salir
        setwd(new_wd)
        on.exit(setwd(old_wd))

        # Renderizar y manejar errores
        # print(render_env())


        tryCatch({
          quarto::quarto_render(
            input = input_file01_rmd_code(),
            output_format = "html",
            output_file = output_file01_html_code(),
            execute_params = render_env()
          )
          count_general01(1)  # Incrementar solo si tiene éxito
        }, error = function(e) {
          message("Error en el renderizado file01: ", e$message)
        })

        step_chain(step_chain() + 1)

      })





      download_counter_output_file01 <- reactiveVal(0)



      output$download_button_output_file01 <- downloadHandler(
        filename = function() {
          basename(output_path01_html_code())
        },
        content = function(file) {
          #file.copy(output_path_html(), file, overwrite = TRUE)
          file.copy(output_path01_html_code(), file, overwrite = TRUE)
          download_counter_output_file01(download_counter_output_file01() + 1)
        }
      )


      # Evento para abrir el archivo HTML en una nueva pestaña
      observeEvent(input$open_html01, {
        req(step_chain() >= 7)

        my_file <- output_file01_html_code()
        my_local_file <- file.path("output_temp_folder", my_file)
        session$sendCustomMessage(type = "openHtml", message = my_local_file)


      })





      observeEvent(list(count_general01(), download_counter_output_file01(), step_chain()),{

        req(step_chain() > 7)


        the_cons01 <- download_counter_output_file01()==0 && count_general01()==0
        the_cons02 <- download_counter_output_file01()>=1 && count_general01()==0
        the_cons03 <- download_counter_output_file01()==0 && count_general01()==1
        the_cons04 <- download_counter_output_file01()>=1 && count_general01()==1

        #print(paste0("download_counter_output_file01(): ", download_counter_output_file01()))
        #print(paste0("count_general01(): ", count_general01()))

        if(the_cons01 | the_cons02) {
          shinyjs::disable("download_button_output_file01")
          runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_output_file01")))
          print("HOLA1 o 2 - A")
        } else
          if(the_cons03){
            shinyjs::enable("download_button_output_file01")
            runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_output_file01")))
            print("HOLA3 - A")
          } else
            if(the_cons04){
              shinyjs::enable("download_button_output_file01")
              runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_output_file01")))
              print("HOLA4 - A")
            }
      })



      output$htmlviewer_temporal01 <- renderText({
        req(step_chain() >= 7)

        my_file <- output_file01_html_code()
        my_local_file <- file.path("output_temp_folder", my_file)

        # Levantamos el html
        armado_v <- paste('<div style="height: 100%; width: 100%; overflow: auto;"><iframe style="height: 2000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")
        armado_v

      })





      ###############################################################################################


      # 8) Path and files 02
      input_file02_rmd_code   <- reactiveVal("file02_master_REPORT.qmd")
      output_file02_html_code <- reactiveVal("file02_master_REPORT.html")
      input_path02_rmd_code   <- reactiveVal()
      output_path02_html_code <- reactiveVal()
      observeEvent(step_chain(), {
        req(step_chain() == 8, special_path_output_folder())
        print(step_chain())


        input_path02_rmd_code(file.path(special_path_output_folder(), input_file02_rmd_code()))
        output_path02_html_code(file.path(special_path_output_folder(), output_file02_html_code()))


        step_chain(step_chain() + 1)

      })


      # 9) Renderizar archivo file02
      count_general02 <- reactiveVal(0)
      observeEvent(step_chain(), {
        req(step_chain() == 9, special_path_output_folder(),  render_env())

        print(step_chain())

        # Directorios
        count_general02(0)
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
            input = input_file02_rmd_code(),
            output_format = "html",
            output_file = output_file02_html_code(),
            execute_params = render_env()
          )
          count_general02(1)  # Incrementar solo si tiene éxito
        }, error = function(e) {
          message("Error en el renderizado file02: ", e$message)
        })

        step_chain(step_chain() + 1)

      })


      download_counter_output_file02 <- reactiveVal(0)


      # Download its local files
      output$download_button_output_file02 <- downloadHandler(
        filename = function() {
          basename(output_path02_html_code())
        },
        content = function(file) {
          #file.copy(output_path_html(), file, overwrite = TRUE)
          file.copy(output_path02_html_code(), file, overwrite = TRUE)
          download_counter_output_file02(download_counter_output_file02() + 1)
        }
      )


      # Evento para abrir el archivo HTML en una nueva pestaña
      observeEvent(input$open_html02, {
        req(step_chain() >= 9)
        my_file <- output_file02_html_code()
        my_local_file <- file.path("output_temp_folder", my_file)
        session$sendCustomMessage(type = "openHtml", message = my_local_file)
      })





      observeEvent(list(count_general02(), download_counter_output_file02(), step_chain()),{

        req(step_chain() > 9)


        the_cons01 <- download_counter_output_file02()==0 && count_general02()==0
        the_cons02 <- download_counter_output_file02()>=1 && count_general02()==0
        the_cons03 <- download_counter_output_file02()==0 && count_general02()==1
        the_cons04 <- download_counter_output_file02()>=1 && count_general02()==1


        if(the_cons01 | the_cons02) {
          shinyjs::disable("download_button_output_file02")
          runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_output_file02")))
          print("HOLA1 o 2 - B")
        } else
          if(the_cons03){
            shinyjs::enable("download_button_output_file02")
            runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_output_file02")))
            print("HOLA3 - B")
          } else
            if(the_cons04){
              shinyjs::enable("download_button_output_file02")
              runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_output_file02")))
              print("HOLA4 - B")
            }
      })



      output$htmlviewer_temporal02 <- renderText({

        req(step_chain() >= 9)

        my_file <- output_file02_html_code()
        my_local_file <- file.path("output_temp_folder", my_file)

        # Levantamos el html
        armado_v <- paste('<div style="height: 100%; width: 100%; overflow: auto;"><iframe style="height: 2000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")
        armado_v
      })

      ###############################################################################################



      # 10) Path and files 03
      input_file03_rmd_code   <- reactiveVal("file03_master_R.qmd")
      output_file03_html_code <- reactiveVal("file03_master_R.html")
      input_path03_rmd_code   <- reactiveVal()
      output_path03_html_code <- reactiveVal()
      observeEvent(step_chain(), {
        req(step_chain() == 10, special_path_output_folder())
        print(step_chain())


        input_path03_rmd_code(file.path(special_path_output_folder(), input_file03_rmd_code()))
        output_path03_html_code(file.path(special_path_output_folder(), output_file03_html_code()))


        step_chain(step_chain() + 1)

      })


      # 11) Renderizar archivo file03
      count_general03 <- reactiveVal(0)
      observeEvent(step_chain(), {
        req(step_chain() == 11, special_path_output_folder(),  render_env())

        print(step_chain())

        ##############
        # Define la ruta del archivo
        file_path <- input_path03_rmd_code()
        file_path_special <- input_path01_rmd_code()

        # Lee el contenido del archivo
        codigo_extraido <- fn_extract_r_chunks(file_path_special, start_chunk = NULL, end_chunk = NULL)
        codigo_extraido <- paste(codigo_extraido, collapse = "\n")
        #print(codigo_extraido)

        # Realiza el reemplazo (por ejemplo, reemplazar "antiguo" por "nuevo")
        file_content <- readLines(file_path)
        file_content <- gsub("_all_code_",  codigo_extraido, file_content)

        # Guarda el contenido modificado en el mismo archivo
        writeLines(file_content, file_path)

        ####################################################


        # Directorios
        count_general03(0)
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
            input = input_file03_rmd_code(),
            output_format = "html",
            output_file = output_file03_html_code(),
            execute_params = render_env()
          )
          count_general03(1)  # Incrementar solo si tiene éxito
        }, error = function(e) {
          message("Error en el renderizado file03: ", e$message)
        })

        step_chain(step_chain() + 1)

      })


      download_counter_output_file03 <- reactiveVal(0)


      # Download its local files
      output$download_button_output_file03 <- downloadHandler(
        filename = function() {
          basename(output_path03_html_code())
        },
        content = function(file) {
          #file.copy(output_path_html(), file, overwrite = TRUE)
          file.copy(output_path03_html_code(), file, overwrite = TRUE)
          download_counter_output_file03(download_counter_output_file03() + 1)
        }
      )


      # Evento para abrir el archivo HTML en una nueva pestaña
      observeEvent(input$open_html03, {
        req(step_chain() >= 12)
        my_file <- output_file03_html_code()
        my_local_file <- file.path("output_temp_folder", my_file)
        session$sendCustomMessage(type = "openHtml", message = my_local_file)
      })





      observeEvent(list(count_general03(), download_counter_output_file03(), step_chain()),{

        req(step_chain() >= 12)


        the_cons01 <- download_counter_output_file03()==0 && count_general03()==0
        the_cons02 <- download_counter_output_file03()>=1 && count_general03()==0
        the_cons03 <- download_counter_output_file03()==0 && count_general03()==1
        the_cons04 <- download_counter_output_file03()>=1 && count_general03()==1


        if(the_cons01 | the_cons02) {
          shinyjs::disable("download_button_output_file03")
          runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_output_file03")))
          print("HOLA1 o 2 - B")
        } else
          if(the_cons03){
            shinyjs::enable("download_button_output_file03")
            runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_output_file03")))
            print("HOLA3 - B")
          } else
            if(the_cons04){
              shinyjs::enable("download_button_output_file03")
              runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_output_file03")))
              print("HOLA4 - B")
            }
      })



      output$htmlviewer_temporal03 <- renderText({

        req(step_chain() >= 12)

        my_file <- output_file03_html_code()
        my_local_file <- file.path("output_temp_folder", my_file)

        # Levantamos el html
        armado_v <- paste('<div style="height: 100%; width: 100%; overflow: auto;"><iframe style="height: 2000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")
        armado_v
      })

      ###############################################################################################




    }
  )

}


#######################################################################################
