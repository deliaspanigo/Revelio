# # # 01) UI - Selection for 'database'
module_pack003_summary_s03_1C_ui <- function(id) {
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
    shiny::h1("Summary - 1C"),
    shiny::fluidRow(
      shiny::column(3, uiOutput(ns("var_selector01"))),
      shiny::column(3, uiOutput(ns("color_picker"))),
      shiny::column(6, uiOutput(ns("box03_control_de_mision")))
    ),
    br(),
    shiny::textOutput(ns("text_control_01")),
    uiOutput(ns("box04_report"))
  )
}

# Var selection - Render - Show Results
module_pack003_summary_s03_1C_server <- function(id, vector_all_colnames_database, database) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      standard_info_var_selection <- reactive({
        req(vector_all_colnames_database())

        vector_colnames <- vector_all_colnames_database()
        vector_pos <- 1:length(vector_all_colnames_database())
        vector_letters <- openxlsx::int2col(vector_pos)

        max_digits <- max(nchar(vector_pos))
        max_digits <- max(max_digits, 2)
        vector_order <- sprintf(paste0("%0", max_digits, "d"), vector_pos)

        vector_names <- paste0(vector_order, " - ", vector_letters, " - ", vector_colnames)
        vector_options <- vector_colnames
        names(vector_options) <- vector_names
        vector_options <- c("Selecciona una..." = "", vector_options)

        output_list <- list(
          vector_pos = vector_pos,
          vector_letters = vector_letters,
          vector_colnames = vector_colnames,
          vector_order = vector_order,
          vector_options = vector_options
        )

        return(output_list)
      })

      output$var_selector01 <- shiny::renderUI({
        req(standard_info_var_selection())

        vector_options <- standard_info_var_selection()$vector_options

        div(
          shiny::selectInput(inputId = ns("sui_var_name"), label = "Variable selection",
                             choices = vector_options, selected = vector_options[1])
        )
      })

      sui_var_name <- reactive({
        req(input$sui_var_name)
        input$sui_var_name
      })

      output$color_picker <- renderUI({


        generar_color_aleatorio <- function() {
          caracteres <- c(0:9, LETTERS[1:6])
          color <- paste0("#", paste0(sample(caracteres, 6, replace = TRUE), collapse = ""))
          return(color)
        }

        color_aleatorio <- generar_color_aleatorio()

        div(
          colourpicker::colourInput(inputId = ns("sui_color"), label = "Color", value = color_aleatorio)
        )
      })

      sui_color <- reactive({
        req(input$sui_color)
        input$sui_color
      })

      control_01 <- reactive({
        validate(
          need(input$sui_var_name != "", 'Select a variable from your database.'),
          errorClass = "WARNING"
        )

        check_vr <- sum(vector_all_colnames_database() == input$sui_var_name) == 1

        validate(
          need(check_vr, 'Error 006: Problems with the selected variable. Reupload the file.'),
          errorClass = "ERROR"
        )

        validate(
          need(input$sui_color != "", 'Select a color.'),
          errorClass = "WARNING"
        )

        return(TRUE)
      })

      output$text_control_01 <- renderText({
        req(control_01())
        ""
      })

      output$box03_control_de_mision <- renderUI({


        div(shinydashboard::box(
          title = "03 - Mission Control",
          status = "primary",
          id = ns("my_box03"),
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          width = 12,
          div(
            actionButton(ns("render_report_button"), "Render Report", width = "100%", disabled = T),
            downloadButton(outputId = ns('download_button_html'), label = "HTML", width = "100%", disabled = TRUE),
            downloadButton(outputId = ns('download_button_zip'), label = "All (ZIP)", width = "100%", disabled = TRUE)
          )
        ))
      })

      render_button_status <- shiny::reactiveVal(FALSE)
      render_button_counter <- shiny::reactiveVal(0)

      observeEvent(input$render_report_button, {
        req(control_01())
        render_button_counter(render_button_counter() + 1)
      })

      observeEvent(list(sui_var_name(), sui_color()), {
        render_button_counter(0)
        if (is.null(control_01())) {
          render_button_status(FALSE)
        } else if (!is.null(control_01())) {
          render_button_status(TRUE)
        } else {
          render_button_status(FALSE)
        }
      })

      pos_check <- reactive({
        req(render_button_counter(), render_button_status())
        mi_pos <- 0

        if (render_button_counter() == 0 && !render_button_status()) {
          mi_pos <- 1
        } else if (render_button_counter() == 0 && render_button_status()) {
          mi_pos <- 2
        } else if (render_button_counter() >= 1 && render_button_status()) {
          mi_pos <- 3
        }

        return(mi_pos)
      })

      observeEvent(pos_check(), {
        button_id <- ns("render_report_button")

        if (pos_check() == 2) {
          shinyjs::disable("render_report_button")
          runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', button_id))
        }

        if (pos_check() == 2) {
          shinyjs::enable("render_report_button")
          runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', button_id))
        }

        if (pos_check() == 3) {
          shinyjs::disable("render_report_button")
          runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', button_id))
        }
      })


      #--------------------------------------------------------------

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
        armado_v <- paste('<div style="height: 100%; width: 100%; overflow: auto;"><iframe style="height: 2000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")

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
        armado_v <- paste('<div style="height: 100%; width: 100%; overflow: auto;"><iframe style="height: 1000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")

        return(armado_v)
      })

      output$box04_report <- renderUI({
        req(render_button_status(), render_button_counter() > 0)

        ns <- NS(id)


        div(
          tabsetPanel(
            selected = 1,
            tabPanel(title = "Código original", value = 1,
                     fluidRow(
                       column(12,
                              shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal"))))
                     )
            ),
            tabPanel(title = "Resumen", value = 2,
                     fluidRow(
                       column(12,
                              shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal2"))))
                     )
            )
          )
        )

      })


      ####################################################









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
              selected_folder <- "pack003_summary_03_1c"

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
              new_sub_folder <- paste0("Rsience_FOLDER_", execution_time)
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
              render_env$"sui_var_name" <- input$"sui_var_name"
              render_env$"sui_color" <- sui_color()

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

            download_counter_html <- reactiveVal()

            observeEvent(special_path_output_html_report(),{

              shinyjs::enable("download_button_html")
              runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_html")))

            })



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

      # aqui esta el archivo
      #

      #download_counter_html(0)
      #shinyjs::disable("download_button_html")




    }
  )
}


