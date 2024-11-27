

# # # 01) UI - Selection for 'database'
module_fm001_s001_ui2 <- function(id){

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
    shiny::h1("General Linear Models - Fixed Effects - Anova 1 Way"),
    shiny::fluidRow(
      shiny::column(3, uiOutput(ns("box02_var_selector"))),
      shiny::column(9, uiOutput(ns("box05_colores")))
      ),
    shiny::fluidRow(
      shiny::column(12,
                    shiny::textOutput(ns("text_control_general")))),
    # Espacio para los selectores de color dinámicos

    # Mostrar los colores seleccionados
    uiOutput(ns("control_de_mision")),
    shiny::fluidRow(
      shiny::column(12,
                    shinycssloaders::withSpinner(uiOutput(ns("report_view"))),
                    br(), br(), br()
      )
    )
  )


}

module_fm001_s001_ui <- function(id){

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
    shiny::h1("General Linear Models (Fixed) - Anova 1 Way"),
    tabsetPanel(
      tabPanel("Variable selection",
               fluidRow(br()),
               fluidRow(
                 column(3,
               selectInput(inputId = ns("selected_vr_name"),
                           label = "Response Variable",
                           choices = NULL)),
                 column(3,
               selectInput(inputId = ns("selected_factor_name"),
                           label = "Factor",
                           choices = NULL)),
                 column(3,
                selectInput(inputId = ns("alpha_value"),
                            label = "Alpha value",
                            choices = NULL)
                )
                 ),
               fluidRow(
               column(9, p(shiny::textOutput(ns("text_control_general"))))
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
      tabPanel("Reports",
               h3("Contenido del Tab 3"),
               p("Este es el contenido del tercer tab."),
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
module_fm001_s001_server <- function(id, vector_all_colnames_database, database){

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
      })
      vector_opt <- standard_info_var_selection()$"vector_options"

      vector_alpha_external <- c("0.10 (10%)", "0.05 (5%)", "0.01 (1%)")
      vector_alpha_internal <- c("0.10", "0.05", "0.01")
      vector_alpha_opt <- setNames(vector_alpha_internal, vector_alpha_external)

      updateSelectInput(session = session,
                        inputId = "selected_vr_name",
                        choices = vector_opt,
                        selected = vector_opt[1])

      updateSelectInput(session = session,
                        inputId = "selected_factor_name",
                        choices = vector_opt,
                        selected = vector_opt[1])

      updateSelectInput(session  = session,
                        inputId  = "alpha_value",
                        choices  = vector_alpha_opt,
                        selected = vector_alpha_opt[2])

      control_01 <- reactive({

        validate(
          need(!is.null(input$selected_vr_name),   ''),
          need(!is.null(input$selected_factor_name),   ''),
          errorClass = "ERROR"
        )

        validate(
          need(input$selected_vr_name != "", 'Select a response variable from your dataset.'),
          need(input$selected_factor_name != "", 'Select a factor from your dataset.'),
          errorClass = "WARNING"
        )



        check_vr <- sum(vector_all_colnames_database() == input$selected_vr_name) == 1
        check_factor <- sum(vector_all_colnames_database() == input$selected_factor_name) == 1

        validate(
          need(check_vr,     'Error 001: The selected response variable does not belong to the database.'),
          need(check_factor, 'Error 002: The selected factor does not belong to the database.'),
          errorClass = "ERROR"
        )


        validate(
          need(input$selected_vr_name != input$selected_factor_name, 'The response variable and the factor must be different variables. Change your choice of variables.'),
          errorClass = "ERROR"
        )
        return(TRUE)

      })


      output$text_control_general <- renderText({
        req(control_01())
        ""
      })



      # # # Step 02 - Basic Details ---------------------------------------

      df_original_info <- reactive({
        req(control_01())

        my_level <- levels(as.factor(as.character(database()[,input$"selected_factor_name"])))
        my_number <- 1:length(my_level)
        my_color <- rainbow(max(my_number))

        df_output <- data.frame(
          "vector_orig_level" = my_level,
          "vector_orig_number" = my_number,
          "vector_orig_color" = my_color
        )
        return(df_output)
      })


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
      })

      output$selected_colors <- renderUI({
        req(control_01(), df_original_info())

        num_colors <- nrow(df_original_info())

        selected_colors <- lapply(1:num_colors, function(i) {
          color <- input[[paste0("color", i)]]
          tags$p(paste("Color", i, "seleccionado:"), tags$span(style = paste("color:", color), color))
        })
        do.call(tagList, selected_colors)
      })

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

      output$df_color_info <- renderTable({
        req(control_01(), df_new_info())
        df_new_info()
      })



      control_02 <- reactive({

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


      output$section04_download <- renderUI({

        fluidRow(
          column(1, downloadButton(outputId = ns('download_button_html'),
                                   label = "HTML", width = "100%", disabled = FALSE)),
          column(1, downloadButton(outputId = ns('download_button_zip'),
                                   label = "All (ZIP)", width = "100%", disabled = TRUE))
        )
      })

      render_button_status  <- shiny::reactiveVal(FALSE)
      render_button_counter <- shiny::reactiveVal(0)

      observeEvent(list(input$selected_vr_name, input$selected_factor_name,
                        df_original_info(), df_new_info()), {

        render_button_status(FALSE)
        render_button_counter(0)


      })


      observeEvent(input$render_report_button, {

        # Todo lo anterior tiene que estar OK.
        req(control_02())

        render_button_counter(render_button_counter() + 1)
      })


      observeEvent(control_02(),{

        if (control_02()) render_button_status(TRUE)

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

      # # # Step 04 - Reports ---------------------------------------

      # Special folder
      input_folder_master <- reactive({
        the_package_name <- "Revelio"
        selected_folder <- "fm001_s001"

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

      # Nombre correspondiente para los archivos modificados
      all_files_mod <- reactive({
        gsub("_master", "_mod", all_files_master())
      })

      # Nombre de los nuevos objetos
      all_files_new <- reactive({
        vector_rmd_mod <- grep("\\.Rmd$", all_files_mod(), value = TRUE)
        vector_html_new <- gsub("\\.Rmd$", ".html", vector_rmd_mod)
        vector_R_new <- grep("_CODE", gsub("\\.Rmd$", ".R", vector_rmd_mod), value = TRUE)
        vector_RData_new <- "R_objects_new.RData"
        c(vector_html_new, vector_R_new, vector_R_new, vector_RData_new)
      })

      special_path_output_folder <- reactiveVal()
      special_path_output_rmd_code <- reactiveVal()
      special_path_output_html_code <- reactiveVal()
      special_path_output_rmd_report <- reactiveVal()
      special_path_output_html_report <- reactiveVal()


      observeEvent(input$render_report_button, {
        req(control_03(), df_new_info())

        original_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        execution_time <- gsub("[[:punct:]]", "_", original_time)
        execution_time <- gsub(" ", "_", execution_time)

        # Usar el directorio temporal directamente
        output_folder_temp <- tempdir()

        # Verificar y eliminar archivos existentes en el directorio temporal
        if (dir.exists(output_folder_temp)) {
          files <- list.files(output_folder_temp, full.names = TRUE)
          if (length(files) > 0) {
            file.remove(files)
          }
        }

        special_path_output_folder(output_folder_temp)
        #dir.create(output_folder_temp, showWarnings = FALSE)

        original_files <- list.files(input_folder_master(), full.names = TRUE)
        file.copy(original_files, output_folder_temp, overwrite = TRUE)

        original_name_path <- file.path(output_folder_temp, all_files_master())
        chance_name_path <- file.path(output_folder_temp, all_files_mod())
        file.rename(original_name_path, chance_name_path)

        render_env <- new.env()
        render_env$database <- database()
        render_env$selected_vr_name <- input$selected_vr_name
        render_env$selected_factor_name <- input$selected_factor_name
        render_env$mis_colores <- df_new_info()$vector_new_color
        render_env$mis_categorias <- df_new_info()$vector_new_level

        output_path_rmd <- grep("_CODE", chance_name_path, value = TRUE)
        output_path_html <- gsub("\\.Rmd$", ".html", output_path_rmd)
        if (file.exists(output_path_html)) file.remove(output_path_html)
        special_path_output_html_code(output_path_html)
        rmarkdown::render(output_path_rmd, rmarkdown::html_document(), output_file = output_path_html, envir = render_env)

        output_path_rmd2 <- grep("_REPORT", chance_name_path, value = TRUE)
        output_path_html2 <- gsub("\\.Rmd$", ".html", output_path_rmd2)
        if (file.exists(output_path_html2)) file.remove(output_path_html2)
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


      output$htmlviewer_temporal <- renderText({

        req(control_03(), special_path_output_folder(), special_path_output_html_code(), render_button_status())

        # # # Definimos como un "alias" o un "bindeo".
        # A la carpeta temporal le damos como un "alias".
        # Esto es por que los HTML no pueden ser tomados de cualquier lado.
        print(special_path_output_folder())
        my_path <- special_path_output_folder()
        addResourcePath(prefix = "output_temp_folder", directoryPath = my_path)

        print(special_path_output_html_code())

        # Armamos ahora un path con el "alias" como folder.
        my_file <- basename(special_path_output_html_code())
        my_local_file <- file.path("output_temp_folder", my_file)

        # Levantamos el html
        armado_v <- paste('<div style="height: 100%; width: 100%; overflow: auto;"><iframe style="height: 2000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")
        #special_path_output_html_code(NULL)
        return(armado_v)
      })

      output$htmlviewer_temporal2 <- renderText({

        req(control_03(), special_path_output_folder(), special_path_output_html_report(), render_button_status())

        # # # Definimos como un "alias" o un "bindeo".
        # A la carpeta temporal le damos como un "alias".
        # Esto es por que los HTML no pueden ser tomados de cualquier lado.
        print(special_path_output_folder())
        my_path <- special_path_output_folder()
        addResourcePath(prefix = "output_temp_folder", directoryPath = my_path)

        print(special_path_output_html_report())
        # Armamos ahora un path con el "alias" como folder.
        my_file <- basename(special_path_output_html_report())
        my_local_file <- file.path("output_temp_folder", my_file)

        # Levantamos el html
        armado_v <- paste('<div style="height: 100%; width: 100%; overflow: auto;"><iframe style="height: 1000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")
        #special_path_output_html_report(NULL)
        return(armado_v)
      })


      output$report_view <- renderUI({
        req(control_01(), control_02(), control_03())

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




    }
  )
}



