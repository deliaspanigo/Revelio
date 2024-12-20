# Instalar y cargar los paquetes necesarios
#if (!require("shiny")) install.packages("shiny")
#if (!require("shinydashboard")) install.packages("shinydashboard")


# https://ubco-biology.github.io/BIOL202/pdf-latex-is-not-found.html
#install.packages("tinytex", dependencies = TRUE)
#library("tinytex")
#tinytex::install_tinytex()

app_001_revelio <- function(){

  library(colourpicker)
  library(DT)
  library(openxlsx)
  library(magrittr) # %>%
  library(markdown)
  library(plotly)
  library(quarto)
  library(rmarkdown)
  library(stringr)
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(tools)
  library(yaml)

  library(Revelio)

  #menu01_summary <- yaml::read_yaml("inst/yml_menu/menu01_summary.yml")

  #menu02_standard_proc <- yaml::read_yaml("inst/yml_menu/menu02_standard_proc.yml")

  #menu03_combinated_proc <- yaml::read_yaml("inst/yml_menu/menu03_combinated_proc.yml")
  #menu04_advanced_proc <- yaml::read_yaml("inst/yml_menu/menu04_advanced_proc.yml")

  menu01_summary         <- fn_import_special_yml("menu01_summary.yml")
  menu02_standard_proc   <- fn_import_special_yml("menu02_standard_proc.yml")
  menu03_combinated_proc <- fn_import_special_yml("menu03_combinated_proc.yml")
  menu04_advanced_proc   <- fn_import_special_yml("menu04_advanced_proc.yml")


  #print(menu_data)

  #ruta_css <- system.file("www", "estilos.css", package = "miPaquete")

  #includeCSS(ruta_css)

  # Definir la interfaz de usuario
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Revelio"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        "Revelio - 0.0.1",
        shiny::br(),
        shiny::br(),
        shinydashboard::menuItem(text = "Inicio", tabName = "tab01_intro", icon = shiny::icon("th")),
        br(),
        shinydashboard::menuItem(text = "Database", tabName = "tab02_database", icon = shiny::icon("th")),
        br(),
        shinydashboard::menuItem(text = "Summary", tabName = "tab04_summary", icon = shiny::icon("th")),
        br(),
        shinydashboard::menuItem(text = "Standard Processing", tabName = "tab03_statistics", icon = shiny::icon("th")),
        br(),
        shinydashboard::menuItem(text = "Combinated Processing", tabName = "tab03_statistics", icon = shiny::icon("th")),
        br(),
        shinydashboard::menuItem(text = "Advanced Processing", tabName = "tab03_statistics", icon = shiny::icon("th")),
        br(),
        shinydashboard::menuItem(text = "Summary", tabName = "tab03_summary", icon = shiny::icon("th"),
                                 shinydashboard::menuSubItem(text = "1Q", tabName = "tab03_sub_1Q"),
                                 shinydashboard::menuSubItem(text = "2Q", tabName = "tab03_sub_2Q"),
                                 shinydashboard::menuSubItem(text = "1C", tabName = "tab03_sub_1C"),
                                 shinydashboard::menuSubItem(text = "2C", tabName = "tab03_sub_2C"),
                                 shinydashboard::menuSubItem(text = "QC", tabName = "tab03_sub_QC")),
        br(),
        shinydashboard::menuItem(text = "MLR", tabName = "tab04_mlr", icon = shiny::icon("th")),
        br(),
        shinydashboard::menuItem(text = "GLM Gamma", tabName = "tab05_mlr", icon = shiny::icon("th")),
        br()#,#,
        #shinydashboard::menuItem(text = "Test 001 - Anova 1 Factor", tabName = "tab_test001_p01", icon = shiny::icon("th")),

        #shinydashboard::menuItem(text = "Test 002 - Kaplan-Maier", tabName = "tab_test002_p01", icon = shiny::icon("th")),

        #shinydashboard::menuItem(text = "Test 003 - Resumen QC", tabName = "tab_test003_p01", icon = shiny::icon("th"))


      )
    ),
    shinydashboard::dashboardBody(


      # Incluir CSS personalizado
      htmltools::includeCSS(system.file("www/style_app.css", package = "Revelio")),
      shinyjs::useShinyjs(),

      shinydashboard::tabItems(

        # Pack001 - Intro ------------------------------------------------------
        shinydashboard::tabItem(tabName = "tab01_intro",
                    module_pack001_intro_ui("space01_intro")), # Final - tab_clase99

        # Pack002 - Database----------------------------------------------------
        shinydashboard::tabItem(tabName = "tab02_database",
                module_pack002_import_s00_general_p01_ui("space02_database_00"),
                module_pack002_import_s00_general_p02_ui("space02_database_00")), #,
                #dataTableOutput("df_database")) # Final - tab_clase99

        # Master Pack Selector - 01 - Summary -------------------------------------------------
        shinydashboard::tabItem(tabName = "tab04_summary",
                                box(
                                  title = "Summary",
                                  status = "primary",
                                  id = "my_box01",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  #closable = TRUE,# Colapsado por defecto
                                  width = 12,
                                  module_super01_module_selection_ui("super01")),
                                  br(),
                                  uiOutput("selected_super01_summary_ui")
        ),
        # Master Pack Selector -------------------------------------------------
        shinydashboard::tabItem(tabName = "tab03_statistics",
                  box(
                      title = "Standard Processing",
                      status = "primary",
                      id = "my_box03",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      collapsed = FALSE,
                      #closable = TRUE,# Colapsado por defecto
                      width = 12,
                      module_super01_module_selection_ui("super")),
                      br(),
                      uiOutput("selected_fms_ui")
         ),



        # Pack002 - Database----------------------------------------------------
        shinydashboard::tabItem(tabName = "tab03_sub_1Q",
              module_pack003_summary_s01_1Q_ui("space03_summary_01_1Q")),
        shinydashboard::tabItem(tabName = "tab03_sub_2Q",
              module_pack003_summary_s02_2Q_ui("space03_summary_02_2Q")),
        shinydashboard::tabItem(tabName = "tab03_sub_1C",
              module_pack003_summary_s03_1C_ui("space03_summary_03_1C")),
        shinydashboard::tabItem(tabName = "tab03_sub_2C",
              module_pack003_summary_s04_2C_ui("space03_summary_04_2C")),
        shinydashboard::tabItem(tabName = "tab03_sub_QC",
              module_pack003_summary_s05_QC_ui("space03_summary_05_QC")),

        shinydashboard::tabItem(tabName = "tab04_mlr",
                                module_pack004_mlr_ui("space04_mlr")),
        shinydashboard::tabItem(tabName = "tab05_mlr",
                                module_pack005_mlr_ui("space05_mlr"))

        # shinydashboard::tabItem(tabName = "tab_test001_p01",
        #                         #   tableOutput("la_tabla"),
        #                         module_mlg_test001_p01_ui("space_test001_p01")),#, # Final - tab_clase01
        #
        # shinydashboard::tabItem(tabName = "tab_test002_p01",
        #                         #   tableOutput("la_tabla"),
        #                         module_mlg_test002_p01_ui("space_test002_p01")), # Final - tab_clase01
        #
        # shinydashboard::tabItem(tabName = "tab_test003_p01",
        #                         #   tableOutput("la_tabla"),
        #                         module_mlg_test003_p01_ui("space_test003_p01"))#, # Final - tab_clase01

      )
    )
  )


  # addResourcePath("tmpuser", getwd())

  # Definir la lógica del servidor
  server <- function(input, output) {

    # Pack 001) Intro
    module_pack001_intro_server("space_intro")

    #----------------------------------------------------------

    # Pack 002) Import Database
    sui_data_source <- module_pack002_import_s00_general_p01_server(id = "space02_database_00")
    output_list_database        <- module_pack002_import_s00_general_p02_server(id = "space02_database_00", sui_data_source)

    # observe({
    #   print(output_list_database())
    # })
    temporal_file_path <- reactive({output_list_database()$"temporal_file_path"})
    str_import_local <- reactive({output_list_database()$"str_import_local"})
    database <- reactive({output_list_database()$"database"})


    #----------------------------------------------------------

    # # # Internal Load - All modules
    #modules_list <- yaml::read_yaml("inst/yml_modules/yml_modules.yml")
    modules_list <-  fn_import_special_yml2("yml_modules.yml")

    # Módulos de fallback
    fallback_ui <- module_fm999_s999_ui
    fallback_server <- module_fm999_s999_server

    # Construir las listas de módulos con manejo de fallback
    module_ui_list <- lapply(modules_list$modules, function(x) {
      if (exists(x$ui)) {
        get(x$ui)  # Obtiene la función si existe
      } else {
        warning("No se encontró el módulo UI: ", x$ui, ". Usando módulo de fallback.")
        fallback_ui  # Usa el módulo predeterminado
      }
    })
    names(module_ui_list) <- names(modules_list$modules)

    module_server_list <- lapply(modules_list$modules, function(x) {
      if (exists(x$server)) {
        get(x$server)  # Obtiene la función si existe
      } else {
        warning("No se encontró el módulo Server: ", x$server, ". Usando módulo de fallback.")
        fallback_server  # Usa el módulo predeterminado
      }
    })
    names(module_server_list) <- names(modules_list$modules)

    # Agregar el módulo de fallback como un caso genérico
    module_ui_list$fallback <- fallback_ui
    module_server_list$fallback <- fallback_server

    #-----------------------------------------------------------------------------------------------------------

    sui_super01_summary <- module_super01_module_selection_server("super01", menu_yml = menu01_summary)

    observeEvent(sui_super01_summary(), {
      selected_module_server <- module_server_list[[sui_super01_summary()]]

      if (!is.null(selected_module_server)) {
        selected_module_server(id = sui_super01_summary(), output_list_database)
      } else {
        warning("Módulo no encontrado para: ", sui_super01_summary())
      }
    })

    output$selected_super01_summary_ui <- renderUI({
      selected_module_ui <- module_ui_list[[sui_super01_summary()]]

      if (!is.null(selected_module_ui)) {
        selected_module_ui(id = sui_super01_summary())
      } else {
        div("Módulo no encontrado para: ", sui_super01_summary())
      }
    })

    #------------------------------------------------------------------------------------------------------------

    sui_fms <- module_super01_module_selection_server("super", menu_yml = menu02_standard_proc)

    observeEvent(sui_fms(), {
      selected_module_server <- module_server_list[[sui_fms()]]

      if (!is.null(selected_module_server)) {
        selected_module_server(id = sui_fms(), database)
      } else {
        warning("Módulo no encontrado para: ", sui_fms())
      }
    })

    output$selected_fms_ui <- renderUI({
      selected_module_ui <- module_ui_list[[sui_fms()]]

      if (!is.null(selected_module_ui)) {
        selected_module_ui(id = sui_fms())
      } else {
        div("Módulo no encontrado para: ", sui_fms())
      }
    })

    # observeEvent(sui_fms(),{
    #
    #   lala <- "fm001_s001"
    #   mi_sentencia <- "module_MY_FMS_server(id = 'super2', vector_all_colnames_database, database)"
    #   mi_sentencia <- gsub(pattern = "MY_FMS", replacement = lala, x = mi_sentencia)
    #   eval(parse(text = mi_sentencia))
    #
    # })
    #
    #
    #
    # output$selected_fms_ui <- renderUI({
    #
    #   lala <- "fm001_s001"
    #   mi_sentencia <- "module_MY_FMS_ui(id = 'super2')"
    #   mi_sentencia <- gsub(pattern = "MY_FMS", replacement = lala, x = mi_sentencia)
    #   eval(parse(text = mi_sentencia))
    #
    #   #module_fm001_s001_ui("super2")
    # })

    #----------------------------------------------------------

    # Pack 003) Summary
    module_pack003_summary_s01_1Q_server(id = "space03_summary_01_1Q",
           vector_all_colnames_database, database)
    module_pack003_summary_s02_2Q_server(id = "space03_summary_02_2Q",
           vector_all_colnames_database, database)
    module_pack003_summary_s03_1C_server(id = "space03_summary_03_1C",
           vector_all_colnames_database, database)
    module_pack003_summary_s04_2C_server(id = "space03_summary_04_2C",
           vector_all_colnames_database, database)
    module_pack003_summary_s05_QC_server(id = "space03_summary_05_QC",
           vector_all_colnames_database, database)


    module_pack004_mlr_server(id = "space04_mlr",
                                         vector_all_colnames_database, database)

    module_pack005_mlr_server(id = "space05_mlr",
                              vector_all_colnames_database, database)

  } #--- Fin server

  # Ejecutar la aplicación
  shiny::shinyApp(ui = ui, server = server, options = base::list(launch.browser = TRUE))


}



