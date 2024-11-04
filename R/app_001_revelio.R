# Instalar y cargar los paquetes necesarios
#if (!require("shiny")) install.packages("shiny")
#if (!require("shinydashboard")) install.packages("shinydashboard")


# https://ubco-biology.github.io/BIOL202/pdf-latex-is-not-found.html
#install.packages("tinytex", dependencies = TRUE)
#library("tinytex")
#tinytex::install_tinytex()

app_001_revelio <- function(){

  library(stringr)
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(openxlsx)
  library(shinyjs)
  library(markdown)
  library(rmarkdown)
  library(Revelio)
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
        shinydashboard::menuItem(text = "database", tabName = "tab02_database", icon = shiny::icon("th")),
        br(),
        shinydashboard::menuItem(text = "summary", tabName = "tab03_summary", icon = shiny::icon("th")),
        br()#,
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

        shinydashboard::tabItem(tabName = "tab01_intro",
                                module_pack001_intro_ui("space01_intro")), # Final - tab_clase99


        shinydashboard::tabItem(tabName = "tab02_database",
                module_pack002_import_s00_general_ui("space02_database_00"),
                uiOutput("salida_general")), #,
                #dataTableOutput("df_database")) # Final - tab_clase99



        shinydashboard::tabItem(tabName = "tab03_summary",
                module_pack003_summary_s03_QC_ui("space03_summary_03"))

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

    database <- reactiveVal(NULL)

    # Pack 001) Intro
    module_pack001_intro_server("space_intro")


    # Pack 002) Import database
    sui_data_source <- module_pack002_import_s00_general_server(id = "space02_database_00")

    # Reactive values to store the database from each module
    rv <- reactiveValues(
      xlsx = NULL,
      csv = NULL,
      revelio = NULL,
      r = NULL
    )

    observeEvent(sui_data_source(), {

      # Call each module server and store the reactive database in rv
      rv$xlsx    <- module_pack002_import_s01_xlsx_server(id = "space02_database_01", sui_data_source)
      rv$csv     <- module_pack002_import_s02_csv_server(id = "space02_database_02", sui_data_source)
      rv$revelio <- module_pack002_import_s03_revelio_server(id = "space02_database_03", sui_data_source)
      rv$r       <- module_pack002_import_s04_r_server(id = "space02_database_04", sui_data_source)

    })

    database <- reactive({
      req(sui_data_source())
      switch(sui_data_source(),
             "source_xlsx" = rv$xlsx(),
             "source_csv" = rv$csv(),
             "source_revelio" = rv$revelio(),
             "source_r" = rv$r())
    })

    output$salida_general <- renderUI({
      req(sui_data_source())

      switch(sui_data_source(),
             "source_xlsx" = module_pack002_import_s01_xlsx_ui("space02_database_01"),
             "source_csv" = module_pack002_import_s02_csv_ui("space02_database_02"),
             "source_revelio" = module_pack002_import_s03_revelio_ui("space02_database_03"),
             "source_r" = module_pack002_import_s04_r_ui("space02_database_04"))

    })
    output$df_database <- renderDataTable({
      req(database())
      database()
    })

    vector_all_colnames_database <- reactive({
      req(database())
      colnames(database())
    })


    # Pack 003) Summary
    module_pack003_summary_s03_QC_server(id = "space03_summary_03",
                                         vector_all_colnames_database, database)


  } #--- Fin server

  # Ejecutar la aplicación
  shiny::shinyApp(ui = ui, server = server, options = base::list(launch.browser = TRUE))


}



