


# # # 01) UI - Selection for 'database'
module_pack002_import_s00_general_p01_ui <- function(id){
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




module_pack002_import_s00_general_p01_server <- function(id){

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
                           "04 - R examples" = "source_rdata"),
               selected = "source_rdata"

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

module_pack002_import_s00_general_p02_ui <- function(id){
  ns <- shiny::NS(id)



  div(uiOutput(ns("salida_general")),
      DTOutput(ns("df_database")))
}




module_pack002_import_s00_general_p02_server <- function(id, sui_data_source){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns


      database <- reactiveVal(NULL)


      # Pack 002) Import database

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
        rv$r       <- module_pack002_import_s04_rdata_server(id = "space02_database_04", sui_data_source)

      })

      database <- reactive({
        req(sui_data_source())
        switch(sui_data_source(),
               "source_xlsx" = rv$xlsx(),
               "source_csv" = rv$csv(),
               "source_revelio" = rv$revelio(),
               "source_rdata" = rv$r())
      })

      output$salida_general <- renderUI({
        req(sui_data_source())

        switch(sui_data_source(),
               "source_xlsx" = module_pack002_import_s01_xlsx_ui(ns("space02_database_01")),
               "source_csv" = module_pack002_import_s02_csv_ui(ns("space02_database_02")),
               "source_revelio" = module_pack002_import_s03_revelio_ui(ns("space02_database_03")),
               "source_rdata" = module_pack002_import_s04_rdata_ui(ns("space02_database_04")))

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
            # columnDefs = list(list(className = 'dt-left', targets = c(0,1,2))),
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
          rownames = TRUE
        )

      })

      return(database)
    }
  )
}


################################################################
