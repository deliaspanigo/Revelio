
menu_data <- yaml::read_yaml("inst/yml_menu/menu_structure.yml")


# # # 01) UI - Selection for 'database'
module_super01_module_selection_ui <- function(id) {
  ns <- shiny::NS(id)

  div(
    titlePanel("Menús Dinámicos con Valores Internos"),
    fluidRow(
      column(4,
      radioButtons(inputId = ns("main_menu"),
                   label = "Seleccionar menú:",width = "120%",
                    choices = sapply(menu_data$menus, `[[`, "name"))),
      column(4, uiOutput(ns("submenu_ui"))),
      column(4,
        textOutput(ns("selection")),
        textOutput(ns("internal_value")) # Muestra el valor interno seleccionado
      )
    )
  )
}

# Var selection - Render - Show Results
module_super01_module_selection_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns



      output$submenu_ui <- renderUI({
        req(input$main_menu)
        selected_menu <- menu_data$menus[[which(sapply(menu_data$menus, `[[`, "name") == input$main_menu)]]
        vector_internal <- sapply(selected_menu$options, `[[`, "value")
        vector_external <- sapply(selected_menu$options, `[[`, "label")
        vector_opt <-   setNames(vector_internal,vector_external)

        radioButtons(ns("sub_menu"), "Seleccionar submenú:",width = "120%",
                    choices = vector_opt) # Los valores internos como valores, y los visibles como etiquetas
      })

      # Mostrar selección final
      output$selection <- renderText({
        req(input$sub_menu)
        paste("Seleccionaste la opción visible:",
              names(which(sapply(menu_data$menus, `[[`, "name") == input$main_menu)),
              "->", input$sub_menu)
      })

      # Mostrar el valor interno seleccionado
      output$internal_value <- renderText({
        req(input$sub_menu)
        paste("El valor interno seleccionado es:", input$sub_menu)
      })


      ####
      sui_fms <- reactive({
        req(input$sub_menu)

        input$sub_menu
      })

      return(sui_fms)









    }
  )
}


