#' metric_overview UI Function
#'
#' @description A shiny Module.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom reactable reactableOutput
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metric_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$style(HTML("
      .reactable-table {
        font-size: 15px;  /* Adjust this value to change the text size */
      }
    ")),
    fluidRow(
      div(class = "reactable-table",
          style = "margin-top: 10px;",
          textOutput(ns("placeholder_ui")),
          column(width = 6,
                 uiOutput(ns("description"))),
          column(width = 6,
                 uiOutput(ns("selectinput")))
      )),
    fluidRow(
      div(class = "reactable-table",
          reactableOutput(ns("table"), width = "100%")
      )
    )
  )
}

#' metric_overview Server Functions
#'
#' @import shiny
#' @importFrom reactable renderReactable
#'
#' @noRd
mod_metric_overview_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_val_local <- reactiveValues(
      characteristics_table = NULL,
      descriptionUI = NULL,
      placeholder_text = "Cliquez sur un axe hydrographique pour afficher la comparaison régionale des métriques. ",
      selectinputUI = NULL,
      unit = NULL
    )

    output$table <- renderReactable(
      r_val_local$characteristics_table
    )

    output$description <- renderUI(
      r_val_local$descriptionUI
    )

    # text placeholder
    output$placeholder_ui <- renderText({
      r_val_local$placeholder_text
    })

    output$selectinput <- renderUI(
      r_val_local$selectinputUI
    )

    observe({
      if (!is.null(r_val$axis_click)) {
        # remove placeholder text
        r_val_local$placeholder_text = NULL

        # create header
        r_val_local$descriptionUI = HTML("<p><strong>Comparaison de moyennes </strong></p>")

        # create metric-select input
        r_val_local$selectinputUI = selectInput(ns("select_unit"), label = NULL,
                                                choices = list("surface relative (%)", "surface absolute (ha)"))
      }
    })

    observeEvent(c(input$select_unit, r_val$network_region, r_val$dgo_axis, r_val$data_dgo_clicked), {

      check <- input$select_unit

      if (!is.null(check) & !is.null(r_val$network_region) & !is.null(r_val$dgo_axis)) {
        # create data for table
        data_df <- fct_table_create_table_df(region_sf = r_val$network_region,
                                             axis_sf = r_val$dgo_axis,
                                             dgo_sf = r_val$data_dgo_clicked)

        # create reactable
        if (check == "surface relative (%)") {
          r_val_local$characteristics_table <- fct_table_create_reactable(data_df, "%")
        } else {
          r_val_local$characteristics_table <- fct_table_create_reactable(data_df, "ha")
        }

      }
    })
  })
}
