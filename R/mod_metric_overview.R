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
    fluidPage(
      useShinyjs(),
      fluidRow(
        style = "margin-top: 20px;",
        column(width = 6,
               uiOutput(ns("description"))),
        column(width = 6,
               uiOutput(ns("selectinput")))
      ),
      fluidRow(
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
      descriptionUI = HTML("<p> Cliquez sur un axe hydrographique pour afficher la comparaison des caractéristiques géomorphologiques. </p>"),
      selectinputUI = NULL,
      unit = NULL
    )

    output$table <- renderReactable(
      r_val_local$characteristics_table
    )

    output$description <- renderUI(
      r_val_local$descriptionUI
    )

    output$selectinput <- renderUI(
      r_val_local$selectinputUI
    )

    observe({
      if (!is.null(r_val$axis_click)) {
        # create header
        r_val_local$descriptionUI = HTML("<p><strong>Comparaison de moyennes </strong></p>")
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
