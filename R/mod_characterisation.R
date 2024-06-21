#' characterisation UI Function
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
mod_characterisation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      useShinyjs(),
      fluidRow(
        style = "margin-top: 20px;",
        reactableOutput(ns("table"), width = "100%")
      )
    )
  )
}

#' characterisation Server Functions
#'
#' @importFrom reactable renderReactable
#'
#' @noRd
mod_characterisation_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_val_local <- reactiveValues(
      characteristics_table = NULL
    )

    output$table <- renderReactable(
      r_val_local$characteristics_table
    )

    observeEvent(c(r_val$network_region, r_val$dgo_axis, r_val$data_dgo_clicked), {

      if (!is.null(r_val$network_region) & !is.null(r_val$dgo_axis)) {

        # create data for table
        data_df <- fct_table_create_table_df(region_sf = r_val$network_region,
                                             axis_sf = r_val$dgo_axis,
                                             dgo_sf = r_val$data_dgo_clicked)

        # create reactable
        r_val_local$characteristics_table <- fct_table_create_reactable(data_df, "%")

      }
    })
  })
}
