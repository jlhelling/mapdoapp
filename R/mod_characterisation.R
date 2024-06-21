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

    observeEvent(r_val$data_dgo_clicked, {
      print("dgo_axis: ")
      print(r_val$dgo_axis)
      print("dgo: ")
      print(r_val$data_dgo_clicked)

      r_val_local$characteristics_table <-
        create_reactable_characteristics(
          region_network = r_val$dgo_axis,
          dgo = r_val$data_dgo_clicked
        )
    })
  })
}
