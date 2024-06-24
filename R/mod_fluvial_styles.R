#' fluvial_styles UI Function
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
mod_fluvial_styles_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      useShinyjs(),
      fluidRow(
        reactableOutput(ns("table"), width = "100%")
      )
    )
  )
}

#' fluvial_styles Server Functions
#'
#' @import shiny
#' @importFrom reactable renderReactable
#'
#' @noRd
mod_fluvial_styles_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$table <- renderReactable(
      create_table_fluvialstyles(params_classes())
    )
  })
}
