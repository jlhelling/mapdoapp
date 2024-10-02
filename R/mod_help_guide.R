#' help_guide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom cicerone use_cicerone
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_help_guide_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("help_btn"), "Guide d'aide"),
    # use_cicerone() # Load the cicerone dependencies
  )
}

#' help_guide Server Functions
#'
#' @noRd
mod_help_guide_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_help_guide_ui("help_guide_1")

## To be copied in the server
# mod_help_guide_server("help_guide_1")
