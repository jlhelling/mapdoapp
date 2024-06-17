#' fluvial_styles UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fluvial_styles_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' fluvial_styles Server Functions
#'
#' @noRd
mod_fluvial_styles_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_fluvial_styles_ui("fluvial_styles_1")

## To be copied in the server
# mod_fluvial_styles_server("fluvial_styles_1")
