#' metric_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metric_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' metric_analysis Server Functions
#'
#' @noRd
mod_metric_analysis_server <- function(id, r_vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_metric_analysis_ui("metric_analysis_1")

## To be copied in the server
# mod_metric_analysis_server("metric_analysis_1")
