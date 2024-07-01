#' classes_distribution UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_classes_distribution_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' classes_distribution Server Functions
#'
#' @noRd
mod_classes_distribution_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

