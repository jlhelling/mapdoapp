#' characterisation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_characterisation_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' characterisation Server Functions
#'
#' @noRd
mod_characterisation_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
