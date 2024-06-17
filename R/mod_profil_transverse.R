#' profil_transverse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_profil_transverse_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' profil_transverse Server Functions
#'
#' @noRd
mod_profil_transverse_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_profil_transverse_ui("profil_transverse_1")

## To be copied in the server
# mod_profil_transverse_server("profil_transverse_1")
