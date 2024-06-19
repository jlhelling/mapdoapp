#' profil_long UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_profil_long_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' profil_long Server Functions
#'
#' @noRd
mod_profil_long_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
