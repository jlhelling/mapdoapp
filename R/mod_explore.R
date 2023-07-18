#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_explore_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' explore Server Functions
#'
#' @noRd 
mod_explore_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_explore_ui("explore_1")
    
## To be copied in the server
# mod_explore_server("explore_1")
