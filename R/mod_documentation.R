#' documentation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_documentation_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    fluidPage(
      tags$a(
        href = "https://evs-gis.github.io/mapdowebsite/",
        icon("book"),
        "Documentation",
        target = "_blank"
      )
    )
  )
}

#' documentation Server Functions
#'
#' @noRd
mod_documentation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_documentation_ui("documentation_1")

## To be copied in the server
# mod_documentation_server("documentation_1")
