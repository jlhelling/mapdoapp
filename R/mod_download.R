#' download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_ui <- function(id) {
  ns <- NS(id)
  tagList(

    tags$head(
      tags$style(
        HTML("
          .form-group{margin-bottom: 10px}
          ")
      )
    ), # head





  )
}

#' download Server Functions
#'
#' @noRd
mod_download_server <- function(id, con, r_val, globals){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_download_ui("download_1")

## To be copied in the server
# mod_download_server("download_1" , con, r_val, globals)
