#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_explore
#'
#' @importFrom leaflet leafletOutput renderLeaflet addProviderTiles
#' @importFrom shiny NS tagList
mod_explore_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # UI elements
    fluidPage(
        fluidRow(
          column(
            width = 6,
            leafletOutput(ns("exploremap"))
          )
        )
    )

  )
}

#' explore Server Functions
#'
#' @noRd
mod_explore_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$exploremap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        )
    })

  })
}

## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
