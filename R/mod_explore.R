#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id shiny id
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_explore
#'
#' @importFrom leaflet leafletOutput renderLeaflet addProviderTiles
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
#' @import mapdotoro
mod_explore_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # UI elements
    fluidPage(
        mainPanel (
          titlePanel("Simple Leaflet Map Example"),
          leafletOutput(ns("ui_exploremap"))
        ) # mainPanel
    ) # fluidPage
) # tagList
}

#' explore Server Functions
#'
#' @noRd
# mod_explore_server <- function(id){
mod_explore_server <- function(input, output, session){

    ns <- session$ns

    output$ui_exploremap <- renderLeaflet({
      leaflet() %>%
        setView(lng = -122.4194, lat = 37.7749, zoom = 12) %>%
        addTiles() %>%
        addMarkers(lng = -122.4194, lat = 37.7749, popup = "San Francisco, CA")
        # addProviderTiles(providers$Stamen.TonerLite,
        #                  options = providerTileOptions(noWrap = TRUE)
        # )
    }) # ui_exploremap

    # output$ui_metrics=renderUI({
    #   axis=rget_axis()
    #   available_choices=table_metrics %>%
    #     dplyr::filter(filename %in% get_available_info(axis),
    #                   include==1,
    #                   typelzk=="z") %>%
    #     dplyr::pull(varname)
    #   result=tagList(radioButtons(inputId=ns("zvar"),
    #                               label="variable",
    #                               choices=available_choices))
    #   result
    # })

  }
# }

## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
