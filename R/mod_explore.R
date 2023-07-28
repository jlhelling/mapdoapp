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
      fluidRow(
        column(
          width = 2,
          titlePanel("Metrics")
        ), # column
        column(
          width = 6,
          titlePanel("Simple Leaflet Map Example"),
          leafletOutput(ns("ui_exploremap"))
        ), # column
        column(
          width = 2,
          titlePanel("Filter")
        ) # column
      ), # fluidRow
      fluidRow(
        tabsetPanel(
          tabPanel("Profil en long"
          ), # tabPanel
          tabPanel("Profil en travers"
          ) # tabPanel
        )# tabsetPanel
      )# fluidRow
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
        # set leaflet CRS https://rstudio.github.io/leaflet/projections.html OR change data to 4326
        setView(lng = -122.4194, lat = 37.7749, zoom = 12) %>%
        addTiles()
        # addPolylines(data = network_metrics_data, color = "blue", weight = 1)
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
