library(leaflet)
library(sf)
library(mapdotoro)
library(DBI)
library(htmltools)
library(dplyr)
library(readr)

#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id shiny id
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_explore
#'
#' @importFrom leaflet leafletOutput renderLeaflet addProviderTiles colorQuantile
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
#' @importFrom dplyr left_join right_join
#' @importFrom readr read_csv2
#' @import sf
#' @import mapdotoro
#' @import DBI
mod_explore_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # UI elements
    fluidPage(
      fluidRow(
        column(
          width = 3,
          titlePanel("Metriques"),
          selectInput(ns("metric"), "Sélectionez une métrique :",
                      choices = c("Largeurs", "Pentes", "Occupation du sol"),
                      selected  = character(0)), # selectInput for dynamic radio buttons
          uiOutput(ns("radioButtonsUI")
          ) # uiOutput radios buttons metrics
        ), # column
        column(
          width = 6,
          titlePanel(""),
          leafletOutput(ns("exploremap")),
          textOutput("coords")
        ), # column
        column(
          width = 2,
          titlePanel("Filtre"),
          textOutput(ns("greeting"))
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
#'
# mod_explore_server <- function(id){
mod_explore_server <- function(input, output, session){

  ns <- session$ns

  bassin_hydro <- st_read(db_con(), layer = "bassin_hydrographique")
  network <- st_read(system.file("network.gpkg", package = "mapdoapp"))
  metrics <- read_csv2(system.file("metrics.csv", package = "mapdoapp"))
  landcover <- read_csv2(system.file("landcover.csv", package = "mapdoapp"))

  # mapping initialization
  output$exploremap <- renderLeaflet({
    leaflet() %>%
      setView(lng = 2.468697, lat = 46.603354, zoom = 5) %>%
      addTiles() %>%
      addPolygons(data = bassin_hydro,
                  layerId = ~cdbh,
                  smoothFactor = 2,
                  fillColor = "black",
                  fillOpacity = 0.01,
                  weight = 2,
                  color="black",
                  highlightOptions = highlightOptions(
                    fillColor = "#a8d1ff",
                    fillOpacity = 0.5),
                  label = ~htmlEscape(lbbh),
                  group = 'A'
                  )
  })

  # zoom on click
  observe(
    {  click = input$exploremap_shape_click
    if(is.null(click) || is.null(click$id))
      return()
    else
      region_hydro <- st_read(db_con(), layer = "region_hydrographique") %>%
        filter(cdbh==click$id)
      leafletProxy("exploremap") %>%
      setView(lng = click$lng , lat = click$lat, zoom = 6.5) %>%
      clearGroup('A') %>%
        addPolygons(data = region_hydro,
                    smoothFactor = 2,
                    fillColor = "black",
                    fillOpacity = 0.01,
                    weight = 2,
                    color="black",
                    highlightOptions = highlightOptions(
                      fillColor = "#a8d1ff",
                      fillOpacity = 0.5),
                    label = ~htmlEscape(lbregionhy),
                    group = 'B'
        )
    }
  ) # observe zoom on click

  # metrics radio buttons UI
  output$radioButtonsUI <- renderUI({
    selected_metric <- input$metric

    if (selected_metric == "Largeurs") {
      radioButtons(ns("dynamicRadio"), "Largeurs :",
                   choiceNames = c(
                     "Chenal actif",
                     "Corridor naturel",
                     "Corridor connecté",
                     "Fond de vallée"
                   ),
                   choiceValues = list("active_channel_width",
                                       "natural_corridor_width",
                                       "connected_corridor_width",
                                       "valley_bottom_width"),
                   selected = character(0))
    } else if (selected_metric == "Pentes") {
      radioButtons(ns("dynamicRadio"), "Pentes :",
                   choiceNames = c(
                     "Pente du talweg",
                     "Pente du fond de valée"
                   ),
                   choiceValues = list("talweg_slope",
                                       "floodplain_slope"),
                   selected = character(0)
      )
    } else if (selected_metric == "Occupation du sol") {
      radioButtons(ns("dynamicRadio"), "Occupation du sol :",
                   choiceNames = c(
                     "Surface en eau",
                     "Bancs sédimentaires",
                     "Espace naturel ouvert",
                     "Forêt",
                     "Prairie permanente",
                     "Culture",
                     "Périurbain",
                     "Urbain dense",
                     "Infrastructure de stransport"
                   ),
                   choiceValues = list("Water Channel",
                                       "Gravel Bars",
                                       "Natural Open",
                                       "Forest",
                                       "Grassland",
                                       "Crops",
                                       "Diffuse Urban",
                                       "Dense Urban",
                                       "Infrastructures"),
                   selected = character(0)
      )
    }
  })

  # dataset
  datamap <- reactive({
    if (input$metric == "Largeurs" | input$metric == "Pentes") {
      network %>%
        left_join(metrics, by = join_by("AXIS"=="AXIS", "M"=="measure"),
                  suffix = c("", ".metrics"), relationship="one-to-one")
    } else if (input$metric == "Occupation du sol") {
      landcover %>%
        filter(landcover == input$dynamicRadio) %>%
        right_join(network, by = join_by("AXIS"=="AXIS", "measure"=="M"),
                   suffix = c("", ".landcover"), relationship="one-to-one") %>%
        st_as_sf()
    }
  })

  # metrics data to display
  varsel <- reactive({
    if (input$metric == "Largeurs" | input$metric == "Pentes") {
      datamap()[[input$dynamicRadio]]
    } else if (input$metric == "Occupation du sol") {
      datamap()[["landcover_area"]]
    }
  })

  # mapping interactive change
  observeEvent(input$dynamicRadio, {
    if (input$metric == "Largeurs" | input$metric == "Pentes") {

      map_metric("exploremap", datamap(), varsel())

    } else if (input$metric == "Occupation du sol") {

      map_metric("exploremap", datamap(), varsel())
    }
  }) # ObserveEvent

}


## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
