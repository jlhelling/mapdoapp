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
          uiOutput(ns("metricUI")),
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
          uiOutput(ns("filterUI"))
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
  network <- st_read(system.file("network_strahler.gpkg", package = "mapdoapp"))
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
                  group = "A"
                  )
  })

  # zoom on click
  observe(
    {  click = input$exploremap_shape_click
    if(is.null(click) || is.null(click$id)){
      return()
    }else if (click$group == "A"){
      region_hydro <- st_read(db_con(), layer = "region_hydrographique") %>%
        filter(cdbh==click$id)

      leafletProxy("exploremap") %>%
      setView(lng = click$lng , lat = click$lat, zoom = 6.5) %>%
      clearGroup("A") %>%
        addPolygons(data = region_hydro,
                    layerId = ~cdregionhy,
                    smoothFactor = 2,
                    fillColor = "black",
                    fillOpacity = 0.01,
                    weight = 2,
                    color="black",
                    highlightOptions = highlightOptions(
                      fillColor = "#a8d1ff",
                      fillOpacity = 0.5),
                    label = ~htmlEscape(lbregionhy),
                    group = "B"
        )
      }else if (click$group == "B"){

        ### IMMEDIATE UPDATE WHEN REGION CLICK

        clickedPolygon <- st_read(db_con(), layer = "region_hydrographique") %>%
          filter(cdregionhy == click$id)

        # filter by region
        network_region <- reactive({
          network %>%
            st_filter(clickedPolygon)
        })

        # display network without metric (add strahler dependent weight function)
        leafletProxy("exploremap") %>%
        setView(lng = click$lng , lat = click$lat, zoom = 7.5) %>%
          addPolygons(data = clickedPolygon,
                      smoothFactor = 2,
                      fillColor = "black",
                      fillOpacity = 0.01,
                      weight = 2,
                      color="black",
                      group = "C"
          ) %>%
          clearGroup("B")

        ### DYNAMIC UI

        # filter UI
        output$filterUI <- renderUI(
          {
            req(network_region())
            sliderInput(ns("strahler"),
                        label="Ordre de strahler",
                        min=min(network_region()$strahler), max=max(network_region()$strahler),
                        value=c(min(network_region()$strahler),max(network_region()$strahler)),
                        step=1)
          }
        )

        # choose metric type
        output$metricUI <- renderUI({
          selectInput(ns("metric"), "Sélectionez une métrique :",
                      choices = c("Largeurs", "Pentes", "Occupation du sol"),
                      selected  = "Largeurs") # selectInput for dynamic radio buttons
        })

        # metrics radio buttons UI
        output$radioButtonsUI <- renderUI({

          req(input$metric)

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

        ### DATA UPDATE

        # data with strahler filter
        network_filter <- reactive({
          req(network_region())
          if(is.null(input$strahler)){
            network_region()
          } else {
            network_region() %>%
              filter(between(strahler, input$strahler[1], input$strahler[2])) %>%
              st_as_sf()}
        }) # strahler filter

        # data with metric
        network_metrics <- reactive({
          if (is.null(input$metric)){
            return(NULL)
          } else if (input$metric == "Largeurs" || input$metric == "Pentes"
              && is.null(input$dynamicRadio)==FALSE) {
            network_filter() %>%
              left_join(metrics, by = join_by("AXIS"=="AXIS", "M"=="measure"),
                        suffix = c("", ".metrics"), relationship="one-to-one")
          } else if (input$metric == "Occupation du sol"
                     && is.null(input$dynamicRadio)==FALSE ) {
            landcover %>%
              filter(landcover == input$dynamicRadio) %>%
              right_join(network_filter(), by = join_by("AXIS"=="AXIS", "measure"=="M"),
                         suffix = c("", ".landcover"), relationship="one-to-one") %>%
              st_as_sf()
          }
        })

        # metrics data to display
        varsel <- reactive({
          req(network_metrics())
          if (is.null(network_metrics())){
            return(NULL)
          } else if (input$metric == "Largeurs" | input$metric == "Pentes") {
            network_metrics()[[input$dynamicRadio]]
          } else if (input$metric == "Occupation du sol") {
            network_metrics()[["landcover_area"]]
          }
        })

        ### UPDATE MAP

        # update map with strahler filter
        observeEvent(input$strahler, {
          if (is.null(input$strahler)) {
            return (NULL)
          } else if (is.null(input$dynamicRadio)) {
            leafletProxy("exploremap") %>%
              clearGroup("D") %>%
              addPolylines(data = network_filter(),
                           weight = 2,
                           color = "blue",
                           group = "D")

          } else {
            if (input$metric == "Largeurs" | input$metric == "Pentes") {

              map_metric("exploremap", network_metrics(), varsel())

            } else if (input$metric == "Occupation du sol") {

              map_metric("exploremap", network_metrics(), varsel())
            }

          }
        }) # ObserveEvent

        # Update map with dynamicRadio
        observeEvent(input$dynamicRadio, {
          if (input$metric == "Largeurs" | input$metric == "Pentes") {

            map_metric("exploremap", network_metrics(), varsel())

          } else if (input$metric == "Occupation du sol") {

            map_metric("exploremap", network_metrics(), varsel())
          }
        }) # ObserveEvent

      }
    }
  ) # observe zoom on click
}


## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
