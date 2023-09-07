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
          uiOutput(ns("strahlerfilterUI")),
          uiOutput(ns("metricsfilterUI"))
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

  # map initialization
  output$exploremap <- renderLeaflet({
    map_init_bassins(bassins_data = get_bassins(), group = "A")
  })

  # clicked polygon data
  click_value <- reactive({
    input$exploremap_shape_click
  })

  # get regions data in clicked bassin
  region_hydro <- reactive({
      req(click_value()$group == "A")
      get_regions_in_bassin(selected_bassin_id = click_value()$id)
    })

  # map regions or selected region
  observeEvent(click_value(), {
    if (click_value()$group == "A"){
    # update map : zoom in clicked bassin, clear bassin data, display region in bassin
    leafletProxy("exploremap") %>%
    map_add_regions_in_bassin(bassin_click = click_value(),
                              regions_data = region_hydro(),
                              bassins_group = "A",
                              regions_group = "B")
    # update map : clear regions in selected bassin, display the region selected
    }else if (click_value()$group == "B"){
      # map region clicked
      leafletProxy("exploremap") %>%
        map_region_clicked(region_click = click_value(),
                           selected_region_feature = selected_region_feature(),
                           regions_group = "B",
                           selected_region_group = "C")
      }

  })

  # get only the region selected feature
  selected_region_feature <- reactive({
    req(click_value()$group == "B")
    get_region(region_click_id = click_value()$id)
  })

  # get network with metrics in region
  network_region_metrics <- reactive({
    req(click_value()$group == "B")
    get_network_region_with_metrics(selected_region_id = click_value()$id)
  })

  ### DYNAMIC UI

  # choose metric type
  output$metricUI <- renderUI({
    req(click_value()$group == "B")
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
                   choiceValues = list("water_channel",
                                       "gravel_bars",
                                       "natural_open",
                                       "forest",
                                       "grassland",
                                       "crops",
                                       "diffuse_urban",
                                       "dense_urban",
                                       "infrastructures"),
                   selected = character(0)
      )
    }
  })

  # filter UI
  output$strahlerfilterUI <- renderUI(
    {
      req(network_region_metrics())
      sliderInput(ns("strahler"),
                  label="Ordre de strahler",
                  min=min(isolate(network_region_metrics()$strahler)),
                  max=max(isolate(network_region_metrics()$strahler)),
                  value=c(min(isolate(network_region_metrics()$strahler)),
                          max(isolate(network_region_metrics()$strahler))),
                  step=1)
    })

  # dynamic filter on metric selected
  output$metricsfilterUI <- renderUI({
    req(input$dynamicRadio)
    if (is.null(input$dynamicRadio)==FALSE){
    sliderInput(ns("metricfilter"),
                input$dynamicRadio,
                min = isolate(round(min(network_region_metrics()[[input$dynamicRadio]], na.rm = TRUE), digits=1)),
                max = isolate(round(max(network_region_metrics()[[input$dynamicRadio]], na.rm = TRUE), digits=1)),
                value = c(
                  isolate(round(min(network_region_metrics()[[input$dynamicRadio]], na.rm = TRUE), digits=1)),
                  isolate(round(max(network_region_metrics()[[input$dynamicRadio]], na.rm = TRUE), digits=1))
                )
    )
    } else {
      return(NULL)
    }
  })

  ### DATA UPDATE

  # data with filter
  network_filter <- reactive({

    req(network_region_metrics())

    print(input$dynamicRadio)
    print(input$strahler)
    print(input$metricfilter)

    # no strahler no metric
    if(is.null(input$strahler) && is.null(input$metricfilter)){
      network_region_metrics()
    # yes strahler no metric
    }else if (is.null(input$strahler)==FALSE && is.null(input$metricfilter)){
      network_region_metrics() %>%
        filter(between(strahler, input$strahler[1], input$strahler[2]))
    # no strahler yes metric
    } else if (is.null(input$strahler) && is.null(input$metricfilter)==FALSE
               && is.null(input$dynamicRadio)==FALSE){
      network_region_metrics() %>%
        filter(between(!!sym(input$dynamicRadio),
                       input$metricfilter[1], input$metricfilter[2]))
    # yes strahler yes metric
    } else if (is.null(input$strahler)==FALSE && is.null(input$metricfilter)==FALSE
               && is.null(input$dynamicRadio)==FALSE){
      network_region_metrics() %>%
        filter(between(strahler, input$strahler[1], input$strahler[2])) %>%
        filter(between(!!sym(input$dynamicRadio),
                       input$metricfilter[1], input$metricfilter[2]))
    }
  })

  # metrics data to display
  varsel <- reactive({
    req(network_filter())
    if (is.null(network_filter())){
      return(NULL)
    } else {
      network_filter()[[input$dynamicRadio]]
    }
  })

  ### UPDATE MAP

  # update map with strahler filter
  observeEvent(network_filter(), {
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
        map_metric("exploremap", network_filter(), varsel())
    }
  }) # ObserveEvent
}


## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
