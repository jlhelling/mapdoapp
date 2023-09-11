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
          uiOutput(ns("percentage")),
          uiOutput(ns("radioButtonsUI")) # uiOutput radios buttons metrics
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

  ### DYNAMIC UI ####

  # choose metric type
  output$metricUI <- renderUI({
    req(click_value()$group == "B")
    selectInput(ns("metric"), "Sélectionez une métrique :",
                choices = names(metrics_choice()),
                selected  = "Largeurs") # selectInput for dynamic radio buttons
  })

  # metrics radio buttons UI
  output$radioButtonsUI <- renderUI({

    req(input$metric)

    selected_metric <- input$metric

    radioButtons(ns("dynamicRadio"), sprintf("%s :", selected_metric),
                 choiceNames = names(metrics_choice()[[selected_metric]]),
                 choiceValues = as.list(unname(metrics_choice()[[selected_metric]])),
                 selected = character(0))
  })

  output$percentage <- renderUI({
    req(input$metric == "Occupation du sol", input$dynamicRadio)

    selectInput(ns("percent"), "Surfaces :",
                choices = c("Hectares", "% du fond de vallée"))
  })

  # filter UI
  output$strahlerfilterUI <- renderUI(
    {
      req(network_region_metrics())
      sliderInput(ns("strahler"),
                  label="Ordre de strahler",
                  min=min(isolate(network_region_metrics()$strahler), na.rm = TRUE),
                  max=max(isolate(network_region_metrics()$strahler), na.rm = TRUE),
                  value=c(min(isolate(network_region_metrics()$strahler), na.rm = TRUE),
                          max(isolate(network_region_metrics()$strahler), na.rm = TRUE)),
                  step=1)
    })

  # dynamic filter on metric selected
  output$metricsfilterUI <- renderUI({
    req(input$dynamicRadio)
    sliderInput(ns("metricfilter"),
                label = names(unlist(metrics_choice()[[input$metric]]))[unlist(metrics_choice()[[input$metric]]) == input$dynamicRadio], # extract key from value
                min = isolate(round(min(network_region_metrics()[[input$dynamicRadio]][is.finite(network_region_metrics()[[input$dynamicRadio]])], na.rm = TRUE), digits=1)),
                max = isolate(round(max(network_region_metrics()[[input$dynamicRadio]][is.finite(network_region_metrics()[[input$dynamicRadio]])], na.rm = TRUE), digits=1)),
                value = c(
                  isolate(round(min(network_region_metrics()[[input$dynamicRadio]][is.finite(network_region_metrics()[[input$dynamicRadio]])], na.rm = TRUE), digits=1)),
                  isolate(round(max(network_region_metrics()[[input$dynamicRadio]][is.finite(network_region_metrics()[[input$dynamicRadio]])], na.rm = TRUE), digits=1))
                )
    )
  })

  #####

  ### DATA ####

  # clicked polygon data
  click_value <- reactive({
    input$exploremap_shape_click
  })

  # get regions data in clicked bassin
  region_hydro <- reactive({
    req(click_value()$group == "A")
    get_regions_in_bassin(selected_bassin_id = click_value()$id)
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

  # data with filter
  network_filter <- eventReactive(c(input$strahler, input$metricfilter), {

    # no strahler no metric
    if(is.null(input$strahler) && is.null(input$metricfilter)){
      data <- network_region_metrics()
      # yes strahler no metric
    }else if (is.null(input$strahler)==FALSE && is.null(input$metricfilter)){
      data <- network_region_metrics() %>%
        filter(!is.na(strahler), between(strahler, input$strahler[1], input$strahler[2]))
      # no strahler yes metric
    } else if (is.null(input$strahler) && is.null(input$metricfilter)==FALSE
               && is.null(input$dynamicRadio)==FALSE){
      data <- network_region_metrics() %>%
        filter(!is.na(!!sym(input$dynamicRadio)), between(!!sym(input$dynamicRadio),
                       input$metricfilter[1], input$metricfilter[2]))
      # yes strahler yes metric
    } else if (is.null(input$strahler)==FALSE && is.null(input$metricfilter)==FALSE
               && is.null(input$dynamicRadio)==FALSE){
      data <- network_region_metrics() %>%
        filter(!is.na(strahler), between(strahler, input$strahler[1], input$strahler[2])) %>%
        filter(!is.na(!!sym(input$dynamicRadio)), between(!!sym(input$dynamicRadio),
                       input$metricfilter[1], input$metricfilter[2]))
    }
    return(data)
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

  #####

  ### MAPPING ####

  # map initialization
  output$exploremap <- renderLeaflet({
    map_init_bassins(bassins_data = get_bassins(), group = "A")
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

  # map network
  observeEvent(network_filter(), {
    if (is.null(input$strahler)) {
      return (NULL)
    } else if (is.null(input$dynamicRadio)) {
      leafletProxy("exploremap") %>%
        map_network_no_metric(datamap = network_filter(), network_group = "D")

    } else {
        map_metric("exploremap", network_filter(), varsel(), network_group = "D")
    }
  }) # ObserveEvent

  #####

}


## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
