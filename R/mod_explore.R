library(leaflet)
library(sf)
library(mapdotoro)
library(DBI)
library(htmltools)
library(dplyr)
library(readr)
library(plotly)
library(reactlog)

reactlog_enable()

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
#' @import plotly
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
          uiOutput(ns("areaUI")),
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
          uiOutput(ns("metricsfilterUI")),
          verbatimTextOutput(ns("printcheck"))
        ) # column
      ), # fluidRow
      fluidRow(
        tabsetPanel(
          tabPanel(title = "Profil en long",
                   plotlyOutput(ns("long_profile"))
          ), # tabPanel
          tabPanel(title = "Profil en travers"
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

  ### BASSIN ####

  # map initialization
  output$exploremap <- renderLeaflet({
    map_init_bassins(bassins_data = get_bassins(), group = "A")
  })

  # clicked polygon data
  click_value <- reactive({
    input$exploremap_shape_click
  })

  ### REGION ####


  # get regions data in clicked bassin
  region_hydro <- reactive({
    req(click_value()$group == "A")
    get_regions_in_bassin(selected_bassin_id = click_value()$id)
  })


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

  # switch unit area
  output$areaUI <- renderUI({
    req(input$metric == "Occupation du sol" || input$metric == "Continuité latérale")

    selectInput(ns("unit_area"), "Surfaces :",
                choices = c("Hectares", "% du fond de vallée"),
                selected = "Hectares")
  })

  # strahler filter UI
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

    if (is.null(input$unit_area) || input$unit_area == "Hectares"){
      metric_selected <- network_region_metrics()[[input$dynamicRadio]]

    }else if (input$unit_area == "% du fond de vallée"){
      metric_selected <- ifelse(is.na(network_region_metrics()[[input$dynamicRadio]]) |
                                  is.na(network_region_metrics()[["sum_area"]]) |
                                  network_region_metrics()[["sum_area"]] == 0, NA,
                                network_region_metrics()[[input$dynamicRadio]] /
                                  network_region_metrics()[["sum_area"]]*100)
    }

    sliderInput(ns("metricfilter"),
                label = names(unlist(metrics_choice()[[input$metric]]))[unlist(metrics_choice()[[input$metric]]) == input$dynamicRadio], # extract key from value
                min = isolate(round(min(metric_selected[is.finite(metric_selected)], na.rm = TRUE), digits=1)),
                max = isolate(round(max(metric_selected[is.finite(metric_selected)], na.rm = TRUE), digits=1)),
                value = c(
                  isolate(round(min(metric_selected[is.finite(metric_selected)], na.rm = TRUE), digits=1)),
                  isolate(round(max(metric_selected[is.finite(metric_selected)], na.rm = TRUE), digits=1))
                )
    )
  })

  ### DATA ####

  output$printcheck = renderPrint({
    tryCatch({
      network_region_metrics()
      print("exists")
    },
    shiny.silent.error = function(e) {
      print("doesn't exist")
    }
    )
  })



  # get only the region selected feature
  selected_region_feature <- reactive({
    req(click_value()$group == "B")
    get_region(region_click_id = click_value()$id)
  })

  selected_axis <- reactive({
    req(click_value()$group == "AXIS")
    get_network_axis(network_data = network_region_metrics(),
                     axis_id = click_value()$id)
  })

  # network_region_metrics
  values <- reactiveValues(network_metrics = NULL)

  observeEvent(click_value()$group, {
    if (!is.null(click_value()$group) && click_value()$group == "B") {
      values$network_metrics <- get_network_region_with_metrics(selected_region_id = click_value()$id)
    }
  })

  network_region_metrics <- reactive({
    # print(values$network_metrics)
    values$network_metrics
  })



  # get network axis in region
  network_region_axis <- reactive({
    req(click_value()$group == "B")
    get_axis(selected_region_id = click_value()$id)
  })

  # data with filter
  network_filter <- eventReactive(c(input$strahler, input$metricfilter), {

    data <- network_region_metrics()

    if (!is.null(input$unit_area) && input$unit_area == "% du fond de vallée"){
      data <- data %>%
        mutate(!!sym(input$dynamicRadio) := ifelse(is.na(network_region_metrics()[[input$dynamicRadio]]) |
                                                     is.na(network_region_metrics()[["sum_area"]]) |
                                                     network_region_metrics()[["sum_area"]] == 0, NA,
                                                   network_region_metrics()[[input$dynamicRadio]] /
                                                     network_region_metrics()[["sum_area"]]*100))
    }

    if (!is.null(input$strahler)) {
      data <- data %>%
        filter(!is.na(strahler), between(strahler, input$strahler[1], input$strahler[2]))
    }

    if (!is.null(input$metricfilter) && !is.null(input$dynamicRadio)){
      data <- data %>%
        filter(!is.na(!!sym(input$dynamicRadio)), between(!!sym(input$dynamicRadio), input$metricfilter[1], input$metricfilter[2]))
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

  ### PROFILE ####

  # longitudinale profile if axis clicked

  observeEvent(selected_axis(), {
    # req(click_value()$group == "AXIS")

    output$long_profile <- renderPlotly({
      plot_ly(data = selected_axis(), x = ~measure, y = ~talweg_height_min,
              type = 'scatter', mode = 'lines', name = 'Ligne')
    })
  })


  ### MAPPING ####

  # Event on click
  observeEvent(click_value(), {
    # map regions or selected region
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
    }
    if (is.null(input$dynamicRadio)) {
      leafletProxy("exploremap") %>%
        map_no_metric(data_network = network_filter(),  network_group = "D",
                      data_axis = network_region_axis(), axis_group = "AXIS")

    }
    if (!is.null(input$dynamicRadio)){
      print("pouet")
        map_metric(map_id = "exploremap", data_map = network_filter(), varsel = varsel(),
                   network_group = "D", data_axis = network_region_axis(), axis_group = "AXIS")
    }
  }) # ObserveEvent


}


## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
