library(leaflet)
library(sf)
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

  # # dev print to debug value
  # output$printcheck = renderPrint({
  #   tryCatch({
  #     network_filter()
  #     print(click_value()$group)
  #     print(network_filter())
  #     print("exists")
  #   },
  #   shiny.silent.error = function(e) {
  #     print("doesn't exist")
  #   }
  #   )
  # })

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
    }
  })

  ### DYNAMIC UI ####

  # UI create choose metric
  output$metricUI <- renderUI({
    # req(click_value()$group == "B")
    req(network_region_metrics())
    selectInput(ns("metric"), "Sélectionez une métrique :",
                choices = names(metrics_choice()),
                selected  = "Largeurs") # selectInput for dynamic radio buttons
  })

  # UI strahler filter
  output$strahlerfilterUI <- renderUI(
    {
      req(network_region_metrics())

      strahler_col <- isolate(network_region_metrics()$strahler)
      sliderInput(ns("strahler"),
                  label="Ordre de strahler",
                  min=min(strahler_col, na.rm = TRUE),
                  max=max(strahler_col, na.rm = TRUE),
                  value=c(min(strahler_col, na.rm = TRUE),
                          max(strahler_col, na.rm = TRUE)),
                  step=1)
    })

  # UI metrics radio buttons
  output$radioButtonsUI <- renderUI({

    req(input$metric)

    selected_metric <- input$metric

    radioButtons(ns("dynamicRadio"), sprintf("%s :", selected_metric),
                 choiceNames = names(metrics_choice()[[selected_metric]]),
                 choiceValues = as.list(unname(metrics_choice()[[selected_metric]])),
                 selected = character(0))
  })

  # UI dynamic filter on metric selected
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

  # UI switch unit area
  output$areaUI <- renderUI({
    req(input$metric == "Occupation du sol" || input$metric == "Continuité latérale")

    selectInput(ns("unit_area"), "Surfaces :",
                choices = c("Hectares", "% du fond de vallée"),
                selected = "Hectares")
  })

  ### DATA ####

  # DATA get network axis in region
  network_region_axis <- reactiveVal()

  observeEvent(click_value(),{
    if (click_value()$group == "B"){
      network_region_axis(get_axis(selected_region_id = click_value()$id))
    }
  })

  # DATA get only the region selected feature
  selected_region_feature <- reactiveVal()

  observeEvent(click_value(),{
    if (click_value()$group == "B"){
      selected_region_feature(get_region(region_click_id = click_value()$id))
    }
  })

  # DATA get network_region_metrics (in reactiveVal keep data even the click_value change)
  network_region_metrics <- reactiveVal()

  observeEvent(click_value(),{
    if (click_value()$group == "B"){
      network_region_metrics(get_network_region_with_metrics(selected_region_id = click_value()$id))
    }
  })

  # DATA data with filter
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

  # DATA metrics to display
  varsel <- reactive({
    req(network_filter())
    if (is.null(network_filter())){
      return(NULL)
    } else {
      network_filter()[[input$dynamicRadio]]
    }
  })

  # DATA network by selected axis
  selected_axis <- reactive({
    req(click_value()$group == "AXIS")
    get_network_axis(network_data = network_region_metrics(), measure_col = "measure",
                     axis_id = click_value()$id)
  })

  ### MAP ####

  # MAP region selected
  observeEvent(click_value(), {
    if (click_value()$group == "B"){
      # map region clicked
      leafletProxy("exploremap") %>%
        map_region_clicked(region_click = click_value(),
                           selected_region_feature = selected_region_feature(),
                           regions_group = "B",
                           selected_region_group = "C")

    }
  })

  # MAP network metric
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
      map_metric(map_id = "exploremap", data_map = network_filter(), varsel = varsel(),
                 network_group = "D", data_axis = network_region_axis(), axis_group = "AXIS")
    }
  })

  ### PROFILE ####

  # PROFILE longitudinale profile if axis clicked
  output$long_profile <- renderPlotly({
    req(click_value()$group == "AXIS")

    # Create the Plotly plot
    plot <- plot_ly(data = selected_axis(), x = ~measure, y = ~talweg_elevation_min,
                    key = ~fid,  # Specify the "id" column for hover text
                    type = 'scatter', mode = 'lines', name = 'Ligne')

    # Add hover information
    plot <- plot %>%
      event_register("plotly_hover")  # Enable hover events

    # Define an observeEvent to capture hover events
    observeEvent(event_data("plotly_hover"), {
      hover_data <- event_data("plotly_hover")

      if (!is.null(hover_data)) {
        hover_fid <- hover_data$key  # Assuming "id" is the name of your id column

        highlighted_feature <- network_region_metrics()[network_region_metrics()$fid == hover_fid, ]

        leafletProxy("exploremap") %>%
          addPolylines(data = highlighted_feature, color = "red", weight = 10, group = "LIGHT")

      }
    })

    return(plot)
  })

  observe({
    if (is.null(event_data("plotly_hover"))) {
      leafletProxy("exploremap") %>%
        clearGroup("LIGHT")
    }
  })
}


## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
