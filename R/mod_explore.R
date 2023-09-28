library(leaflet)
library(sf)
library(DBI)
library(htmltools)
library(dplyr)
library(readr)
library(plotly)
library(reactlog)
library(glue)
library(httr)

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
          uiOutput(ns("legendUI")),
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
    map_init_bassins(bassins_data = data_get_bassins())
  })

  # clicked polygon data
  click_value <- reactive({
    input$exploremap_shape_click
  })


  ### REGION ####

  # get regions data in clicked bassin
  region_hydro <- reactive({
    req(click_value()$group == params_map_group()[["bassin"]])
    data_get_regions_in_bassin(selected_bassin_id = click_value()$id)
  })

  # Event on click
  observeEvent(click_value(), {
    # map regions or selected bassin
    if (click_value()$group == params_map_group()[["bassin"]]){
      # update map : zoom in clicked bassin, clear bassin data, display region in bassin
      leafletProxy("exploremap") %>%
        map_add_regions_in_bassin(bassin_click = click_value(),
                                  regions_data = region_hydro())
    }
  })

  ### DYNAMIC UI ####

  # UI create choose metric
  output$metricUI <- renderUI({
    # req(click_value()$group == params_map_group()[["region"]])
    req(region_click_id())
    selectInput(ns("metric"), "Sélectionez une métrique :",
                choices = names(params_metrics_choice()),
                selected  = "Largeurs") # selectInput for dynamic radio buttons
  })

  # UI strahler filter
  output$strahlerfilterUI <- renderUI(
    {
      req(region_click_id())
      # query data from database
      strahler <- isolate(data_get_min_max_strahler(selected_region_id = region_click_id()))

      sliderInput(ns("strahler"),
                  label="Ordre de strahler",
                  min=strahler[["min"]],
                  max=strahler[["max"]],
                  value=c(strahler[["min"]],
                          strahler[["max"]]),
                  step=1)
    })

  # UI metrics radio buttons
  output$radioButtonsUI <- renderUI({

    req(input$metric)

    selected_metric <- input$metric

    radioButtons(ns("dynamicRadio"), sprintf("%s :", selected_metric),
                 choiceNames = names(params_metrics_choice()[[selected_metric]]),
                 choiceValues = as.list(unname(params_metrics_choice()[[selected_metric]])),
                 selected = character(0))
  })

  # UI dynamic filter on metric selected
  output$metricsfilterUI <- renderUI({
    req(selected_metric())

    metric <- data_get_min_max_metric(selected_region_id = region_click_id(), selected_metric = selected_metric())

    sliderInput(ns("metricfilter"),
                label = names(unlist(params_metrics_choice()[[input$metric]]))[unlist(params_metrics_choice()[[input$metric]]) == selected_metric()], # extract key from value
                min = isolate(metric[["min"]]),
                max = isolate(metric[["max"]]),
                value = c(
                  isolate(metric[["min"]]),
                  isolate(metric[["max"]])
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

  # metric selected by user
  selected_metric <- reactiveVal()

  # change field if unit_area in percentage
  observeEvent(!is.null(input$dynamicRadio) && !is.null(input$unit_area), ignoreInit = TRUE, {
    if (!is.null(input$unit_area) && input$unit_area == "% du fond de vallée"
        && (input$metric %in% c("Occupation du sol", "Continuité latérale"))){
      selected_metric(paste0(input$dynamicRadio,"_pc"))
    } else {
      selected_metric(input$dynamicRadio)
    }
  })

  # DATA get network axis in region
  network_region_axis <- reactiveVal()

  observeEvent(click_value(),{
    if (click_value()$group == params_map_group()[["region"]]){
      network_region_axis(data_get_axis(selected_region_id = click_value()$id))
    }
  })

  # DATA get only the region selected feature
  selected_region_feature <- reactiveVal()
  region_click_id <- reactiveVal()

  observeEvent(click_value(),{
    if (click_value()$group == params_map_group()[["region"]]){
      region_click_id(click_value()$id)
      selected_region_feature(data_get_region(region_click_id = region_click_id()))
    }
  })

  # DATA network by selected axis
  selected_axis <- reactive({
    req(click_value()$group == params_map_group()[["axis"]])
    data_get_network_axis(selected_axis_id = click_value()$id)
  })

  ### MAP ####

  # MAP region selected
  observeEvent(click_value(), {
    if (click_value()$group == params_map_group()[["region"]]){

      # map region clicked with region clicked and overlayers
      leafletProxy("exploremap") %>%
        map_region_clicked(region_click = click_value(),
                           selected_region_feature = selected_region_feature())
    }
  })

  # reactive list to activate map update
  map_update <- reactive({
    list(region_click_id(), input$strahler, input$metricfilter)
  })

  # MAP network metric
  observeEvent(map_update(), {

    if (is.null(input$strahler)) {
      return (NULL)
    }
    # no metric selected
    if (is.null(selected_metric())) {
      # build WMS filter
      cql_filter=paste0("gid_region=", selected_region_feature()[["gid"]],
                        " AND strahler>=", input$strahler[1],
                        " AND strahler <= ", input$strahler[2])
      # update map with basic style
      leafletProxy("exploremap") %>%
        map_metric(wms_params = params_wms()$metric_basic, # metric_basic to have blue network
                      cql_filter = cql_filter, sld_body = NULL,
                      data_axis = network_region_axis())

    }
    # metric selected
    if (!is.null(selected_metric())){

      # build SLD symbology
      sld_body <- sld_get_style(breaks = sld_get_quantile_metric(selected_region_id = region_click_id(), selected_metric = selected_metric()),
                                colors = sld_get_quantile_colors(quantile_breaks = sld_get_quantile_metric(selected_region_id = region_click_id(),
                                                                                                           selected_metric = selected_metric())),
                                metric = selected_metric())

      # build WMS filter
      cql_filter=paste0("gid_region=",selected_region_feature()[["gid"]],
                        " AND strahler>=",input$strahler[1],
                        " AND strahler <= ",input$strahler[2],
                        " AND ",selected_metric(),">=",input$metricfilter[1],
                        " AND ",selected_metric(),"<=",input$metricfilter[2])

      # update map
      leafletProxy("exploremap") %>%
        map_metric(wms_params = params_wms()$metric,
                   cql_filter = cql_filter, sld_body = sld_body,
                   data_axis = network_region_axis())

      # update legend
      metric_legend(map_legend_metric(sld_body = sld_body))
    }
  })

  ### MAP LEGEND ####

  metric_legend <- reactiveVal(NULL)

  output$legendUI <- renderUI({
    div(
      HTML('<label class="control-label" id="legend-label">Légende</label>'),
      # metric
      div(
        style = "display: flex; align-items: center;",
        metric_legend(),
      ),
      # zone inondable
      if (any(input$exploremap_groups %in% params_map_group()$inondation)) {
        map_legend_wms_overlayer(wms_params = params_wms()$inondation)
      },
      # ouvrage de protection
      if (any(input$exploremap_groups %in% params_map_group()[["ouvrage_protection"]])) {
        map_legend_wms_overlayer(wms_params = params_wms()$ouvrage_protection)
      },
      # ROE
      if (any(input$exploremap_groups %in% params_map_group()[["roe"]])) {
        map_legend_vector_overlayer(layer_label = "ROE")
      }
    ) # div
  })

  ### PROFILE ####

  # PROFILE longitudinale profile if axis clicked
  output$long_profile <- renderPlotly({
    req(click_value()$group == params_map_group()[["axis"]])

    selected_axis_df <- selected_axis() %>%
      as.data.frame()

    plot <- plot_ly(data = selected_axis_df, x = ~measure, y = ~talweg_elevation_min,
                    key = ~fid,  # Specify the "id" column for hover text
                    type = 'scatter', mode = 'lines', name = 'Ligne')

    # Add hover information
    plot <- plot %>%
      event_register("plotly_hover")  # Enable hover events

    # Define an observeEvent to capture hover events
    observeEvent(event_data("plotly_hover"), {
      hover_data <- event_data("plotly_hover")

      if (!is.null(hover_data)) {
        hover_fid <- hover_data$key

        highlighted_feature <- selected_axis()[selected_axis()$fid == hover_fid, ]

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
