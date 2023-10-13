library(shinythemes)
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
      theme = shinytheme("spacelab"),
      fluidRow(
        column(
          width = 3,
          titlePanel(""),
          uiOutput(ns("metricUI")),
          uiOutput(ns("areaUI")),
          uiOutput(ns("radioButtonsUI")) # uiOutput radios buttons metrics
        ), # column
        column(
          width = 7,
          titlePanel(""),
          leafletOutput(ns("exploremap"))
        ), # column
        column(
          width = 2,
          titlePanel("Filtres"),
          uiOutput(ns("strahlerfilterUI")),
          uiOutput(ns("metricsfilterUI")),
          uiOutput(ns("legendUI")),
          verbatimTextOutput(ns("printcheck"))
        ) # column
      ), # fluidRow
      fluidRow(
        tabsetPanel(
          tabPanel(
            "Profil en long",
              div(
                fluidRow(
                  column(width = 9,
                         plotlyOutput(ns("long_profile"))
                  ),
                  column(
                    width = 3,
                    style = "margin-top: 20px;",
                    uiOutput(ns("profilemetricUI")),
                    uiOutput(ns("profileareaUI")),
                    uiOutput(ns("profileradiobuttonUI"))
                  )
                )
              )
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
  #     click_value()
  #     print(click_value())
  #     print(network_region_axis()$fid)
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

  ### DYNAMIC PROFILE UI ####

  # add input UI for profile additional metric
  output$profilemetricUI <- renderUI({

    req(axis_click(), selected_metric())
    selectInput(ns("profile_metric_type"), "Ajoutez une métrique :",
                choices = names(params_metrics_choice()),
                selected  = names(params_metrics_choice())[1])
  })

  # add radiobutton for profile additional metric
  output$profileradiobuttonUI <- renderUI({

    req(input$profile_metric_type)
    metric_type <- input$profile_metric_type

    radioButtons(ns("profile_metric"), sprintf("%s :", metric_type),
                 choiceNames = as.list(unname(params_metrics_choice()[[metric_type]])),
                 choiceValues = names(params_metrics_choice()[[metric_type]]),
                 selected = character(0))
  })

  # UI switch unit area for profile additional metric
  output$profileareaUI <- renderUI({
    req(input$profile_metric_type == "Occupation du sol" || input$profile_metric_type == "Continuité latérale")

    selectInput(ns("profile_unit_area"), "Surfaces :",
                choices = c("Hectares", "% du fond de vallée"),
                selected = "Hectares")
  })

  ### DYNAMIC METRIC UI ####

  # UI create choose metric
  output$metricUI <- renderUI({

    # wait region hydrographic to display metric selection
    if (!is.null(region_click())){
    selectInput(ns("metric_type"), "Sélectionez une métrique :",
                choices = names(params_metrics_choice()),
                selected  = names(params_metrics_choice())[1])
    } else {
      HTML('<label class="control-label" id="wait-metric-label">
           Cliquez sur une région hydrographique pour afficher la sélection des métriques</label>')
    }
  })

  # UI metrics radio buttons
  output$radioButtonsUI <- renderUI({

    req(input$metric_type)

    metric_type <- input$metric_type

    radioButtons(ns("metric"), sprintf("%s :", metric_type),
                 choiceNames = as.list(unname(params_metrics_choice()[[metric_type]])),
                 choiceValues = names(params_metrics_choice()[[metric_type]]),
                 selected = character(0))
  })

  # UI switch unit area
  output$areaUI <- renderUI({
    req(input$metric_type == "Occupation du sol" || input$metric_type == "Continuité latérale")

    selectInput(ns("unit_area"), "Surfaces :",
                choices = c("Hectares", "% du fond de vallée"),
                selected = "Hectares")
  })

  ### DYNAMIC FILTER UI ####

  # UI strahler filter
  output$strahlerfilterUI <- renderUI(
    {
      req(region_click())
      # query data from database
      strahler <- isolate(data_get_min_max_strahler(selected_region_id = region_click()$id))

      sliderInput(ns("strahler"),
                  label="Ordre de strahler",
                  min=strahler[["min"]],
                  max=strahler[["max"]],
                  value=c(strahler[["min"]],
                          strahler[["max"]]),
                  step=1)
    })

  # UI dynamic filter on metric selected
  output$metricsfilterUI <- renderUI({
    req(selected_metric(), selected_metric_name())

    metric <- data_get_min_max_metric(selected_region_id = region_click()$id, selected_metric = selected_metric())

    sliderInput(ns("metricfilter"),
                label = selected_metric_name(),
                min = isolate(metric[["min"]]),
                max = isolate(metric[["max"]]),
                value = c(
                  isolate(metric[["min"]]),
                  isolate(metric[["max"]])
                )
    )
  })

  ### DATA ####

  # store clicked data
  click_value <- reactive({
    input$exploremap_shape_click
  })

  # DATA get network axis
  network_region_axis <- reactiveVal()
  # get data on map click
  selected_region_feature <- reactiveVal()
  region_click <- reactiveVal()
  axis_click <- reactiveVal()
  dgo_axis <- reactiveVal()
  axis_start_end <- reactiveVal()

  observeEvent(click_value(),{
    # when region clicked get data axis in region
    if (click_value()$group == params_map_group()$region){
      # store the region click values
      region_click(click_value())
      # save the selected region feature for mapping
      selected_region_feature(data_get_region(region_click_id = region_click()$id))
      # get the axis in the region
      network_region_axis(data_get_axis(selected_region_id = click_value()$id))
    }
    # when axis clicked get axis region without the axis selected
    if (click_value()$group == params_map_group()$axis) {
      # save the clicked axis values
      axis_click(click_value())
      # reget the axis in the region without the selected axis
      network_region_axis(data_get_axis(selected_region_id = region_click()$id) %>%
                            filter(fid != axis_click()$id))
      # get the DGO axis data
      dgo_axis(data_get_network_axis(selected_axis_id = axis_click()$id) %>%
        mutate(measure = measure/1000))
      # extract axis start end point
      axis_start_end (data_get_axis_start_end(dgo_axis = dgo_axis()))
    }
  })

  # metric selected by user
  selected_metric <- reactiveVal()
  selected_metric_name <- reactiveVal()

  # set metric value and name
  observeEvent(!is.null(input$metric) && !is.null(input$unit_area),
               ignoreInit = TRUE, {
    # change field if unit_area in percentage
    if (!is.null(input$unit_area) && input$unit_area == "% du fond de vallée"
        && (input$metric_type %in% c("Occupation du sol", "Continuité latérale"))){
      selected_metric(paste0(input$metric,"_pc"))
      selected_metric_name(utile_get_metric_name(selected_metric = input$metric))
    } else if (!is.null(input$metric)) {
      selected_metric(input$metric)
      selected_metric_name(utile_get_metric_name(selected_metric = input$metric))
    }

  })

  # additional profile metric selected by user
  selected_profile_metric <- reactiveVal()
  selected_profile_metric_name <- reactiveVal()

  # set profile metric value and name
  observeEvent(!is.null(input$profile_metric) && !is.null(input$profile_unit_area),
               ignoreInit = TRUE, {
    # change field if unit_area in percentage
    if (!is.null(input$profile_unit_area) && input$profile_unit_area == "% du fond de vallée"
        && (input$profile_metric_type %in% c("Occupation du sol", "Continuité latérale"))){
      selected_profile_metric(paste0(input$profile_metric,"_pc"))
      selected_profile_metric_name(utile_get_metric_name(selected_metric = input$profile_metric))
    } else if (!is.null(input$profile_metric)) {
      selected_profile_metric(input$profile_metric)
      selected_profile_metric_name(utile_get_metric_name(selected_metric = input$profile_metric))
    }
  })

  # store mouseover map DGO axis feature
  datamoveover <- reactive({
    input$exploremap_shape_mouseover
  })

  ### MAP ####

  # MAP region selected
  observeEvent(click_value(), {
    if (click_value()$group == params_map_group()$region){

      # map region clicked with region clicked and overlayers
      leafletProxy("exploremap") %>%
        map_region_clicked(region_click = click_value(),
                           selected_region_feature = selected_region_feature())
    }
  })

  # reactive list to activate map update
  map_update <- reactive({
    list(region_click()$id, input$strahler, input$metricfilter)
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
      sld_body <- sld_get_style(breaks = sld_get_quantile_metric(selected_region_id = region_click()$id, selected_metric = selected_metric()),
                                colors = sld_get_quantile_colors(quantile_breaks = sld_get_quantile_metric(selected_region_id = region_click()$id,
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

  # map dgo axis when axis clicked and metric selected
  observe({
    req(network_region_axis())
    if(!is.null(dgo_axis()) && !is.null(selected_metric())){

      leafletProxy("exploremap") %>%
        map_dgo_axis(selected_axis = dgo_axis(), region_axis = network_region_axis())
    }
  })

  # map axis start and end point
  observeEvent(axis_start_end(), {
    req(axis_start_end())
    leafletProxy("exploremap") %>%
      map_axis_start_end(axis_start_end = axis_start_end(), region_axis = network_region_axis())
  })



  ### MAP LEGEND ####

  metric_legend <- reactiveVal(NULL)

  output$legendUI <- renderUI({

    div(
      HTML('<label class="control-label" id="legend-label">Légende</label>'),
      img(
        title = "Information sur la légende",
        src = "www/information-icon-6068.png",
        style = "width: 10%; height: auto;",
        class="responsive"
      ), # img
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

  output$long_profile <- renderPlotly({

    if (!is.null(axis_click()) && !is.null(selected_metric())){
      selected_axis_df <- dgo_axis() %>%
        as.data.frame()
      # no additional metric
      if (!is.null(selected_metric()) && is.null(selected_profile_metric())){
        plot <- lg_profile_main(data = selected_axis_df,
                                y = selected_metric(),
                                y_label = selected_metric_name()
        )
      }
      # the user select an additional metric
      if (!is.null(selected_metric()) && !is.null(selected_profile_metric())){
        plot <- lg_profile_second(data = selected_axis_df,
                                  y = selected_metric(),
                                  y_label = selected_metric_name(),
                                  y2 = selected_profile_metric(),
                                  y2_label = selected_profile_metric_name()
        )
      }

      # Add hover information
      plot <- plot %>%
        event_register("plotly_hover")  # Enable hover events

      # Define an observeEvent to capture hover events
      observeEvent(event_data("plotly_hover"), {
        hover_data <- event_data("plotly_hover")

        if (!is.null(hover_data)) {
          hover_fid <- hover_data$key
          highlighted_feature <- dgo_axis()[dgo_axis()$fid == hover_fid, ]
          leafletProxy("exploremap") %>%
            addPolylines(data = highlighted_feature, color = "red", weight = 10, group = "LIGHT")

        }
      })
      return(plot)
    }
    else {
      # create empty plotly graph with user message
      plot <- lg_profile_empty()
      return(plot)
    }
  })

  # clear previous point on map when moving along profile to not display all the point move over
  observe({
    if (is.null(event_data("plotly_hover"))) {
      leafletProxy("exploremap") %>%
        clearGroup("LIGHT")
    }
  })

  # add vertical line on profil on map user mouseover axis
  observe({
    if (datamoveover()$group == params_map_group()$dgo_axis && !is.null(datamoveover())){
      # extract dgo axis fid from map
      select_measure <- dgo_axis() %>%
        filter(fid == datamoveover()$id)
      # change profile layout with vertial line
      plotlyProxy("long_profile", session) %>%
        plotlyProxyInvoke("relayout", list(shapes = list(lg_vertical_line(select_measure$measure))))
    }
  })
}


## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
