# library(reactlog)
# reactlog_enable()

#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_explore
#'
#' @import shiny
#' @import shinythemes
#' @importFrom shinycssloaders withSpinner
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
          withSpinner(leafletOutput(ns("exploremap")))
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
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom leaflet leafletProxy clearGroup leafletOutput renderLeaflet
#' @importFrom htmltools HTML div img
#' @importFrom dplyr filter mutate
#' @importFrom plotly event_register event_data plotlyProxy plotlyProxyInvoke renderPlotly plotlyOutput
#'
mod_explore_server <- function(id){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    # suppress warnings
    # options(warn = -1)

    # # dev print to debug value
    # output$printcheck = renderPrint({
    #   tryCatch({
    #     input$exploremap_shape_click
    #     print(input$exploremap_shape_click)
    #     print(r_val$network_region_axis$fid)
    #     print("exists")
    #   },
    #   shiny.silent.error = function(e) {
    #     print("doesn't exist")
    #   }
    #   )
    # })

    ### R_VAL ####
    r_val <- reactiveValues(
      regions_in_bassin = NULL, # all the regions in selected bassin
      network_region_axis = NULL, # all the axis in the selected region
      selected_region_feature = NULL,
      region_click = NULL,
      axis_click = NULL,
      dgo_axis = NULL, # all selected axis DGO
      axis_start_end = NULL, # start / end df coordinates to add pin on map
      # metric selected by user
      selected_metric = NULL,
      selected_metric_name = NULL,
      select_metric_category = NULL,
      # profile metric selected by user
      selected_profile_metric = NULL,
      selected_profile_metric_name = NULL,
      select_profile_metric_category = NULL,
      strahler = NULL,
      ui_strahler_filter = NULL,
      ui_metric_type = NULL,
      ui_metric = NULL,
      ui_unit_area = NULL,
      min_max_metric = NULL,
      ui_metric_filter = NULL,
      ui_profile_metric_type = NULL,
      ui_profile_metric = NULL
    )

    ### BASSIN INIT MAP ####

    # map initialization
    output$exploremap <- renderLeaflet({
      map_init_bassins(bassins_data = data_get_bassins())
    })

    ### DYNAMIC PROFILE UI ####

    # add input UI for profile additional metric
    output$profilemetricUI <- renderUI({
      r_val$ui_profile_metric_type
    })

    # add radiobutton for profile additional metric
    output$profileradiobuttonUI <- renderUI({
      r_val$ui_profile_metric
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
      if (!is.null(r_val$ui_metric_type)){
        r_val$ui_metric_type
      } else {
        HTML('<label class="control-label" id="wait-metric-label">
             Cliquez sur une région hydrographique pour afficher la sélection des métriques</label>')
      }
    })

    # UI metrics radio buttons
    output$radioButtonsUI <- renderUI({
      r_val$ui_metric
    })

    # UI switch unit area
    output$areaUI <- renderUI({
      r_val$ui_unit_area
    })

    ### DYNAMIC FILTER UI ####

    # UI strahler filter
    output$strahlerfilterUI <- renderUI(
      {
        r_val$ui_strahler_filter
      })

    # UI dynamic filter on metric selected
    output$metricsfilterUI <- renderUI({
      r_val$ui_metric_filter
    })

    ### EVENT MAP CLICK ####

    observeEvent(input$exploremap_shape_click,{
      #### bassin clicked ####
      if (input$exploremap_shape_click$group == params_map_group()[["bassin"]]){
        # get the regions data in selected bassin
        r_val$regions_in_bassin = data_get_regions_in_bassin(selected_bassin_id = input$exploremap_shape_click$id)
        # update map : zoom in clicked bassin, clear bassin data, display region in bassin
        leafletProxy("exploremap") %>%
          map_add_regions_in_bassin(bassin_click = input$exploremap_shape_click,
                                    regions_data = r_val$regions_in_bassin)
      }
      ### region clicked ####
      if (input$exploremap_shape_click$group == params_map_group()$region){
        # store the region click values
        r_val$region_click = input$exploremap_shape_click
        # save the selected region feature for mapping
        r_val$selected_region_feature = data_get_region(region_click_id = r_val$region_click$id)
        # get the axis in the region
        r_val$network_region_axis = data_get_axis(selected_region_id = input$exploremap_shape_click$id)
        # get strahler data
        r_val$strahler = isolate(data_get_min_max_strahler(selected_region_id = r_val$region_click$id))
        # build strahler slider
        r_val$ui_strahler_filter = sliderInput(ns("strahler"),
                                       label="Ordre de strahler",
                                       min=r_val$strahler[["min"]],
                                       max=r_val$strahler[["max"]],
                                       value=c(r_val$strahler[["min"]],
                                               r_val$strahler[["max"]]),
                                       step=1)
        # build WMS filter
        cql_filter=paste0("gid_region=", r_val$selected_region_feature[["gid"]],
                          " AND strahler>=", input$strahler[1],
                          " AND strahler <= ", input$strahler[2])


        # map region clicked with region clicked and overlayers
        leafletProxy("exploremap") %>%
          map_region_clicked(region_click = input$exploremap_shape_click,
                             selected_region_feature = r_val$selected_region_feature)

        # build metric selectInput
        r_val$ui_metric_type = selectInput(ns("metric_type"), "Sélectionez une métrique :",
                                             choices = names(params_metrics_choice()),
                                             selected  = names(params_metrics_choice())[1])

      }
      ### axis clicked ####
      if (input$exploremap_shape_click$group == params_map_group()$axis) {
        # save the clicked axis values
        r_val$axis_click = input$exploremap_shape_click
        # reget the axis in the region without the selected axis
        r_val$network_region_axis = data_get_axis(selected_region_id = r_val$region_click$id) %>%
          filter(fid != r_val$axis_click$id)
        # get the DGO axis data
        r_val$dgo_axis = data_get_network_axis(selected_axis_id = r_val$axis_click$id) %>%
          mutate(measure = measure/1000)
        # extract axis start end point
        r_val$axis_start_end = data_get_axis_start_end(dgo_axis = r_val$dgo_axis)

        # map dgo axis when axis clicked and metric selected
        leafletProxy("exploremap") %>%
          map_dgo_axis(selected_axis = r_val$dgo_axis, region_axis = r_val$network_region_axis) %>%
          map_axis_start_end(axis_start_end = r_val$axis_start_end, region_axis = r_val$network_region_axis)
      }
    })

    ### EVENT METRIC TYPE SELECT ####

    observeEvent(input$metric_type, {
      # build metric radioButtons
      r_val$ui_metric = radioButtons(ns("metric"), sprintf("%s :", input$metric_type),
                                                  choiceNames = as.list(unname(params_metrics_choice()[[input$metric_type]])),
                                                  choiceValues = names(params_metrics_choice()[[input$metric_type]]),
                                                  selected = character(0))

      # build selectInput unit area for Occupation du sol or continuity
      if (input$metric_type == "Occupation du sol" || input$metric_type == "Continuité latérale"){
        r_val$ui_unit_area = selectInput(ns("unit_area"), "Surfaces :",
                                         choices = c("Hectares", "% du fond de vallée"),
                                         selected = "Hectares")
      }else{
        r_val$ui_unit_area = NULL
      }
    })


    ### EVENT METRIC SELECT ####

    observeEvent(c(input$metric, input$unit_area), ignoreInit = TRUE, {
      # change field if unit_area in percentage
      if (!is.null(input$unit_area) && input$unit_area == "% du fond de vallée"
          && (input$metric_type %in% c("Occupation du sol", "Continuité latérale"))){
        r_val$selected_metric = paste0(input$metric,"_pc")
        r_val$selected_metric_name = utile_get_metric_name(selected_metric = input$metric)
        r_val$select_metric_category = utile_get_category_name(selected_metric = input$metric)
      } else if (!is.null(input$metric)) {
        r_val$selected_metric = input$metric
        r_val$selected_metric_name = utile_get_metric_name(selected_metric = input$metric)
        r_val$select_metric_category = utile_get_category_name(selected_metric = input$metric)
      }

      # build metric filter slider
      r_val$min_max_metric <- data_get_min_max_metric(selected_region_id = r_val$region_click$id, selected_metric = r_val$selected_metric)

      r_val$ui_metric_filter = sliderInput(ns("metricfilter"),
                                           label = r_val$selected_metric_name,
                                           min = isolate(r_val$min_max_metric[["min"]]),
                                           max = isolate(r_val$min_max_metric[["max"]]),
                                           value = c(
                                             isolate(r_val$min_max_metric[["min"]]),
                                             isolate(r_val$min_max_metric[["max"]])
                                           )
      )

    })

    ### EVENT METRIC & AXIS ####

    observeEvent(c(input$metric, input$exploremap_shape_click), {
      if (is.null(input$metric)==FALSE && input$exploremap_shape_click$group == params_map_group()$axis){

        # build input for profile metric type
        r_val$ui_profile_metric_type = selectInput(ns("profile_metric_type"), "Ajoutez une métrique :",
                                                   choices = names(params_metrics_choice()),
                                                   selected  = names(params_metrics_choice())[1])
      }
    })

    ### EVENT PROFILE METRIC TYPE SELECT ####

    observeEvent(input$profile_metric_type, {

      # build profile metric radio button
      r_val$ui_profile_metric = radioButtons(
        ns("profile_metric"),
        sprintf("%s :", input$profile_metric_type),
        choiceNames = as.list(unname(params_metrics_choice()[[input$profile_metric_type]])),
        choiceValues = names(params_metrics_choice()[[input$profile_metric_type]]),
        selected = character(0)
      )

    })


    ### EVENT PROFILE METRIC SELECT ####

    observeEvent(c(input$profile_metric, input$profile_unit_area), ignoreInit = TRUE, {
      # change field if unit_area in percentage
      if (!is.null(input$profile_unit_area) && input$profile_unit_area == "% du fond de vallée"
          && (input$profile_metric_type %in% c("Occupation du sol", "Continuité latérale"))){
        r_val$selected_profile_metric = paste0(input$profile_metric,"_pc")
        r_val$selected_profile_metric_name = utile_get_metric_name(selected_metric = input$profile_metric)
        r_val$select_profile_metric_category = utile_get_category_name(selected_metric = input$profile_metric)
      } else if (!is.null(input$profile_metric)) {
        r_val$selected_profile_metric = input$profile_metric
        r_val$selected_profile_metric_name = utile_get_metric_name(selected_metric = input$profile_metric)
        r_val$select_profile_metric_category = utile_get_category_name(selected_metric = input$profile_metric)
      }
    })

    ### MAP ####

    # reactive list to activate map update
    map_update <- reactive({
      list(r_val$region_click$id, input$strahler, input$metricfilter)
    })

    # MAP network metric
    observeEvent(map_update(), {

      if (is.null(input$strahler)) {
        return (NULL)
      }
      # no metric selected
      if (is.null(r_val$selected_metric)) {
        # build WMS filter
        cql_filter=paste0("gid_region=", r_val$selected_region_feature[["gid"]],
                          " AND strahler>=", input$strahler[1],
                          " AND strahler <= ", input$strahler[2])

        # update map with basic style
        leafletProxy("exploremap") %>%
          map_metric(wms_params = params_wms()$metric_basic, # metric_basic to have blue network
                        cql_filter = cql_filter, sld_body = NULL,
                        data_axis = r_val$network_region_axis)

      }
      # metric selected
      if (!is.null(r_val$selected_metric)){

        # build SLD symbology
        sld_body <- sld_get_style(breaks = sld_get_quantile_metric(selected_region_id = r_val$region_click$id, selected_metric = r_val$selected_metric),
                                  colors = sld_get_quantile_colors(quantile_breaks = sld_get_quantile_metric(selected_region_id = r_val$region_click$id,
                                                                                                             selected_metric = r_val$selected_metric)),
                                  metric = r_val$selected_metric)

        # build WMS filter
        cql_filter=paste0("gid_region=",r_val$selected_region_feature[["gid"]],
                          " AND strahler>=",input$strahler[1],
                          " AND strahler <= ",input$strahler[2],
                          " AND ",r_val$selected_metric,">=",input$metricfilter[1],
                          " AND ",r_val$selected_metric,"<=",input$metricfilter[2])

        # update map
        leafletProxy("exploremap") %>%
          map_metric(wms_params = params_wms()$metric,
                     cql_filter = cql_filter, sld_body = sld_body,
                     data_axis = r_val$network_region_axis)

        # update legend
        metric_legend(map_legend_metric(sld_body = sld_body))
      }
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

      if (!is.null(r_val$axis_click) && !is.null(r_val$selected_metric)){
        selected_axis_df <- r_val$dgo_axis %>%
          as.data.frame()
        # no additional metric
        if (!is.null(r_val$selected_metric) && is.null(r_val$selected_profile_metric)){
          plot <- lg_profile_main(data = selected_axis_df,
                                  y = r_val$selected_metric,
                                  y_label = r_val$selected_metric_name,
                                  y_label_category = r_val$select_metric_category
          )
        }
        # the user select an additional metric
        if (!is.null(r_val$selected_metric) && !is.null(r_val$selected_profile_metric)){
          plot <- lg_profile_second(data = selected_axis_df,
                                    y = r_val$selected_metric,
                                    y_label = r_val$selected_metric_name,
                                    y_label_category = r_val$select_metric_category,
                                    y2 = r_val$selected_profile_metric,
                                    y2_label = r_val$selected_profile_metric_name,
                                    y2_label_category = r_val$select_profile_metric_category
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
            highlighted_feature <- r_val$dgo_axis[r_val$dgo_axis$fid == hover_fid, ]
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
      if (input$exploremap_shape_mouseover$group == params_map_group()$dgo_axis && !is.null(input$exploremap_shape_mouseover)){
        # extract dgo axis fid from map
        select_measure <- r_val$dgo_axis %>%
          filter(fid == input$exploremap_shape_mouseover$id)
        # change profile layout with vertial line
        plotlyProxy("long_profile", session) %>%
          plotlyProxyInvoke("relayout", list(shapes = list(lg_vertical_line(select_measure$measure))))
      }
    })
  })
}


## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
