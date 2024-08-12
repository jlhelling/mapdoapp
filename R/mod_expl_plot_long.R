#' expl_plot_long UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom shinyjs useShinyjs
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_expl_plot_long_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    div(
      fluidRow(
        style = "margin-top: 10px;",
        column(width = 10,
               plotlyOutput(ns("long_profile"))
        ),
        column(
          width = 2,
          style = "margin-top: 20px;",
          uiOutput(ns("profile_first_metricUI")),
          uiOutput(ns("add_sec_axeUI"),
                   style = "margin-top: 30px;"),
          uiOutput(ns("profile_sec_metricUI"),
                   style = "margin-left : 23px; margin-top: 10px"),
          uiOutput(ns("profile_roeUI")),
          uiOutput(ns("profile_backgroundUI"))
        )
      )
    )
  )
}

#' expl_plot_long Server Functions
#'
#' @import shiny
#' @importFrom dplyr filter pull
#' @importFrom plotly event_register plotlyOutput renderPlotly event_data plotlyProxy
#' @importFrom leaflet addPolylines clearGroup
#' @importFrom bsicons bs_icon
#'
#' @noRd
mod_expl_plot_long_server <- function(id, r_val, globals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### REACTIVES ####
    r_val_local <- reactiveValues(
      plot = lg_profile_empty(), # plotly render longitudinal profile output (default empty)
      leaflet_hover_shapes = NULL, # list to store vertical lines to display on longitudinal profile
      ui_roe_profile = NULL, # UI placeholder for ROE checkbox
      ui_background_profile = NULL, # UI placeholder for background classes checkbox
      roe_vertical_line = NULL, # list with verticale line to plot on longitudinal profile

      # first metric
      profile_first_metric = NULL,

      selected_profile_metric_title = NULL, # metric title to be displayed instead of pure variable name
      selected_profile_metric_type = NULL, # metric type title

      # second metric
      proxy_second_axe = NULL,
      profile_sec_metric = NULL, # second metric selection
      add_sec_axe = NULL, # add second axis
      sec_metric_name = NULL, # title
      sec_metric_type = NULL, # metric type title

      # plotly shapes
      shapes_dgo = NULL, # list with shapes to plot clicked dgo element as line
      shapes_roe = NULL, # list with shapes to plot ROE obstacles as lines
      shapes_background = NULL, # list with shapes to plot classes in background

    )

    #### UI ####

    output$long_profile <- renderPlotly({
      return(r_val_local$plot)
    }) %>%
      bindCache(r_val_local$plot)

    # selectinput for metric
    output$profile_first_metricUI <- renderUI({
      if (!is.null(r_val_local$profile_first_metric)) {
        div(
          style = "display: flex; align-items: center",
          r_val_local$profile_first_metric,
          span(
            style = "display: flex; margin-left: 10px; margin-top: 20px",
            popover(
              trigger = bsicons::bs_icon("info-circle"),
              "",
              placement = "right",
              id = ns("popover_metric")
            )
          )
        )
      }
    })

    # add selectinput for additional metric
    output$profile_sec_metricUI <- renderUI({

      if (!is.null(r_val_local$profile_sec_metric)) {
        div(
          style = "display: flex; align-items: center",
          r_val_local$profile_sec_metric,
          span(
            style = "margin-left: 10px; margin-top: -10px",
            popover(
              trigger = bsicons::bs_icon("info-circle"),
              "",
              placement = "right",
              id = ns("popover_metric2")
            )
          )
        )
      }
    })

    # button to add second axe
    output$add_sec_axeUI <- renderUI({
      r_val_local$add_sec_axe
    })

    # checkbox display ROE
    output$profile_roeUI <- renderUI({
      r_val_local$ui_roe_profile
    })

    # checkbox display background based on classification
    output$profile_backgroundUI <- renderUI({
      r_val_local$ui_background_profile
    })


    # make plot available to other
    observe({
      r_val$plot_long_proxy <- plotlyProxy("long_profile")
    })


    ##### Metric selectors ####

    observeEvent(r_val$axis_clicked, {

      if (!is.null(r_val$axis_data) & (r_val$axis_clicked == TRUE)) {

        # build second axis input and add and remove buttons
        r_val_local$profile_first_metric = selectInput(ns("profile_first_metric"), label = "Métrique :",
                                                       choices = globals$metric_choices,
                                                       selected  = globals$metric_choices[1])

        # build second axis input and add and remove buttons
        r_val_local$add_sec_axe = checkboxInput(ns("sec_axis"),
                                                label = "Ajoutez 2éme métrique :",
                                                value = FALSE)
        r_val_local$profile_sec_metric = selectInput(ns("profile_sec_metric"), label = NULL,
                                                     choices = globals$metric_choices,
                                                     selected  = globals$metric_choices[[2]][1],
                                                     width = "100%")
      }
    })


    ##### Metric info ####

    # update infobutton when metric selected changes for the first and second metric
    observe({
      if (!is.null(input$profile_first_metric)) {
        update_popover("popover_metric",
                       HTML(globals$metrics_params %>%
                              filter(metric_name == input$profile_first_metric) %>%
                              pull(metric_description)))
      }
    })

    observe({
      if (!is.null(input$profile_sec_metric)) {
        update_popover("popover_metric2",
                       HTML(globals$metrics_params %>%
                              filter(metric_name == input$profile_sec_metric) %>%
                              pull(metric_description)))
      }
    })

    ##### plot 1st metric ####

    #### build longitudinal profile plot ####
    observeEvent(c(input$profile_first_metric, r_val$axis_data), {

      if (!is.null(input$profile_first_metric) & !is.null(r_val$axis_data)) {
        r_val_local$plot <-
          lg_profile_main(
            data = r_val$axis_data,
            y = r_val$axis_data[[input$profile_first_metric]],
            y_label = globals$metrics_params[globals$metrics_params$metric_name == input$profile_first_metric,]$metric_title,
            y_label_category = globals$metrics_params[globals$metrics_params$metric_name == input$profile_first_metric,]$metric_type_title
          ) %>%
          event_register("plotly_hover")

        # build ROE checkboxInput
        r_val_local$ui_roe_profile = NULL # delete checkbox before creating new one
        r_val_local$ui_roe_profile = checkboxInput(ns("roe_profile"),
                                                   label = "Obstacles à l'Ecoulement",
                                                   value = FALSE)

        # build background classification checkboxInput
        r_val_local$ui_background_profile = NULL # delete checkbox before creating new one
        r_val_local$ui_background_profile = checkboxInput(ns("background_profile"),
                                                          label = "Classifications en arrière-plan",
                                                          value = FALSE)
      } else {
        r_val_local$plot = lg_profile_empty()
      }
    })

  })
}
