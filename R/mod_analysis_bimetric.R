#' analysis_bimetric UI Function
#'
#' @description A shiny Module.
#'
#' @import shiny
#' @import shinyWidgets
#' @importFrom plotly plotlyOutput
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_bimetric_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      style = "margin-top: 10px; margin-bottom: 10px; margin-left: 10px;",
      column(
        width = 9,
        hr(),
        plotlyOutput(ns("plot")),
        hr(),
        uiOutput(ns("linear_dependency_text"), style = "margin-top: 10px;  margin-left: 20px;")
      ),
      column(
        width = 3,
        div(
          style = "display: flex; align-items: center",
          selectInput(ns("x_metric"), label = "Métrique X:",
                      choices = globals$metric_choices,
                      selected  = globals$metric_choices[1]),
          span(
            style = "display: flex; margin-left: 10px; margin-top: 20px",
            popover(
              trigger = bsicons::bs_icon("info-circle"),
              "",
              placement = "right",
              id = ns("popover_metric_x")
            )
          )
        ),
        div(
          style = "display: flex; align-items: center",
          selectInput(ns("y_metric"), label = "Métrique Y:",
                      choices = globals$metric_choices,
                      selected  = globals$metric_choices[2]),
          span(
            style = "display: flex; margin-left: 10px; margin-top: 20px",
            popover(
              trigger = bsicons::bs_icon("info-circle"),
              "",
              placement = "right",
              id = ns("popover_metric_y")
            )
          )
        ),
        checkboxInput(ns("apply_classes"), "Colorier par classification", value = FALSE),
        hr(),
        checkboxInput(ns("apply_lm"), "Appliquer régression linéaire", value = FALSE),
        uiOutput(ns("entity_baseUI")),
        actionButton(ns("create_plot"), "Créer graphe !", icon = icon("chart-line"))
      )
    )
  )
}

#' analysis_bimetric Server Functions
#'
#' @import shiny
#' @importFrom echarts4r renderEcharts4r
#' @noRd
mod_analysis_bimetric_server <- function(id, con, r_val, globals){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### REACTIVES ####
    r_val_local <- reactiveValues(
      entity_basis = NULL, # selected entity basis (basin, region, axis)
      plot = NULL, # plot object
      base = NULL, #
      linear_dependency_text = "" # text describing linear dependency
    )

    #### UI ####

    output$plot <- renderPlotly({
      r_val_local$plot
    })

    output$entity_baseUI <- renderUI({
      r_val_local$base
    })

    output$linear_dependency_text <- renderText({
      r_val_local$linear_dependency_text
    })


    ##### Metric info ####

    # update infobutton when metric selected changes for the first and second metric
    observe({
      if (!is.null(input$x_metric)) {
        update_popover("popover_metric_x",
                       HTML(globals$metrics_params %>%
                              filter(metric_name == input$x_metric) %>%
                              pull(metric_description)))
      }
    })

    observe({
      if (!is.null(input$y_metric)) {
        update_popover("popover_metric_y",
                       HTML(globals$metrics_params %>%
                              filter(metric_name == input$y_metric) %>%
                              pull(metric_description)))
      }
    })

    #### ENTITY BASE UI ####
    observeEvent(c(r_val$basin_id, r_val$region_id, r_val$axis_id, r_val$tab_analysis), {

      # france, basin, region, axis as basis possible !
      # add info-button

      if (r_val$tab_analysis == "Analyse Bimétrique") {

        if (is.null(r_val$region_id)) {
          r_val_local$base = "Selectionnez une région oú axe"

        }
        # France, basin, region
        else if (!is.null(r_val$region_id) && is.null(r_val$axis_id)) {
          r_val_local$base = selectInput(ns("base_select"),
                                         "Base",
                                         choices = c("Région"),
                                         selected = "Région")
        }
        # France, Basin, Region, Axis
        else if (!is.null(r_val$axis_id)) {
          r_val_local$base = selectInput(ns("base_select"),
                                         "Base",
                                         choices = c("Région", "Axe"),
                                         selected = "Axe")
        }
      }
    })

    # create plot
    observeEvent(input$create_plot, {

      if (!is.null(r_val$region_id)) {

        browser()

        if (input$base_select == "Région") {

          r_val_local$plot <- create_analysis_biplot(df = globals$region_data(), metric_x = input$x_metric, metric_y = input$y_metric, classes = input$apply_classes, lm = input$apply_lm)
        }

        else if (input$base_select == "Axe") {

          r_val_local$plot <- create_analysis_biplot(df = r_val$axis_data_classified, metric_x = input$x_metric, metric_y = input$y_metric, classes = input$apply_classes, lm = input$apply_lm)
        }

      }
    })

  })
}
