#' analysis_bimetric UI Function
#'
#' @description A shiny Module.
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
        hr(),
        uiOutput(ns("entity_baseUI"))
      )
    )
  )
}

#' analysis_bimetric Server Functions
#'
#' @noRd
mod_analysis_bimetric_server <- function(id, r_val, globals){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### REACTIVES ####
    r_val_local <- reactiveValues(
      entity_basis = NULL, # selected entity basis (basin, region, axis)
      plot = NULL, # plot object
      base = NULL, #
    )

    #### UI ####

    output$long_profile <- renderPlotly({
      return(r_val_local$plot)
    })

    output$entity_baseUI <- renderUI({
      r_val_local$base
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

        if (is.null(r_val$basin_id)) {
          r_val_local$base = selectInput(ns("base_select"),
                                                   "Base de classification",
                                                   choices = c("France"),
                                                   selected = "France")

        }
        # France, Basin
        else if (!is.null(r_val$basin_id)) {
          r_val_local$base = selectInput(ns("base_select"),
                                                   "Base de classification",
                                                   choices = c("France", "Bassin"),
                                                   selected = "Bassin")
        }
        # France, basin, region
        else if (!is.null(r_val$basin_id) && !is.null(r_val$region_id)) {
          r_val_local$base = selectInput(ns("base_select"),
                                                   "Base de classification",
                                                   choices = c("France", "Bassin", "Région"),
                                                   selected = "Région")
        }
        # France, Basin, Region, Axis
        else if (!is.null(r_val$basin_id) && !is.null(r_val$region_id) && !is.null(r_val$axis_id)) {
          r_val_local$base = selectInput(ns("base_select"),
                                                   "Base de classification",
                                                   choices = c("France", "Bassin", "Région", "Axe"),
                                                   selected = "Axe")
        }
      }
    })

  })
}

## To be copied in the UI
# mod_analysis_bimetric_ui("analysis_bimetric_1")

## To be copied in the server
# mod_analysis_bimetric_server("analysis_bimetric_1")
