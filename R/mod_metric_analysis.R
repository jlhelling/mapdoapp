
# UI ----------------------------------------------------------------------

#' metric_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom bslib accordion accordion_panel
#' @importFrom shinyjs useShinyjs
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metric_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    fluidPage(
      useShinyjs(),
      fluidRow(
        textOutput(ns("metric_placeholder_descriptionUI"))
      ),
      fluidRow(
        uiOutput(ns("metric_selectUI"))
      ),
      fluidRow(
        textOutput(ns("metric_descriptionUI"))
      ),
      fluidRow(
        uiOutput(ns("accordeonUI"))
      )
    )
  )
}


# SERVER ------------------------------------------------------------------

#' metric_analysis Server Functions
#'
#' @import shiny
#' @importFrom htmltools HTML div img
#' @importFrom dplyr filter mutate if_else pull
#' @importFrom shinyjs onclick runjs
#' @noRd
mod_metric_analysis_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ### REACTIVES ####
    r_val_local <- reactiveValues(
      metric_placeholder_description = "Cliquez sur une axe fluvial pour afficher l'analyse de métriques. ",
      ui_metric = NULL, # metric selection element
      metric_description = NULL, # information on selected metric
      accordeon_ui = NULL, # accordeon navigation element

      # plots
      violinplots_metric = NULL,
      barplots_classes_metric = NULL

    )

    ### OUTPUTS ####
    output$metric_placeholder_descriptionUI <- renderText({
      r_val_local$metric_placeholder_description
    })

    output$metric_selectUI <- renderUI({
      r_val_local$ui_metric
    })

    output$metric_descriptionUI <- renderText({
      r_val_local$metric_description
    })

    output$accordeonUI <- renderUI({
      r_val_local$accordeon_ui
    })

    output$violinplots_metricUI <- renderPlotly({
      r_val_local$violinplots_metric
    })

    observe({

      if (!is.null(r_val$axis_click)) {
        # create elements of manual grouping pane
        r_val_local$ui_metric = selectInput(ns("metric"), NULL,
                                            choices = params_get_metric_choices(),
                                            selected  = params_get_metric_choices()[5],
                                            width = "100%")

        # remove placeholder text
        r_val_local$metric_placeholder_description = NULL
      }
    })

    observeEvent(input$metric, {

      # print(params_metrics_choice_analysis()$index$metric_type_values[[input$metric]])
      # create metric description
      r_val_local$metric_description = params_metrics_info()[[input$metric]]

      # combine networks of axis and region for violinplots
      merged_network <- merge_regional_axis_dfs(r_val$network_region,
                                                r_val$dgo_axis,
                                                input$metric)

      # create plots
      r_val_local$violinplots_metric <- create_plotly_violinplot(merged_network, input$metric)

      r_val_local$accordeon_ui <- accordion(
        accordion_panel(
          "Comparaison regionale",
          fluidRow(
            column(width = 6,
                   plotlyOutput(ns("violinplots_metricUI"))),
            column(width = 6,
                   plotlyOutput(ns("barplots_classes_metricUI")))
          )
        ),
        accordion_panel(
          "Classification",
          fluidRow(
            column(width = 5,
                   fluidRow(
                     column(width = 6,
                            numericInput(inputId = ns("man_grouping_quantile"),
                                         "Quantile [%]", value = 95, min = 0, max = 100)
                     ),
                     column(width = 6,
                            numericInput(inputId = ns("man_grouping_no_classes"),
                                         "Classes", value = 4, min = 2, max = 10, step = 1)
                     ),
                     radioButtons(ns("man_grouping_scale_select"),
                                  "Base de classification",
                                  c("Région", "Axe fluvial"),
                                  selected = "Region",
                                  inline = TRUE)
                   )
            ),
            column(width = 7,
                   uiOutput(ns("man_grouping_editable_tableUI")),
                   actionButton(inputId = ns("man_grouping_apply_changes"), "Appliquer")
            )
          )
        ), open = FALSE
      )
    })

    # apply selection to global reactive values
    observeEvent(input$metric, {
      r_val$selected_metric <- input$metric
    })
  })
}
