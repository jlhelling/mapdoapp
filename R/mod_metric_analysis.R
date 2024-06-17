
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
#' @importFrom dplyr filter mutate if_else pull bind_cols
#' @importFrom rhandsontable rHandsontableOutput rhandsontable hot_context_menu renderRHandsontable hot_to_r
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
      barplots_classes_metric = NULL,

      # classification
      # grouping
      man_grouping_editable_table = NULL,
      grouping_table_data = NULL
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

    # editable table for classification
    output$man_grouping_editable_tableUI <- renderUI({
      r_val_local$man_grouping_editable_table
    })


    ### EVENTS ####

    #### axis clicked ####
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

    #### metric changed ####
    observeEvent(input$metric, {

      r_val$selected_metric <- input$metric

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
                                  selected = "Région",
                                  inline = TRUE)
                   )
            ),
            column(width = 7,
                   rHandsontableOutput(ns("man_grouping_editable_table"), width = "100%"),
                   actionButton(inputId = ns("man_grouping_apply_changes"), "Appliquer")
            )
          )
        ), open = FALSE
      )
    })

    #### classification inputs changed ####
    observeEvent(list(input$man_grouping_quantile,
                      input$man_grouping_no_classes,
                      input$man_grouping_scale_select
    ), {

      # track input
      track_inputs(input = input)

      # check for valid values
      if (!is.null(input$metric) &
          !is.null(input$man_grouping_scale_select) &
          !is.null(input$man_grouping_quantile) &
          !is.null(input$man_grouping_no_classes)) {

        if ((input$man_grouping_scale_select == "Région") &
            !is.null(r_val$network_region)) {

          # create classes-table
          r_val_local$grouping_table_data = create_df_input(
            axis_data = r_val$network_region,
            variable_name = input$metric,
            no_classes = input$man_grouping_no_classes,
            quantile = input$man_grouping_quantile
          )
        }

        else if (input$man_grouping_scale_select == "Axe fluvial" &
                 !is.null(r_val$dgo_axis) ) {

          # create classes-table
          r_val_local$grouping_table_data = create_df_input(
            axis_data = r_val$dgo_axis,
            variable_name = input$metric,
            no_classes = input$man_grouping_no_classes,
            quantile = input$man_grouping_quantile
          )
        }
      }
    })

    # update table when values are edited (either via editing the table or setting the variables in the UI)
    observeEvent(r_val_local$grouping_table_data, {

      if (!is.null(r_val_local$grouping_table_data)) {
        output$man_grouping_editable_table <- renderRHandsontable({
          tmp <- isolate(r_val_local$grouping_table_data %>% select(!variable))# Gotta isolate it or it'll cause infinite loop, see https://github.com/jrowen/rhandsontable/issues/166
          rownames(tmp) <- NULL
          rhandsontable( tmp, rowHeaders = NULL) %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        })
      }
    })

    # Update the reactive values when user edits table in the UI
    observeEvent(input$man_grouping_editable_table, {

      r_val_local$grouping_table_data <- hot_to_r(input$man_grouping_editable_table) %>%
        bind_cols(
          r_val_local$grouping_table_data %>%
            select(variable)
        )
    })


  })
}
