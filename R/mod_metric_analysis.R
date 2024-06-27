
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
        style = "margin-top: 20px;",
        textOutput(ns("metric_placeholder_descriptionUI"))
      ),
      fluidRow(
        uiOutput(ns("metric_selectUI"))
      ),
      fluidRow(
        textOutput(ns("metric_descriptionUI"))
      ),
      fluidRow(
        uiOutput(ns("classificationUI"))
      )
    )
  )
}


# SERVER ------------------------------------------------------------------

#' metric_analysis Server Functions
#'
#' @import shiny
#' @importFrom htmltools HTML div img
#' @importFrom dplyr filter mutate if_else pull bind_cols arrange add_row
#' @importFrom tibble tibble
#' @importFrom bslib sidebar page_sidebar
#' @importFrom leaflet removeControl clearGroup
#' @importFrom leaflet.extras addWMSLegend
#' @importFrom shinyjs onclick runjs
#' @importFrom colourpicker colourInput
#' @noRd
mod_metric_analysis_server <- function(id, con, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ### REACTIVES ####
    r_val_local <- reactiveValues(
      metric_placeholder_description = "Cliquez sur une axe fluvial pour afficher l'analyse de métriques. ",
      ui_metric = NULL, # metric selection element
      metric_description = NULL, # information on selected metric

      # plots
      barplots_classes_metric = NULL,

      # classification
      classification_ui = NULL, # UI placeholder
      man_grouping_editable_table = NULL, # table which reacts on user input and datainput
      initial_classes_table = NULL, # datainput for table
      classes_table = NULL, # dataoutput from table for classifications

      reactableUI = NULL # reactable table
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

    output$classificationUI <- renderUI({
      r_val_local$classification_ui
    })

    # barplots showing distribution of classes
    output$barplots_classes_metricUI <- renderPlotly({
      r_val_local$barplots_classes_metric
    })

    # reactable table of classes
    output$reactable_classes <- renderUI({
      r_val_local$reactableUI
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
                                            selected  = params_get_metric_choices()[1],
                                            width = "100%")

        # remove placeholder text
        r_val_local$metric_placeholder_description = NULL
      }
    })

    #### metric changed ####
    observeEvent(input$metric, {

      # set metric names and info
      r_val$selected_metric = input$metric
      r_val$selected_metric_title =
        params_metrics() |> filter(metric_name == r_val$selected_metric) |> pull(metric_title)
      r_val$selected_metric_type =
        params_metrics() |> filter(metric_name == r_val$selected_metric) |> pull(metric_type_title)
      r_val$selected_metric_description =
        params_metrics() |> filter(metric_name == r_val$selected_metric) |> pull(metric_description)

      # create metric description
      r_val_local$metric_description = r_val$selected_metric_description

      # combine networks of axis and region for violinplots
      merged_network <- merge_regional_axis_dfs(r_val$network_region,
                                                r_val$dgo_axis,
                                                input$metric)


      r_val_local$classification_ui <- fluidPage(
        fluidRow(
          page_sidebar(
          sidebar = sidebar(
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
                                           inline = TRUE),
                              actionButton(inputId = ns("recalculate_classes_button"), "Recalculer classes")
                            ), open = "closed", width = 240, position = "right"
          ),
          uiOutput(ns("reactable_classes")),
          actionButton(inputId = ns("apply_to_map_button"), "Ajouter à la carte")
        )),
        fluidRow(
          plotlyOutput(ns("barplots_classes_metricUI"))
        )

      )
    })

    # #### classification inputs changed ####
    # observeEvent(list(input$man_grouping_quantile,
    #                   input$man_grouping_no_classes,
    #                   input$man_grouping_scale_select
    # ), {
    #
    #   # track input
    #   track_inputs(input = input)
    #
    #   # check for valid values
    #   if (!is.null(input$metric) &
    #       !is.null(input$man_grouping_scale_select) &
    #       !is.null(input$man_grouping_quantile) &
    #       !is.null(input$man_grouping_no_classes)) {
    #
    #     if ((input$man_grouping_scale_select == "Région") &
    #         !is.null(r_val$network_region)) {
    #
    #       # create classes-table
    #       r_val_local$initial_classes_table = create_df_input(
    #         axis_data = r_val$network_region,
    #         variable_name = input$metric,
    #         no_classes = input$man_grouping_no_classes,
    #         quantile = input$man_grouping_quantile
    #       )
    #     }
    #
    #     else if (input$man_grouping_scale_select == "Axe fluvial" &
    #              !is.null(r_val$dgo_axis) ) {
    #
    #       # create classes-table
    #       r_val_local$initial_classes_table = create_df_input(
    #         axis_data = r_val$dgo_axis,
    #         variable_name = input$metric,
    #         no_classes = input$man_grouping_no_classes,
    #         quantile = input$man_grouping_quantile
    #       )
    #     }
    #   }
    # })

    #### recalculate classes ####
    # when button clicked or metric selection changed
    observeEvent(c(input$metric, input$recalculate_classes_button), {

      # track input
      track_inputs(input = input)

      r_val_local$barplots_classes_metric = NULL

      # check for valid values
      if (!is.null(input$metric) &
          !is.null(input$man_grouping_scale_select) &
          !is.null(input$man_grouping_quantile) &
          !is.null(input$man_grouping_no_classes)) {

        if ((input$man_grouping_scale_select == "Région") &
            !is.null(r_val$network_region)) {

          # create classes-table
          r_val_local$initial_classes_table = create_df_input(
            axis_data = r_val$network_region,
            variable_name = input$metric,
            no_classes = input$man_grouping_no_classes,
            quantile = input$man_grouping_quantile
          )
        }

        else if (input$man_grouping_scale_select == "Axe fluvial" &
                 !is.null(r_val$dgo_axis) ) {

          # create classes-table
          r_val_local$initial_classes_table = create_df_input(
            axis_data = r_val$dgo_axis,
            variable_name = input$metric,
            no_classes = input$man_grouping_no_classes,
            quantile = input$man_grouping_quantile
          )
        }
      }
    })


    #### UI classes creation ####
    # update input fields for classification when setting the variables in the UI
    observeEvent(r_val_local$initial_classes_table, {

      if (!is.null(r_val_local$initial_classes_table)) {

        # create classes UI
        r_val_local$reactableUI =
          renderUI({
            # Start with a list to collect UI elements
            ui_elements <- list()

            # Add the first row of inputs
            ui_elements[[1]] <- fluidRow(
              column(width = 5, textInput(ns("class1"), label = "Classe", value = r_val_local$initial_classes_table$class[1])),
              column(width = 4, numericInput(ns("greaterthan1"), label = "supérieur à", value = r_val_local$initial_classes_table$greaterthan[1])),
              column(width = 3, colourInput(ns("color1"), label = "Couleur",
                                            value = r_val_local$initial_classes_table$color[1],
                                            showColour = "background", closeOnClick = TRUE))
            )

            # Loop through the remaining rows and add inputs
            for (row in 2:nrow(r_val_local$initial_classes_table)) {
              ui_elements[[row]] <- fluidRow(
                column(width = 5, textInput(ns(paste0("class", row)), label = NULL, value = r_val_local$initial_classes_table$class[row])),
                column(width = 4, numericInput(ns(paste0("greaterthan", row)), label = NULL, value = r_val_local$initial_classes_table$greaterthan[row])),
                column(width = 3, colourInput(ns(paste0("color", row)), label = NULL,
                                              value = r_val_local$initial_classes_table$color[row],
                                              showColour = "background", closeOnClick = TRUE))
              )
            }

            # Return the list of UI elements wrapped in tagList
            do.call(tagList, ui_elements)
          })
      }
    })

    #### apply-to-map button clicked ####
    observeEvent(input$apply_to_map_button,{

      r_val$visualization = "metric"

      # create classes table from input
      # Initialize an empty tibble
      r_val_local$classes_table <- tibble(variable = character(), class = character(), greaterthan = numeric(), color = character())

      # Add rows to the tibble within a loop
      for (row in 1:input$man_grouping_no_classes) {
        r_val_local$classes_table <- r_val_local$classes_table %>%
          add_row(variable = input$metric,
                  class = input[[paste0("class", row)]],
                  greaterthan = input[[paste0("greaterthan", row)]],
                  color = input[[paste0("color", row)]])
      }

      # sort classes
      classes <- r_val_local$classes_table %>%
        dplyr::arrange(greaterthan) %>%
        dplyr::mutate(greaterthan = round(greaterthan, 2))

      # build SLD symbology
      r_val$sld_body = sld_get_style(
        breaks = classes$greaterthan,
        colors = classes$color,
        metric = r_val$selected_metric
      )

      # add classified network to map
      r_val$map_proxy %>%
        clearGroup(params_map_group()$dgo_axis) %>%
        removeControl(layerId = "legend_metric") %>%
        map_metric(wms_params = params_wms()$metric,
                   cql_filter = paste0("gid_region=",r_val$selected_region_feature[["gid"]]),
                   sld_body = r_val$sld_body,
                   data_axis = r_val$network_region_axis) %>%
        addWMSLegend(uri = map_legend_metric(sld_body = r_val$sld_body),
                     position = "bottomright",
                     layerId = "legend_metric")

      # classify and merge networks
      # Create classified network by adding the classes and colors
      classified_network <- r_val$network_region %>%
        assign_classes(classes = r_val_local$classes_table)

      # create classified axis network
      classified_axis <- r_val$dgo_axis %>%
        na.omit() %>%
        assign_classes(classes = r_val_local$classes_table)

      # merge regional and axis network in one df
      merged_network_classified <- merge_regional_axis_dfs(classified_network,
                                                           classified_axis,
                                                           r_val$selected_metric,
                                                           classes = TRUE)

      # create barplots of classes distribution
      r_val_local$barplots_classes_metric <- create_plotly_barplot(merged_network_classified)


      # add data to longitudinal plot
      r_val$plot_long_proxy %>%
        plotlyProxyInvoke("relayout", list(shapes = create_classes_background(classified_axis)))

    })
  })
}
