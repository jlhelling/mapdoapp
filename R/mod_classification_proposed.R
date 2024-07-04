#' classification_proposed UI Function
#'
#' @description A shiny Module.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom reactable reactableOutput
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_classification_proposed_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      useShinyjs(),
      tags$script(HTML("
    Shiny.addCustomMessageHandler('deselectRow', function(message) {
      var table = document.getElementById(message.id);
      if (table) {
        // Reactable uses class names to mark selected rows
        var selectedRows = table.querySelectorAll('.rt-tr.selected');
        selectedRows.forEach(function(row) {
          row.classList.remove('selected');
        });
      }
    });
  ")),
      fluidRow(
        reactableOutput(ns("table"), width = "100%")
      )
    )
  )
}

#' classification_proposed Server Functions
#'
#' @import shiny
#' @importFrom reactable renderReactable getReactableState updateReactable
#'
#' @noRd
mod_classification_proposed_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_val_local <- reactiveValues(
      selected = NULL,
      classes_tbl = NULL,
      table = NULL
    )

    output$table <- renderReactable(
      r_val_local$table
    )

    # build table when region first time clicked
    observeEvent(r_val$region_clicked, {

      if (!is.null(r_val$region_click)) {
        r_val_local$classes_tbl <- params_classes()
        r_val_local$table = create_table_fluvialstyles(r_val_local$classes_tbl)
      }
    })
#
#     # build table when region clicked
#     observeEvent(r_val$region_click, {
#
#       if (!is.null(r_val$region_click) & (r_val$visualization == "classes")) {
#         r_val_local$classes_tbl <- params_classes()
#       }
#     })


    # create table output and add classification to map when region changed or other variable selected
    observeEvent(c(input$table__reactable__selected, r_val$region_click), {

    r_val_local$selected <- getReactableState("table", "selected")

    # check if row is actually selected
    if(!is.null(r_val_local$selected)) {

      r_val$visualization = "classes"

      r_val$sld_body = r_val_local$classes_tbl[r_val_local$selected,]$class_sld

      r_val$map_proxy %>%
        map_class(wms_params = params_wms()$class,
                  cql_filter = paste0("gid_region=",r_val$selected_region_feature[["gid"]]),
                  sld_body = r_val$sld_body,
                  data_axis = r_val$network_region_axis) %>%
        addWMSLegend(uri = map_legend_metric(sld_body = r_val$sld_body),
                     position = "bottomright",
                     layerId = "legend_metric")

    }
    })


    # TODO --------------------------------------------------------------------


    # # check if dgo axis clicked
    #
    # # classify and merge networks
    # # Create classified network by adding the classes and colors
    # r_val$network_region_classified <- r_val$network_region %>%
    #   assign_classes(classes = r_val_local$classes_table)
    #
    # # create classified axis network
    # r_val$dgo_axis_classified <- r_val$dgo_axis %>%
    #   na.omit() %>%
    #   assign_classes(classes = r_val_local$classes_table)
    #
    # # merge regional and axis network in one df
    # r_val$merged_networks_classified <- merge_regional_axis_dfs(r_val$network_region_classified,
    #                                                             r_val$dgo_axis_classified,
    #                                                             r_val$selected_metric,
    #                                                             classes = TRUE)


    # check if other visualization is applied to map and de-select all proposed classifications
    observeEvent(r_val$visualization, {
      if (r_val$visualization != "classes") {
        updateReactable("table", selected = NA)
      }
    })
  })
}
