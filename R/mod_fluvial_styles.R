#' fluvial_styles UI Function
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
mod_fluvial_styles_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      useShinyjs(),
      fluidRow(
        uiOutput(ns("apply_buttonUI"))
      ),
      fluidRow(
        reactableOutput(ns("table"), width = "100%")
      )
    )
  )
}

#' fluvial_styles Server Functions
#'
#' @import shiny
#' @importFrom reactable renderReactable getReactableState
#'
#' @noRd
mod_fluvial_styles_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_val_local <- reactiveValues(
      apply_button = NULL,
      selected = NULL,
      classes_tbl = NULL
    )



    # apply button
    output$apply_buttonUI <- renderUI(
      r_val_local$apply_button
    )

    output$table <- renderReactable(
      r_val_local$table
    )

    # build table when region clicked
    observeEvent(r_val$region_click, {

      if (!is.null(r_val$region_click)) {
        r_val_local$classes_tbl <- params_classes()
        r_val_local$table = create_table_fluvialstyles(r_val_local$classes_tbl)
      }
    })


    # create table output
    observeEvent(input$table__reactable__selected, {
      r_val_local$selected <- getReactableState("table", "selected")

      r_val$sld_body = r_val_local$classes_tbl[r_val_local$selected,]$class_sld

      r_val$map_proxy %>%
        map_class(wms_params = params_wms()$class,
                   cql_filter = paste0("gid_region=",r_val$selected_region_feature[["gid"]]),
                   sld_body = r_val$sld_body,
                   data_axis = r_val$network_region_axis) %>%
        addWMSLegend(uri = map_legend_metric(sld_body = r_val$sld_body),
                     position = "bottomright",
                     layerId = "legend_metric")

      print(r_val_local$selected)
    })


    # check if other visualization is applied to map and create button to re-apply fluvial styles
    observeEvent(r_val$visualization, {
      if (r_val$visualization != "classes") {
        r_val_local$apply_button = actionButton(ns("apply_button"), label = "Remettre la visualisation")
      } else {
        r_val_local$apply_button = NULL
      }
    })

    # check if apply button clicked
    observeEvent(input$apply_button, {
      r_val$visualization = "classes"
      r_val_local$apply_button = NULL
    })
  })
}
