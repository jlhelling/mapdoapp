#' classes_distribution UI Function
#'
#' @description A shiny Module.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom plotly plotlyOutput
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_classes_distribution_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      useShinyjs(),
      fluidRow(
        plotlyOutput(ns("barplots_classes_metricUI"))
      )
    )
  )
}

#' classes_distribution Server Functions
#'
#' @import shiny
#' @importFrom plotly renderPlotly
#' @noRd
mod_classes_distribution_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_val_local <- reactiveValues(

      barplots_classes_metric = NULL

    )


    # barplots showing distribution of classes
    output$barplots_classes_metricUI <- renderPlotly({
      r_val_local$barplots_classes_metric
    })

    # create barplots of classes distribution
    observeEvent(r_val$merged_networks_classified , {

      if (!is.null(r_val$merged_networks_classified)) {
        r_val_local$barplots_classes_metric <- create_plotly_barplot(r_val$merged_networks_classified)
      }
    })



  })
}

