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
        style = "margin-top: 10px;",
        textOutput(ns("placeholder_ui")),
        column(width = 8,
               plotlyOutput(ns("barplots_classes_metricUI"))
               )
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
      barplots_classes_metric = NULL,
      placeholder_text = "Sélectionnez un cours d'eau sur la carte et appliquez une classification pour afficher le graphique."
    )

    # text placeholder
    output$placeholder_ui <- renderText({
      r_val_local$placeholder_text
    })

    # barplots showing distribution of classes
    output$barplots_classes_metricUI <- renderPlotly({
      r_val_local$barplots_classes_metric
    })

    # create barplots of classes distribution
    observe({

      if (!is.null(r_val$merged_networks_classified)) {
        r_val_local$barplots_classes_metric <- create_plotly_barplot(r_val$merged_networks_classified)
        r_val_local$placeholder_text = NULL
        } else {
          r_val_local$placeholder_text = "Sélectionnez un cours d'eau sur la carte et appliquez une classification pour afficher le graphique."
          r_val_local$barplots_classes_metric = NULL
        }
    })



  })
}

