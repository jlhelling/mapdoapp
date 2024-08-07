#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets pickerInput pickerOptions multiInput
#' @importFrom htmltools HTML div img
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
      useShinyjs(),
      tags$head(
        tags$style(
          HTML("
          .form-group{margin-bottom: 10px}
          ")
        )
      ), # head
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = ns("level_select"),
            label = "Level",
            choices = c("France", "Bassins", "Régions", "Axe selectionné"),
            multiple = TRUE,
          )
        ),
        column(
          width = 4,
          selectInput(
            inputId = ns("strahler_select"),
            label = "Ordre de Strahler",
            choices = c(6,5,4,3,2,1),
            selected = c(6,5,4,3,2,1),
            multiple = TRUE
          )
        ),
        column(
          width = 4,
          multiInput(
            inputId = ns("metric_select"),
            label = "Métriques",
            choices = c("one", "two"),
            options = pickerOptions(
              actionsBox = TRUE,
              size = 10,
              selectedTextFormat = "count > 3",
              liveSearch = TRUE,
              selectAllText = "Tout sélectionner",
              deselectAllText = "Tout désélectionner",
            )
          )
        )
      ),
      # fluidRow(
      #   reactableOutput(ns("table"), width = "100%")
      # )
  )
}

#' analysis Server Functions
#'
#' @noRd
mod_analysis_server <- function(id, con, r_val, globals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_analysis_ui("analysis_1")

## To be copied in the server
# mod_analysis_server("analysis_1")
