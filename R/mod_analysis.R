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

    tabsetPanel(
      id = ns("tabset"),
      tabPanel("Sélection actuelle",
               # show table with France, basin, region (+ same stats but just for the strahler order of selected axis),
               # stats together with selected axis
               # below show distribution plots of selection
               fluidRow(
                 column(
                   width = 9,
                   # table and plots here
                 ),
                 column(
                   width = 3,
                   multiInput(
                     inputId = ns("metric_select"),
                     label = "Métriques",
                     choices = params_metrics()$metric_title
                   )
                 ))),
      tabPanel("Régions",
               fluidRow(
                 column(
                   width = 9,
                   # table and plots here
                 ),
                 column(
                   width = 3,
                   selectInput(
                     inputId = ns("strahler_select"),
                     label = "Ordre de Strahler",
                     choices = c(6,5,4,3,2,1),
                     selected = c(6,5,4,3,2,1),
                     multiple = TRUE
                   ),
                   multiInput(
                     inputId = ns("metric_select"),
                     label = "Métriques",
                     choices = params_metrics()$metric_title
                   )
                 )
               )
      ),
      tabPanel("Axes",
               fluidRow(
                 column(
                   width = 9,
                   # table and plots here
                 ),
                 column(
                   width = 3,
                   selectInput(
                     inputId = ns("region_select"),
                     label = "Régions",
                     choices = c("one", "two"),
                     selected = c("one", "two"),
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("strahler_select"),
                     label = "Ordre de Strahler",
                     choices = c(6,5,4,3,2,1),
                     selected = c(6,5,4,3,2,1),
                     multiple = TRUE
                   ),
                   multiInput(
                     inputId = ns("metric_select"),
                     label = "Métriques",
                     choices = params_metrics()$metric_title
                   )
                 )
               )
      ),
      tabPanel("Classes",
      ),
      type = "pills"
    ) #tabsetpanel
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
