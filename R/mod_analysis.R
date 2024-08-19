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
                   uiOutput(ns("selact_tableUI")), # overview table
                   uiOutput(ns("selact_plotUI")), # distribution plot
                 ),
                 column(
                   width = 3,
                   multiInput(
                     inputId = ns("sel_metric_select"),
                     label = "Métriques",
                     choices = params_metrics()$metric_title
                   ),
                   actionButton(inputId = ns("selact_apply_button"), "Actualiser")
                 ))),
      tabPanel("Régions",
               fluidRow(
                 column(
                   width = 9,
                   reactableOutput(ns("regions_table"), width = "100%")
                   # table and plots here
                 ),
                 column(
                   width = 3,
                   selectInput(
                     inputId = ns("regions_strahler_select"),
                     label = "Ordre de Strahler",
                     choices = c(6,5,4,3,2,1),
                     selected = c(6,5,4,3,2,1),
                     multiple = TRUE
                   ),
                   multiInput(
                     inputId = ns("regions_metric_select"),
                     label = "Métriques",
                     choices = params_metrics()$metric_title,
                     selected = c(1,2,3,4,5)
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
                     inputId = ns("axes_region_select"),
                     label = "Régions",
                     choices = c("one", "two"),
                     selected = c("one", "two"),
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("axes_strahler_select"),
                     label = "Ordre de Strahler",
                     choices = c(6,5,4,3,2,1),
                     selected = c(6,5,4,3,2,1),
                     multiple = TRUE
                   ),
                   multiInput(
                     inputId = ns("axes_metric_select"),
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
#' @importFrom reactable renderReactable reactable
#'
#' @noRd
mod_analysis_server <- function(id, con, r_val, globals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_val_local <- reactiveValues(
      # actual selection
      selact_table = NULL,
      selact_plot = NULL,

      # regions
      regions_table = NULL
    )

    # current selection table
    output$selact_tableUI <- renderUI({
      r_val_local$selact_table
    })

    # current selection distribution plot
    output$selact_plotUI <- renderUI({
      r_val_local$selact_plot
    })

    # render table for regions
    output$regions_table <- renderReactable({
      r_val_local$regions_table
    })

    # listen to changes of inputs
    # observeEvent(c(input$regions_metric_select, input$regions_strahler_select), {
    #
    #   r_val_local$regions_table <- reactable()
    # })

  })
}

## To be copied in the UI
# mod_analysis_ui("analysis_1")

## To be copied in the server
# mod_analysis_server("analysis_1")
