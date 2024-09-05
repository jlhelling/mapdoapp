#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets pickerInput pickerOptions multiInput materialSwitch
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
                 textOutput(ns("selection_textUI")),
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
                     choices = params_metrics()$metric_title,
                     selected = params_metrics()$metric_title[1:5]
                   ),
                   materialSwitch(
                     inputId = ns("sel_strahler_switch"),
                     label = "Statistiques de l'ordre de Strahler selon l'axe",
                     value = FALSE,
                     status = "primary",
                     inline = TRUE
                   ),
                   actionButton(inputId = ns("selact_apply_button"), "Actualiser")
                 ))),
      tabPanel("Régions",
               fluidRow(
                 column(
                   width = 9,
                   reactableOutput(ns("regions_table"), width = "100%"),
                   plotlyOutput(ns("regions_plotUI"), width = "100%")
                 ),
                 column(
                   width = 3,
                   selectInput(
                     inputId = ns("regions_strahler_select"),
                     label = "Ordre de Strahler",
                     choices = c(6,5,4,3,2,1),
                     selected = NULL,
                     multiple = TRUE
                   ),
                   multiInput(
                     inputId = ns("regions_metric_select"),
                     label = "Métriques",
                     choiceNames = params_metrics()$metric_title,
                     choiceValues = params_metrics()$metric_name,
                     selected = params_metrics()$metric_name[1:5]
                   ),
                   actionButton(inputId = ns("regions_apply_button"), "Actualiser")
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
      regions_table = NULL, # reactable
      region_stats_prep = NULL, # prepared stats for reactable
      regions_plot = NULL
    )

    #### Description Text ####

    output$selection_textUI <- renderText({
      r_val$selection_text
    })

    ### UI ####

    ##### Selection tab ####

    # current selection table
    output$selact_tableUI <- renderUI({
      r_val_local$selact_table
    })

    # current selection distribution plot
    output$selact_plotUI <- renderPlotly({
      r_val_local$selact_plot
    })

    ##### Regions tab ####
    # render table for regions
    output$regions_table <- renderReactable({
      r_val_local$regions_table
    })

    # render regions plot
    output$regions_plotUI <- renderPlotly({
      r_val_local$regions_plot
    })


    ### Observers ####

    ##### Regions tab ####
    # listen to opening of tab --> load stats if not already loaded, prepare them for reactable
    observe({
      if (exists("metric_stats", where = globals)) {

        # prepare stats for reactable
        r_val_local$region_stats_prep = prepare_stats_df(globals$metric_stats(), type = c("Région (total)", "Région"))
        r_val_local$regions_table = create_table(r_val_local$region_stats_prep, params_metrics()$metric_name[1:5], 0)
        if (!is.null(globals$classes_stats())) {
          r_val_local$regions_plot = analysis_plot_classes_distr(data = globals$classes_stats(),
                                                                 region_id = globals$regions[globals$regions$click==TRUE,]$gid,
                                                                 region_names = globals$regions)
        }
      }
    })


    # listen to actualisation button --> create reactable with selected metric and strahler order
    observeEvent(input$regions_apply_button, {


      # check if stats are already loaded and if metric is selected
      if (!is.null(r_val_local$region_stats_prep) && !is.null(input$regions_metric_select)){

        # check if strahler order is selected or whole region should be shown
        if (is.null(input$regions_strahler_select)){
          strahler_sel <- 0
        } else {
          strahler_sel <- input$regions_strahler_select
        }

        # create reactable table
        r_val_local$regions_table = create_table(r_val_local$region_stats_prep, input$regions_metric_select, strahler_sel)
      }

    })

  })
}

## To be copied in the UI
# mod_analysis_ui("analysis_1")

## To be copied in the server
# mod_analysis_server("analysis_1")
