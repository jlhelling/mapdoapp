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
#' @importFrom plotly plotlyOutput
#' @importFrom reactable reactableOutput
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
      tabPanel("Comparaison Sélection actuelle",
               # show table with France, basin, region (+ same stats but just for the strahler order of selected axis),
               # stats together with selected axis
               # below show distribution plots of selection
               fluidRow(
                 style = "margin-top: 10px; margin-bottom: 10px; margin-left: 10px;",
                 textOutput(ns("selection_textUI")),
                 column(
                   width = 9,
                   hr(), # horizontal line
                   uiOutput(ns("selact_tableUI")), # overview table
                   hr(), # horizontal line
                   fluidRow(style = "margin-top: 20px;"),
                   plotlyOutput(ns("selact_plotUI")), # distribution plot
                 ),
                 column(
                   width = 3,
                   multiInput(
                     inputId = ns("selact_metric_select"),
                     label = "Métriques",
                     choiceNames = params_metrics()$metric_title,
                     choiceValues = params_metrics()$metric_name,
                     selected = params_metrics()$metric_name[1:5]
                   ),
                   selectInput(
                     inputId = ns("selact_strahler_select"),
                     label = "Ordre de Strahler",
                     choices = setNames(c(6,5,4,3,2,1,0), c("6","5","4","3","2","1","tous ensemble")),
                     selected = 0,
                     multiple = TRUE
                   ),
                   actionButton(inputId = ns("selact_apply_button"), "Actualiser")
                 ))),
      tabPanel("Comparaison des Régions",
               fluidRow(
                 style = "margin-top: 10px; margin-bottom: 10px; margin-left: 10px;",
                 column(
                   width = 9,
                   hr(), # horizontal line
                   reactableOutput(ns("regions_table"), width = "100%"),
                   hr(),
                   fluidRow(style = "margin-top: 20px;"),
                   plotlyOutput(ns("regions_plotUI"), width = "100%")
                 ),
                 column(
                   width = 3,
                   multiInput(
                     inputId = ns("regions_metric_select"),
                     label = "Métriques",
                     choiceNames = params_metrics()$metric_title,
                     choiceValues = params_metrics()$metric_name,
                     selected = params_metrics()$metric_name[1:5]
                   ),
                   selectInput(
                     inputId = ns("regions_strahler_select"),
                     label = "Ordre de Strahler",
                     choices = setNames(c(6,5,4,3,2,1,0), c("6","5","4","3","2","1","tous ensemble")),
                     selected = 0,
                     multiple = TRUE
                   ),
                   actionButton(inputId = ns("regions_apply_button"), "Actualiser")
                 )
               )
      ),
      type = "pills"
    ) #tabsetpanel
  )
}

#' analysis Server Functions
#'
#' @importFrom reactable renderReactable reactable
#' @importFrom plotly renderPlotly
#' @import shiny
#'
#' @noRd
mod_analysis_server <- function(id, con, r_val, globals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_val_local <- reactiveValues(
      # actual selection
      selact_stats_prep = NULL,
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

    ##### Current Selection ####

    observeEvent(c(globals$classes_stats(), r_val$basin_id, r_val$axis_strahler,
                   r_val$region_id, r_val$axis_data_classified), {

                     if (exists("classes_stats", where = globals) && exists("metric_stats", where = globals)) {
                       if (!is.null(globals$classes_stats()) && !is.null(globals$metric_stats())) {

                         # check if strahler order is selected or whole dataset should be shown
                         if (!is.null(input$selact_strahler_select)  && !is.null(r_val$basin_id)) {
                           strahler_sel <- input$selact_strahler_select }
                         else if (!is.null(input$selact_strahler_select) && is.null(r_val$basin_id)) {
                           strahler_sel <- c("6", "5", "4", "3", "2", "1", "0") }
                         else { strahler_sel <- "0" }

                         # get dataset of actual selection
                         r_val_local$selact_stats_prep = prepare_selact_stats_for_table(globals$metric_stats(),
                                                                                        basin_id = r_val$basin_id,
                                                                                        region_id = r_val$region_id,
                                                                                        axis_data = r_val$axis_data_classified)

                         # create table
                         r_val_local$selact_table = create_analysis_table(
                           r_val_local$selact_stats_prep  %>%
                             filter(strahler %in% strahler_sel),
                           params_metrics()$metric_name[1:5],
                           scale_name = "Sélection"
                         )

                         # current selection
                         r_val_local$selact_plot = analysis_plot_classes_distr(
                           df = prepare_selact_data_for_plot(globals$classes_stats(),
                                                             basin_id = r_val$basin_id,
                                                             region_id = r_val$region_id,
                                                             strahler = strahler_sel,
                                                             axis_data = r_val$axis_data_classified)
                         )
                       }
                     }
                   })


    # listen to actualization button --> create reactable with selected metric and strahler order
    observeEvent(c(input$selact_apply_button, r_val_local$selact_stats_prep),{

      # check if stats are already loaded and if metric is selected
      if (!is.null(r_val_local$selact_stats_prep) && !is.null(input$selact_metric_select)) {

        # check if strahler order is selected or whole dataset should be shown
        if (is.null(input$selact_strahler_select) ) { strahler_sel <- "0" }
        else { strahler_sel <- input$selact_strahler_select}


        # update reactable table and plot
        r_val_local$selact_table = create_analysis_table(r_val_local$selact_stats_prep %>%
                                                           filter(strahler %in% strahler_sel | name == "Axe"),
                                                         input$selact_metric_select, scale_name = "Sélection")
        # current selection
        r_val_local$selact_plot = analysis_plot_classes_distr(
          df = prepare_selact_data_for_plot(globals$classes_stats(),
                                            basin_id = r_val$basin_id,
                                            region_id = r_val$region_id,
                                            strahler = strahler_sel,
                                            axis_data = r_val$axis_data_classified)
        )
      }

    })



    ##### Regions tab ####
    # listen to opening of tab --> load stats if not already loaded, prepare them for reactable
    observe({
      if (exists("metric_stats", where = globals)) {



        # prepare stats for reactable
        r_val_local$region_stats_prep = prepare_regions_stats_for_table(globals$metric_stats(),
                                                                        region_names = globals$regions)
        r_val_local$regions_table = create_analysis_table(r_val_local$region_stats_prep %>%
                                                            filter(strahler == 0),
                                                          params_metrics()$metric_name[1:5],
                                                          scale_name = "Région")
      }
    })

    # plot initialisation
    observeEvent(globals$classes_stats(), {
      if (!is.null(globals$classes_stats())) {

        # check if strahler order is selected or whole region should be shown
        if (is.null(input$regions_strahler_select) ) { strahler_sel <- 0 }
        else { strahler_sel <- input$regions_strahler_select}

        dfset <- prepare_regions_data_for_plot(globals$classes_stats(),
                                               region_id = globals$regions[globals$regions$click == TRUE,]$gid,
                                               region_strahler = strahler_sel,
                                               region_names = globals$regions)
        # regions plot
        r_val_local$regions_plot = analysis_plot_classes_distr(
          df = dfset
        )
      }
    })

    # listen to actualisation button --> create reactable with selected metric and strahler order
    observeEvent(input$regions_apply_button, {

      # check if stats are already loaded and if metric is selected
      if (!is.null(r_val_local$region_stats_prep) && !is.null(input$regions_metric_select)){

        # check if strahler order is selected or whole dataset should be shown
        if (is.null(input$regions_strahler_select) ) { strahler_sel <- "0" }
        else { strahler_sel <- input$regions_strahler_select }

        # update reactable table and plot
        r_val_local$regions_table = create_analysis_table(r_val_local$region_stats_prep %>%
                                                            filter(strahler %in% strahler_sel),
                                                          input$regions_metric_select,
                                                          scale_name = "Région")
        r_val_local$regions_plot = analysis_plot_classes_distr(
          df = prepare_regions_data_for_plot(globals$classes_stats(),
                                             region_id = globals$regions[globals$regions$click == TRUE,]$gid,
                                             region_strahler = strahler_sel,
                                             region_names = globals$regions)
        )
      }

    })

  })
}

## To be copied in the UI
# mod_analysis_ui("analysis_1")

## To be copied in the server
# mod_analysis_server("analysis_1")
