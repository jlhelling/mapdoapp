

# UI ----------------------------------------------------------------------

#' profil_long UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_profil_long_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidPage(
      useShinyjs(),
      div(
        fluidRow(
          column(width = 10,
                 plotlyOutput(ns("long_profile"))
          ),
          column(
            width = 2,
            style = "margin-top: 20px;",
            uiOutput(ns("profile_first_metricUI")),
            uiOutput(ns("add_sec_axeUI"),
                     style = "margin-top: 30px;"),
            uiOutput(ns("profile_sec_metricUI")),
            uiOutput(ns("profileroeUI")),
            uiOutput(ns("profile_backgroundUI"))
          )
        )
      )
    )
  )
}


# Server ------------------------------------------------------------------

#' profil_long Server Functions
#'
#' @importFrom plotly event_register plotlyOutput renderPlotly event_data
#' @importFrom leaflet addPolylines clearGroup
#'
#' @noRd
mod_profil_long_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### INITIALIZATION ####

    #### REACTIVES ####
    r_val_local <- reactiveValues(
      plot = lg_profile_empty(), # plotly render longitudinal profile output (default empty)
      leaflet_hover_shapes = NULL, # list to store vertical lines to display on longitudinal profile
      ui_roe_profile = NULL, # UI placeholder for ROE checkbox
      ui_background_profile = NULL, # UI placeholder for background classes checkbox
      roe_vertical_line = NULL, # list with verticale line to plot on longitudinal profile

      # first metric
      profile_first_metric = NULL,

      selected_profile_metric_title = NULL, # metric title to be displayed instead of pure variable name
      selected_profile_metric_type = NULL, # metric type title

      # second metric
      proxy_second_axe = NULL,
      profile_sec_metric = NULL, # second metric selection
      add_sec_axe = NULL, # add second axis
      sec_metric_name = NULL, # title
      sec_metric_type = NULL, # metric type title

      # plotly shapes
      shapes_dgo = NULL, # list with shapes to plot clicked dgo element as line
      shapes_roe = NULL, # list with shapes to plot ROE obstacles as lines
      shapes_background = NULL, # list with shapes to plot classes in background

    )

    #### OUTPUTS ####
    output$long_profile <- renderPlotly({
      return(r_val_local$plot)
    })

    # selectinput for metric
    output$profile_first_metricUI <- renderUI({
      r_val_local$profile_first_metric
    })

    # add selectinput for additional metric
    output$profile_sec_metricUI <- renderUI({
      r_val_local$profile_sec_metric
    })

    # button to add second axe
    output$add_sec_axeUI <- renderUI({
      r_val_local$add_sec_axe
    })

    # checkbox display ROE
    output$profileroeUI <- renderUI({
      r_val_local$ui_roe_profile
    })

    # checkbox display background based on classification
    output$profile_backgroundUI <- renderUI({
      r_val_local$ui_background_profile
    })


    # make plot available to other
    observe({
      r_val$plot_long_proxy <- plotlyProxy("long_profile")
    })

    #### axis select ####

    observeEvent(r_val$axis_clicked, {

      # track input
      track_inputs(input = input)

      if (!is.null(r_val$dgo_axis) & r_val$axis_clicked == TRUE) {

        # build second axis input and add and remove buttons
        r_val_local$profile_first_metric = selectInput(ns("profile_first_metric"), label = "Métrique :",
                                                       choices = params_get_metric_choices(),
                                                       selected  = params_get_metric_choices()[1],
                                                       width = "100%")
      }
    })

    #### build longitudinal profile plot ####
    observeEvent(c(input$profile_first_metric, r_val$dgo_axis), {
      if (!is.null(input$profile_first_metric)) {
        r_val_local$plot <-
          lg_profile_main(
            data = r_val$dgo_axis,
            y = r_val$dgo_axis[[input$profile_first_metric]],
            y_label = params_metrics()[params_metrics()$metric_name == input$profile_first_metric,]$metric_title,
            y_label_category = params_metrics()[params_metrics()$metric_name == input$profile_first_metric,]$metric_type_title
          ) %>%
          event_register("plotly_hover")



        # build second axis input and add and remove buttons
        r_val_local$add_sec_axe = checkboxInput(ns("sec_axis"),
                                                label = "Ajoutez 2éme métrique :",
                                                value = FALSE)
        r_val_local$profile_sec_metric = selectInput(ns("profile_sec_metric"), label = NULL,
                                                     choices = params_get_metric_choices(),
                                                     selected  = params_get_metric_choices()[[2]][1],
                                                     width = "100%")

        # build ROE checkboxInput
        r_val_local$ui_roe_profile = NULL # delete checkbox before creating new one
        r_val_local$ui_roe_profile = checkboxInput(ns("roe_profile"),
                                                   label = "Obstacles à l'Ecoulement",
                                                   value = FALSE)

        # build background classification checkboxInput
        r_val_local$ui_background_profile = NULL # delete checkbox before creating new one
        r_val_local$ui_background_profile = checkboxInput(ns("background_profile"),
                                                          label = "Classifications en arrière-plan",
                                                          value = FALSE)
      }
    })


    #### add or remove second axis ####

    observeEvent(c(input$sec_axis, input$profile_sec_metric), {

      # track input
      track_inputs(input = input)

      # add second axis
      if (input$sec_axis == TRUE) {
        # get metric title and type
        r_val_local$sec_metric_name =
          params_metrics() |> filter(metric_name == input$profile_sec_metric) |> pull(metric_title)
        r_val_local$sec_metric_type =
          params_metrics() |> filter(metric_name == input$profile_sec_metric) |> pull(metric_type_title)

        # create the list to add trace and layout to change second axe plot
        r_val_local$proxy_second_axe <- lg_profile_second(data = r_val$dgo_axis,
                                                          y = r_val$dgo_axis[[input$profile_sec_metric]],
                                                          y_label = r_val_local$sec_metric_name,
                                                          y_label_category = r_val_local$sec_metric_type)
        # add second metric to plot
        plotlyProxy("long_profile") %>%
          plotlyProxyInvoke("deleteTraces", 1) %>%
          plotlyProxyInvoke("addTraces", r_val_local$proxy_second_axe$trace, 1) %>%
          plotlyProxyInvoke("relayout", r_val_local$proxy_second_axe$layout)
      }
      # remove second axis
      else {
        plotlyProxy("long_profile") %>%
          plotlyProxyInvoke("deleteTraces", 1)

        r_val_local$proxy_second_axe = NULL
      }


    })

    ### SHAPES Plotly ####

    #### clicked dgo ####

    observeEvent(r_val$data_dgo_clicked, {

      # track input
      track_inputs(input = input)

      # remove the previous element
      r_val_local$shapes_dgo <- r_val_local$dgo_shape[-1]
      # get new shapes-element for longitudinal plot marker of clicked dgo for
      r_val_local$shapes_dgo <- list(lg_vertical_line(r_val$data_dgo_clicked %>% pull(measure)))
    })

    #### ROE ####

    observeEvent(input$roe_profile, {

      # track input
      track_inputs(input = input)

      if (input$roe_profile == TRUE) {
        # create the vertical line from ROE distance_axis
        r_val_local$shapes_roe = list(lg_roe_vertical_line(r_val$roe_axis$distance_axis))

      } else {
        # remove the previous ROE vertical lines if exist
        r_val_local$shapes_roe = NULL
      }
    })

    #### background classification ####

    observeEvent(input$background_profile, {

      # track input
      track_inputs(input = input)

      # add background classification shapes
      if ((input$background_profile == TRUE) & !is.null(r_val$dgo_axis_classified)) {
        r_val_local$shapes_background = create_classes_background(r_val$dgo_axis_classified)
      }
      # remove background classification
      else if (input$background_profile == FALSE) {
        r_val_local$shapes_background = NULL
      }
    })

    #### COMBINE shapes ####

    # reactive that listens to all changes in shapes and returns a combined list of them
    combined_shapes <- reactive({
      c(r_val_local$shapes_dgo, r_val_local$shapes_roe, r_val_local$shapes_background, r_val_local$leaflet_hover_shapes)
    })

    # observe the combined shapes and update the plotly plot
    observe({
      shapes <- combined_shapes()

      # update profile with changed shapes
      plotlyProxy("long_profile") %>%
        plotlyProxyInvoke("relayout", list(shapes = shapes))
    })


    ### EVENT MOUSEOVER ####

    #### plotly profile ####

    # capture hover events on map to display dgo on profile-plot
    observeEvent(event_data("plotly_hover", source = 'L'), {
      # event data
      hover_event <- event_data("plotly_hover", source = 'L')

      # add line to profile-plot
      if (!is.null(hover_event)) {
        hover_fid <- hover_event$key[1]
        highlighted_feature <- r_val$dgo_axis[r_val$dgo_axis$fid == hover_fid, ]
        r_val$map_proxy %>%
          addPolylines(data = highlighted_feature, color = "red", weight = 10,
                       group = params_map_group()$light)
      }
    })

    # clear previous point on map when moving along profile to not display all the point move over
    observe({
      if (is.null(event_data("plotly_hover", source = 'L'))) {
        r_val$map_proxy %>%
          clearGroup(params_map_group()$light)
      }
    })


    #### leaflet map dgo mouseover ####

    observeEvent(r_val$leaflet_hover_measure, {
      # remove the first element (hover dgo vertical line)
      r_val_local$leaflet_hover_shapes = NULL
      # add the new hover dgo vertical line
      r_val_local$leaflet_hover_shapes = list(lg_vertical_line(r_val$leaflet_hover_measure, color = "red"))

    })
  })
}
