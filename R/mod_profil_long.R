

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
            uiOutput(ns("profile_sec_metricUI")),
            fluidRow(
              column(width = 6,
                     uiOutput(ns("add_sec_axeUI")),
              ),
              column(width = 6,
                     uiOutput(ns("remove_sec_axeUI")),
              )
            ),
            uiOutput(ns("profileroeUI"),
                     style = "margin-top: 30px;"),
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
      remove_sec_axe = NULL, # remove second axis
      sec_metric_name = NULL, # title
      sec_metric_type = NULL, # metric type title

      # plotly shapes
      leaflet_hover_shapes = list(shapes = NULL), # list with roe elements to add as lines to plot
      background_shapes = list(shapes = NULL), # list with shapes to plot classes in background

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

    # button to remove second axe
    output$remove_sec_axeUI <- renderUI({
      r_val_local$remove_sec_axe
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

    observeEvent(r_val$axis_click, {

      # track input
      track_inputs(input = input)

      if (!is.null(r_val$dgo_axis)) {

        # build second axis input and add and remove buttons
        r_val_local$profile_first_metric = selectInput(ns("profile_first_metric"), label = "Métrique :",
                                                       choices = params_get_metric_choices(),
                                                       selected  = params_get_metric_choices()[1],
                                                       width = "100%")

        # build second axis input and add and remove buttons
        r_val_local$profile_sec_metric = selectInput(ns("profile_sec_metric"), label = "Ajoutez 2éme métrique :",
                                                     choices = params_get_metric_choices(),
                                                     selected  = params_get_metric_choices()[[2]][1],
                                                     width = "100%")
        r_val_local$add_sec_axe = actionButton(inputId = ns("add_sec_axe"), "Ajouter", width = "100%")
        r_val_local$remove_sec_axe = actionButton(inputId = ns("remove_sec_axe"), "Retirer", width = "100%")

        # build ROE checkboxInput
        r_val_local$ui_roe_profile = NULL # delete checkbox before creating new one
        r_val_local$ui_roe_profile = checkboxInput(ns("roe_profile"),
                                                   label = "Obstacles à l'Ecoulement",
                                                   value = FALSE)
      }
    })

    #### build longitudinal profile plot ####
    observeEvent(input$profile_first_metric, {
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
        r_val_local$profile_sec_metric = selectInput(ns("profile_sec_metric"), label = "Ajoutez 2éme métrique :",
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

        # # add second metric to plot if valid
        # if (!is.null(r_val_local$proxy_second_axe)) {
        #   plotlyProxy("long_profile") %>%
        #     plotlyProxyInvoke("deleteTraces", 1) %>%
        #     plotlyProxyInvoke("addTraces", r_val_local$proxy_second_axe$trace, 1) %>%
        #     plotlyProxyInvoke("relayout", r_val_local$proxy_second_axe$layout)
        # }
      }
    })


    #### add second axis ####

    observeEvent(input$add_sec_axe, {

      # track input
      track_inputs(input = input)

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
    })

    #### remove second axis ####

    observeEvent(input$remove_sec_axe, {

      # track input
      track_inputs(input = input)

      plotlyProxy("long_profile") %>%
        plotlyProxyInvoke("deleteTraces", 1)

      r_val_local$proxy_second_axe = NULL

    })

    # #### add ROE ####
    # observeEvent(input$roe_profile, {
    #   # track input
    #   track_inputs(input = input)
    #
    #   if (input$roe_profile == TRUE){
    #     if (!is.null(r_val_local$roe_vertical_line)){
    #       # remove the previous ROE vertical lines if exist
    #       r_val_local$roe_vertical_line <- NULL
    #     }
    #     # create the vertical line from ROE distance_axis
    #     r_val_local$roe_vertical_line <- lg_roe_vertical_line(r_val$roe_axis$distance_axis)
    #
    #     # increment the vertical list shape to keep the hover map vertical line
    #     r_val_local$leaflet_hover_shapes$shapes <- c(r_val_local$leaflet_hover_shapes$shapes,
    #                                                  r_val_local$roe_vertical_line)
    #
    #   } else {
    #     # remove the previous ROE vertical lines if exist
    #     r_val_local$leaflet_hover_shapes = NULL
    #   }
    #
    #   # Combine shapes and update plot
    #   combined_shapes <- c(
    #     if (!is.null(r_val_local$leaflet_hover_shapes)) r_val_local$leaflet_hover_shapes else list(),
    #     if (!is.null(r_val_local$background_shapes)) r_val_local$background_shapes else list()
    #   )
    #
    #   plotlyProxy("long_profile") %>%
    #     plotlyProxyInvoke("relayout", list(shapes = combined_shapes))
    # })
    #
    #### add background classification ####
    # observeEvent(input$background_profile, {
    #
    #   if ((input$background_profile == TRUE) & !is.null(r_val$dgo_axis_classified)) {
    #     # add data to longitudinal plot
    #     r_val_local$background_shapes <- create_classes_background(r_val$dgo_axis_classified)
    #   } else {
    #     # remove background classification
    #     r_val_local$background_shapes = NULL
    #   }
    #
    #   # Combine shapes and update plot
    #   combined_shapes <- c(
    #     if (!is.null(r_val_local$leaflet_hover_shapes)) r_val_local$leaflet_hover_shapes else list(),
    #     if (!is.null(r_val_local$background_shapes)) r_val_local$background_shapes else list()
    #   )
    #
    #   plotlyProxy("long_profile") %>%
    #     plotlyProxyInvoke("relayout", list(shapes = combined_shapes))
    # })

    #### add ROE ####

    observeEvent(input$roe_profile, {

      # track input
      track_inputs(input = input)

      if (input$roe_profile == TRUE) {
        if (!is.null(r_val_local$roe_vertical_line)){
          # remove the previous ROE vertical lines if exist
          r_val_local$leaflet_hover_shapes$shapes = list(r_val_local$leaflet_hover_shapes$shapes[[1]])
        }
        # create the vertical line from ROE distance_axis
        r_val_local$roe_vertical_line <- lg_roe_vertical_line(r_val$roe_axis$distance_axis)
        # increment the vertical list shape to keep the hover map vertical line
        r_val_local$leaflet_hover_shapes = c(r_val_local$leaflet_hover_shapes$shapes,
                                                     r_val_local$roe_vertical_line)

      } else {
        # remove the previous ROE vertical lines if exist
        r_val_local$leaflet_hover_shapes = list(r_val_local$leaflet_hover_shapes$shapes[[1]])
        # list(r_val_local$leaflet_hover_shapes$shapes[[1]])
      }

    })

    #### add background classification ####

    observeEvent(input$background_profile, {

      print(paste0("clicked background: ", input$background_profile))

      # track input
      track_inputs(input = input)

      # add background classification shapes
      if ((input$background_profile == TRUE) & !is.null(r_val$dgo_axis_classified)) {

        r_val_local$background_shapes = create_classes_background(r_val$dgo_axis_classified)
      }
      # remove background classification
      else {
        r_val_local$background_shapes = NULL
      }
    })

    #### listen to shapes changes ####
    observeEvent(c(r_val_local$leaflet_hover_shapes, r_val_local$background_shapes), {

      # Combine shapes
      combined_shapes <- c(
        r_val_local$leaflet_hover_shapes,
        r_val_local$background_shapes
      )

      # print(paste0("roe: ", r_val_local$leaflet_hover_shapes))
      # print(paste0("background: ", r_val_local$background_shapes))
      # print(paste0("combi: ", combined_shapes))

      # update profile with change shapes
      plotlyProxy("long_profile") %>%
        plotlyProxyInvoke("relayout", list(shapes = combined_shapes))
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
      r_val_local$leaflet_hover_shapes <- list(shapes = r_val_local$leaflet_hover_shapes$shapes[-1])
      # add the new hover dgo vertical line
      r_val_local$leaflet_hover_shapes$shapes <- c(list(lg_vertical_line(r_val$leaflet_hover_measure)), r_val_local$leaflet_hover_shapes$shapes)
      # change profile layout with vertical line
      plotlyProxy("long_profile") %>%
        plotlyProxyInvoke("relayout", r_val_local$leaflet_hover_shapes)
    })
  })
}
