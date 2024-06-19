

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
          column(width = 9,
                 plotlyOutput(ns("long_profile"))
          ),
          column(
            width = 3,
            style = "margin-top: 20px;",
            # uiOutput(ns("profilemetricUI")),
            # uiOutput(ns("profileareaUI")),
            # uiOutput(ns("profileradiobuttonUI")),
            # uiOutput(ns("removeprofileaxeUI"),
            #          style = "margin-top: 10px;"), # more space above button
            uiOutput(ns("profileroeUI"),
                     style = "margin-top: 10px;")
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
      leaflet_hover_measure = 2.5, # measure field from mesure to add vertical line on longitudinal profile
      leaflet_hover_shapes = list(shapes = list(lg_vertical_line(2.5))), # list to store vertical lines to display on longitudinal profile
      ui_roe_profile = NULL, # UI placeholder for ROE checkbox
      roe_vertical_line = NULL, # list with verticale line to plot on longitudinal profile
    )

    #### OUTPUTS ####
    output$long_profile <- renderPlotly({
      return(r_val_local$plot)
    })

    # # add input UI for profile additional metric
    # output$profilemetricUI <- renderUI({
    #   r_val$ui_profile_metric_type
    # })

    # # add radiobutton for profile additional metric
    # output$profileradiobuttonUI <- renderUI({
    #   r_val$ui_profile_metric
    # })

    # # UI switch unit area for profile additional metric
    # output$profileareaUI <- renderUI({
    #   r_val$ui_profile_unit_area
    # })

    # # button to remove second axe
    # output$removeprofileaxeUI <- renderUI({
    #   r_val$ui_remove_profile_axe
    # })

    # checkbox display ROE
    output$profileroeUI <- renderUI({
      r_val_local$ui_roe_profile
    })


    #### metric select ####

    observeEvent(c(r_val$selected_metric, r_val$axis_click), {

      # track input
      track_inputs(input = input)

      if (!is.null(r_val$selected_metric) & !is.null(r_val$dgo_axis)) {
        # r_val$selected_profile_metric_name = params_metrics_choice()[[input$profile_metric_type]]$metric_type_values[[input$profile_metric]]$metric_title
        # r_val$selected_profile_metric_type = params_metrics_choice()[[input$profile_metric_type]]$metric_type_title

        # create longitudinal profile plot
        r_val_local$plot <-
          lg_profile_main(
            data = r_val$dgo_axis,
            y = r_val$dgo_axis[[r_val$selected_metric]],
            y_label = r_val$selected_metric,
            y_label_category = "type"
          ) %>%
          event_register("plotly_hover")

        # built ROE checkboxInput

        r_val_local$ui_roe_profile = NULL # delete checkbox before creating new one
        r_val_local$ui_roe_profile = checkboxInput(ns("roe_profile"),
                                                   label = "Obstacles à l'Ecoulement",
                                                   value = FALSE)





        # if(!is.null(input$profile_metric)){ # second metric selected = update second metric profile
        #   # create the list to add trace and layout to change second axe plot
        #   proxy_second_axe <- lg_profile_second(data = r_val$selected_axis_df,
        #                                         y = r_val$selected_axis_df[[r_val$selected_profile_metric]],
        #                                         y_label = r_val$selected_profile_metric_name,
        #                                         y_label_category = r_val$selected_profile_metric_type)
        #
        #   plotlyProxy("long_profile") %>%
        #     plotlyProxyInvoke("deleteTraces", 1) %>%
        #     plotlyProxyInvoke("addTraces", proxy_second_axe$trace, 1) %>%
        #     plotlyProxyInvoke("relayout", proxy_second_axe$layout)
        # }
        #
        # # update map to change tooltip labels
        # r_val$map_proxy %>%
        #   map_dgo_axis(selected_axis = r_val$dgo_axis, region_axis = r_val$network_region_axis,
        #                main_metric = r_val$selected_metric, second_metric = r_val$selected_profile_metric)
        #
        # # create the list to add trace and layout to change second axe plot
        # proxy_second_axe <- lg_profile_second(data = r_val$selected_axis_df,
        #                                       y = r_val$selected_axis_df[[r_val$selected_profile_metric]],
        #                                       y_label = r_val$selected_profile_metric_name,
        #                                       y_label_category = r_val$selected_profile_metric_type)
        #
        # plotlyProxy("long_profile") %>%
        #   plotlyProxyInvoke("deleteTraces", 1) %>%
        #   plotlyProxyInvoke("addTraces", proxy_second_axe$trace, 1) %>%
        #   plotlyProxyInvoke("relayout", proxy_second_axe$layout)
      }
    })

    #### remove axe ####

    # observeEvent(input$remove_profile_axe, {
    #
    #   # track input
    #   track_inputs(input = input)
    #
    #   plotlyProxy("long_profile") %>%
    #     plotlyProxyInvoke("deleteTraces", 1)
    #
    #   updateRadioButtons(session, "profile_metric", selected = character(0))
    #
    #   r_val$selected_profile_metric = NULL
    #   # update dgo on axis to reset tooltip
    #   leafletProxy("exploremap") %>%
    #     map_dgo_axis(selected_axis = r_val$dgo_axis, region_axis = r_val$network_region_axis,
    #                  main_metric = r_val$selected_metric, second_metric = r_val$selected_profile_metric)
    #
    # })

    #### add ROE ####

    observeEvent(input$roe_profile, {

      # track input
      track_inputs(input = input)

      if (input$roe_profile == TRUE){
        if (!is.null(r_val_local$roe_vertical_line)){
          # remove the previous ROE vertical lines if exist
          r_val_local$leaflet_hover_shapes$shapes <- list(r_val_local$leaflet_hover_shapes$shapes[[1]])
        }
        # create the vertical line from ROE distance_axis
        r_val_local$roe_vertical_line <- lg_roe_vertical_line(r_val$roe_axis$distance_axis)
        # increment the vertical list shape to keep the hover map vertical line
        r_val_local$leaflet_hover_shapes$shapes <- c(r_val_local$leaflet_hover_shapes$shapes,
                                                     r_val_local$roe_vertical_line)
        # update profile
        plotlyProxy("long_profile") %>%
          plotlyProxyInvoke("relayout",  r_val_local$leaflet_hover_shapes)
      }else{
        # remove the previous ROE vertical lines if exist
        r_val_local$leaflet_hover_shapes$shapes <- list(r_val_local$leaflet_hover_shapes$shapes[[1]])
        # update profile
        plotlyProxy("long_profile") %>%
          plotlyProxyInvoke("relayout",  r_val_local$leaflet_hover_shapes)
      }
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
  })
}
