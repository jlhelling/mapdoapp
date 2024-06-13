
# UI ----------------------------------------------------------------------



#' mapdo_app UI Function
#'
#' @description A shiny Module.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinycssloaders withSpinner
#' @importFrom bslib popover
#' @importFrom bsicons bs_icon
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mapdo_app_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    fluidPage(
      useShinyjs(),
      tags$head(
        tags$style(
          HTML("
               .form-group{margin-bottom: 10px} /* less space below selectInput metric_type */
               ")
        )
      ), # head

      fluidRow(
        column(
          width = 6,
          withSpinner(leafletOutput(ns("map"), height = 700)),
          textOutput(ns("selection_textUI"))
        ),
        column(
          width = 6,
          tabsetPanel(
            id = "tabset",
            tabPanel("Styles fluviaux",
            ),
            tabPanel("Analyse métrique",
            ),
            tabPanel("Profil transversal",
            )
            , type = "pills"
          ) #tabsetpanel
        ) #column
      ) #row
    ) #page

  )
}



# SERVER ------------------------------------------------------------------


#' mapdo_app Server Functions
#'
#' @import shiny
#' @importFrom leaflet leafletProxy clearGroup leafletOutput renderLeaflet
#' @importFrom htmltools HTML div img
#' @importFrom dplyr filter mutate if_else pull
#' @importFrom plotly event_register event_data plotlyProxy plotlyProxyInvoke renderPlotly plotlyOutput
#' @importFrom bslib popover update_popover
#' @importFrom bsicons bs_icon
#' @importFrom sf st_write
#' @importFrom shinyjs onclick runjs
#' @noRd
mod_mapdo_app_server <- function(id, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### R_VAL ####
    r_val <- reactiveValues(

      # UI
      selection_text = "", # description text indicating basin, region, axis

      # data
      bassins = NULL,
      bassin_name = NULL,

      # others variables
      opacity = list(clickable = 0.01, not_clickable = 0.10) # opacity value to inform the user about available bassins and regions

    )


    ### INITIALIZATION ####

    #### Map ####

    output$map <- renderLeaflet({
      r_val$bassins = data_get_bassins(opacity = r_val$opacity, con = con)
      map_init_bassins(bassins_data = r_val$bassins,
                       id_logo_ign_remonterletemps = ns("logo_ign_remonterletemps"))
    })

    onclick(id = "logo_ign_remonterletemps", expr =
              runjs(sprintf("window.open('%s', '_blank')",
                            utils_url_remonterletemps(lng = input$map_center$lng,
                                                      lat = input$map_center$lat,
                                                      zoom = input$map_zoom)))
    )

    #### Description Text ####

    output$selection_textUI <- renderText({
      r_val$selection_text
    })

    ### EVENT MAP CLICK ####

    observeEvent(input$map_shape_click,{

      # track input
      track_inputs(input = input)

      #### bassin clicked ####
      if (input$map_shape_click$group == params_map_group()[["bassin"]]){
        # get bassin name
        r_val$bassin_name = r_val$bassins %>%
          filter(cdbh == input$map_shape_click$id) %>%
          pull(lbbh)
        # disable the click interactivity for the bassin selected
        r_val$bassins = r_val$bassins %>%
          mutate(click = if_else(display == TRUE, TRUE, click)) %>%
          mutate(click = if_else(display == TRUE & cdbh == input$map_shape_click$id, FALSE, click))
        # get the regions data in selected bassin
        r_val$regions_in_bassin = data_get_regions_in_bassin(selected_bassin_id = input$map_shape_click$id,
                                                             opacity = r_val$opacity,
                                                             con = con)
        # update map : zoom in clicked bassin, clear bassin data, display region in bassin
        leafletProxy("map") %>%
          map_add_regions_in_bassin(bassin_click = input$map_shape_click,
                                    regions_data = r_val$regions_in_bassin,
                                    bassins_data = r_val$bassins)
        # print name of basin below map
        r_val$selection_text = paste0("Bassin: ", r_val$bassin_name)
      }

      ### region clicked ####
      if (input$map_shape_click$group == params_map_group()$region){
        # store the region click values
        r_val$region_click = input$map_shape_click
        # disable the click interactivity for the bassin selected
        r_val$regions_in_bassin = r_val$regions_in_bassin %>%
          mutate(click = if_else(display == TRUE,
                                 TRUE,
                                 click)) %>%
          mutate(click = if_else(display == TRUE & gid == r_val$region_click$id,
                                 FALSE,
                                 click))

        # save the selected region feature for mapping
        r_val$selected_region_feature = data_get_region(region_click_id = r_val$region_click$id,
                                                        con = con)
        # set region name to download
        r_val$region_name = utils_normalize_string(r_val$selected_region_feature$lbregionhy)
        # get the axis in the region
        r_val$network_region_axis = data_get_axis(selected_region_id = r_val$region_click$id,
                                                  con = con)
        # get ROE in region
        r_val$roe_region = data_get_roe_in_region(r_val$region_click$id,
                                                  con = con)
        # get hydro sites in region
        r_val$hydro_sites_region = data_get_hydro_sites(r_val$region_click$id,
                                                        con = con)

        # map region clicked with region clicked and overlayers
        leafletProxy("map") %>%
          map_region_clicked(region_click = input$map_shape_click,
                             selected_region_feature = r_val$selected_region_feature,
                             regions_data = r_val$regions_in_bassin,
                             roe_region = r_val$roe_region,
                             hydro_sites_region = r_val$hydro_sites_region)
      }
      ### axis clicked ####

      # if (input$map_shape_click$group == params_map_group()$axis) {
      #   # save the clicked axis values
      #   r_val$axis_click = input$map_shape_click
      #   # reget the axis in the region without the selected axis
      #   r_val$network_region_axis = data_get_axis(selected_region_id = r_val$region_click$id,
      #                                             con = con) %>%
      #     filter(axis != r_val$axis_click$id)
      #   # get the DGO axis data
      #   r_val$dgo_axis = data_get_network_axis(selected_axis_id = r_val$axis_click$id,
      #                                          con = con) %>%
      #     mutate(measure = measure/1000)
      #   # extract axis start end point
      #   r_val$axis_start_end = data_get_axis_start_end(dgo_axis = r_val$dgo_axis)
      #   # get ROE in axis clicked
      #   r_val$roe_axis = r_val$roe_region %>%
      #     filter(axis == r_val$axis_click$id)
      #
      #   # map dgo axis when axis clicked and metric selected
      #   leafletProxy("map") %>%
      #     map_dgo_axis(selected_axis = r_val$dgo_axis, region_axis = r_val$network_region_axis,
      #                  main_metric = r_val$selected_metric, second_metric = r_val$selected_profile_metric) %>%
      #     map_axis_start_end(axis_start_end = r_val$axis_start_end, region_axis = r_val$network_region_axis)
      #
      #   # create or update profile dataset with new axis
      #   r_val$selected_axis_df = r_val$dgo_axis %>%
      #     as.data.frame()
      #
      #   # update profile with new metric selected
      #   if (r_val$profile_display == TRUE){
      #     proxy_main_axe <-
      #       lg_profile_update_main(
      #         data = r_val$selected_axis_df,
      #         y = r_val$selected_axis_df[[r_val$selected_metric]],
      #         y_label = r_val$selected_metric_name,
      #         y_label_category = r_val$selected_metric_type
      #       )
      #
      #     plotlyProxy("long_profile") %>%
      #       plotlyProxyInvoke("deleteTraces", 0) %>%
      #       plotlyProxyInvoke("addTraces", proxy_main_axe$trace, 0) %>%
      #       plotlyProxyInvoke("relayout", proxy_main_axe$layout)
      #
      #     # update ROE vertical lines
      #     if (input$roe_profile == TRUE){
      #       if (!is.null(r_val$roe_vertical_line)){
      #         # remove the previous ROE vertical lines if exist
      #         r_val$leaflet_hover_shapes$shapes <- list(r_val$leaflet_hover_shapes$shapes[[1]])
      #       }
      #       # create the vertical line from ROE distance_axis
      #       r_val$roe_vertical_line <- lg_roe_vertical_line(r_val$roe_axis$distance_axis)
      #       # increment the vertical list shape to keep the hover map vertical line
      #       r_val$leaflet_hover_shapes$shapes <- c(r_val$leaflet_hover_shapes$shapes,
      #                                              r_val$roe_vertical_line)
      #       # update profile
      #       plotlyProxy("long_profile") %>%
      #         plotlyProxyInvoke("relayout",  r_val$leaflet_hover_shapes)
      #     }else{
      #       # remove the previous ROE vertical lines if exist
      #       r_val$leaflet_hover_shapes$shapes <- list(r_val$leaflet_hover_shapes$shapes[[1]])
      #       # update profile
      #       plotlyProxy("long_profile") %>%
      #         plotlyProxyInvoke("relayout",  r_val$leaflet_hover_shapes)
      #     }
      #
      #
      #     if(!is.null(input$profile_metric)){ # second metric selected = update second metric profile
      #       # create the list to add trace and layout to change second axe plot
      #       proxy_second_axe <- lg_profile_second(data = r_val$selected_axis_df,
      #                                             y = r_val$selected_axis_df[[r_val$selected_profile_metric]],
      #                                             y_label = r_val$selected_profile_metric_name,
      #                                             y_label_category = r_val$selected_profile_metric_type)
      #
      #       plotlyProxy("long_profile") %>%
      #         plotlyProxyInvoke("deleteTraces", 1) %>%
      #         plotlyProxyInvoke("addTraces", proxy_second_axe$trace, 1) %>%
      #         plotlyProxyInvoke("relayout", proxy_second_axe$layout)
      #     }
      #   }
      # }
      #
      # ### dgo clicked ####
      #
      # if (input$map_shape_click$group == params_map_group()$dgo_axis) {
      #   # get data with dgo id
      #   r_val$data_section = data_get_elevation_profiles(selected_dgo_fid = input$map_shape_click$id,
      #                                                    con = con)
      #   # plot cross section
      #   r_val$section = cr_profile_main(data = r_val$data_section,
      #                                   axis_toponyme = unique(r_val$selected_axis_df$toponyme))
      #   # get dgo clicked feature
      #   r_val$data_dgo_clicked = r_val$dgo_axis %>%
      #     filter(fid == input$map_shape_click$id)
      #   # Highlight clicked DGO
      #   leafletProxy("map") %>%
      #     map_dgo_cross_section(selected_dgo = r_val$data_dgo_clicked)
      # }
    })

  })
}

## To be copied in the UI
# mod_mapdo_app_ui("mapdo_app_1")

## To be copied in the server
# mod_mapdo_app_server("mapdo_app_1")
