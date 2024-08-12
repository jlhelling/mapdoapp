
# UI ----------------------------------------------------------------------



#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom bslib popover
#' @importFrom bsicons bs_icon
#' @importFrom leaflet leafletOutput
#' @importFrom htmltools HTML div img
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_explore_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    fluidPage(
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
          width = 8,
          leafletOutput(ns("map"), height = 500),
          textOutput(ns("selection_textUI"))
        ),
        column(
          width = 4,
          tabsetPanel(
            id = ns("tabset_classes"),
            tabPanel("Classes proposées",
                     mod_expl_classes_proposed_ui("expl_classes_proposed_1")
            ),
            tabPanel("Classification manuelle",
                     # mod_expl_classes_manual_ui("expl_classes_manual_1")
            ), type = "pills"
          ) #tabsetpanel
        ) #column
      ), #row
      fluidRow(
        style = "margin-top: 10px;
        margin-bottom: 10px;",
        tabsetPanel(
          id = ns("tabset_plots"),
          tabPanel(
            "Évolution longitudinale",
            mod_expl_plot_long_ui("expl_plot_long_1")
          ),
          tabPanel("Profil transversal",
                   # mod_profil_transverse_ui("profil_transverse_1")
          ), type = "pills"
        )
      )
    ) #page

  )
}



# SERVER ------------------------------------------------------------------


#' explore Server Functions
#'
#' @import shiny
#' @importFrom leaflet leafletProxy clearGroup leafletOutput renderLeaflet removeControl addLegend addControl
#' @importFrom leaflet.extras addWMSLegend
#' @importFrom htmltools HTML div img
#' @importFrom dplyr filter mutate if_else pull
#' @importFrom plotly event_register event_data plotlyProxy plotlyProxyInvoke renderPlotly plotlyOutput
#' @importFrom bslib popover update_popover
#' @importFrom bsicons bs_icon
#' @importFrom sf st_write
#' @importFrom shinyjs onclick runjs
#' @noRd
mod_explore_server <- function(id, con, r_val, globals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### Map ####
    # create main map
    output$map <- renderLeaflet({
      map_initialize(params_wms = globals$wms_params,
                     params_map_group = globals$map_group_params,
                     id_logo_ign_remonterletemps = ns("logo_ign_remonterletemps"),
                     basins_data = globals$basins(),
                     regions_data = globals$regions,
                     axes_data = globals$axes(),
                     roe_sites = globals$roe_sites(),
                     hydro_sites = globals$hydro_sites()
      )
    }) %>%
      bindCache(globals$regions_gids_key)

    # add remonter-le-temps-button functionality
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

    observe({
      r_val$map_proxy <- leafletProxy("map")
    })

    ### TABSET LISTENERS ####
    # save current tabs in reactive values

    # classification tab
    observeEvent(input$tabset_classes, {
      r_val$tab_classes = input$tabset_classes
    })

    # plots tab
    observeEvent(input$tabset_plots, {
      r_val$tab_plots = input$tabset_plots
    })


    ### CLICK EVENTS ####

    observeEvent(input$map_shape_click, {

      #### Basin ####
      if (input$map_shape_click$group == globals$map_group_params[["bassin"]]) {

        # check if basin not already selected
        if (!is.null(r_val$basin_id) && (r_val$basin_id == input$map_shape_click$id)) {

        } else {

          # get basin name
          r_val$basin_name = globals$basins() %>%
            filter(cdbh == input$map_shape_click$id) %>%
            pull(lbbh)

          # get basin id
          r_val$basin_id = input$map_shape_click$id

          # reset others
          r_val$region_name = NULL
          r_val$region_id = NULL
          r_val$axis_name = NULL
          r_val$axis_id = NULL
          r_val$swath_id = NULL
          r_val$swath_data_section = NULL
          r_val$swath_data_dgo = NULL

          # clean map
          r_val$map_proxy %>%
            clearGroup(globals$map_group_params[["dgo_axis"]]) %>%
            clearGroup(globals$map_group_params[["axis_start_end"]]) %>%
            clearGroup(globals$map_group_params[["dgo"]])
        }
      }

      #### Region ####
      if (input$map_shape_click$group == globals$map_group_params[["region"]]) {

        # check if region not already selected
        if (!is.null(r_val$region_id) && (r_val$region_id == input$map_shape_click$id)) {

        } else {

          # get region id
          r_val$region_id = input$map_shape_click$id

          # get region name
          r_val$region_name = globals$regions %>%
            filter(gid == r_val$region_id) %>%
            pull(lbregionhy)

          # check if region is in other basin than the one that is selected
          if (!is.null(r_val$basin_id) && (r_val$basin_id == (globals$regions %>%
                                                              filter(gid == input$map_shape_click$id) %>%
                                                              pull(cdbh)))) {
          } else {

            r_val$basin_name = globals$basins() %>%
              filter(cdbh == globals$regions %>%
                       filter(gid == input$map_shape_click$id) %>%
                       pull(cdbh)) %>%
              pull(lbbh)

            r_val$basin_id = globals$regions %>%
              filter(gid == input$map_shape_click$id) %>%
              pull(cdbh)
          }

          # reset others
          r_val$axis_name = NULL
          r_val$axis_id = NULL
          r_val$swath_id = NULL

          # clean map
          r_val$map_proxy %>%
            clearGroup(globals$map_group_params[["dgo_axis"]]) %>%
            clearGroup(globals$map_group_params[["axis_start_end"]]) %>%
            clearGroup(globals$map_group_params[["dgo"]])

        }
      }

      #### Axis ####
      if (input$map_shape_click$group == globals$map_group_params[["axis"]]) {

        # signal that axis is clicked for first time
        r_val$axis_clicked = TRUE

        # check if axis not already selected
        if (!is.null(r_val$axis_id) && (r_val$axis_id == input$map_shape_click$id)) {

        } else {

          # get axis id
          r_val$axis_id = input$map_shape_click$id

          # get axis name
          r_val$axis_name = globals$axes() %>%
            filter(axis == r_val$axis_id) %>%
            pull(toponyme)

          # reset others
          r_val$swath_id = NULL
          r_val$swath_data_section = NULL
          r_val$swath_data_dgo = NULL

          # load axis sf frame
          r_val$axis_data = data_get_axis_dgos(selected_axis_id = r_val$axis_id, con)

          # get start- and end-coordinates of axis
          r_val$axis_start_end = data_get_axis_start_end(dgo_axis = r_val$axis_data)

          # add axis to map
          r_val$map_proxy %>%
            map_add_axes(globals$axes(), group = globals$map_group_params[["axis"]], selected_axis_id = r_val$axis_id) %>%
            map_add_axis_dgos(r_val$axis_data, group = globals$map_group_params[["dgo_axis"]]) %>%
            map_add_axis_start_end(axis_start_end = r_val$axis_start_end,
                                   group = globals$map_group_params[["axis_start_end"]]) %>%
            clearGroup(globals$map_group_params[["dgo"]])

          # check if axis is in other region than the one that is selected
          if (!is.null(r_val$region_id) && (r_val$region_id == (globals$axes() %>%
                                                                filter(axis == r_val$axis_id) %>%
                                                                pull(gid_region)))) {
          } else {

            # set region id
            r_val$region_id = globals$axes() %>%
              filter(axis == r_val$axis_id) %>%
              pull(gid_region)

            # set region name
            r_val$region_name = globals$regions %>%
              filter(gid == r_val$region_id) %>%
              pull(lbregionhy)

            # check if region is in other basin than the one that is selected
            if (!is.null(r_val$basin_id) && (r_val$basin_id == (globals$regions %>%
                                                                filter(gid == r_val$region_id) %>%
                                                                pull(cdbh)))) {
            } else {

              r_val$basin_name = globals$basins() %>%
                filter(cdbh == globals$regions %>%
                         filter(gid == r_val$region_id) %>%
                         pull(cdbh)) %>%
                pull(lbbh)

              r_val$basin_id = globals$regions %>%
                filter(gid == r_val$region_id) %>%
                pull(cdbh)
            }
          }
        }
      }

      #### Swath ####

      if (input$map_shape_click$group == globals$map_group_params[["dgo_axis"]]) {


        # check if axis not already selected
        if (!is.null(r_val$swath_id) && (r_val$swath_id == input$map_shape_click$id)) {

        } else {

          # get swath id
          r_val$swath_id = input$map_shape_click$id

          # get data with dgo id
          r_val$swath_data_section = data_get_elevation_profiles(selected_dgo_fid = r_val$swath_id, con = con)

          # get dgo clicked feature
          r_val$swath_data_dgo = r_val$axis_data %>%
            filter(fid == r_val$swath_id)

          # Highlight clicked DGO
          r_val$map_proxy %>%
            map_dgo_cross_section(selected_dgo = r_val$swath_data_dgo, group = globals$map_group_params[["dgo"]])
        }

      }
    })
  })
}
