
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
            # mod_profil_long_ui("profil_long_1")
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
    # create mein map
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
      if (input$map_shape_click$group == globals$map_group_params[["bassin"]]){

        # check if basin not already selected
        if (r_val$basin_id != input$map_shape_click$id) {

          # get bassin name
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
        }
      }

    #### Region ####

    #### Axis ####

    #### SWATH ####

    })

  })
}
