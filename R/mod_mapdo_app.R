
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
          textOutput(ns("selection_text"))
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

      # data
      bassins = NULL,

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

  })
}

## To be copied in the UI
# mod_mapdo_app_ui("mapdo_app_1")

## To be copied in the server
# mod_mapdo_app_server("mapdo_app_1")
