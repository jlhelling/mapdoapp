#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id shiny id
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_explore
#'
#' @importFrom leaflet leafletOutput renderLeaflet addProviderTiles colorQuantile
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
#' @importFrom dplyr left_join
#' @import sf
#' @import mapdotoro
mod_explore_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # UI elements
    fluidPage(
      fluidRow(
        column(
          width = 3,
          titlePanel("Metriques"),
          selectInput(ns("choice"), "Sélectionez une métrique :",
                      choices = c("Largeurs", "Pentes"),
                      selected  = "Largeurs"), # selectInput for dynamic radio buttons
          uiOutput(ns("radioButtonsUI")
                   ) # uiOutput radios buttons metrics
        ), # column
        column(
          width = 6,
          titlePanel(""),
          leafletOutput(ns("ui_exploremap"))
        ), # column
        column(
          width = 2,
          titlePanel("Filtre"),
          textOutput(ns("greeting"))
        ) # column
      ), # fluidRow
      fluidRow(
        tabsetPanel(
          tabPanel("Profil en long"
          ), # tabPanel
          tabPanel("Profil en travers"
          ) # tabPanel
        )# tabsetPanel
      )# fluidRow
    ) # fluidPage
) # tagList
}

#' explore Server Functions
#'
#' @noRd
#'
# mod_explore_server <- function(id){
mod_explore_server <- function(input, output, session){

    ns <- session$ns

    output$radioButtonsUI <- renderUI({
      selected_choice <- input$choice

      if (selected_choice == "Largeurs") {
        radioButtons(ns("dynamicRadio"), "Largeurs :",
                     choiceNames = c(
                       "Chenal actif",
                       "Corridor naturel",
                       "Corridor connecté",
                       "Fond de vallée"
                     ),
                     choiceValues = list("active_channel_width",
                                         "natural_corridor_width",
                                         "connected_corridor_width",
                                         "valley_bottom_width"),
                     selected = "active_channel_width")
      } else if (selected_choice == "Pentes") {
        radioButtons(ns("dynamicRadio"), "Pentes :",
                     choiceNames = c(
                       "Pente du talweg",
                       "Pente du fond de valée"
                     ),
                     choiceValues = list("talweg_slope",
                                         "floodplain_slope"),
                     selected = "talweg_slope"
        )
      } else {
        # Default case
        NULL
      }
    })

    datamap <- network_data %>%
      left_join(metrics_data, by = join_by("AXIS"=="AXIS", "M"=="measure"),
                suffix = c("", ".metrics"), relationship="one-to-one")

    var <- reactive({
      switch(input$dynamicRadio,
             # Width
             "active_channel_width" = datamap$active_channel_width,
             "natural_corridor_width" = datamap$natural_corridor_width,
             "connected_corridor_width" = datamap$connected_corridor_width,
             "valley_bottom_width" = datamap$valley_bottom_width,
             # Slope
             "talweg_slope" = datamap$talweg_slope,
             "floodplain_slope" = datamap$floodplain_slope
      )
    })

    output$ui_exploremap <- renderLeaflet({
      qpal <- colorQuantile("Reds", domain = var(), n = 5, na.color = "#808080")
            leaflet() %>%
              setView(lng = 2.468697, lat = 46.603354, zoom = 6) %>%
              addTiles() %>%
              addPolylines(data = datamap, color = ~qpal(var()))
    }) # ui_exploremap

  }

## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
