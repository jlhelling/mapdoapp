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
#' @importFrom dplyr left_join right_join
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
          selectInput(ns("metric"), "Sélectionez une métrique :",
                      choices = c("Largeurs", "Pentes", "Occupation du sol"),
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

  # mapping initialization
  output$ui_exploremap <- renderLeaflet({
    leaflet() %>%
      setView(lng = 2.468697, lat = 46.603354, zoom = 6) %>%
      addTiles()
  })

  # metrics radio buttons UI
  output$radioButtonsUI <- renderUI({
    selected_metric <- input$metric

    if (selected_metric == "Largeurs") {
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
    } else if (selected_metric == "Pentes") {
      radioButtons(ns("dynamicRadio"), "Pentes :",
                   choiceNames = c(
                     "Pente du talweg",
                     "Pente du fond de valée"
                   ),
                   choiceValues = list("talweg_slope",
                                       "floodplain_slope"),
                   selected = "talweg_slope"
      )
    } else if (selected_metric == "Occupation du sol") {
      radioButtons(ns("dynamicRadio"), "Occupation du sol :",
                   choiceNames = c(
                     "Surface en eau",
                     "Bancs sédimentaires",
                     "Espace naturel ouvert",
                     "Forêt",
                     "Prairie permanente",
                     "Culture",
                     "Périurbain",
                     "Urbain dense",
                     "Infrastructure de stransport"
                   ),
                   choiceValues = list("Water Channel",
                                       "Gravel Bars",
                                       "Natural Open",
                                       "Forest",
                                       "Grassland",
                                       "Crops",
                                       "Diffuse Urban",
                                       "Dense Urban",
                                       "Infrastructures"),
                   selected = "Water Channel"
      )
    }
  })

  # dataset
  datamap <- reactive({
    if (input$metric == "Largeurs" | input$metric == "Pentes") {
      network_data %>%
        left_join(metrics_data, by = join_by("AXIS"=="AXIS", "M"=="measure"),
                  suffix = c("", ".metrics"), relationship="one-to-one")
    } else if (input$metric == "Occupation du sol") {
      landcover_data %>%
        filter(landcover == input$dynamicRadio) %>%
        right_join(network_data, by = join_by("AXIS"=="AXIS", "measure"=="M"),
                   suffix = c("", ".landcover"), relationship="one-to-one") %>%
        st_as_sf()
    }
  })

  # metrics data to display
  varsel <- reactive({
    if (input$metric == "Largeurs" | input$metric == "Pentes") {
      print(input$dynamicRadio)
      datamap()[[input$dynamicRadio]]
    } else if (input$metric == "Occupation du sol") {
      print(input$dynamicRadio)
      datamap()[["landcover_area"]]
    }
  })

  # mapping interactive change
  observeEvent(input$dynamicRadio, {
    if (input$metric == "Largeurs" | input$metric == "Pentes") {

      breaks <-  unique(quantile(varsel(), probs = seq(0, 1, 0.25), na.rm = TRUE))

      # Define color palette for Reds
      color_palette <- colorRampPalette(c("green", "red"))(length(breaks))

      leafletProxy("ui_exploremap") %>%
        clearShapes() %>%
        addPolylines(data = datamap(), color = ~ {
          ifelse(is.na(varsel()), "grey", color_palette[findInterval(varsel(), breaks, all.inside = TRUE)])
        }
        )

    } else if (input$metric == "Occupation du sol") {

      breaks <-  unique(quantile(varsel(), probs = seq(0, 1, 0.25), na.rm = TRUE))

      # Define color palette for Reds
      color_palette <- colorRampPalette(c("green", "red"))(length(breaks))

      leafletProxy("ui_exploremap") %>%
        clearShapes() %>%
        addPolylines(data = datamap(), color = ~ {
          ifelse(is.na(varsel()), "grey", color_palette[findInterval(varsel(), breaks, all.inside = TRUE)])
        }
        )
    }
  }) # ObserveEvent

}


## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
