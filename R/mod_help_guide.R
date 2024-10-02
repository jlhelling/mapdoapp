#' help_guide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom cicerone use_cicerone
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_help_guide_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("help_btn"), "Guide d'aide"),
    use_cicerone() # Load the cicerone dependencies
  )
}

#' help_guide Server Functions
#'
#' @importFrom cicerone Cicerone
#'
#' @noRd
mod_help_guide_server <- function(id, r_val){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Define the cicerone tours for each tab
    tour <- Cicerone$new()

    # Set up a dynamic observer based on the active tab
    observeEvent(input$help_btn, {

      # Reset tour steps for each new tab
      tour$reset()

      if (r_val$tab_page == "Exploration") {
        tour$
          step("explore_1-map", "Carte",
                  description = "General overview about all rivers for which data is available. The map has different functionalities : \n
                  Layer-Selection :
                    - Selection of baselayers, including satellite imagery, landuse and geologic maps, and the IGN Plan
                    - Selection of Feature-layers, including geographic entities of basins and sub-basin regions for the hydrographic network,
                  ",
                  position = "right")$
          step("explore_1-selection_textUI", "Sélection actuelle", "Names of currently selected basin, region and axis.",
               position = "right")
      }
      # Add more tabs and steps as needed...

      tour$init()$start() # Start the tour
    })
  })
}
