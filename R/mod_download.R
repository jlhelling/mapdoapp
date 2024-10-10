#' download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_ui <- function(id) {
  ns <- NS(id)
  tagList(

    tags$head(
      tags$style(
        HTML("
          .form-group{margin-bottom: 10px}
          ")
      )
    ), # head

    fluidRow(
      id = ns("network"),
      column(
        width = 3,
        # Input: choose dataset type
        selectInput(
          ns("select_type"),
          "Sélectionnez un ensemble de données :",
          choices = c("Réseau hydrographique", "Obstacles à l'écoulement", "Sites hydrométriques")
        ),

        # Input: choose scale
        uiOutput(ns("select_scaleUI")),

        # apply button
        actionButton("apply_download", "Préparer et visualiser les données", icon = icon("magnifying-glass-chart"))
      ),
      column(
        width = 9,

        tableOutput(ns("table_data")),

        # Output: download button
        downloadButton(ns("download_button"), "Télécharger les données", icon = icon("download"))
      )
    )



  )
}

#' download Server Functions
#'
#' @noRd
mod_download_server <- function(id, con, r_val, globals){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # reactive values ----
    r_val_local <- reactiveValues(
      scale = NULL
    )


    # UI ----

    output$select_scaleUI <- renderUI({
      r_val_local$scale
    })

    # create select-input based on selected hydrographic entity
    observeEvent(c(r_val$tab_page, r_val$basin_id, r_val$region_id, r_val$axis_id), {

      # check if tab open
      if (r_val$tab_page == "Télechargement" ) {

        if (!is.null(r_val$basin_id) && is.null(r_val$region_id)) {
          r_val_local$scale <- selectInput(
            ns("select_scale"),
            "Sélectionnez une échelle :",
            choices = c("Bassin"),
            selected = "Bassin"
          )
        }

        else if (!is.null(r_val$region_id) && is.null(r_val$axis_id)) {
          r_val_local$scale <- selectInput(
            ns("select_scale"),
            "Sélectionnez une échelle :",
            choices = c("Bassin", "Région"),
            selected = "Région"
          )
        }

        else if (!is.null(r_val$axis_id)) {
          r_val_local$scale <- selectInput(
            ns("select_scale"),
            "Sélectionnez une échelle :",
            choices = c("Bassin", "Région", "Axe"),
            selected = "Axe"
          )
        } else {
          r_val_local$scale <- HTML("Choisissez une entité hydrographique (bassin / région / axe) pour continuer.")
        }


      }
    })

  })
}
