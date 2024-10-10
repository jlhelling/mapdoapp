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
        actionButton(ns("prepare_download_button"), "Préparer et visualiser les données", icon = icon("magnifying-glass-chart"))
      ),
      column(
        width = 9,

        # table
        div(style = 'overflow-x: auto; width: 100%;',  # Horizontal scroll and full-width div
            tableOutput(ns("table_data"))
        ),

        # Output: download button
        downloadButton(ns("download_button"), "Télécharger les données", icon = icon("download"))
      )
    )



  )
}

#' download Server Functions
#'
#' @import shiny
#' @importFrom sf st_as_text st_drop_geometry
#'
#' @noRd
mod_download_server <- function(id, con, r_val, globals){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # reactive values ----
    r_val_local <- reactiveValues(
      scale = NULL, # scale selectInput
      dataset_input = NULL, # dataset input to be download
      dataset_input_name = NULL # dataset input name
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


    # Download actions ----

    # prepare data for download
    observeEvent(input$prepare_download_button, {

      # check if tab open
      if (r_val$tab_page == "Télechargement" && input$select_scale %in% c("Bassin", "Région", "Axe")) {


        if (input$select_type == "Réseau hydrographique") {
          if (input$select_scale == "Axe") {
            r_val_local$dataset_input <- r_val$axis_data_classified
            r_val_local$dataset_input_name <- paste0("axe_", r_val$axis_id, "_réseau_classifié")
          }


          #   else if (input$select_scale == "Région") {
          #     r_val_local$dataset_input <- data_get_regions(con)
          #     r_val_local$dataset_input_name <- "reseau_hydrographique_regions"
          #   } else if (input$select_scale == "Axe") {
          #     r_val_local$dataset_input <- data_get_axes(con)
          #     r_val_local$dataset_input_name <- "reseau_hydrographique_axes"
          #   }
        } else if (input$select_type == "Obstacles à l'écoulement") {
          if (input$select_scale == "Axe") {
            r_val_local$dataset_input <- globals$roe_sites() %>%
              filter(axis == r_val$axis_id)
            r_val_local$dataset_input$geom_wkt <- st_as_text(r_val_local$dataset_input$geom)
            r_val_local$dataset_input <- r_val_local$dataset_input %>%
              st_drop_geometry()
            r_val_local$dataset_input_name <- paste0("axe_", r_val$axis_id, "_roe")
          }

          else if (input$select_scale == "Région") {
            r_val_local$dataset_input <- globals$roe_sites() %>%
              filter(gid_region == r_val$region_id)
            r_val_local$dataset_input$geom_wkt <- st_as_text(r_val_local$dataset_input$geom)
            r_val_local$dataset_input <- r_val_local$dataset_input %>%
              st_drop_geometry()
            r_val_local$dataset_input_name <- paste0("region_", r_val$region_id, "_roe")
          }
          #   else if (input$select_scale == "Bassin") {
          #     r_val_local$dataset_input <- data_get_obstacles(con)
          #     r_val_local$dataset_input_name <- "obstacles_ecoulement_bassins"
          #   }
        }
        # } else if (input$select_type == "Sites hydrométriques") {
        #   if (input$select_scale == "Bassin") {
        #     r_val_local$dataset_input <- data_get_hydro_sites(con)
        #     r_val_local$dataset_input_name <- "sites_hydrometriques_bassins"
        #   } else if (input$select_scale == "Région") {
        #     r_val_local$dataset_input <- data_get_hydro_sites(con)
        #     r_val_local$dataset_input_name <- "sites_hydrometriques_regions"
        #   } else if (input$select_scale == "Axe") {
        #     r_val_local$dataset_input <- data_get_hydro_sites(con)
        #     r_val_local$dataset_input_name <- "sites_hydrometriques_axes"
        #   }


      }
    })

    # Download csv of selected dataset ----
    observe({

      if (!is.null(r_val_local$dataset_input)) {
        output$table_data <- renderTable(
          head(r_val_local$dataset_input),
          striped = TRUE,
          hover = TRUE,
          bordered = TRUE,
          width = "100%"
        )

        output$download_button <- downloadHandler(
          filename = function() {
            paste0(r_val_local$dataset_input_name, ".csv")
          },
          content = function(file) {
            write.csv(r_val_local$dataset_input, file, row.names = FALSE)
          }
        )
      }

    })


  })
}
