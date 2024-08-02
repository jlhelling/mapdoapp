#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DBI dbDisconnect
#'
#'
#' @noRd
app_server <- function(input, output, session) {

  # track session
  track_session(session = session)

  ### DB connection ####
  con <- db_con()

  ### R_VAL ####
  r_val <- reactiveValues(

    # UI
    selection_text = "", # description text indicating basin, region, axis
    visualization = "classes", # stating which visualization is currently active
    classes_proposed_selected = NULL,

    # map
    map_proxy = NULL,
    opacity_basins = list(clickable = 0.01, not_clickable = 0.10) # opacity value to inform the user about available bassins and regions
  )

  ### GLOBALS ####
  # create empty list to store fixed global values which can be accessed by other modules
  # globals <- list()

  # load regions sf data
  globals$regions = data_get_regions(con, opacity = list(clickable = 0.01, not_clickable = 0.10))

  # Create a unique key based on the regions_gids content
  globals$regions_gids_key = paste(collapse = "_", sort(globals$regions$gid))

  # load basins sf data (cached)
  globals$basins <- reactive({
    data_get_basins(con, opacity = list(clickable = 0.01, not_clickable = 0.10))
  }) %>%
    bindCache(globals$regions_gids_key)

  # load axes sf data (cached)
  globals$axes <- reactive({
    data_get_axes(con)
  }) %>%
    bindCache(globals$regions_gids_key)

  # load roe sf data (cached)
  globals$roe_sites <- reactive({
    data_get_roe_sites(con)
  }) %>%
    bindCache(globals$regions_gids_key)

  # load discharge stations sf data (cached)
  globals$hydro_sites <- reactive({
    data_get_hydro_sites(con)
  }) %>%
    bindCache(globals$regions_gids_key)




  ### Server activation ####
  # main servers
  mod_mapdo_app_server("mapdo_app_1", con, r_val, globals)
  mod_documentation_server("documentation_1")

  # tabs
  mod_classification_proposed_server("classification_proposed_1", r_val, globals)
  # mod_classification_manual_server("classification_manual_1", con, r_val)
  # mod_metric_overview_server("metric_overview_1", r_val)
  # mod_profil_long_server("profil_long_1", r_val)
  # mod_profil_transverse_server("profil_transverse_1", r_val)
  # mod_classes_distribution_server("classes_distribution_1", r_val)

  ### DB disconnect when closing session ####
  onStop(function() {
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })
}
