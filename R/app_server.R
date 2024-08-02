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

  ### R_VAL ####
  r_val <- reactiveValues(

    # UI
    selection_text = "", # description text indicating basin, region, axis

    # map
    map_proxy = NULL,
    opacity_basins = list(clickable = 0.01, not_clickable = 0.10) # opacity value to inform the user about available bassins and regions

  )

  ### DB connection ####
  con <- db_con()

  ### Server activation ####
  # main servers
  mod_mapdo_app_server("mapdo_app_1", con, r_val)
  mod_documentation_server("documentation_1")

  # tabs
  # mod_classification_proposed_server("classification_proposed_1", r_val)
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
