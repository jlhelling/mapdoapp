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
    selected_region_feature = NULL,
    region_click = NULL,
    axis_click = NULL,

    selected_metric = "talweg_elevation_min", # select main metric column name

    # data hydrography
    bassins = NULL,
    bassin_name = NULL,
    regions_in_bassin = NULL,
    region_name = NULL,
    selected_region_feature = NULL,
    network_region_axis = NULL,
    network_region = NULL,
    axis_name = NULL,
    axis_click = NULL,
    dgo_axis = NULL,
    axis_start_end = NULL,

    # data external
    roe_region = NULL,
    hydro_sites_region = NULL,
    roe_axis = NULL,

    # others variables
    opacity = list(clickable = 0.01, not_clickable = 0.10) # opacity value to inform the user about available bassins and regions

  )

  # set database connection
  con <- db_con()

  # Your application server logic
  mod_mapdo_app_server("mapdo_app_1", con, r_val)
  mod_explore_server("explore_1", con)
  mod_documentation_server("documentation_1")

  ### server activation ####
  mod_fluvial_styles_server("fluvial_styles_1", r_val)
  mod_metric_analysis_server("metric_analysis_1", r_val)
  mod_profil_transverse_server("profil_transverse_1", r_val)

  # disconnect database when closing session
  onStop(function() {
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })
}
