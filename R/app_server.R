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
    tab_page = NULL, # selected tab in main navbarpage
    tab_classes = NULL, # selected tab in classes tabset
    tab_plots = NULL, # selected tab in plots tabset

    # map
    map_proxy = NULL,
    opacity_basins = list(clickable = 0.01, not_clickable = 0.10), # opacity value to inform the user about available bassins and regions
    leaflet_hover_measure = NULL, # measure to be displayed in the leaflet hover

    # geo objects
    basin_name = NULL,
    basin_id = NULL,
    region_name = NULL,
    region_id = NULL,
    axis_name = NULL,
    axis_id = NULL,
    axis_data = NULL,
    axis_data_classified = NULL,
    axis_strahler = NULL,
    swath_id = NULL,
    swath_data_section = NULL,
    swath_data_dgo = NULL,

    # first time clicked
    axis_clicked = FALSE,

    manual_classes_table = NULL, # values of classes and assigned colors from manual classification
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


  #### Axis data caching ####
  globals$axis_data <- reactive({
    data_get_axis_dgos(selected_axis_id = r_val$axis_id, con)
  }) %>%
    bindCache(c(r_val$axis_id, globals$regions_gids_key))

  #### Metric stats caching ####
  globals$metric_stats <- reactive({
    data_get_stats_metrics(con)
  }) %>%
    bindCache(globals$regions_gids_key)

  #### Classes stats caching ####
  globals$classes_stats <- reactive({
    if (!is.null(r_val$classes_proposed_selected)) {
      data_get_distr_class(con = con, class_name = globals$classes_proposed[r_val$classes_proposed_selected,]$class_name)
    } else {
      NULL
    }

  }) %>%
    bindCache(globals$regions_gids_key, r_val$classes_proposed_selected)


  # navbarPage identifier
  observeEvent(input$navbarPage, {
    r_val$tab_page = input$navbarPage
  })

  ### Server activation ####
  # main servers
  mod_explore_server("explore_1", con, r_val, globals)
  mod_analysis_server("analysis_1", con, r_val, globals)
  mod_documentation_server("documentation_1")

  # tabs
  mod_expl_classes_proposed_server("expl_classes_proposed_1", r_val, globals)
  mod_expl_classes_manual_server("expl_classes_manual_1", con, r_val, globals)

  mod_expl_plot_long_server("expl_plot_long_1", r_val, globals)
  mod_expl_plot_crosssection_server("expl_plot_crosssection_1", r_val)


  # mod_metric_overview_server("metric_overview_1", r_val)
  # mod_classes_distribution_server("classes_distribution_1", r_val)

  ### DB disconnect when closing session ####
  onStop(function() {
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })
}
