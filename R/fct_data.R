#' Get Hydrographic Basins
#'
#' This function retrieves hydrographic basins.
#'
#' @param opacity list that contain numeric values clickable and not_clickable to inform the user the non available features.
#' @param con PqConnection to Postgresql database.
#'
#' @return sf data frame containing information about hydrographic basins.
#'
#' @examples
#' con <- db_con()
#' opacity = list(clickable = 0.01,
#'                not_clickable = 0.10)
#'
#' data <- data_get_bassins(opacity = opacity, con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom dplyr mutate if_else
#'
#' @export
data_get_bassins <- function(opacity, con) {
  query <- "SELECT * FROM bassin_hydrographique"
  data <- sf::st_read(dsn = con, query = query) %>%
    mutate(click = if_else(display == TRUE, TRUE, FALSE)) %>%
    mutate(opacity = if_else(display == TRUE, opacity$clickable, opacity$not_clickable))
  return(data)
}


#' Get all the hydrological regions in a Hydrographic Basin
#'
#' This function retrieves regions within a specified hydrographic basin based on its ID.
#'
#' @param selected_bassin_id text ID of the selected hydrographic basin.
#' @param opacity list that contain numeric values clickable and not_clickable to inform the user the non available features.
#' @param con PqConnection to Postgresql database.
#'
#' @return A df data frame containing regions within the specified hydrographic basin.
#'
#' @examples
#' con <- db_con()
#' opacity = list(clickable = 0.01,
#'                not_clickable = 0.10)
#' data <- data_get_regions_in_bassin(selected_bassin_id = "06",
#'                                    opacity = opacity,
#'                                    con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom dplyr mutate if_else
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_regions_in_bassin <- function(selected_bassin_id, opacity, con) {
  sql <- "SELECT * FROM region_hydrographique WHERE cdbh LIKE ?selected_bassin_id"
  query <- sqlInterpolate(con, sql, selected_bassin_id = selected_bassin_id)
  data <- sf::st_read(dsn = con, query = query) %>%
    mutate(click = if_else(display == TRUE, TRUE, FALSE)) %>%
    mutate(opacity = if_else(display == TRUE, opacity$clickable, opacity$not_clickable))
  return(data)
}


#' Get hydrological region selected by user
#'
#' This function retrieves hydrographical data for a specified region based on its ID.
#'
#' @param region_click_id The ID of the selected region.
#' @param con PqConnection to Postgresql database.
#'
#' @return A sf data frame containing hydrographical data for the specified region.
#'
#' @examples
#' con <- db_con()
#' data <- data_get_region(region_click_id = 11, con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_region <- function(region_click_id, con) {
  sql <- "SELECT * FROM region_hydrographique
           WHERE gid = ?region_click_id"
  query <- sqlInterpolate(con, sql, region_click_id = region_click_id)
  data <- sf::st_read(dsn = con, query = query)
  return(data)
}

#' Get all Network Metrics Data for a Specific region
#'
#' This function retrieves data about network metrics for a specific region based on its ID.
#'
#' @param selected_region_id The ID of the selected region
#' @param con PqConnection to Postgresql database.
#'
#' @return A sf data frame containing information about network metrics for the specified region
#'
#' @examples
#' con <- db_con()
#' network_metrics_data <- data_get_network_axis(selected_region_id = 11, con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom dplyr arrange
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_network_region <- function(selected_region_id, con) {

  sql <- "
      SELECT
        network_metrics.fid, axis, measure, toponyme, strahler, talweg_elevation_min,
        active_channel_width, natural_corridor_width,
        connected_corridor_width, valley_bottom_width, talweg_slope, floodplain_slope,
        water_channel, gravel_bars, natural_open, forest, grassland, crops,
        diffuse_urban, dense_urban, infrastructures, active_channel, riparian_corridor,
        semi_natural, reversible, disconnected, built_environment,
        water_channel_pc, gravel_bars_pc, natural_open_pc, forest_pc, grassland_pc, crops_pc,
        diffuse_urban_pc, dense_urban_pc, infrastructures_pc, active_channel_pc,
        riparian_corridor_pc, semi_natural_pc, reversible_pc, disconnected_pc,
        built_environment_pc, sum_area, idx_confinement, gid_region, network_metrics.geom
      FROM network_metrics
      WHERE  gid_region = ?selected_region_id"
  query <- sqlInterpolate(con, sql, selected_region_id = selected_region_id)

  data <- sf::st_read(dsn = con, query = query) %>%
    dplyr::arrange(measure)

  return(data)
}


#' Get Minimum and Maximum Strahler Values for a Selected Region
#'
#' This function retrieves the minimum and maximum values of the Strahler metric for a specified region.
#'
#' @param selected_region_id The ID of the selected region.
#' @param con PqConnection to Postgresql database.
#'
#' @return A data frame containing two columns: 'min' and 'max', representing the minimum and maximum Strahler values for the specified region.
#'
#' @examples
#' con <- db_con()
#' data <- data_get_min_max_strahler(selected_region_id = 11, con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom DBI dbGetQuery
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_min_max_strahler <- function(selected_region_id, con) {
  sql <- "SELECT
        MIN(strahler) AS min,
        MAX(strahler) AS max
      FROM network_metrics
      WHERE gid_region = ?selected_region_id"
  query <- sqlInterpolate(con, sql, selected_region_id = selected_region_id)

  data <- DBI::dbGetQuery(conn = con, statement = query)

  return(data)
}


#' Get Minimum and Maximum Metric Values for a Selected Region
#'
#' This function retrieves the minimum and maximum values of a selected metric for a specified region
#'
#' @param selected_region_id The ID of the selected region.
#' @param selected_metric The name of the selected metric.
#' @param con PqConnection to Postgresql database.
#'
#' @return A data frame containing two columns: 'min' and 'max', representing the minimum and maximum values of the selected metric for the specified region.
#'
#' @examples
#' con <- db_con()
#' data <- data_get_min_max_metric(selected_region_id = 11,
#'                                 selected_metric = "active_channel_width",
#'                                 con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom DBI dbGetQuery sqlInterpolate dbQuoteIdentifier SQL
#'
#' @export
data_get_min_max_metric <- function(selected_region_id, selected_metric, con) {
  sql <- "
      SELECT
        ROUND(MIN(?selected_metric)::numeric, 1) AS min,
        ROUND(MAX(?selected_metric)::numeric, 1) AS max
      FROM network_metrics
      WHERE gid_region = ?selected_region_id"

  query <- sqlInterpolate(conn = con, sql,
                 selected_metric = DBI::dbQuoteIdentifier(con, selected_metric),
                 selected_region_id = DBI::SQL(selected_region_id)
  )

  data <- DBI::dbGetQuery(conn = con, statement = query)

  return(data)
}

#' Get Referentiel des Obstacles aux Ecoulement Data for a Region
#'
#' This function retrieves data about Referentiel des Obstacles aux Ecoulement (ROE) within a specified region based on its ID.
#'
#' @param selected_region_id The ID of the selected region.
#' @param con PqConnection to Postgresql database.
#'
#' @return A sf data frame containing information about ROE within the specified region.
#'
#' @examples
#' con <- db_con()
#' roe_data <- data_get_roe_in_region(selected_region_id = 11, con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_roe_in_region <- function(selected_region_id, con) {
  sql <- "
      SELECT
      roe.gid, axis, distance_axis, nomprincip, lbtypeouvr, lbhautchut, gid_region, roe.geom
      FROM roe
      WHERE gid_region = ?selected_region_id
          AND (roe.cdetouvrag LIKE '2')
          AND (roe.stobstecou LIKE 'Validé')"
  query <- sqlInterpolate(con, sql, selected_region_id = selected_region_id)

  data <- sf::st_read(dsn = con, query = query)
  return(data)
}


#' Get Network Axis Data for a Region
#'
#' This function retrieves data about the network axis within a specified region based on its ID.
#'
#' @param selected_region_id The ID of the selected region.
#' @param con PqConnection to Postgresql database.
#'
#' @return A sf data frame containing information about the network axis within the specified region.
#'
#' @examples
#' con <- db_con()
#' axis_data <- data_get_axis(selected_region_id = 11, con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_axis <- function(selected_region_id, con) {
  sql <- "
      SELECT
      network_axis.fid, axis, toponyme, gid_region, network_axis.geom
      FROM network_axis
      WHERE gid_region = ?selected_region_id"
  query <- sqlInterpolate(con, sql, selected_region_id = selected_region_id)

  data <- sf::st_read(dsn = con, query = query)
  return(data)
}


#' Get Network Metrics Data for a Specific Network Axis
#'
#' This function retrieves data about network metrics for a specific network axis based on its ID.
#'
#' @param selected_axis_id The ID of the selected network axis.
#' @param con PqConnection to Postgresql database.
#'
#' @return A sf data frame containing information about network metrics for the specified network axis.
#'
#' @examples
#' con <- db_con()
#' network_metrics_data <- data_get_network_axis(selected_axis_id = 2000796122, con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom dplyr arrange
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_network_axis <- function(selected_axis_id, con) {

  sql <- "
      SELECT
        network_metrics.fid, axis, measure, toponyme, strahler, talweg_elevation_min,
        active_channel_width, natural_corridor_width,
        connected_corridor_width, valley_bottom_width, talweg_slope, floodplain_slope,
        water_channel, gravel_bars, natural_open, forest, grassland, crops,
        diffuse_urban, dense_urban, infrastructures, active_channel, riparian_corridor,
        semi_natural, reversible, disconnected, built_environment,
        water_channel_pc, gravel_bars_pc, natural_open_pc, forest_pc, grassland_pc, crops_pc,
        diffuse_urban_pc, dense_urban_pc, infrastructures_pc, active_channel_pc,
        riparian_corridor_pc, semi_natural_pc, reversible_pc, disconnected_pc,
        built_environment_pc, sum_area, idx_confinement, gid_region, network_metrics.geom
      FROM network_metrics
      WHERE  axis = ?selected_axis_id"
  query <- sqlInterpolate(con, sql, selected_axis_id = selected_axis_id)

  data <- sf::st_read(dsn = con, query = query) %>%
    dplyr::arrange(measure)

  return(data)
}


#' Get the start and end coordinates of a spatial object's axis
#'
#' This function takes a spatial object with a LINESTRING geometry and returns
#' a data frame containing the start and end coordinates of the axis.
#'
#' @param dgo_axis A spatial sf object with a LINESTRING geometry representing an axis.
#'
#' @return A data frame with two rows, where the first row contains the start
#'         coordinates (x and y) and the second row contains the end coordinates (x and y).
#'
#' @importFrom sf st_coordinates st_cast st_sf st_linestring
#' @importFrom utils tail head
#'
#' @examples
#' library(sf)
#' line_coords <- matrix(c(0, 0, 1, 1), ncol = 2)
#' # Create an sf object with the LINESTRING
#' line_sf <- st_sf(geometry = st_sfc(st_linestring(line_coords)))
#' df <- data_get_axis_start_end(line_sf)
#'
#' @export
data_get_axis_start_end <- function(dgo_axis) {

  # Extract the start and end points of the axis
  axis_point_start <- st_coordinates(head(st_cast(tail(dgo_axis, n = 1), "POINT")$geom, n = 1))
  axis_point_end <- st_coordinates(tail(st_cast(head(dgo_axis, n = 1), "POINT")$geom, n = 1))

  # Combine the coordinates into a data frame
  axis_start_end <- data.frame(rbind(axis_point_start, axis_point_end))

  return(axis_start_end)
}

#' Get Network DGO Data in a Region
#'
#' This function retrieves data about the network DGO with the metrics within a specified region based on its ID.
#'
#' @param selected_region_id The ID of the selected region.
#' @param con PqConnection to Postgresql database.
#'
#' @return A sf data frame containing information about the network axis within the specified region.
#'
#' @examples
#' con <- db_con()
#' axis_data <- data_get_dgo_in_region(selected_region_id = 11, con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_dgo_in_region <- function(selected_region_id, con){
  sql <- "
      SELECT
        network_metrics.fid, axis, measure, toponyme, strahler, talweg_elevation_min,
        active_channel_width, natural_corridor_width,
        connected_corridor_width, valley_bottom_width, talweg_slope, floodplain_slope,
        water_channel, gravel_bars, natural_open, forest, grassland, crops,
        diffuse_urban, dense_urban, infrastructures, active_channel, riparian_corridor,
        semi_natural, reversible, disconnected, built_environment,
        water_channel_pc, gravel_bars_pc, natural_open_pc, forest_pc, grassland_pc, crops_pc,
        diffuse_urban_pc, dense_urban_pc, infrastructures_pc, active_channel_pc,
        riparian_corridor_pc, semi_natural_pc, reversible_pc, disconnected_pc,
        built_environment_pc, sum_area, idx_confinement, gid_region, network_metrics.geom
      FROM network_metrics
      WHERE  gid_region = ?selected_region_id"
  query <- sqlInterpolate(con, sql, selected_region_id = selected_region_id)

  data <- sf::st_read(dsn = con, query = query)
  return(data)
}

#' Get hydrometric sites.
#'
#' This function retrieves data about the hydrometric sites from Hubeau.
#'
#' @param selected_region_id The ID of the selected region.
#' @param con PqConnection to Postgresql database.
#'
#' @return sf data frame containing information about the hydrometric sites within the specified region.
#'
#' @examples
#' con <- db_con()
#' hydro_sites <- data_get_hydro_sites(selected_region_id = 11, con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_hydro_sites <- function(selected_region_id, con){

  sql <- "
          SELECT
            code_site, libelle_site, url_site, geom
          FROM hydro_sites
          WHERE gid_region = ?selected_region_id"
  query <- sqlInterpolate(con, sql, selected_region_id = selected_region_id)

  data <- sf::st_read(dsn = con, query = query)

  return(data)
}

#' Get elevation profiles data from selected dgo fid.
#'
#' @param selected_dgo_fid integer selected dgo fid.
#' @param con PqConnection to Postgresql database.
#'
#' @importFrom DBI dbGetQuery sqlInterpolate
#' @importFrom dplyr arrange mutate
#'
#' @return data.frame
#' @export
#'
#' @examples
#' con <- db_con()
#' data_get_elevation_profiles(selected_dgo_fid = 95, con = con)
#' DBI::dbDisconnect(con)
data_get_elevation_profiles <- function(selected_dgo_fid, con){

  sql <- "
          SELECT
          	id, hydro_swaths_gid, axis, measure_medial_axis, distance, profile
          FROM elevation_profiles
          WHERE hydro_swaths_gid = ?selected_dgo_fid"
  query <- sqlInterpolate(con, sql, selected_dgo_fid = selected_dgo_fid)

  data <- DBI::dbGetQuery(conn = con, statement = query) %>%
    arrange(distance) %>%
    mutate(profile = round(profile, digits = 2))
  return(data)
}


#' Create initial dataframe for 1-variable classification, to be displayed on the UI table
#'
#' @param axis_data sf-object of an axis, containing all dgos inside the axis
#' @param variable_name name of variable for which the classification should be undertaken
#' @param no_classes number of classes to be generated
#' @param quantile size of quantile which provides value-range of classification
#'
#' @return dataframe with 4 columns: class (name of each class, here automatically set from A-Z),
#'         variable (variable chosen for classification), greaterthan (values defining the threshold of each class),
#'         and color (defining the coloring for the map)
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#'df <- create_df_input(
#'       axis_data = network_dgo,
#'       variable_name = input$variable,
#'       no_classes = input$no_classes,
#'       quantile = input$quantile
#'       )
#'
create_df_input <- function(axis_data, variable_name, no_classes = 4, quantile = 95){

  # set upper and lower boundaries of quantile interval
  q_low <- (1 - quantile/100)/2
  q_high <- 1 - q_low

  # calculate quantile values (max, min) and steps
  q_values <- quantile(axis_data[[variable_name]], probs = c(q_low, q_high), na.rm = TRUE)
  q_steps <- (q_values[[2]] - q_values[[1]])/no_classes

  # empty dataframe to store class thresholds
  classes <- rep(0, no_classes)

  # set threshold values of all classes
  for (i in 1:no_classes) {
    classes[i] <- round(q_steps*(no_classes - i), 2)
  }

  # create dataframe
  df <- data.frame(class = LETTERS[1:no_classes],
                   variable = variable_name,
                   greaterthan = classes,
                   color = {if (no_classes == 2) {c("#B2182B", "#2166AC")} else {brewer.pal(no_classes, "RdBu")}},
                   stringsAsFactors = FALSE)

  return(df)
}


#' Assign classes to network dgos
#'
#' @param data dataframe or sf object to which classes
#' @param variables vector of variable names on which the classification is based
#' @param greater_thans vector of class-thresholds for classification
#' @param class_names vector containing the names of all classes to be assigned
#'
#' @return classified dataframe/sf object with additional variable: class
#' @importFrom rlang parse_exprs
#' @importFrom dplyr mutate case_when left_join join_by
#' @importFrom sf st_as_sf
#'
#' @examples
#' classified_network <- network_dgo %>%
#'     assign_classes(variables = as.character(r_val$grouping_table_data$variable),
#'     greater_thans = r_val$grouping_table_data$greaterthan,
#'     class_names = r_val$grouping_table_data$class)
#'
assign_classes <- function(data, classes) {

  variables <- as.character(classes$variable)
  greater_thans <- classes$greaterthan
  class_names <- classes$class
  colors <- classes %>% select(class, color)

  df <-
    data %>%
    mutate(
      class_name = case_when(
        !!!parse_exprs(paste0(variables, ' >= ', greater_thans, ' ~ "', class_names, '"')
        )
      )
    ) %>%
    left_join(colors, by = join_by(class_name == class))

  return(df)
}
