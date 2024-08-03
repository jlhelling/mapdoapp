

# START load --------------------------------------------------------------

#' Get Hydrographic Basins
#'
#' This function retrieves hydrographic basins.
#'
#' @param opacity list that contain numeric values clickable and not_clickable to inform the user the non available features.
#' @param con Connection to Postgresql database.
#'
#' @return sf data frame containing information about hydrographic basins.
#'
#' @examples
#' con <- db_con()
#' opacity = list(clickable = 0.01,
#'                not_clickable = 0.10)
#'
#' data <- data_get_basins(con = con, opacity = opacity)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom dplyr mutate if_else
#'
#' @export
data_get_basins <- function(con, opacity) {

  query <- "SELECT * FROM bassin_hydrographique"

  data <- sf::st_read(dsn = con, query = query) %>%
    mutate(click = if_else(display == TRUE, TRUE, FALSE)) %>%
    mutate(opacity = if_else(display == TRUE, opacity$clickable, opacity$not_clickable))

  return(data)
}

#' Get hydrological regions
#'
#' This function retrieves all hydrological regions available on the server and converts them to sf
#'
#' @param con Connection to Postgresql database.
#'
#' @return A sf data frame containing hydrological regions.
#'
#' @examples
#' con <- db_con()
#' data <- data_get_regions(con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_regions <- function(con, opacity) {

  query <- "SELECT * FROM region_hydrographique"

  data <- sf::st_read(dsn = con, query = query) %>%
    mutate(click = if_else(display == TRUE, TRUE, FALSE)) %>%
    mutate(opacity = if_else(display == TRUE, opacity$clickable, opacity$not_clickable))

  return(data)
}

#' Get Network Axes Data
#'
#' This function retrieves data about the network axes available on the server
#'
#' @param con Connection to Postgresql database.
#'
#' @return A sf data frame containing the network axes.
#'
#' @examples
#' con <- db_con()
#' axis_data <- data_get_axes(con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_axes <- function(con) {
  query <- "SELECT network_axis.fid, axis, toponyme, gid_region, network_axis.geom FROM network_axis"

  data <- sf::st_read(dsn = con, query = query)
  return(data)
}

#' Get Referentiel des Obstacles aux Ecoulement Data
#'
#' This function retrieves the datapoints of the Referentiel des Obstacles aux Ecoulement (ROE).
#'
#' @param con Connection to Postgresql database.
#'
#' @return A sf data frame containing the ROE datapoints.
#'
#' @examples
#' con <- db_con()
#' roe_data <- data_get_roe_sites(con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_roe_sites <- function(con) {
  query <- "
      SELECT roe.gid, axis, distance_axis, nomprincip, lbtypeouvr, lbhautchut, gid_region, roe.geom
      FROM roe
      WHERE (roe.cdetouvrag LIKE '2') AND (roe.stobstecou LIKE 'Validé')"

  data <- sf::st_read(dsn = con, query = query)

  return(data)
}

#' Get hydrometric sites.
#'
#' This function retrieves the locations of the hydrometric sites from Hubeau.
#'
#' @param con Connection to Postgresql database.
#'
#' @return sf data frame containing the datapoints of the hydrometric sites available on the server
#'
#' @examples
#' con <- db_con()
#' hydro_sites <- data_get_hydro_sites(con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_hydro_sites <- function(con){

  query <- "
          SELECT code_site, libelle_site, url_site, geom
          FROM hydro_sites"

  data <- sf::st_read(dsn = con, query = query)

  return(data)
}

#' Get statistics on network metrics for different levels (france, basin, region)
#'
#' @param con Connection to Postgresql database.
#'
#' @return Dataframe which contains the statistics for all metrics for different entities: France, Basins, Regions
#'
#' @examples
data_get_stats_metrics <- function(con) {

  variables <- c(
    "talweg_elevation_min", "active_channel_width", "natural_corridor_width",
    "connected_corridor_width", "valley_bottom_width", "talweg_slope", "floodplain_slope",
    "water_channel", "gravel_bars", "natural_open", "forest",
    "grassland", "crops", "diffuse_urban", "dense_urban",
    "infrastructures", "active_channel", "riparian_corridor", "semi_natural",
    "reversible", "disconnected", "built_environment", "water_channel_pc",
    "gravel_bars_pc", "natural_open_pc", "forest_pc", "grassland_pc",
    "crops_pc", "diffuse_urban_pc", "dense_urban_pc", "infrastructures_pc",
    "active_channel_pc", "riparian_corridor_pc", "semi_natural_pc", "reversible_pc",
    "disconnected_pc", "built_environment_pc", "idx_confinement"
  )

  query_stats <-
    paste0(
      paste(
        lapply(variables, function(var) {
          paste0(
            "  AVG(", var, ") AS ", var, "_avg,\n",
            "  MIN(", var, ") AS ", var, "_min,\n",
            "  percentile_cont(0.025) WITHIN GROUP (ORDER BY ", var, ") AS ", var, "_0025,\n",
            "  percentile_cont(0.25) WITHIN GROUP (ORDER BY ", var, ") AS ", var, "_025,\n",
            "  percentile_cont(0.5) WITHIN GROUP (ORDER BY ", var, ") AS ", var, "_05,\n",
            "  percentile_cont(0.75) WITHIN GROUP (ORDER BY ", var, ") AS ", var, "_075,\n",
            "  percentile_cont(0.975) WITHIN GROUP (ORDER BY ", var, ") AS ", var, "_0975,\n",
            "  MAX(", var, ") AS ", var, "_max"
          )
        }),
        collapse = ",\n"
      ),
      "\nFROM network_metrics\n"
    )

  # Constructing the SQL query
  query <- paste0(
    "SELECT\n",
    "'France' AS level_type,\n",
    "'France' AS level_name,\n",
    query_stats,
    "WHERE network_metrics.gid_region IS NOT NULL\n",

    "\nUNION ALL\n",

    "SELECT\n",
    "'Basin' AS level_type,\n",
    "region_hydrographique.cdbh AS level_name,\n",
    query_stats,
    "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
    "WHERE network_metrics.gid_region IS NOT NULL\n",
    "GROUP BY region_hydrographique.cdbh\n",

    "\nUNION ALL\n",

    "SELECT\n",
    "'Region' AS level_type,\n",
    "CAST(network_metrics.gid_region as varchar(10)) AS level_name,\n",
    query_stats,
    "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
    "WHERE network_metrics.gid_region IS NOT NULL\n",
    "GROUP BY network_metrics.gid_region"
  )

  data <- DBI::dbGetQuery(conn = con, statement = query)

  return(data)
}



# data_get_stats_classes_proposed <- function(con) {
#
# }
#
# data_get_stats_classes_manual <- function(con) {
#
# }

# EVENT load --------------------------------------------------------------



data_get_dgos_of_axis <- function() {

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
