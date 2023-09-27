#' Get Hydrographic Basins
#'
#' This function retrieves hydrographic basins.
#'
#' @return A sf data frame containing information about hydrographic basins.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   data <- data_get_bassins()
#' }
#'
#' @importFrom sf st_read
#'
#' @export
data_get_bassins <- function() {
  query <- "SELECT * FROM bassin_hydrographique"
  data <- sf::st_read(dsn = db_con(), query = query)
  return(data)
}


#' Get all the hydrological regions in a Hydrographic Basin
#'
#' This function retrieves regions within a specified hydrographic basin based on its ID.
#'
#' @param selected_bassin_id The ID of the selected hydrographic basin.
#'
#' @return A df data frame containing regions within the specified hydrographic basin.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   data <- data_get_regions_in_bassin(selected_bassin_id = 'cdbh_id')
#' }
#'
#' @importFrom sf st_read
#'
#' @export
data_get_regions_in_bassin <- function(selected_bassin_id = bassin_click$id) {
  query <-
    sprintf("SELECT * FROM region_hydrographique WHERE cdbh LIKE '%s'",
            selected_bassin_id)
  data <- sf::st_read(dsn = db_con(), query = query)
  return(data)
}


#' Get hydrological region selected by user
#'
#' This function retrieves hydrographical data for a specified region based on its ID.
#'
#' @param region_click_id The ID of the selected region.
#'
#' @return A sf data frame containing hydrographical data for the specified region.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   data <- data_get_region(region_click_id = 'some_region_id')
#' }
#'
#' @importFrom sf st_read
#'
#' @export
data_get_region <- function(region_click_id = click_value()$id) {
  query <- sprintf("SELECT * FROM region_hydrographique
                   WHERE gid = '%s'", region_click_id)
  data <- sf::st_read(dsn = db_con(), query = query)
  return(data)
}


#' Get Minimum and Maximum Strahler Values for a Selected Region
#'
#' This function retrieves the minimum and maximum values of the Strahler metric for a specified region.
#'
#' @param selected_region_id The ID of the selected region.
#'
#' @return A data frame containing two columns: 'min' and 'max', representing the minimum and maximum Strahler values for the specified region.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   data <- data_get_min_max_strahler(selected_region_id = 1)
#' }
#'
#' @importFrom glue glue
#' @importFrom DBI dbGetQuery
#'
#' @export
data_get_min_max_strahler <- function(selected_region_id = region_click_id()) {
  query <- glue::glue("
      SELECT
        MIN(strahler) AS min,
        MAX(strahler) AS max
      FROM network_metrics
      WHERE gid_region = {selected_region_id}")

  data <- DBI::dbGetQuery(conn = db_con(), statement = query)

  return(data)
}


#' Get Minimum and Maximum Metric Values for a Selected Region
#'
#' This function retrieves the minimum and maximum values of a selected metric for a specified region
#'
#' @param selected_region_id The ID of the selected region.
#' @param selected_metric The name of the selected metric.
#'
#' @return A data frame containing two columns: 'min' and 'max', representing the minimum and maximum values of the selected metric for the specified region.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   data <- data_get_min_max_metric(selected_region_id = 1, selected_metric = "some_metric")
#' }
#'
#' @importFrom glue glue
#' @importFrom DBI dbGetQuery
#'
#' @export
data_get_min_max_metric <- function(selected_region_id = region_click_id(), selected_metric = selected_metric()) {
  query <- glue::glue("
      SELECT
        ROUND(MIN({selected_metric})::numeric, 1) AS min,
        ROUND(MAX({selected_metric})::numeric, 1) AS max
      FROM network_metrics
      WHERE gid_region = {selected_region_id}")

  data <- DBI::dbGetQuery(conn = db_con(), statement = query)

  return(data)
}



#' Get Basemaps Data Frame
#'
#' This function retrieves information about available basemaps, including their names, URLs, layers, and attribution.
#'
#' @return A data frame containing information about available basemaps, with columns 'name', 'url', 'layer', and 'attribution'.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   basemaps <- data_basemaps_df()
#' }
#'
#' @export
# data_basemaps_df <- function(){
#   basemaps <- data.frame(
#     name = c("Elévation", "Plan IGN", "Satellite IGN", "Occupation du sol", "Géologie"),
#     url = c("https://wxs.ign.fr/altimetrie/geoportail/r/wms", "https://wxs.ign.fr/cartes/geoportail/r/wms", "https://wxs.ign.fr/ortho/geoportail/r/wms", "https://geoserver-dev.evs.ens-lyon.fr/geoserver/mapdo/wms", "http://geoservices.brgm.fr/geologie"),
#     layer = c("ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES", "GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2", "HR.ORTHOIMAGERY.ORTHOPHOTOS", "mapdo:mapdo_landuse_1m", "GEOLOGIE"),
#     attribution = c("IGN-F/Géoportail", "IGN-F/Géoportail", "IGN-F/Géoportail", "CNRS-EVS", "BRGM"),
#     style = c("hypso","", "", "mapdo:MAPDO landuse", "")
#   )
#   return(basemaps)
# }


#' Get Overlayers Data Frame
#'
#' This function retrieves information about available overlayers, including their names, URLs, layers, and attribution.
#'
#' @return A data frame containing information about available overlayers, with columns 'name', 'url', 'layer', and 'attribution'.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   overlayers <- data_overlayers_df()
#' }
#'
#' @export
# data_overlayers_df <- function(){
#   overlayers <- data.frame(
#     name = c("Courbe de niveau", "Zone inondable débordement centenale", "Ouvrage protection inondation"),
#     url = c("https://wxs.ign.fr/altimetrie/geoportail/r/wms", "https://georisques.gouv.fr/services", "https://georisques.gouv.fr/services"),
#     layer = c("ELEVATION.CONTOUR.LINE", "ALEA_SYNT_01_02MOY_FXX", "OUV_PROTECTION_FXX"),
#     attribution = c("IGN-F/Géoportail", "Georisques", "Georisques")
#   )
#   return(overlayers)
# }


#' Get Referentiel des Obstacles aux Ecoulement Data for a Region
#'
#' This function retrieves data about Referentiel des Obstacles aux Ecoulement (ROE) within a specified region based on its ID.
#'
#' @param selected_region_id The ID of the selected region.
#'
#' @return A sf data frame containing information about ROE within the specified region.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   roe_data <- data_get_roe_in_region(selected_region_id = 1)
#' }
#'
#' @importFrom glue glue
#' @importFrom sf st_read
#'
#' @export
data_get_roe_in_region <- function(selected_region_id = region_click$id) {
  query <- glue::glue("
      SELECT
      roe.gid, nomprincip, lbtypeouvr, lbhautchut, gid_region, roe.geom
      FROM roe
      WHERE gid_region = {selected_region_id}
          AND (roe.cdetouvrag LIKE '2')")

  data <- sf::st_read(dsn = db_con(), query = query)
  return(data)
}


#' Get Network Axis Data for a Region
#'
#' This function retrieves data about the network axis within a specified region based on its ID.
#'
#' @param selected_region_id The ID of the selected region.
#'
#' @return A sf data frame containing information about the network axis within the specified region.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   axis_data <- data_get_axis(selected_region_id = 1)
#' }
#'
#' @importFrom glue glue
#' @importFrom sf st_read
#'
#' @export
data_get_axis <- function(selected_region_id = region_click$id) {
  query <- glue::glue("
      SELECT
      network_axis.fid, axis, gid_region, network_axis.geom
      FROM network_axis
      WHERE gid_region = {selected_region_id}")

  data <- sf::st_read(dsn = db_con(), query = query)
  return(data)
}


#' Get Network Metrics Data for a Specific Network Axis
#'
#' This function retrieves data about network metrics for a specific network axis based on its ID.
#'
#' @param selected_axis_id The ID of the selected network axis.
#'
#' @return A sf data frame containing information about network metrics for the specified network axis.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   network_metrics_data <- data_get_network_axis(selected_axis_id = 11)
#' }
#'
#' @importFrom glue glue
#' @importFrom sf st_read
#' @importFrom dplyr arrange
#'
#' @export
data_get_network_axis <- function(selected_axis_id = click_value()$id) {

  query <- glue::glue("
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
      WHERE  axis = {selected_axis_id}")

  data <- sf::st_read(dsn = db_con(), query = query) %>%
    dplyr::arrange(measure)

  return(data)
}
