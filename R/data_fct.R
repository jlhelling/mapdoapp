#' get hydrographic bassin from database
#'
#' @return sf data.frame
#' @export
#'
#' @examples
#' bassin_hydro <- get_bassins()
get_bassins <- function() {
  query <- "SELECT * FROM bassin_hydrographique"
  data <- st_read(dsn = db_con(), query = query)
  return(data)
}

#' get the hydrographic regions from the selected bassin
#'
#' @param selected_region
#'
#' @return sf data.frame
#' @export
#'
#' @examples
#' region_hydro <- get_regions_in_bassin(selected_bassin_id = bassin_click$id)
get_regions_in_bassin <- function(selected_bassin_id = bassin_click$id) {
  query <-
    sprintf("SELECT * FROM region_hydrographique WHERE cdbh LIKE '%s'",
            selected_bassin_id)
  data <- st_read(dsn = db_con(), query = query)
  return(data)
}

#' get region clicked by user
#'
#' @param region_click_id
#'
#' @return sf data.frame
#' @export
#'
#' @examples
#' selected_region_feature <- get_region(region_click_id = click_value()$id)
get_region <- function(region_click_id = click_value()$id){
  query <- sprintf("SELECT * FROM region_hydrographique
                   WHERE gid = '%s'", region_click_id)
  data <- st_read(dsn = db_con(), query = query)
  return(data)
}

#' get network in region selected with metrics data
#'
#' @param selected_region_id
#'
#' @return sf data.frame
#' @export
#'
#' @examples
#' data <- get_network_region_with_metrics(selected_region_id = region_click$id)
get_network_region_with_metrics <- function(selected_region_id = region_click$id){
  query <- sprintf("
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
      WHERE  gid_region = '%s'", selected_region_id)

  data <- st_read(dsn = db_con(), query = query)
  return(data)
}

data_get_min_max_strahler <- function(selected_region_id = region_click_id()){
  query <- sprintf("
      SELECT MIN(strahler) AS min_strahler, MAX(strahler) AS max_strahler FROM network_metrics
        WHERE gid_region = '%s'", selected_region_id)

  data <- dbGetQuery(conn = db_con(), statement = query)

  return(data)
}

#' Create basemaps dataframe
#'
#' @return data.frame
#' @export
#'
#' @examples
#' basemaps_df()
basemaps_df <- function(){
  basemaps <- data.frame(
    name = c("Plan IGN", "Satellite IGN", "Géologie"),
    url = c("https://wxs.ign.fr/cartes/geoportail/r/wms", "https://wxs.ign.fr/ortho/geoportail/r/wms", "http://geoservices.brgm.fr/geologie"),
    layer = c("GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2", "HR.ORTHOIMAGERY.ORTHOPHOTOS", "GEOLOGIE"),
    attribution = c("IGN-F/Géoportail", "IGN-F/Géoportail", "BRGM")
  )
  return(basemaps)
}

#' Create overlayers dataframe
#'
#' @return data.frame
#' @export
#'
#' @examples
#' overlayers_df()
overlayers_df <- function(){
  overlayers <- data.frame(
    name = c("Zone inondable débordement centenale", "Ouvrage protection inondation"),
    url = c("https://georisques.gouv.fr/services", "https://georisques.gouv.fr/services"),
    layer = c("ALEA_SYNT_01_02MOY_FXX", "OUV_PROTECTION_FXX"),
    attribution = c("Georisques", "Georisques")
  )
  return(overlayers)
}

#' get ROE data from PostgreSQL
#'
#' @param selected_region_id selected region id by user
#'
#' @return sf data.frame
#' @export
#'
#' @examples
#' get_roe_in_region(region_click$id)
get_roe_in_region <- function(selected_region_id = region_click$id){
  query <- sprintf("
      SELECT
      roe.gid, nomprincip, lbtypeouvr, lbhautchut, gid_region, roe.geom
      FROM roe
      WHERE gid_region = '%s'
          AND (roe.cdetouvrag LIKE '2')", selected_region_id)

  data <- st_read(dsn = db_con(), query = query)
  return(data)
}

#' get axis network data
#'
#' @param selected_region_id selected region id by user
#'
#' @return sf data.frame
#' @export
#'
#' @examples
#' get_axis(selected_region_id = click_value()$id)
get_axis <- function(selected_region_id = region_click$id){
  query <- sprintf("
      SELECT
      network_axis.fid, axis, gid_region, network_axis.geom
      FROM network_axis
      WHERE gid_region = '%s'", selected_region_id)

  data <- st_read(dsn = db_con(), query = query)
  return(data)
}

get_network_axis <- function(selected_axis_id = click_value()$id){

  query <- sprintf("
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
      WHERE  axis = '%s'", selected_axis_id)

  data <- st_read(dsn = db_con(), query = query) %>%
    arrange(measure)

  # data <- network_data %>%
  #   as.data.frame() %>%
  #   filter(axis == axis_id) %>%
  #   arrange("measure")
  return(data)
}




#' map available metrics
#'
#' @return list
#' @export
#'
#' @examples
#' metrics_choice()
metrics_choice <- function() {
  choices_map <- list(
    "Largeurs (m)" = c(
      "Chenal actif" = "active_channel_width",
      "Corridor naturel" = "natural_corridor_width",
      "Corridor connecté" = "connected_corridor_width",
      "Fond de vallée" = "valley_bottom_width"
    ),
    "Pentes" = c(
      "Pente du talweg" = "talweg_slope",
      "Pente du fond de vallée" = "floodplain_slope"
    ),
    "Occupation du sol" = c(
      "Surface en eau" = "water_channel",
      "Bancs sédimentaires" = "gravel_bars",
      "Espace naturel ouvert" = "natural_open",
      "Forêt" = "forest",
      "Prairie permanente" = "grassland",
      "Culture" = "crops",
      "Périurbain" = "diffuse_urban",
      "Urbain dense" = "dense_urban",
      "Infrastructure de stransport" = "infrastructures"
    ),
    "Continuité latérale" = c(
      "Bande active" = "active_channel",
      "Corridor naturel" = "riparian_corridor",
      "Corridor semi-naturel" = "semi_natural",
      "Espace de réversibilité" = "reversible",
      "Espace déconnecté" = "disconnected",
      "Espace artificialisé" = "built_environment"
    ),
    "Indices" = c(
      "Indice de confinement" = "idx_confinement"
    )
  )

  return(choices_map)
}
