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
#' selected_region_feature <- get_region(region_click_id = region_click$id)
get_region <- function(region_click_id = region_click$id){
  query <- sprintf("SELECT * FROM region_hydrographique
                   WHERE cdregionhy LIKE '%s'", region_click_id)
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
      network_metrics.fid, toponyme, strahler, active_channel_width, natural_corridor_width,
      connected_corridor_width, valley_bottom_width, talweg_slope, floodplain_slope,
      water_channel, gravel_bars, natural_open, forest, grassland, crops,
      diffuse_urban, dense_urban, infrastructures, network_metrics.geom
      FROM network_metrics, region_hydrographique
      WHERE ST_Intersects(network_metrics.geom, region_hydrographique.geom)
          AND region_hydrographique.cdregionhy = '%s'", selected_region_id)

  data <- st_read(dsn = db_con(), query = query)
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
    "Largeurs" = c(
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
    )
  )

  return(choices_map)
}
