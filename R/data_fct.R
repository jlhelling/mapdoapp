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
      SELECT *
      FROM network_metrics, region_hydrographique
      WHERE ST_Intersects(network_metrics.geom, region_hydrographique.geom)
          AND region_hydrographique.cdregionhy = '%s'", selected_region_id)

  data <- st_read(dsn = db_con(), query = query)
  return(data)
}

# TO REMOVE

#' get network in region selected with landcover data
#'
#' @param selected_region_id
#'
#' @return sf data.frame
#' @export
#'
#' @examples
#' data <- get_network_with_landcover(selected_region_id = region_click$id)
get_network_with_landcover <- function(selected_region_id = region_click$id){
  query <- sprintf("WITH region_select AS(
    SELECT network.fid, network.toponyme, network.strahler, network.axis, network.measure, network.geom
    FROM network, region_hydrographique
    WHERE ST_Intersects(network.geom, region_hydrographique.geom)
        AND region_hydrographique.cdregionhy = '%s'
    )
    SELECT region_select.fid, region_select.toponyme,
    region_select.strahler, region_select.axis, region_select.measure,
    landcover.landcover, landcover.landcover_area, landcover.landcover_width,
    region_select.geom
    FROM landcover
    RIGHT JOIN region_select ON landcover.axis = region_select.axis
    	AND landcover.measure = region_select.measure", selected_region_id)

  data <- st_read(dsn = db_con(), query = query)
  return(data)
}

