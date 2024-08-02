#' Create Map with basemaps and basic functionalities
#'
#' @description Initializes the map
#'
#' @param params_wms parameters defining properties of wms maps to be added
#' @param id_logo_ign_remonterletemps id of the IGN remonter le temps image.
#'
#' @importFrom leaflet leaflet setView addPolygons addScaleBar addLayersControl addControl
#' @importFrom leaflet layersControlOptions addProviderTiles scaleBarOptions providers leafletOptions
#' @importFrom leaflet.extras addSearchOSM searchOptions addFullscreenControl gpsOptions addControlGPS
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
map_initialize <- function(params_wms, id_logo_ign_remonterletemps) {

  leaflet() %>%
    # zoom on France
    setView(lng = 2.468697, lat = 46.603354, zoom = 5) %>%
    # scale bar
    addScaleBar(position = "bottomleft",
                scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
    # background map
    addProviderTiles(providers$CartoDB.Positron) %>%
    # search button
    addSearchOSM(options = leaflet.extras::searchOptions(hideMarkerOnCollapse = TRUE)) %>%
    # GPS location button
    addControlGPS(options = leaflet.extras::gpsOptions(
      position = "topleft",
      activate = FALSE,
      autoCenter = FALSE,
      maxZoom = NULL,
      setView = FALSE
    )) %>%
    # fullscreen option
    addFullscreenControl(pseudoFullscreen = TRUE) %>%
    # further basemaps
    map_add_basemaps(params_wms) %>%
    addLayersControl(
      baseGroups = c("CartoDB Positron", unlist(sapply(params_wms, function(x) if (x$basemap) x$name else NULL), use.names = FALSE)),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addControl(
      className = "img_div_ign_remonterletemps",
      position = "topleft",
      html = tags$a(href = "javascript:void(0);",
                    tags$img(
                      id = id_logo_ign_remonterletemps,
                      src = "www/logo_ign_remonterletemps.jpg",
                      width = 50, height = 50,
                      title="Vers le site IGN remonterletemps"))
    ) # %>%
    # map_background(wms_params = params_wms$background)
}


#' Add Basemap Layers to an Existing Leaflet Map
#'
#' This function adds basemap layers to an existing Leaflet map.
#'
#' @param map An existing Leaflet map to which basemap layers will be added.
#' @param params_wms parameters defining properties of wms maps to be added
#'
#' @importFrom leaflet addWMSTiles WMSTileOptions addTiles tileOptions
#'
#' @return An updated Leaflet map with basemap layers added.
#'
#' @examples
#' \dontrun{
#'   # Used in map_init_bassins() function, its use is in the function
#' }
#'
#' @export
map_add_basemaps <- function(map, params_wms) {
  for (i in params_wms) {
    if (i$basemap == TRUE){
      if ((i$name == "Occupation du sol") || (i$name == "Géologie")){
        map <- map %>%
          addWMSTiles(
            baseUrl = i$url,
            layers = i$layer,
            attribution = i$attribution,
            options = WMSTileOptions(
              format = i$format,
              transparent = TRUE,
              opacity = 0.6,
              styles = i$style,
            ),
            group = i$name
          )
      } else {
        map <- map %>%
          addTiles(
            urlTemplate = i$url,
            options = tileOptions(
              attribution = i$attribution,
              transparent = TRUE,
              opacity = 0.7,
              format = i$format,
              style = i$style
            ),
            group = i$name
          )
      }

    }
  }
  return(map)
}
