#' Create Map with basemaps and basic functionalities
#'
#' @description Initializes the map
#'
#' @param params_wms parameters defining properties of wms maps to be added
#' @param id_logo_ign_remonterletemps id of the IGN remonter le temps image.
#'
#' @importFrom leaflet leaflet setView addPolygons addScaleBar addLayersControl addControl hideGroup addCircleMarkers
#' @importFrom leaflet layersControlOptions addProviderTiles scaleBarOptions providers leafletOptions pathOptions highlightOptions
#' @importFrom leaflet.extras addSearchOSM searchOptions addFullscreenControl gpsOptions addControlGPS
#' @importFrom htmltools htmlEscape
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
map_initialize <- function(params_wms, params_map_group,
                           id_logo_ign_remonterletemps,
                           basins_data, regions_data,
                           roe_sites, hydro_sites) {

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
    addPolygons(data = basins_data,
                layerId = ~cdbh,
                fillColor = "black",
                fillOpacity = ~opacity,
                weight = 2,
                color = "black",
                opacity = 0.20,
                highlightOptions = highlightOptions(
                  fillColor = "#000000",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbbh),
                options = pathOptions(clickable = ~click),
                group = params_map_group[["bassin"]]
    ) %>%
    addPolygons(data = regions_data,
                layerId = ~gid,
                smoothFactor = 2,
                fillColor = "black",
                fillOpacity = ~opacity,
                weight = 2,
                color = "black",
                highlightOptions = highlightOptions(
                  fillColor = "#a8d1ff",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbregionhy),
                options = pathOptions(clickable = ~click),
                group = params_map_group[["region"]]
    ) %>%
    # ROE layer hidden by default
    hideGroup(params_map_group[["region"]]) %>%
    # add ROE overlayers from PostgreSQL
    addCircleMarkers(data = roe_sites,
                     radius = 4.5,
                     weight = 0.5,
                     opacity = 0.9,
                     color = "#D0D0D0",
                     fillColor = "#323232",
                     fillOpacity = 0.9,
                     popup = ~nomprincip,
                     group = params_map_group[["roe"]]
    ) %>%
    # ROE layer hidden by default
    hideGroup(params_map_group[["roe"]]) %>%
    addCircleMarkers(data = hydro_sites,
                     radius = 4.5,
                     weight = 0.5,
                     opacity = 0.9,
                     color = "#E5F6FF",
                     fillColor = "#33B1FF",
                     fillOpacity = 0.9,
                     popup = ~paste0("<a href=\"", url_site, "\",  target = \'_blank\'>", libelle_site, "</a>"),
                     group = params_map_group[["hydro_sites"]]
    ) %>%
    # Hydrometric sites layer hidden by default
    hideGroup(params_map_group[["hydro_sites"]]) %>%
    # add WMS overlayers
    map_add_wms_overlayers(params_wms) %>%
    map_add_network(params_wms$network, group = params_map_group[["network"]]) %>%
    # add controller
    addLayersControl(
      baseGroups = c("CartoDB Positron", unlist(sapply(params_wms, function(x) if (x$basemap) x$name else NULL), use.names = FALSE)),
      options = layersControlOptions(collapsed = TRUE),
      overlayGroups = c(params_map_group[["bassin"]],
                        params_map_group[["region"]],
                        params_map_group[["roe"]],
                        params_map_group[["hydro_sites"]],
                        unlist(sapply(params_wms, function(x) if (x$overlayer) x$name else NULL), use.names = FALSE))
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
    )
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

#' Add Overlayer Layers to an Existing Leaflet Map
#'
#' This function adds overlayer layers to an existing Leaflet map.
#'
#' @param map An existing Leaflet map to which overlayer layers will be added.
#'
#' @return An updated Leaflet map with overlayer layers added.
#'
#' @examples
#' \dontrun{
#'   # Used in map_region_clicked() function, its use is in the function
#' }
#'
#' @importFrom leaflet addWMSTiles hideGroup WMSTileOptions
#'
#' @export
map_add_wms_overlayers <- function(map, params_wms) {
  for (i in params_wms) {
    if (i$overlayer == TRUE){
      map <- map %>%
        addWMSTiles(
          baseUrl = i$url,
          layerId = i$name,
          layers = i$layer,
          attribution = i$attribution,
          options = WMSTileOptions(
            format = i$format,
            transparent = TRUE
          ),
          group = i$name
        ) %>%
        hideGroup(i$name)
    }
  }
  return(map)
}

#' Map the hydrographique network
#'
#' @description
#' Adds the hydrographique network with a custom styling and regional constraint to the map
#'
#'
#' @param map An existing Leaflet map to which WMS tiles will be added.
#' @param wms_params Netowrk-WMS parameters.
#' @param cql_filter A CQL filter to apply to the WMS request.
#'
#' @return An updated Leaflet map with WMS tiles of the styled network. Standard styling
#' according to strahler-order and standard regional constraint: whole France.
#'
#' @examples
#' map %>%
#' map_add_network(wms_params = params_wms()$network, cql_filter = "gid_region <> 11", style = "mapdo:classes_proposed_urban")
map_add_network <- function(map, wms_params_network,
                            group,
                            cql_filter = "",
                            style = "mapdo:classes_proposed_strahler") {

  map %>%
    addWMSTiles(
      baseUrl = wms_params_network$url,
      layers = wms_params_network$layer,
      attribution = wms_params_network$attribution,
      options = WMSTileOptions(
        format = wms_params_network$format,
        request = "GetMap",
        transparent = TRUE,
        styles = style,
        cql_filter = cql_filter,
        opacity = 0.7,
        zIndex = 90
      ),
      group = group
    )

}
