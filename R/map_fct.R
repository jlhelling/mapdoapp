#' Map initialization with hydrographic bassins data
#'
#' @param bassins_data hydrographic bassins sf
#' @param group layer group
#'
#' @return leaflet map
#' @export
#'
#' @examples
#' map <- map_init_bassins(bassins_data = get_bassins(), group = "A")
map_init_bassins <- function(bassins_data = get_bassins(), group = "A") {
  leaflet() %>%
    setView(lng = 2.468697, lat = 46.603354, zoom = 5) %>%
    addTiles() %>%
    addPolygons(data = bassins_data,
                layerId = ~cdbh,
                smoothFactor = 2,
                fillColor = "black",
                fillOpacity = 0.01,
                weight = 2,
                color="black",
                highlightOptions = highlightOptions(
                  fillColor = "#a8d1ff",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbbh),
                group = group
    )
}

#' Update initial map to hydrographic regions
#'
#' @param map map object for pipe
#' @param bassin_click bassin selected by user
#' @param regions_data hydrographic regions data
#' @param bassins_group bassins layer group
#' @param regions_group regions layer group
#'
#' @return leaflet updated
#' @export
#'
#' @examples
#' map_add_regions_in_bassin(bassin_click = bassin_click,
#' regions_data = region_hydro,
#' bassins_group = "A",
#' regions_group = "B")
map_add_regions_in_bassin <- function(map, bassin_click = bassin_click,
                                      regions_data = region_hydro,
                                      bassins_group= "A",
                                      regions_group = "B"){
  map %>%
    setView(lng = bassin_click$lng , lat = bassin_click$lat, zoom = 6.5) %>%
    clearGroup(bassins_group) %>%
    addPolygons(data = regions_data,
                layerId = ~cdregionhy,
                smoothFactor = 2,
                fillColor = "black",
                fillOpacity = 0.01,
                weight = 2,
                color="black",
                highlightOptions = highlightOptions(
                  fillColor = "#a8d1ff",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbregionhy),
                group = regions_group
    )
}

#' Map region click
#'
#' @param map map object for pipe
#' @param region_click region clicked parameter app
#' @param selected_region_feature region clicked feature
#' @param regions_group regions group to clear
#' @param selected_region_group selected regions group
#'
#' @return leaflet update
#' @export
#'
#' @examples
#' map_region_clicked(region_click = region_click,
#' selected_region_feature = selected_region_feature,
#' regions_group = "B",
#' selected_region_group = "C")
map_region_clicked <- function(map,
                               region_click = region_click,
                               selected_region_feature = selected_region_feature,
                               regions_group = "B",
                               selected_region_group = "C"){
  map %>%
  setView(lng = region_click$lng , lat = region_click$lat, zoom = 7.5) %>%
    addPolygons(data = selected_region_feature,
                smoothFactor = 2,
                fillColor = "black",
                fillOpacity = 0.01,
                weight = 2,
                color="black",
                group = selected_region_group
    ) %>%
    clearGroup(regions_group)
}

#' Update metric mapping
#'
#' @param mapId
#' @param data_map
#' @param varsel
#'
#' @return
#' @export
#'
#' @examples map_metric("exploremap", datamap(), varsel())
map_metric <- function(mapId, data_map, varsel) {

  breaks <-  unique(quantile(varsel, probs = seq(0, 1, 0.25), na.rm = TRUE))

  # Define color palette for Reds
  color_palette <- colorRampPalette(c("green", "red"))(length(breaks))

  leafletProxy(mapId) %>%
    clearGroup("D") %>%
    addPolylines(data = data_map, color = ~ {
      ifelse(is.na(varsel), "grey",
             color_palette[findInterval(varsel, breaks, all.inside = TRUE)])
    },
    group = "D")
}
