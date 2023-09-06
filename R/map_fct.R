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
