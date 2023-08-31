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
    clearShapes() %>%
    addPolylines(data = data_map, color = ~ {
      ifelse(is.na(varsel), "grey", color_palette[findInterval(varsel, breaks, all.inside = TRUE)])
    })
}
