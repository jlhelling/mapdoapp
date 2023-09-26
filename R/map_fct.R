#' Initialize a Leaflet Map with hydrological Bassins
#'
#' This function initializes a Leaflet map with bassins data and various map layers.
#'
#' @param bassins_data A hydrological bassins sf data frame.
#'
#' @return A Leaflet map object with basemaps, scale and layer control.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   map <- map_init_bassins(bassins_data = some_bassins_data)
#' }
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet setView
#' @importFrom leaflet addPolygons
#' @importFrom leaflet addScaleBar
#' @importFrom leaflet addLayersControl
#' @importFrom htmltools htmlEscape
#'
#' @export
map_init_bassins <- function(bassins_data = get_bassins()) {
  leaflet() %>%
    setView(lng = 2.468697, lat = 46.603354, zoom = 5) %>%
    map_add_basemaps(data_basemaps_df()) %>%
    addPolygons(data = bassins_data,
                layerId = ~cdbh,
                smoothFactor = 2,
                fillColor = "black",
                fillOpacity = 0.01,
                weight = 2,
                color = "black",
                highlightOptions = highlightOptions(
                  fillColor = "#a8d1ff",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbbh),
                group = params_map_group()[["bassin"]]
    ) %>%
    addScaleBar(pos = "bottomleft",
                scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addLayersControl(
      baseGroups = c("CartoDB Positron", data_basemaps_df()$name),
      options = layersControlOptions(collapsed = TRUE)
    )
}

#' Add hydrological Regions in a Bassin to an existing Leaflet Map
#'
#' This function adds regions within a bassin to an existing Leaflet map.
#'
#' @param map An existing Leaflet map to which regions will be added.
#' @param bassin_click A vector containing information about the clicked bassin.
#' @param regions_data A sf data frame containing information about regions within the bassin.
#'
#' @return An updated Leaflet map with regions added.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   updated_map <- map_add_regions_in_bassin(map = existing_map, bassin_click = some_bassin_data, regions_data = some_regions_data)
#' }
#'
#' @importFrom leaflet setView
#' @importFrom leaflet clearGroup
#' @importFrom leaflet addPolygons
#' @importFrom htmltools htmlEscape
#'
#' @export
map_add_regions_in_bassin <- function(map, bassin_click = bassin_click,
                                      regions_data = region_hydro) {
  map %>%
    setView(lng = bassin_click$lng , lat = bassin_click$lat, zoom = 6.5) %>%
    clearGroup(params_map_group()[["bassin"]]) %>%
    addPolygons(data = regions_data,
                layerId = ~gid,
                smoothFactor = 2,
                fillColor = "black",
                fillOpacity = 0.01,
                weight = 2,
                color = "black",
                highlightOptions = highlightOptions(
                  fillColor = "#a8d1ff",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbregionhy),
                group = params_map_group()[["region"]]
    )
}


#' Update Leaflet Map for a Clicked Region
#'
#' This function updates an existing Leaflet map when a region is clicked, displaying the region and overlayers.
#'
#' @param map An existing Leaflet map to be updated.
#' @param region_click A vector containing information about the clicked region.
#' @param selected_region_feature A sf data frame containing information about the selected region feature.
#'
#' @return An updated Leaflet map with relevant layers and information displayed.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   updated_map <- map_region_clicked(map = existing_map,
#'     region_click = clicked_region_data, selected_region_feature = selected_feature_data)
#' }
#'
#' @importFrom leaflet setView
#' @importFrom leaflet addPolygons
#' @importFrom leaflet clearGroup
#' @importFrom leaflet addCircleMarkers
#' @importFrom leaflet addLayersControl
#'
#' @export
map_region_clicked <- function(map,
                               region_click = region_click,
                               selected_region_feature = selected_region_feature) {
  map %>%
    setView(lng = region_click$lng , lat = region_click$lat, zoom = 7.5) %>%
    # display the region clicked
    addPolygons(data = selected_region_feature,
                smoothFactor = 2,
                fillColor = "black",
                fillOpacity = 0.01,
                weight = 2,
                color = "black",
                group = params_map_group()[["select_region"]],
                options = pathOptions(interactive = FALSE)
    ) %>%
    clearGroup(params_map_group()[["region"]]) %>%
    # add ROE overlayers from PostgreSQL
    addCircleMarkers(data = data_get_roe_in_region(region_click$id),
                     radius = 3,
                     weight = 0.5,
                     opacity = 0.9,
                     color = "orange",
                     fillColor = "orange",
                     fillOpacity = 0.9,
                     popup = ~nomprincip,
                     group = params_map_group()[["roe"]]
    ) %>%
    # add WMS overlayers
    map_add_overlayers(data_overlayers_df()) %>%
    addLayersControl(
      baseGroups = c("CartoDB Positron", data_basemaps_df()$name),
      options = layersControlOptions(collapsed = TRUE),
      overlayGroups = c(params_map_group()[["roe"]], data_overlayers_df()$name)
    ) %>%
    # ROE layer hidden by default
    hideGroup(c(params_map_group()[["roe"]], data_overlayers_df()$name))
}

#' Add WMS Tiles with Metric Data to an Existing Leaflet Map
#'
#' This function adds WMS tiles with metric data to an existing Leaflet map, allowing for customization of style and filtering.
#'
#' @param map An existing Leaflet map to which WMS tiles will be added.
#' @param style The style to apply to the WMS tiles.
#' @param cql_filter A CQL filter to apply to the WMS request.
#' @param sld_body A custom SLD (Styled Layer Descriptor) body for symbology customization.
#'
#' @return An updated Leaflet map with WMS tiles containing metric data added.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   updated_map <- map_wms_metric(map = existing_map, style = "custom_style", cql_filter = "metric > 100", sld_body = "<sld>...</sld>")
#' }
#'
#' @importFrom leaflet addWMSTiles
#'
#' @export
map_wms_metric <-function(map, style = params_geoserver()[["metric_basic_style"]],
                          cql_filter = "", sld_body = "") {
  map %>%
    addWMSTiles(
      baseUrl = params_geoserver()[["url"]],
      layers = params_geoserver()[["layer"]],
      attribution = params_geoserver()[["attribution"]],
      options = WMSTileOptions(
        format = params_geoserver()[["format"]],
        request = params_geoserver()[["query_map"]],
        transparent = TRUE,
        style = style,
        cql_filter = cql_filter,
        sld_body = sld_body
      ),
      group = params_map_group()[["metric"]]
    )
}

#' Add Axis Data to an Existing Leaflet Map
#'
#' This function adds axis data as polylines to an existing Leaflet map.
#'
#' @param map An existing Leaflet map to which axis data will be added.
#' @param data_axis A sf data frame containing axis data.
#'
#' @return An updated Leaflet map with axis data added.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   updated_map <- map_axis(map = existing_map, data_axis = some_axis_data)
#' }
#'
#' @importFrom leaflet addPolylines
#'
#' @export
map_axis <- function(map, data_axis) {
  map %>%
    addPolylines(data = data_axis,
                 layerId = ~fid,
                 weight = 5,
                 color = "#ffffff00",
                 opacity = 1,
                 highlight = highlightOptions(
                   opacity = 1,
                   color = "red"
                 ),
                 group = params_map_group()[["axis"]]
    )
}

#' Add hydrological network when no metric is selected to existing map
#'
#' This function clears metric layers, removes the legend, add wms metric and adds a transparent axis to an existing Leaflet map.
#'
#' @param map An existing Leaflet map to be updated.
#' @param style The style to apply to the WMS tiles.
#' @param cql_filter A CQL filter to apply to the WMS request.
#' @param sld_body A custom SLD (Styled Layer Descriptor) body for symbology customization.
#' @param data_axis A data frame containing axis data.
#'
#' @return An updated Leaflet map with metric layers cleared, the legend removed, and a transparent axis added.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   updated_map <- map_no_metric(map = existing_map, style = "custom_style", cql_filter = "metric > 100", sld_body = "<sld>...</sld>", data_axis = some_axis_data)
#' }
#'
#' @importFrom leaflet clearGroup
#' @importFrom leaflet removeControl
#'
#' @export
map_no_metric <- function(map, style = params_geoserver()[["metric_basic_style"]],
                          cql_filter = "", sld_body = "",
                          data_axis = network_region_axis()) {
  map %>%
    clearGroup(params_map_group()[["metric"]]) %>%
    removeControl(params_map_group()[["legend"]]) %>%
    map_wms_metric(style = style,
                   cql_filter = cql_filter, sld_body = sld_body) %>%
    map_axis(data_axis = data_axis)
}


map_metric <- function(map, style = params_geoserver()[["metric_basic_style"]],
                       cql_filter = "", sld_body = "", legend_url = "",
                       data_axis = network_region_axis()) {
  map %>%
    clearGroup(params_map_group()[["axis"]]) %>%
    clearGroup(params_map_group()[["metric"]]) %>%
    # add metric with custom symbology
    map_wms_metric(style = style,
                   cql_filter = cql_filter, sld_body = sld_body) %>%
    # add legend with custom symbology
    addControl(html = paste0("<img src=",legend_url,">"),
               position = "bottomright", layerId = params_map_group()[["legend"]]) %>%
    # add transparent axis
    map_axis(data_axis = data_axis)
}

#' Add Basemap Layers to an Existing Leaflet Map
#'
#' This function adds basemap layers to an existing Leaflet map.
#'
#' @param map An existing Leaflet map to which basemap layers will be added.
#' @param basemaps A data frame containing information about the basemap layers to be added.
#'
#' @return An updated Leaflet map with basemap layers added.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   updated_map <- map_add_basemaps(map = existing_map, basemaps = basemaps_data)
#' }
#'
#' @importFrom leaflet addWMSTiles
#'
#' @export
map_add_basemaps <- function(map, basemaps) {
  for (i in 1:nrow(basemaps)) {
    map <- map %>%
      addWMSTiles(
        baseUrl = basemaps$url[i],
        layers = basemaps$layer[i],
        attribution = basemaps$attribution[i],
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          opacity = 0.7
        ),
        group = basemaps$name[i]
      )
  }
  return(map)
}


#' Add Overlayer Layers to an Existing Leaflet Map
#'
#' This function adds overlayer layers to an existing Leaflet map.
#'
#' @param map An existing Leaflet map to which overlayer layers will be added.
#' @param overlayers A data frame containing information about the overlayer layers to be added.
#'
#' @return An updated Leaflet map with overlayer layers added.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   updated_map <- map_add_overlayers(map = existing_map, overlayers = overlayers_data)
#' }
#'
#' @importFrom leaflet addWMSTiles
#'
#' @export
map_add_overlayers <- function(map, overlayers) {
  for (i in 1:nrow(overlayers)) {
    map <- map %>%
      addWMSTiles(
        baseUrl = overlayers$url[i],
        layers = overlayers$layer[i],
        attribution = overlayers$attribution[i],
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE
        ),
        group = overlayers$name[i]
      )
  }
  return(map)
}

