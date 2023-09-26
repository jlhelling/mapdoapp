#' Map initialization with hydrographic bassins data
#'
#' @param bassins_data hydrographic bassins sf
#' @param group layer group
#'
#' @return leaflet map
#' @export
#'
#' @examples
#' map <- map_init_bassins(bassins_data = get_bassins())
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
                color="black",
                highlightOptions = highlightOptions(
                  fillColor = "#a8d1ff",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbbh),
                group = params_map_group()[["bassin"]]
    ) %>%
    addScaleBar(pos = "bottomleft",
                scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
    addLayersControl(
      baseGroups = c(data_basemaps_df()$name),
      options = layersControlOptions(collapsed = TRUE)
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
#' regions_data = region_hydro)
map_add_regions_in_bassin <- function(map, bassin_click = bassin_click,
                                      regions_data = region_hydro){
  map %>%
    setView(lng = bassin_click$lng , lat = bassin_click$lat, zoom = 6.5) %>%
    clearGroup(params_map_group()[["bassin"]]) %>%
    addPolygons(data = regions_data,
                layerId = ~gid,
                smoothFactor = 2,
                fillColor = "black",
                fillOpacity = 0.01,
                weight = 2,
                color="black",
                highlightOptions = highlightOptions(
                  fillColor = "#a8d1ff",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbregionhy),
                group = params_map_group()[["region"]]
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
#' selected_region_feature = selected_region_feature)
map_region_clicked <- function(map,
                               region_click = region_click,
                               selected_region_feature = selected_region_feature){
  map %>%
  setView(lng = region_click$lng , lat = region_click$lat, zoom = 7.5) %>%
    # display the region clicked
    addPolygons(data = selected_region_feature,
                smoothFactor = 2,
                fillColor = "black",
                fillOpacity = 0.01,
                weight = 2,
                color="black",
                group = params_map_group()[["select_region"]],
                options = pathOptions(interactive = FALSE)
    ) %>%
    clearGroup(params_map_group()[["region"]]) %>%
    # add ROE overlayers from postgresql
    addCircleMarkers (data = data_get_roe_in_region(region_click$id),
                      radius = 3,
                      weight = 0.5,
                      opacity = 0.9,
                      color = "orange",
                      fillColor = "orange",
                      fillOpacity = 0.9,
                      popup = ~nomprincip,
                      group = params_map_group()[["roe"]]) %>%
    # add WMS overlayers
    map_add_overlayers(data_overlayers_df()) %>%
    addLayersControl(
      baseGroups = c(data_basemaps_df()$name),
      options = layersControlOptions(collapsed = TRUE),
      overlayGroups = c(params_map_group()[["roe"]], data_overlayers_df()$name)) %>%
    # ROE layer hidden by defaut
    hideGroup(c(params_map_group()[["roe"]], data_overlayers_df()$name))
}

map_wms_metric <-function(map, style = params_geoserver()[["metric_basic_style"]],
                          cql_filter = "", sld_body = ""){
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
        # filter WMS
        cql_filter=cql_filter,
        # custom symbology based on data if need
        sld_body = sld_body
      ),
      group = params_map_group()[["metric"]])
}

map_axis <- function(map, data_axis, layerId){
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
                 group = params_map_group()[["axis"]])
}


map_no_metric <- function(map, style = params_geoserver()[["metric_basic_style"]],
                          cql_filter = "", sld_body = "",
                          data_axis = network_region_axis()){
  map %>%
    clearGroup(params_map_group()[["metric"]]) %>%
    removeControl(params_map_group()[["legend"]]) %>%
    # add metric with basic style
    map_wms_metric(style = style,
                   cql_filter = cql_filter, sld_body = sld_body) %>%
    # add transparent axis
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

#' Add all the basemaps
#'
#' @param map leaflet map
#' @param basemaps basemap data.frame
#'
#' @return basemaps for leaflet
#' @export
#'
#' @examples
#' map %>%
#' map_add_basemaps(data_basemaps_df())
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

#' Add all the overlayers
#'
#' @param map leaflet map
#' @param overlayers overlayers data.frame
#'
#' @return overlayers for leaflet
#' @export
#'
#' @examples
#' map %>%
#' map_add_overlayers(data_overlayers_df())
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
