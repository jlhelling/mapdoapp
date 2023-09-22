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
    add_basemaps(basemaps_df()) %>%
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
    ) %>%
    addScaleBar(pos = "bottomleft",
                scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
    addLayersControl(
      baseGroups = c(basemaps_df()$name),
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
    # display the region clicked
    addPolygons(data = selected_region_feature,
                smoothFactor = 2,
                fillColor = "black",
                fillOpacity = 0.01,
                weight = 2,
                color="black",
                group = selected_region_group,
                options = pathOptions(interactive = FALSE)
    ) %>%
    clearGroup(regions_group) %>%
    # add ROE overlayers from postgresql
    addCircleMarkers (data = get_roe_in_region(region_click$id),
                      radius = 3,
                      weight = 0.5,
                      opacity = 0.9,
                      color = "orange",
                      fillColor = "orange",
                      fillOpacity = 0.9,
                      popup = ~nomprincip,
                      group = "ROE") %>%
    # add WMS overlayers
    add_overlayers(overlayers_df()) %>%
    addLayersControl(
      baseGroups = c(basemaps_df()$name),
      options = layersControlOptions(collapsed = TRUE),
      overlayGroups = c("ROE", overlayers_df()$name)) %>%
    # ROE layer hidden by defaut
    hideGroup(c("ROE", overlayers_df()$name))
}


#' Update metric mapping
#'
#' @param map_id map shiny id
#' @param data_map network data
#' @param varsel metric selected data
#' @param network_group network group
#' @param data_axis axis data
#' @param axis_group axis group
#'
#' @return leaflet update
#' @export
#'
#' @examples
#' map_metric(map_id = "exploremap", data_map = network_filter(), varsel = varsel(),
#' network_group = "D", data_axis = network_region_axis(), axis_group = "AXIS")
map_metric <- function(map_id = "exploremap", data_map = network_filter(), varsel = varsel(),
                       network_group = "D", data_axis = network_region_axis(), axis_group = "AXIS") {

  breaks <-  unique(quantile(varsel, probs = seq(0, 1, 0.2), na.rm = TRUE))

  rounded_breaks <- round(breaks, 1)

  # Define color palette for Reds
  color_palette <- colorRampPalette(c("green", "red"))(length(breaks))

  leafletProxy(map_id) %>%
    clearGroup(network_group) %>%
    clearGroup(axis_group) %>%
    # removeControl(1) %>%
    addPolylines(data = data_map, weight = 5, color = ~ {
      ifelse(is.na(varsel), "grey",
             color_palette[findInterval(varsel, breaks, all.inside = TRUE)])
    },
    group = network_group,
    options = pathOptions(interactive = FALSE)) %>%
    addLegend("bottomright", colors = color_palette, labels = rounded_breaks,
              title = "Légende", opacity = 1, layerId = 1) %>%
    addPolylines(data = data_axis,
                 layerId = ~fid,
                 weight = 5,
                 color = color_palette,
                 opacity = 0,
                 highlight = highlightOptions(
                   opacity = 1,
                   color = "red"
                 ),
                 group = axis_group)
}


#' Update map without metric selected
#'
#' @param map map to update
#' @param data_axis axis data
#' @param axis_group axis layer group
#' @param data_network network data
#' @param network_group network layer group
#'
#' @return update leaflet map
#' @export
#'
#' @examples
#' leafletProxy("exploremap") %>%
#'   map_no_metric(data_network = network_filter(),  network_group = "D", data_axis = network_region_axis(), axis_group = "AXIS")
map_no_metric <- function(map, data_network = network_filter(),  network_group = "D", data_axis = network_region_axis(), axis_group = "AXIS"){
  map %>%
    clearGroup(network_group) %>%
    addPolylines(data = data_axis,
                 layerId = ~fid,
                 weight = 5,
                 color = "blue",
                 opacity = 1,
                 highlight = highlightOptions(
                   opacity = 1,
                   color = "red"
                 ),
                 group = axis_group)
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
#' add_basemaps(basemaps_df())
add_basemaps <- function(map, basemaps) {
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
#' add_overlayers(overlayers_df())
add_overlayers <- function(map, overlayers) {
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
