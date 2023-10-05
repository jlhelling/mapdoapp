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


  # Build the legend URL
  # legend_url <- modify_url("http://mapsref.brgm.fr/legendes/geoservices/Geologie1000_legende.jpg")

  leaflet() %>%
    setView(lng = 2.468697, lat = 46.603354, zoom = 5) %>%
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
    map_add_basemaps() %>%
    addLayersControl(
      baseGroups = c("CartoDB Positron", unlist(sapply(params_wms(), function(x) if (x$basemap) x$name else NULL), use.names = FALSE)),
      options = layersControlOptions(collapsed = TRUE)
    )

    # addControl(html = legend_image,
    #            position = "bottomright", layerId = params_map_group()[["legend"]]) %>%
    # addControl(html = icon(name = "circle-info", class="fa-solid fa-circle-info fa-xl", lib = "font-awesome"),
    #            position = "topright", layerId = "GEOL_LEGEND")
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
    # ROE layer hidden by default
    hideGroup(params_map_group()[["roe"]]) %>%
    # add WMS overlayers
    map_add_wms_overlayers() %>%
    addLayersControl(
      baseGroups = c("CartoDB Positron", unlist(sapply(params_wms(), function(x) if (x$basemap) x$name else NULL), use.names = FALSE)),
      options = layersControlOptions(collapsed = TRUE),
      overlayGroups = c(params_map_group()[["roe"]], unlist(sapply(params_wms(), function(x) if (x$overlayer) x$name else NULL), use.names = FALSE))
    )
}

#' Add WMS Tiles with Metric Data to an Existing Leaflet Map
#'
#' This function adds WMS tiles with metric data to an existing Leaflet map, allowing for customization of style and filtering.
#'
#' @param map An existing Leaflet map to which WMS tiles will be added.
#' @param wms_params A list of WMS parameters.
#' @param cql_filter A CQL filter to apply to the WMS request.
#' @param sld_body A custom SLD (Styled Layer Descriptor) body for symbology customization.
#'
#' @return An updated Leaflet map with WMS tiles containing metric data added.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   updated_map <- map_wms_metric(map = existing_map, wms_params = params_wms()$metric, cql_filter = "metric > 100", sld_body = "<sld>...</sld>")
#' }
#'
#' @importFrom leaflet addWMSTiles
#'
#' @export
map_wms_metric <-function(map, wms_params = params_wms()$metric,
                          cql_filter = "", sld_body = "") {
  map %>%
    addWMSTiles(
      baseUrl = wms_params$url,
      layers = wms_params$layer,
      attribution = wms_params$attribution,
      options = WMSTileOptions(
        format = wms_params$format,
        request = "GetMap",
        transparent = TRUE,
        style = wms_params$style,
        cql_filter = cql_filter,
        sld_body = sld_body,
        zIndex = 100
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


#' Add a metric layer with custom symbology to a map.
#'
#' This function adds a metric layer with custom symbology to a leaflet map. It allows you to specify custom parameters for the Web Map Service (WMS) request, apply a CQL (Common Query Language) filter, and provide a custom SLD (Styled Layer Descriptor) body for styling the layer. Additionally, you can specify the data axis to display on the map.
#'
#' @param map A leaflet map object to which the metric layer will be added.
#' @param wms_params A list containing WMS parameters for the metric layer. If not provided, default parameters are retrieved using the \code{\link{params_wms}} function.
#' @param cql_filter A character string representing a CQL filter to apply to the metric layer.
#' @param sld_body A character string representing the SLD (Styled Layer Descriptor) body for custom styling of the metric layer.
#' @param data_axis A data axis to display on the map.
#'
#' @return A leaflet map object with the metric layer added.
#'
#' @examples
#' # Create a leaflet map
#' my_map <- leaflet() %>%
#'   addTiles() %>%
#'   setView(lng = 0, lat = 0, zoom = 3)
#'
#' # Add a custom metric layer to the map
#' map_metric(my_map, wms_params = params_wms()$metric, cql_filter = "value > 100", sld_body = my_custom_sld, data_axis = my_axis_data)
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet setView
#' @importFrom leaflet clearGroup
#' @importFrom leaflet addWMSTiles
#'
#' @export
map_metric <- function(map, wms_params = params_wms()$metric,
                       cql_filter = "", sld_body = "",
                       data_axis = network_region_axis()) {
  map %>%
    clearGroup(params_map_group()[["axis"]]) %>%
    clearGroup(params_map_group()[["metric"]]) %>%
    # add metric with custom symbology
    map_wms_metric(wms_params = wms_params,
                   cql_filter = cql_filter, sld_body = sld_body) %>%
    # add transparent axis
    map_axis(data_axis = data_axis)
}

#' Add DGO axis to a Leaflet map
#'
#' This function adds DGO axis to a Leaflet map with the option to highlight selected axes.
#'
#' @param map A Leaflet map object.
#' @param selected_axis A data frame containing selected axe to be displayed.
#' @param region_axis A data frame containing region-specific axes to be displayed.
#' @return A modified Leaflet map object with DGO axes added.
#'
#' @importFrom leaflet clearGroup addPolylines highlightOptions pathOptions
#'
#' @examples
#' # Create a basic Leaflet map
#' my_map <- leaflet() %>%
#'   setView(lng = -73.985, lat = 40.748, zoom = 12)
#'
#' # Define selected and region-specific axes data frames
#' selected_axes <- data.frame(...)
#' region_axes <- data.frame(...)
#'
#' # Add DGO axes to the map
#' my_map <- map_dgo_axis(my_map, selected_axes, region_axes)
#' my_map
#'
#' @export
map_dgo_axis <- function(map, selected_axis, region_axis) {
  map %>%
    clearGroup(params_map_group()$dgo_axis) %>%
    clearGroup(params_map_group()$axis) %>%
    map_axis(data_axis = region_axis) %>%
    addPolylines(
      data = selected_axis,
      layerId = ~fid,
      weight = 5,
      color = "#ffffff00",
      opacity = 1,
      highlight = highlightOptions(
        opacity = 1,
        color = "red"
      ),
      options = pathOptions(zIndex = 100),
      group = params_map_group()$dgo_axis
    )
}


#' Add start and end markers to a leaflet map
#'
#' This function adds start and end markers to a Leaflet map based on the provided
#' start and end coordinates.
#'
#' @param map A Leaflet map object created using the 'leaflet' package.
#' @param axis_start_end A data frame containing start and end coordinates with
#'        columns 'X' for longitude and 'Y' for latitude.
#' @return A Leaflet map object with start and end markers added.
#'
#' @importFrom leaflet addMarkers clearGroup makeIcon pathOptions
#'
#' @examples
#' # Create a simple Leaflet map
#' my_map <- leaflet() %>%
#'   setView(lng = -73.985, lat = 40.748, zoom = 12)
#'
#' # Create a data frame with start and end coordinates
#' coordinates_df <- data.frame(
#'   X = c(-73.985, -73.995),
#'   Y = c(40.748, 40.755)
#' )
#'
#' # Add start and end markers to the map
#' my_map <- map_axis_start_end(my_map, coordinates_df)
#' my_map
#'
#' @export
map_axis_start_end <- function(map, axis_start_end = axis_start_end()) {

  # Define the start and end icon
  start_end_icon <- makeIcon(
    iconUrl = system.file("pin-sharp.png", package = "mapdoapp"),
    iconWidth = 24,
    iconHeight = 24,
    iconAnchorX = 16,
    iconAnchorY = 24
  )

  # Clear the previous group of markers and add new markers to the map
  map %>%
    clearGroup(params_map_group()$axis_start_end) %>%
    addMarkers(
      lng = axis_start_end$X,
      lat = axis_start_end$Y,
      options = pathOptions(interactive = FALSE),
      icon = start_end_icon,
      group = params_map_group()$axis_start_end
    )
}


#' Add Basemap Layers to an Existing Leaflet Map
#'
#' This function adds basemap layers to an existing Leaflet map.
#'
#' @param map An existing Leaflet map to which basemap layers will be added.
#'
#' @return An updated Leaflet map with basemap layers added.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   updated_map <- map_add_basemaps(map = existing_map)
#' }
#'
#' @importFrom leaflet addWMSTiles
#'
#' @export
map_add_basemaps <- function(map) {
  for (i in params_wms()) {
    if (i$basemap == TRUE){
      map <- map %>%
        addWMSTiles(
          baseUrl = i$url,
          layers = i$layer,
          attribution = i$attribution,
          options = WMSTileOptions(
            format = i$format,
            transparent = TRUE,
            opacity = 0.7,
            style = i$style,
          ),
          group = i$name
        )
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
#'   # Example usage:
#'   updated_map <- map_add_wms_overlayers(map = existing_map)
#' }
#'
#' @importFrom leaflet addWMSTiles
#'
#' @export
map_add_wms_overlayers <- function(map) {
  for (i in params_wms()) {
    if (i$overlayer == TRUE){
    map <- map %>%
      addWMSTiles(
        baseUrl = i$url,
        layers = i$layer,
        attribution = i$attribution,
        options = WMSTileOptions(
          format = i$format,
          transparent = TRUE
        ),
        group = i$name
      )%>%
      hideGroup(i$name)
    }
  }
  return(map)
}


#' Generate and display a legend for a map layer using a provided SLD (Styled Layer Descriptor) body.
#'
#' This function constructs a legend for a map layer by sending a GetLegendGraphic request to a WMS (Web Map Service) server.
#'
#' @param sld_body A character string containing the SLD (Styled Layer Descriptor) body that defines the map layer's styling.
#'
#' @return An HTML img tag representing the legend for the specified map layer.
#'
#' @examples
#' # Define an SLD body for a map layer
#' sld_body <- '<StyledLayerDescriptor>...</StyledLayerDescriptor>'
#'
#' # Generate and display the legend for the map layer
#' legend <- map_legend_metric(sld_body)
#' print(legend)
#'
#' @importFrom httr modify_url
#' @importFrom htmltools tags
#'
#' @export
map_legend_metric <- function(sld_body){

  # Construct the query parameters for legend
  query_params <- list(
    REQUEST = "GetLegendGraphic",
    VERSION = params_wms()$metric$version,
    FORMAT = params_wms()$metric$format,
    SLD_BODY = sld_body,
    LAYER = params_wms()$metric$layer
  )

  # Build the legend URL
  legend_url <- modify_url(params_wms()$metric$url, query = query_params)

  # create an html img tag to display the legend
  legend <- tags$img(src = legend_url, responsive = "width: 100%; height: auto;", class="responsive")

  return(legend)
}


#' Generate and display a legend for a WMS (Web Map Service) layer overlay using specified WMS parameters.
#'
#' This function constructs a legend for a WMS layer overlay by sending a GetLegendGraphic request to a WMS server.
#'
#' @param wms_params A list containing the following parameters for the WMS layer overlay:
#'   \itemize{
#'     \item \code{language} (character): The language to use for the legend.
#'     \item \code{version} (character): The version of the WMS service.
#'     \item \code{service} (character): The WMS service type (e.g., "WMS").
#'     \item \code{sld_version} (character): The SLD (Styled Layer Descriptor) version.
#'     \item \code{layer} (character): The name of the WMS layer for which the legend is generated.
#'     \item \code{format} (character): The desired format of the legend image (e.g., "image/png").
#'     \item \code{style} (character): The style to use for rendering the legend.
#'     \item \code{url} (character): The URL of the WMS server.
#'   }
#'
#' @return An HTML div element containing an img tag representing the legend for the specified WMS layer overlay.
#'
#' @examples
#' # Define WMS parameters for a layer overlay
#' wms_params <- list(
#'   language = "en",
#'   version = "1.3.0",
#'   service = "WMS",
#'   request = "GetLegendGraphic",
#'   sld_version = "1.1.0",
#'   layer = "overlay_layer",
#'   format = "image/png",
#'   style = "default",
#'   url = "http://wms.example.com/wms"
#' )
#'
#' # Generate and display the legend for the WMS layer overlay
#' legend <- map_legend_wms_overlayer(wms_params)
#' print(legend)
#'
#' @importFrom httr modify_url
#' @importFrom htmltools div
#' @importFrom htmltools img
#'
#' @export
map_legend_wms_overlayer <- function(wms_params){

  # Construct the query parameters for legend
  query_params <- list(
    LANGUAGE = wms_params$language,
    VERSION = wms_params$version,
    SERVICE = wms_params$service,
    REQUEST = "GetLegendGraphic",
    SLD_VERSION = wms_params$sld_version,
    LAYER = wms_params$layer,
    FORMAT = wms_params$format,
    STYLE = wms_params$style
  )

  # Build the legend URL
  legend_url <- modify_url(wms_params$url, query = query_params)

  # Create a div with centered alignment
  div(
    style = "display: flex; align-items: center;",
    img(
      src = legend_url,
      responsive = "width: 100%; height: auto;",
      class="responsive",
      ""
    ), # img
    img(
      src = "www/information-icon-6068.png",
      style = "width: 24px; height: auto;",
      class="responsive"
    ) # Text div
  ) # div
}


#' Generate a legend entry for a vector overlay layer.
#'
#' This function generates an HTML representation of a legend entry for a vector overlay layer. The legend entry consists of a colored circle with a label indicating the layer's name.
#'
#' @param layer_label A character string representing the label or name of the vector overlay layer.
#'
#' @return An HTML div element representing the legend entry for the vector overlay layer.
#'
#' @examples
#' # Create a legend entry for a vector overlay layer
#' legend_entry <- map_legend_vector_overlayer("Vector Layer A")
#' print(legend_entry)
#'
#' @importFrom htmltools div
#' @importFrom htmltools span
#'
#' @export
map_legend_vector_overlayer <- function(layer_label){

  div(
    style = "display: flex; align-items: center;",
    div(
      style = "background-color: orange; border-radius: 50%; width: 10px; height: 10px; margin-top: 3px;",
      ""
    ),
    span(
      style = "margin-left: 5px;",
      layer_label
    ) # span
  ) # div
}


