#' Define Web Map Service (WMS) parameters for different map layers and basemaps.
#'
#' This function defines a set of WMS parameters for various map layers and basemaps. The parameters include information such as the layer name, URL of the WMS server, version, format, style, and more. These parameters are organized into a list, making it easy to configure and access them for map display and legend generation.
#'
#' @return A list containing WMS parameters for different map layers and basemaps.
#'
#' @examples
#' # Retrieve WMS parameters for a specific map layer
#' wms_params <- params_wms()
#' metric_wms_params <- wms_params$metric
#'
#' # Access specific WMS parameters
#' metric_name <- metric_wms_params$name
#' metric_url <- metric_wms_params$url
#'
#' @export
params_wms <- function(){
  wms <- list(metric = list(name = "Métrique",
                            url = "https://geoserver-dev.evs.ens-lyon.fr/geoserver/mapdo/wms",
                            language = "",
                            service = "WMS",
                            version = "1.0.0",
                            sld_version = "",
                            layer = "mapdo:network_metrics",
                            format = "image/png",
                            sld = "",
                            style = "", # no style, sld_body define style and legend
                            attribution = "CNRS - EVS",
                            basemap = FALSE,
                            overlayer = FALSE),
              metric_basic = list(name = "Métrique",
                                  url = "https://geoserver-dev.evs.ens-lyon.fr/geoserver/mapdo/wms",
                                  language = "",
                                  service = "WMS",
                                  version = "1.0.0",
                                  sld_version = "",
                                  layer = "mapdo:network_metrics",
                                  format = "image/png",
                                  sld = "",
                                  style = "mapdo:network_basic_style", # basic blue style when no metric selected
                                  attribution = "CNRS - EVS",
                                  basemap = FALSE,
                                  overlayer = FALSE),
              carteign = list(name = "Plan IGN",
                              url = "https://wxs.ign.fr/cartes/geoportail/r/wms",
                              language = "",
                              service = "WMS",
                              version = "",
                              sld_version = "",
                              layer = "GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2",
                              format = "image/png",
                              sld = "",
                              style = "",
                              attribution = "IGN-F/Géoportail",
                              basemap = TRUE,
                              overlayer = FALSE),
              ortho = list(name = "Satellite IGN",
                           url = "https://wxs.ign.fr/ortho/geoportail/r/wms",
                           language = "",
                           service = "WMS",
                           version = "",
                           sld_version = "",
                           layer = "HR.ORTHOIMAGERY.ORTHOPHOTOS",
                           format = "image/png",
                           sld = "",
                           style = "",
                           attribution = "IGN-F/Géoportail",
                           basemap = TRUE,
                           overlayer = FALSE),
              elevation = list(name = "Elévation IGN",
                               url = "https://wxs.ign.fr/altimetrie/geoportail/r/wms",
                               language = "",
                               service = "WMS",
                               version = "",
                               sld_version = "",
                               layer = "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES",
                               format = "image/png",
                               sld = "",
                               style = "hypso",
                               attribution = "IGN-F/Géoportail",
                               basemap = TRUE,
                               overlayer = FALSE),
              landuse = list(name = "Occupation du sol",
                             url = "https://geoserver-dev.evs.ens-lyon.fr/geoserver/mapdo/wms",
                             language = "",
                             service = "WMS",
                             version = "1.0.0",
                             sld_version = "",
                             layer = "mapdo:mapdo_landuse_1m",
                             format = "image/png",
                             sld = "",
                             style = "mapdo:MAPDO landuse",
                             attribution = "CNRS - EVS",
                             basemap = TRUE,
                             overlayer = FALSE),
              geologie = list(name = "Géologie",
                              url = "http://geoservices.brgm.fr/geologie",
                              language = "",
                              service = "WMS",
                              version = "",
                              sld_version = "",
                              layer = "GEOLOGIE",
                              format = "image/png",
                              sld = "",
                              style = "",
                              attribution = "BRGM",
                              basemap = TRUE,
                              overlayer = FALSE),
              inondation = list(name = "Zone inondable débordement centenale",
                                url = "https://georisques.gouv.fr/services",
                                language = "fre",
                                service = "WMS",
                                version = "1.3.0",
                                sld_version = "1.1.0",
                                layer = "ALEA_SYNT_01_02MOY_FXX",
                                format = "image/png",
                                sld = "",
                                style = "inspire_common:DEFAULT",
                                attribution = "Georisques",
                                basemap = FALSE,
                                overlayer = TRUE),
              ouvrage_protection = list(name = "Ouvrage protection inondation",
                                        url = "https://georisques.gouv.fr/services",
                                        language = "fre",
                                        service = "WMS",
                                        version = "1.3.0",
                                        sld_version = "1.1.0",
                                        layer = "OUV_PROTECTION_FXX",
                                        format = "image/png",
                                        sld = "",
                                        style = "inspire_common:DEFAULT",
                                        attribution = "Georisques",
                                        basemap = FALSE,
                                        overlayer = TRUE)
  )
  return(wms)
}


#' Get Parameters for Map Layer Groups
#'
#' This function returns a list of parameters representing different map layer groups.
#'
#' @return A list of parameters including names for groups such as "BASSIN," "REGION," "SELECT_REGION," "METRIC," "AXIS," "LEGEND," and "ROE."
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   map_group_params <- params_map_group()
#' }
#'
#' @export
params_map_group <- function(){
  params <- list(
    bassin = "BASSIN",
    region = "REGION",
    select_region = "SELECT_REGION",
    metric = "METRIC",
    axis = "AXIS",
    dgo_axis = "DGOAXIS",
    axis_start_end = "AXIS_START_END",
    axis_opacity = "AXIS_OPACITY",
    legend = "LEGEND",
    roe = "ROE",
    inondation = params_wms()$inondation$name,
    ouvrage_protection = params_wms()$ouvrage_protection$name,
    landuse = params_wms()$landuse$name,
    carteign = params_wms()$carteign$name,
    ortho = params_wms()$ortho$name,
    elevation = params_wms()$elevation$name,
    geologie = params_wms()$geologie$name
  )

  return(params)
}

#' Get Choices for Metric Selection
#'
#' This function returns a list of choices for selecting metrics organized into categories.
#'
#' @return A list of choices for selecting metrics, categorized into "Largeurs (m)," "Pentes," "Occupation du sol," "Continuité latérale," and "Indices."
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   metric_choices <- params_metrics_choice()
#' }
#'
#' @export
params_metrics_choice <- function() {
  choices_map <- list(
    "Largeurs (m)" = c(
      active_channel_width = "Chenal actif",
      natural_corridor_width = "Corridor naturel",
      connected_corridor_width = "Corridor connecté",
      valley_bottom_width = "Fond de vallée"
    ),
    "Elévation (m)" = c(
      talweg_elevation_min = "Talweg"
    ),
    "Pentes" = c(
      talweg_slope = "Talweg",
      floodplain_slope = "Fond de vallée"
    ),
    "Occupation du sol" = c(
      water_channel = "Surface en eau",
      gravel_bars = "Bancs sédimentaires",
      natural_open = "Espace naturel ouvert",
      forest = "Forêt",
      grassland = "Prairie permanente",
      crops = "Culture",
      diffuse_urban = "Périurbain",
      dense_urban = "Urbain dense",
      infrastructures = "Infrastructure de transport"
    ),
    "Continuité latérale" = c(
      active_channel = "Bande active",
      riparian_corridor = "Corridor naturel",
      semi_natural = "Corridor semi-naturel",
      reversible = "Espace de réversibilité",
      disconnected = "Espace déconnecté",
      built_environment = "Espace artificialisé"
    ),
    "Indices" = c(
      idx_confinement = "Indice de confinement"
    )
  )

  return(choices_map)
}
