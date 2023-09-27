#' Get Parameters for GeoServer Configuration
#'
#' This function returns a list of parameters for configuring GeoServer connections and styles.
#'
#' @return A list of parameters including the GeoServer URL, layer name, format, queries, version, style, and attribution.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   geoserver_params <- params_geoserver()
#' }
#'
#' @export
params_geoserver <- function(){
  params <- list(
    url = "https://geoserver-dev.evs.ens-lyon.fr/geoserver/mapdo/wms",
    layer = "mapdo:network_metrics",
    format = "image/png",
    query_legend = "GetLegendGraphic",
    query_map = "GetMap",
    version = "1.0.0",
    metric_basic_style = "mapdo:network_basic_style",
    attribution = "CNRS - EVS"
  )

  return(params)
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
    legend = "LEGEND",
    roe = "ROE",
    zone_inondable = "Zone inondable débordement centenale",
    ouvrage_protection = "Ouvrage protection inondation"
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
      "Chenal actif" = "active_channel_width",
      "Corridor naturel" = "natural_corridor_width",
      "Corridor connecté" = "connected_corridor_width",
      "Fond de vallée" = "valley_bottom_width"
    ),
    "Pentes" = c(
      "Pente du talweg" = "talweg_slope",
      "Pente du fond de vallée" = "floodplain_slope"
    ),
    "Occupation du sol" = c(
      "Surface en eau" = "water_channel",
      "Bancs sédimentaires" = "gravel_bars",
      "Espace naturel ouvert" = "natural_open",
      "Forêt" = "forest",
      "Prairie permanente" = "grassland",
      "Culture" = "crops",
      "Périurbain" = "diffuse_urban",
      "Urbain dense" = "dense_urban",
      "Infrastructure de transport" = "infrastructures"
    ),
    "Continuité latérale" = c(
      "Bande active" = "active_channel",
      "Corridor naturel" = "riparian_corridor",
      "Corridor semi-naturel" = "semi_natural",
      "Espace de réversibilité" = "reversible",
      "Espace déconnecté" = "disconnected",
      "Espace artificialisé" = "built_environment"
    ),
    "Indices" = c(
      "Indice de confinement" = "idx_confinement"
    )
  )

  return(choices_map)
}

