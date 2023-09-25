params_geoserver <- function(){
  params <- list(
    url = "https://geoserver-dev.evs.ens-lyon.fr/geoserver/mapdo/wms",
    layer = "mapdo:network_metrics",
    format = "image/png",
    query_legend = "GetLegendGraphic",
    query_map = "GetMap",
    version = "1.0.0",
    metric_basic_style = "mapdo:network_basic_style",
    attribution = "CNRS - EVS")

  return(params)
}


params_map_group <- function(){
  params <- list(
    bassin = "BASSIN",
    region = "REGION",
    select_region = "SELECT_REGION",
    metric = "METRIC",
    axis = "AXIS",
    legend = "LEGEND",
    roe = "ROE"
  )

  return(params)
}

#' Get Choices for Network Metrics
#'
#' This function retrieves a list of choices for selecting network metrics.
#'
#' @return A list containing choices for various network metrics categories and their corresponding metrics.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   metrics_choices <- params_metrics_choice()
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
