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
                            url = Sys.getenv("GEOSERVER"),
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
              class = list(name = "Style Fluvial",
                           url = Sys.getenv("GEOSERVER"),
                           language = "",
                           service = "WMS",
                           version = "1.1.0",
                           sld_version = "",
                           layer = "mapdo:network_metrics",
                           format = "image/png",
                           sld = "",
                           style = "", # no style, will be defined depending on selection
                           attribution = "CNRS - EVS",
                           basemap = FALSE,
                           overlayer = FALSE),
              network = list(name = "Réseau hydrographique",
                           url = Sys.getenv("GEOSERVER"),
                           language = "",
                           service = "WMS",
                           version = "1.1.0",
                           sld_version = "",
                           layer = "mapdo:network_metrics",
                           format = "image/png",
                           sld = "",
                           style = "", # no style, will be defined depending on selection
                           attribution = "CNRS - EVS",
                           basemap = FALSE,
                           overlayer = FALSE),
              background = list(name = "Background",
                                url = Sys.getenv("GEOSERVER"),
                                language = "",
                                service = "WMS",
                                version = "1.1.0",
                                sld_version = "",
                                layer = "mapdo:network_metrics",
                                format = "image/png",
                                sld = "",
                                style = "mapdo:classes_proposed_strahler",
                                attribution = "CNRS - EVS",
                                basemap = FALSE,
                                overlayer = FALSE),
              carteign = list(name = "Plan IGN",
                              url = 'https://data.geopf.fr/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE={style}&TILEMATRIXSET=PM&FORMAT={format}&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}',
                              language = "",
                              service = "WMTS",
                              version = "",
                              sld_version = "",
                              layer = "GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2",
                              format = "image/png",
                              sld = "",
                              style = "normal",
                              attribution = "IGN-F/Géoportail",
                              basemap = TRUE,
                              overlayer = FALSE),
              ortho = list(name = "Satellite IGN",
                           url = 'https://data.geopf.fr/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE={style}&TILEMATRIXSET=PM&FORMAT={format}&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}',
                           language = "",
                           service = "WMTS",
                           version = "",
                           sld_version = "",
                           layer = "HR.ORTHOIMAGERY.ORTHOPHOTOS",
                           format = "image/jpeg",
                           sld = "",
                           style = "normal",
                           attribution = "IGN-F/Géoportail",
                           basemap = TRUE,
                           overlayer = FALSE),
              # elevation = list(name = "Elévation IGN",
              #                  url = 'https://data.geopf.fr/wms-r?REQUEST=GetTile&SERVICE=WMS&VERSION=1.3.0&STYLE={style}&TILEMATRIXSET=PM&FORMAT={format}&LAYER=ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES&tilePixelRatio=0',
              #                  language = "",
              #                  service = "WMS",
              #                  version = "",
              #                  sld_version = "",
              #                  layer = "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES",
              #                  format = "image/jpeg",
              #                  sld = "",
              #                  style = "normal",
              #                  attribution = "IGN-F/Géoportail",
              #                  basemap = TRUE,
              #                  overlayer = FALSE),
              landuse = list(name = "Occupation du sol",
                             url = Sys.getenv("GEOSERVER"),
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
              detrend_dem = list(name = "MNT détendancé",
                                 url = Sys.getenv("GEOSERVER"),
                                 language = "",
                                 service = "WMS",
                                 version = "1.0.0",
                                 sld_version = "",
                                 layer = " 	mapdo:mapdo_nearest_height_hillshade_1m ",
                                 format = "image/png",
                                 sld = "",
                                 style = "",
                                 attribution = "CNRS - EVS",
                                 basemap = FALSE,
                                 overlayer = TRUE),
              valley_bottom = list(name = "Fond de vallée",
                                   url = Sys.getenv("GEOSERVER"),
                                   language = "",
                                   service = "WMS",
                                   version = "1.0.0",
                                   sld_version = "",
                                   layer = "mapdo:mapdo_valley_bottom_1m",
                                   format = "image/png",
                                   sld = "",
                                   style = "mapdo:MAPDO valley bottom",
                                   attribution = "CNRS - EVS",
                                   basemap = FALSE,
                                   overlayer = TRUE),
              continuity = list(name = "Continuité latérale",
                                url = Sys.getenv("GEOSERVER"),
                                language = "",
                                service = "WMS",
                                version = "1.0.0",
                                sld_version = "",
                                layer = "mapdo:mapdo_continuity_1m",
                                format = "image/png",
                                sld = "",
                                style = "mapdo:MAPDO continuity",
                                attribution = "CNRS - EVS",
                                basemap = FALSE,
                                overlayer = TRUE),
              inondation = list(name = "Zone inondable centennale",
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
#' @return A list of parameters including names for groups such as "BASSIN," "REGION," "SELECT_REGION," "METRIC," "AXIS," "LEGEND," and "ROE".
#'
#' @examples
#' # all group available
#' map_group_params <- params_map_group()
#' # get specific group name
#' map_metric_group <- params_map_group()$metric
#' map_selected_region_group <- params_map_group()$select_region
#'
#' @export
params_map_group <- function(wms_params){
  params <- list(
    bassin = "Bassins",
    region = "Régions",
    select_region = "SELECT_REGION",
    network = "Réseau hydrographique",
    metric = "METRIC",
    class = "CLASS",
    background = "BACKGROUND",
    axis = "AXIS",
    dgo_axis = "DGOAXIS",
    dgo = "DGO",
    axis_start_end = "AXIS_START_END",
    axis_opacity = "AXIS_OPACITY",
    legend = "LEGEND",
    roe = "Obstacles à l'ecoulement",
    hydro_sites = "Sites hydrométriques",
    light = "LIGHT",
    inondation = wms_params$inondation$name,
    ouvrage_protection = wms_params$ouvrage_protection$name,
    landuse = wms_params$landuse$name,
    continuity = wms_params$continuity$name,
    valley_bottom = wms_params$valley_bottom$name,
    detrend_dem = wms_params$detrend_dem$name,
    carteign = wms_params$carteign$name,
    ortho = wms_params$ortho$name,
    elevation = wms_params$elevation$name,
    geologie = wms_params$geologie$name
  )

  return(params)
}

#' Get names and description for proposed classifications
#'
#' @importFrom tibble tibble
#'
#' @return tibble with names of classes and their descriptions
#' @export
#'
params_classes <- function() {

  df <- tibble(
    class_title = c(
      "Nombre de Strahler",
      "Topographie",
      "Utilisation dominante des sols",
      "Pression urbaine",
      "Pression agricole",
      "Utilisation naturelle des sols",
      "Présence de bancs sédimentaires",
      "Confinement de la bande active",
      "Connectivité des habitats riverains"
    ),
    description = c(
      # strahler
      "Répresent la complexité du réseaux hydrographique. Le nombre de Strahler est de 1 pour tout cours d'eau entre sa source et sa première confluence et mont avec chaque confluence.",
      # topographie
      "Classification simple basée sur la pente et la hauteur du cours de la rivière :
      - plaines de basse altitude (> 0 m & < 0.5 % pente)
      - plaines de moyenne altitude (> 300 m & < 0.5 % pente)
      - plaines de montagne (> 1000 m & < 0.5 % pente)
      - pentes de basse altitude (> 0 m & > 0.5 % pente)
      - pentes de moyenne altitude (> 300 m & > 0.5 % pente)
      - pentes de montagne (> 1000 m & > 0.5 % pente)
      ",
      # dominant land use
      "Indique la classe d'utilisation des sols la plus dominante dans la zone du fond de vallée de chaque segment de cours d'eau :
      - Forêt
      - Prairies
      - Cultures
      - Espaces construits (zones urbaines et infrastructures)
      ",
      # urban areas
      "Indique le degré de couverture urbaine du fond de vallée du segment :
      - fortement urbanisé (> 70 % zones construites)
      - urbanisé  (> 40 % zones construites)
      - modérément urbanisé (> 10 % zones construites)
      - Presque pas/pas urbanisé (< 10 % zones construites)",
      # agriculture
      "indique la part de l'utilisation des terres agricoles dans la zone du fond de vallée de chaque segment de cours d'eau
      - Forte impact agricole (> 70 % cultures)
      - Impact agricole élevé (> 40 % cultures)
      - Impact agricole modéré (> 10 % cultures)
      - Presque pas/pas d'impact agricole (< 10 % cultures)",
      # natural
      "indique la part de l'occupation naturelle des sols dans la zone du fond de vallée de chaque tronçon fluvial :
      - Très forte utilisation naturelle (> 70 % espaces naturels)
      - Forte utilisation naturelle (> 40 % espaces naturels)
      - Utilisation naturelle modérée (> 10 % espaces naturels)
      - Presque pas/pas naturelle (< 10 % espaces naturels)",
      # gravel bars
      "la présence de bancs sédimentaires. Basé sur le ratio entre la surface des sédiments et la surface du chenal actif, qui se compose des surfaces de sédiments et d'eau :
      - Absent (pas des bancs sédimentaires)
      - occasionnel (bancs sédimentaires < 50 % du chenal actif)
      - fréquent (bancs sédimentaires >= 50 % du chenal actif)",
      # confinement
      "Indique le dégrée du confinement du chenal actif. Basé sur le ratio entre la largeur du chenal actif et la largeur du fond de la vallée.
      - espace abondant (chenal actif > 70 % du fond de la vallée)
      - modérement espace (chenal actif > 40 % du fond de la vallée)
      - confiné (chenal actif > 10 % du fond de la vallée)
      - très confiné (chenal actif < 10 % du fond de la vallée)",
      # habitat connectivity
      "Indique la présence d'un corridor riverain naturel. Basé sur ratio de la surface du corridor connecté (comprenant le chenal actif, le corridor naturel et les corridors semi-naturels) et le fond de la vallée :
      - très bien connecté (>= 70 %)
      - bien connecté (>= 40 %)
      - moyen connecté (>= 10 % )
      - faible / absente (< 10 %)"
    ),
    class_name = c(
      "class_strahler",
      "class_topographie",
      "class_lu_dominante",
      "class_urban",
      "class_agriculture",
      "class_nature",
      "class_gravel",
      "class_confinement",
      "class_habitat"
    ),
    sld_style = c(
      "classes_proposed_strahler",
      "classes_proposed_topographie",
      "classes_proposed_lu_dominante",
      "classes_proposed_urban",
      "classes_proposed_agriculture",
      "classes_proposed_nature",
      "classes_proposed_gravel",
      "classes_proposed_confinement",
      "classes_proposed_habitat"
    )
  ) # %>%
    # # join sld styles for legend
    # left_join(sld_get_fluvialstyles(), by = join_by(class_name))

  return(df)
}
