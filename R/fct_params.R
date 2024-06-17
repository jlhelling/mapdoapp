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
params_map_group <- function(){
  params <- list(
    bassin = "BASSIN",
    region = "REGION",
    select_region = "SELECT_REGION",
    metric = "METRIC",
    axis = "AXIS",
    dgo_axis = "DGOAXIS",
    dgo = "DGO",
    axis_start_end = "AXIS_START_END",
    axis_opacity = "AXIS_OPACITY",
    legend = "LEGEND",
    roe = "ROE",
    hydro_sites = "Site hydrométrique",
    light = "LIGHT",
    inondation = params_wms()$inondation$name,
    ouvrage_protection = params_wms()$ouvrage_protection$name,
    landuse = params_wms()$landuse$name,
    continuity = params_wms()$continuity$name,
    valley_bottom = params_wms()$valley_bottom$name,
    detrend_dem = params_wms()$detrend_dem$name,
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
#' @return A list of choices for selecting metric type."
#'
#' @examples
#' metric_choices <- params_metrics_choice()
#'
#' @export
params_metrics_choice <- function() {
  choices_map <- list(
    largeur = list(
      metric_type_title = "Largeurs (m)",
      metric_type_info = "Largeurs moyennes par tronçon de 200m sur le corridor considéré.",
      metric_type_values = list(
        active_channel_width = list(
          metric_title = "Chenal actif",
          metric_info = "Surface en eau et bancs sédimentaires."),
        natural_corridor_width = list(
          metric_title = "Corridor naturel",
          metric_info = "Surface en eau, bancs sédimentaires et végétation rivulaire connectée."),
        connected_corridor_width = list(
          metric_title = "Corridor connecté",
          metric_info = "Surface en eau, bancs sédimentaires, végétation rivulaire connectée et surfaces agricoles connectées."),
        valley_bottom_width = list(
          metric_title = "Fond de vallée",
          metric_info = "Fond de vallée déterminé par seuil de pente et d'élévation.")
      )
    ),
    elevation = list(
      metric_type_title = "Elévations (m)",
      metric_type_info = "Elévations par tronçon de 200m.",
      metric_type_values = list(
        talweg_elevation_min = list(
          metric_title = "Talweg min",
          metric_info = "Elévation minimale du talweg."
        )
      )
    ),
    pente = list(
      metric_type_title = "Pentes (%)",
      metric_type_info = "Pentes longitudinales par tronçon de 200m.",
      metric_type_values = list(
        talweg_slope = list(
          metric_title = "Talweg",
          metric_info = "Pente moyenne du talweg."
        ),
        floodplain_slope = list(
          metric_title = "Fond de vallée",
          metric_info = "Pente moyenne du fond de vallée."
        )
      )
    ),
    landuse = list(
      metric_type_title = "Occupation du sol",
      metric_type_info = "Occupation du sol en hectares ou en pourcentage de la surface du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique. \n La carte et les données ont sont issus de traitements de la BD TOPO® et du RPG®, la démarche et la méthode sont détaillées sur <a href='https://github.com/EVS-GIS/landuse-fct'>github.com</a>.",
      metric_type_values = list(
        water_channel = list(
          metric_title = "Surface en eau",
          metric_info = "Surface en eau défini par la BD TOPO® de l'IGN."
        ),
        gravel_bars = list(
          metric_title = "Banc sédimentaire",
          metric_info = "Surface des eaux intermittentes de la BD TOPO® de l'IGN."
        ),
        natural_open = list(
          metric_title = "Espace naturel ouvert",
          metric_info = "Zone de végétation ouverte telles que les forêts ouvertes, les haies ou bandes ligneuses."
        ),
        forest = list(
          metric_title = "Forêt",
          metric_info = "Zone de végétation fermée."
        ),
        grassland = list(
          metric_title = "Prairie permanente",
          metric_info = "Parcelle de prairie permanente défini dans le RPG®."
        ),
        crops = list(
          metric_title = "Culture",
          metric_info = "Zone de culture rassemblant les grandes cultures, l'arboricultre et les vignes."
        ),
        diffuse_urban = list(
          metric_title = "Périurbain",
          metric_info = "Zone d'habitation diffus proche de la zone d'habitation de la BD TOPO®."
        ),
        dense_urban = list(
          metric_title = "Urbain dense",
          metric_info = "Zone continue de l'espace bâti dense ou artificialisée."
        ),
        infrastructures = list(
          metric_title = "Infrastructure de transport",
          metric_info = "Infrastructure routières et férrovières."
        )
      )
    ),
    continuity = list(
      metric_type_title = "Continuité latérale",
      metric_type_info = "Surface de continuité latérale par corridor fluvial depuis le chenal en eau dans le fond de vallée à partir des surfaces d'occupation du sol continues. \n La surface peut être exprimée en hectares ou en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
      metric_type_values = list(
        active_channel = list(
          metric_title = "Bande active",
          metric_info = "Les surfaces en eau et les bancs sédimentaires connectées."
        ),
        riparian_corridor = list(
          metric_title = "Corridor naturel",
          metric_info = "Le chenal actif avec la végétation ouverte et fermée connectées."
        ),
        semi_natural = list(
          metric_title = "Corridor semi-naturel",
          metric_info = "Le corridor naturel avec les prairies permanentes connectées."
        ),
        reversible = list(
          metric_title = "Espace de réversibilité",
          metric_info = "Le corridor Corridor semi-naturel avec les cultures connectées."
        ),
        disconnected = list(
          metric_title = "Espace déconnecté",
          metric_info = "Espace non urbanisé déconnecté du corridor fluvial par des infrastructures ou du bâti."
        ),
        built_environment = list(
          metric_title = "Espace artificialisé",
          metric_info = "Zone bâti, dense ou peu dense, et les infrastructures de transport."
        )
      )
    ),
    index = list(
      metric_type_title = "Indices",
      metric_type_info = "Indice géomorphologique par tronçon de 200m.",
      metric_type_values = list(
        idx_confinement = list(
          metric_title = "Indice de confinement",
          metric_info = "Ratio de la largeur de la bande active sur la largeur du fond de vallée. \n Il permet d'estimer si le cours d'eau est contraint par la topographie. Plus l'indice est faible plus le cours d'eau a d'espace potentiel pour s'élargir."
        )
      )
    )
  )
  return(choices_map)
}

#' Get a list of available unit areas.
#'
#' This function returns a vector of available unit areas with their respective labels.
#'
#' @return A named character vector containing unit areas and their labels.
#'
#' @examples
#' unit_areas <- params_unit_area()
#' unit_areas["Hectares"]
#'
#' @export
params_unit_area <- function(){
  unit_area <- c(
    "Hectares" = "hectare",
    "% du fond de vallée" = "percent"
  )
  return(unit_area)
}

#' Get Choices for Metric Selection
#'
#' This function returns a list of choices for selecting metrics organized into categories.
#'
#' @return A list of choices for selecting metric type."
#'
#' @examples
#' metric_choices <- params_metrics_choice_analysis()
#'
#' @export
params_metrics_choice_analysis <- function() {
  choices_map <- list(
    largeur = list(
      metric_type_title = "Largeurs (m)",
      metric_type_info = "Largeurs moyennes par tronçon de 200m sur le corridor considéré.",
      metric_type_values = list(
        active_channel_width = list(
          metric_title = "Chenal actif",
          metric_info = "Surface en eau et bancs sédimentaires."),
        natural_corridor_width = list(
          metric_title = "Corridor naturel",
          metric_info = "Surface en eau, bancs sédimentaires et végétation rivulaire connectée."),
        connected_corridor_width = list(
          metric_title = "Corridor connecté",
          metric_info = "Surface en eau, bancs sédimentaires, végétation rivulaire connectée et surfaces agricoles connectées."),
        valley_bottom_width = list(
          metric_title = "Fond de vallée",
          metric_info = "Fond de vallée déterminé par seuil de pente et d'élévation.")
      )
    ),
    elevation = list(
      metric_type_title = "Elévations (m)",
      metric_type_info = "Elévations par tronçon de 200m.",
      metric_type_values = list(
        talweg_elevation_min = list(
          metric_title = "Talweg min",
          metric_info = "Elévation minimale du talweg."
        )
      )
    ),
    pente = list(
      metric_type_title = "Pentes (%)",
      metric_type_info = "Pentes longitudinales par tronçon de 200m.",
      metric_type_values = list(
        talweg_slope = list(
          metric_title = "Talweg",
          metric_info = "Pente moyenne du talweg."
        ),
        floodplain_slope = list(
          metric_title = "Fond de vallée",
          metric_info = "Pente moyenne du fond de vallée."
        )
      )
    ),
    landuse = list(
      metric_type_title = "Occupation du sol (ha)",
      metric_type_info = "Occupation du sol en hectares découpée à partir des tronçons de 200m du réseau hydrographique. \n La carte et les données ont sont issus de traitements de la BD TOPO® et du RPG®, la démarche et la méthode sont détaillées sur <a href='https://github.com/EVS-GIS/landuse-fct'>github.com</a>.",
      metric_type_values = list(
        water_channel = list(
          metric_title = "Surface en eau",
          metric_info = "Surface en eau défini par la BD TOPO® de l'IGN."
        ),
        gravel_bars = list(
          metric_title = "Banc sédimentaire",
          metric_info = "Surface des eaux intermittentes de la BD TOPO® de l'IGN."
        ),
        natural_open = list(
          metric_title = "Espace naturel ouvert",
          metric_info = "Zone de végétation ouverte telles que les forêts ouvertes, les haies ou bandes ligneuses."
        ),
        forest = list(
          metric_title = "Forêt",
          metric_info = "Zone de végétation fermée."
        ),
        grassland = list(
          metric_title = "Prairie permanente",
          metric_info = "Parcelle de prairie permanente défini dans le RPG®."
        ),
        crops = list(
          metric_title = "Culture",
          metric_info = "Zone de culture rassemblant les grandes cultures, l'arboricultre et les vignes."
        ),
        diffuse_urban = list(
          metric_title = "Périurbain",
          metric_info = "Zone d'habitation diffus proche de la zone d'habitation de la BD TOPO®."
        ),
        dense_urban = list(
          metric_title = "Urbain dense",
          metric_info = "Zone continue de l'espace bâti dense ou artificialisée."
        ),
        infrastructures = list(
          metric_title = "Infrastructure de transport",
          metric_info = "Infrastructure routières et férrovières."
        )
      )
    ),
    landuse_pc = list(
      metric_type_title = "Occupation du sol (%)",
      metric_type_info = "Occupation du sol en pourcentage de la surface du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique. \n La carte et les données ont sont issus de traitements de la BD TOPO® et du RPG®, la démarche et la méthode sont détaillées sur <a href='https://github.com/EVS-GIS/landuse-fct'>github.com</a>.",
      metric_type_values = list(
        water_channel_pc = list(
          metric_title = "Surface en eau",
          metric_info = "Surface en eau défini par la BD TOPO® de l'IGN."
        ),
        gravel_bars_pc = list(
          metric_title = "Banc sédimentaire",
          metric_info = "Surface des eaux intermittentes de la BD TOPO® de l'IGN."
        ),
        natural_open_pc = list(
          metric_title = "Espace naturel ouvert",
          metric_info = "Zone de végétation ouverte telles que les forêts ouvertes, les haies ou bandes ligneuses."
        ),
        forest_pc = list(
          metric_title = "Forêt",
          metric_info = "Zone de végétation fermée."
        ),
        grassland_pc = list(
          metric_title = "Prairie permanente",
          metric_info = "Parcelle de prairie permanente défini dans le RPG®."
        ),
        crops_pc = list(
          metric_title = "Culture",
          metric_info = "Zone de culture rassemblant les grandes cultures, l'arboricultre et les vignes."
        ),
        diffuse_urban_pc = list(
          metric_title = "Périurbain",
          metric_info = "Zone d'habitation diffus proche de la zone d'habitation de la BD TOPO®."
        ),
        dense_urban_pc = list(
          metric_title = "Urbain dense",
          metric_info = "Zone continue de l'espace bâti dense ou artificialisée."
        ),
        infrastructures_pc = list(
          metric_title = "Infrastructure de transport",
          metric_info = "Infrastructure routières et férrovières."
        )
      )
    ),
    continuity = list(
      metric_type_title = "Continuité latérale (ha)",
      metric_type_info = "Surface de continuité latérale par corridor fluvial depuis le chenal en eau dans le fond de vallée à partir des surfaces d'occupation du sol continues. \n La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
      metric_type_values = list(
        active_channel = list(
          metric_title = "Bande active",
          metric_info = "Les surfaces en eau et les bancs sédimentaires connectées. \n La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique."
        ),
        riparian_corridor = list(
          metric_title = "Corridor naturel",
          metric_info = "Le chenal actif avec la végétation ouverte et fermée connectées. \n La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique."
        ),
        semi_natural = list(
          metric_title = "Corridor semi-naturel",
          metric_info = "Le corridor naturel avec les prairies permanentes connectées. \n La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique."
        ),
        reversible = list(
          metric_title = "Espace de réversibilité",
          metric_info = "Le corridor Corridor semi-naturel avec les cultures connectées. \n La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique."
        ),
        disconnected = list(
          metric_title = "Espace déconnecté",
          metric_info = "Espace non urbanisé déconnecté du corridor fluvial par des infrastructures ou du bâti. \n La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique."
        ),
        built_environment = list(
          metric_title = "Espace artificialisé",
          metric_info = "Zone bâti, dense ou peu dense, et les infrastructures de transport. \n La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique."
        )
      )
    ),
    continuity_pc = list(
      metric_type_title = "Continuité latérale (%)",
      metric_type_info = "Surface de continuité latérale par corridor fluvial depuis le chenal en eau dans le fond de vallée à partir des surfaces d'occupation du sol continues. \n La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
      metric_type_values = list(
        active_channel_pc = list(
          metric_title = "Bande active",
          metric_info = "Les surfaces en eau et les bancs sédimentaires connectées."
        ),
        riparian_corridor_pc = list(
          metric_title = "Corridor naturel",
          metric_info = "Le chenal actif avec la végétation ouverte et fermée connectées."
        ),
        semi_natural_pc = list(
          metric_title = "Corridor semi-naturel",
          metric_info = "Le corridor naturel avec les prairies permanentes connectées."
        ),
        reversible_pc = list(
          metric_title = "Espace de réversibilité",
          metric_info = "Le corridor Corridor semi-naturel avec les cultures connectées."
        ),
        disconnected_pc = list(
          metric_title = "Espace déconnecté",
          metric_info = "Espace non urbanisé déconnecté du corridor fluvial par des infrastructures ou du bâti."
        ),
        built_environment_pc = list(
          metric_title = "Espace artificialisé",
          metric_info = "Zone bâti, dense ou peu dense, et les infrastructures de transport."
        )
      )
    ),
    index = list(
      metric_type_title = "Indices",
      metric_type_info = "Indice géomorphologique par tronçon de 200m.",
      metric_type_values = list(
        idx_confinement = list(
          metric_title = "Indice de confinement",
          metric_info = "Ratio de la largeur de la bande active sur la largeur du fond de vallée. \n Il permet d'estimer si le cours d'eau est contraint par la topographie. Plus l'indice est faible plus le cours d'eau a d'espace potentiel pour s'élargir."
        )
      )
    )
  )
  return(choices_map)
}


#' get nested list-object with all variables for Metric-selection in selectInput()-Elements
#'
#' @return list-object with first level the names of metric types and second levels the corresponding metrics for each type
#'
#' @examples
#' params_get_metric_choices()
params_get_metric_choices <- function(){
  y <- list()
  types <- utils_get_metric_type(params_metrics_choice_analysis())

  for (i in c(1:length(types))) {
    y[names(types[i])] <-
      list(
        # swap names and values
        setNames(names(utils_get_metric_name_value_analysis(types[i])),
                 utils_get_metric_name_value_analysis(types[i]))
      )
  }
  return(y)
}


params_metrics_info <- function() {
  # Create a simplified vector with metric names and info texts
  metric_info <- c(
    idx_confinement = "Ratio de la largeur de la bande active sur la largeur du fond de vallée. \n Il permet d'estimer si le cours d'eau est contraint par la topographie. Plus l'indice est faible plus le cours d'eau a d'espace potentiel pour s'élargir.",

    active_channel_width = "Surface en eau et bancs sédimentaires.",
    natural_corridor_width = "Surface en eau, bancs sédimentaires et végétation rivulaire connectée.",
    connected_corridor_width = "Surface en eau, bancs sédimentaires, végétation rivulaire connectée et surfaces agricoles connectées.",
    valley_bottom_width = "Fond de vallée déterminé par seuil de pente et d'élévation.",

    talweg_elevation_min = "Elévation minimale du talweg.",

    talweg_slope = "Pente moyenne du talweg.",
    floodplain_slope = "Pente moyenne du fond de vallée.",

    water_channel = "Surface en eau définie par la BD TOPO® de l'IGN.",
    gravel_bars = "Surface des eaux intermittentes de la BD TOPO® de l'IGN.",
    natural_open = "Zone de végétation ouverte telles que les forêts ouvertes, les haies ou bandes ligneuses.",
    forest = "Zone de végétation fermée.",
    grassland = "Parcelle de prairie permanente définie dans le RPG®.",
    crops = "Zone de culture rassemblant les grandes cultures, l'arboriculture et les vignes.",
    diffuse_urban = "Zone d'habitation diffus proche de la zone d'habitation de la BD TOPO®.",
    dense_urban = "Zone continue de l'espace bâti dense ou artificialisée.",
    infrastructures = "Infrastructure routière et ferroviaire.",

    water_channel_pc = "Surface en eau définie par la BD TOPO® de l'IGN.",
    gravel_bars_pc = "Surface des eaux intermittentes de la BD TOPO® de l'IGN.",
    natural_open_pc = "Zone de végétation ouverte telles que les forêts ouvertes, les haies ou bandes ligneuses.",
    forest_pc = "Zone de végétation fermée.",
    grassland_pc = "Parcelle de prairie permanente définie dans le RPG®.",
    crops_pc = "Zone de culture rassemblant les grandes cultures, l'arboriculture et les vignes.",
    diffuse_urban_pc = "Zone d'habitation diffus proche de la zone d'habitation de la BD TOPO®.",
    dense_urban_pc = "Zone continue de l'espace bâti dense ou artificialisée.",
    infrastructures_pc = "Infrastructure routière et ferroviaire.",

    active_channel = "Les surfaces en eau et les bancs sédimentaires connectés. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
    riparian_corridor = "Le chenal actif avec la végétation ouverte et fermée connectée. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
    semi_natural = "Le corridor naturel avec les prairies permanentes connectées. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
    reversible = "Le corridor semi-naturel avec les cultures connectées. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
    disconnected = "Espace non urbanisé déconnecté du corridor fluvial par des infrastructures ou du bâti. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
    built_environment = "Zone bâti, dense ou peu dense, et les infrastructures de transport. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",

    active_channel_pc = "Les surfaces en eau et les bancs sédimentaires connectés.",
    riparian_corridor_pc = "Le chenal actif avec la végétation ouverte et fermée connectée.",
    semi_natural_pc = "Le corridor naturel avec les prairies permanentes connectées.",
    reversible_pc = "Le corridor semi-naturel avec les cultures connectées.",
    disconnected_pc = "Espace non urbanisé déconnecté du corridor fluvial par des infrastructures ou du bâti.",
    built_environment_pc = "Zone bâti, dense ou peu dense, et les infrastructures de transport."
  )

  return(metric_info)

}
