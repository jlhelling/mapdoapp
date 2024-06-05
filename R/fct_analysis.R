#' Create initial dataframe for 1-variable classification, to be displayed on the UI table
#'
#' @param axis_data sf-object of an axis, containing all dgos inside the axis
#' @param variable_name name of variable for which the classification should be undertaken
#' @param no_classes number of classes to be generated
#' @param quantile size of quantile which provides value-range of classification
#'
#' @return dataframe with 4 columns: class (name of each class, here automatically set from A-Z),
#'         variable (variable chosen for classification), greaterthan (values defining the threshold of each class),
#'         and color (defining the coloring for the map)
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#'df <- create_df_input(
#'       axis_data = network_dgo,
#'       variable_name = input$variable,
#'       no_classes = input$no_classes,
#'       quantile = input$quantile
#'       )
#'
create_df_input <- function(axis_data, variable_name, no_classes, quantile = 95){

  # set upper and lower boundaries of quantile interval
  q_low <- (1 - quantile/100)/2
  q_high <- 1 - q_low

  # calculate quantile values (max, min) and steps
  q_values <- quantile(axis_data[[variable_name]], probs = c(q_low, q_high), na.rm = TRUE)
  q_steps <- (q_values[[2]] - q_values[[1]])/no_classes

  # empty dataframe to store class thresholds
  classes <- rep(0, no_classes)

  # set threshold values of all classes
  for (i in 1:no_classes) {
    classes[i] <- q_steps*(no_classes - i)
  }

  # create dataframe
  df <- data.frame(class = LETTERS[1:no_classes],
                   variable = variable_name,
                   greaterthan = classes,
                   color = {if (no_classes == 2) {c("#B2182B", "#2166AC")} else {brewer.pal(no_classes, "RdBu")}},
                   stringsAsFactors = FALSE)

  return(df)
}

#' Assign classes to network dgos
#'
#' @param data dataframe or sf object to which classes
#' @param variables vector of variable names on which the classification is based
#' @param greater_thans vector of class-thresholds for classification
#' @param class_names vector containing the names of all classes to be assigned
#'
#' @return classified dataframe/sf object with additional variable: class
#' @importFrom rlang parse_exprs
#' @importFrom dplyr mutate case_when left_join join_by
#' @importFrom sf st_as_sf
#'
#' @examples
#' classified_network <- network_dgo %>%
#'     assign_classes(variables = as.character(r_val$grouping_table_data$variable),
#'     greater_thans = r_val$grouping_table_data$greaterthan,
#'     class_names = r_val$grouping_table_data$class)
#'
assign_classes <- function(data, classes) {

  variables <- as.character(classes$variable)
  greater_thans <- classes$greaterthan
  class_names <- classes$class
  colors <- classes %>% select(class, color)

  df <-
    data %>%
    mutate(
      class_name = case_when(
        !!!parse_exprs(paste0(variables, ' >= ', greater_thans, ' ~ "', class_names, '"')
        )
      )
    ) %>%
    left_join(colors, by = join_by(class_name == class))

  return(df)
}


#' Get all Network Metrics Data for a Specific region
#'
#' This function retrieves data about network metrics for a specific region based on its ID.
#'
#' @param selected_region_id The ID of the selected region
#' @param con PqConnection to Postgresql database.
#'
#' @return A sf data frame containing information about network metrics for the specified region
#'
#' @examples
#' con <- db_con()
#' network_metrics_data <- data_get_network_axis(selected_region_id = 11, con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom dplyr arrange
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_network_region <- function(selected_region_id, con) {

  sql <- "
      SELECT
        network_metrics.fid, axis, measure, toponyme, strahler, talweg_elevation_min,
        active_channel_width, natural_corridor_width,
        connected_corridor_width, valley_bottom_width, talweg_slope, floodplain_slope,
        water_channel, gravel_bars, natural_open, forest, grassland, crops,
        diffuse_urban, dense_urban, infrastructures, active_channel, riparian_corridor,
        semi_natural, reversible, disconnected, built_environment,
        water_channel_pc, gravel_bars_pc, natural_open_pc, forest_pc, grassland_pc, crops_pc,
        diffuse_urban_pc, dense_urban_pc, infrastructures_pc, active_channel_pc,
        riparian_corridor_pc, semi_natural_pc, reversible_pc, disconnected_pc,
        built_environment_pc, sum_area, idx_confinement, gid_region, network_metrics.geom
      FROM network_metrics
      WHERE  gid_region = ?selected_region_id"
  query <- sqlInterpolate(con, sql, selected_region_id = selected_region_id)

  data <- sf::st_read(dsn = con, query = query) %>%
    dplyr::arrange(measure)

  return(data)
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


#' Get a named vector of all the metric from the metric type from the params metric list.
#'
#' This function extracts metric names and value with metric type stored in params metric list.
#'
#' @param metric_type A character with metric type.
#'
#' @return A named character vector with all the metric names and values.
#'
#' @examples
#' metrics <- utils_get_metric_name_value_analysis("largeur")
#'
#' @export
utils_get_metric_name_value_analysis <- function(metric_type){
  metric_name <- sapply(params_metrics_choice_analysis()[[metric_type]]$metric_type_value, function(x) x$metric_title)
  return(metric_name)
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



#' Combine classified regional network and axis network in one frame
#'
#' @param data_region sf-df with all dgos of selected region
#' @param data_axis sf-df with all dgos of selected axis
#' @param var variable for which classification was undertaken
#'
#' @importFrom dplyr mutate add_row select
#' @importFrom sf st_drop_geometry
#' @importFrom stats na.omit
#'
#' @return merged df with regional and axis dgos, identifiable by factor-variable "scale"
#'
#' @examples
#' merge_regional_axis_dfs(data_classified,
#'                         data_classified %>% filter(toponyme == "l'Isère"),
#'                         "forest_pc")
merge_regional_axis_dfs <- function(data_region, data_axis, var){
  df <-
    data_region %>%
    mutate(scale = as.factor("Region")) %>%
    add_row(
      data_axis %>%
        mutate(scale = as.factor("Axe fluvial"))
    ) %>%
    sf::st_drop_geometry() %>%
    select(fid, class_name, color, scale, {{var}}) %>%
    na.omit()

  return(df)
}


#' create dataframe of color-classes and values
#'
#' @param data classified network with corresponding colors for each class
#'
#' @importFrom dplyr select
#' @importFrom tibble deframe
#' @importFrom sf st_drop_geometry
#'
#' @return color vector
#'
#' @examples
#' get_colors_char_df(network)
get_colors_char_df <- function(data){
  df <-
    data %>%
    sf::st_drop_geometry() %>% # remove geometry if sf-object
    dplyr::select(class_name, color) %>%
    unique() %>%
    tibble::deframe()

  return(df)
}


#' Create interactive stacked barplots of class-distribution for region and axis
#'
#' @param data classified network data with entries for regional and axis dgos
#' @param colors color vector
#'
#' @importFrom dplyr count group_by mutate ungroup
#' @importFrom plotly plot_ly layout event_register
#'
#' @return interactive stacked barplot with class distribution in % for each scale-group (region and axis)
#'
#' @examples
#' create_plotly_barplot(data_plots)
create_plotly_barplot <- function(data){

  # create color-vector
  colors <- get_colors_char_df(data)

  # create summary df
  data_plots_summarized <- data %>%
    count(class_name, scale) %>%
    group_by(scale) %>%
    mutate(share = round((n / sum(n) * 100), 2)) %>%
    ungroup()


  # Create the stacked bar plot
  plot <-
    plotly::plot_ly(data = data_plots_summarized,
                    x = ~scale,
                    y = ~share,
                    color = ~class_name,
                    colors = colors,
                    type = 'bar',
                    text = ~paste0("Classe ", class_name, ": ", share, " % \n (", n, " tronçons)"),
                    hoverinfo = 'text',
                    marker = list(line = list(color = 'white', width = 2)),
                    source = 'B'
    ) %>%
    plotly::layout(
      barmode = 'stack',
      bargap = 0.5,
      title = "Proportion de classes",
      xaxis = list(title = "", showgrid = F),
      yaxis = list(title = "Pourcentage", showgrid = T, showticklabels = T),
      showlegend = FALSE
    )

  return(plot)
}

#' Create interactive violinplots for a specific variable for region and axis
#'
#' @param data classified network data with entries for regional and axis dgos
#' @param var variable based on which the violinplots should be created
#'
#' @importFrom plotly plot_ly layout event_register
#' @importFrom stats as.formula
#'
#' @return plotly interactive violinplots for each scale-group (region and axis)
#'
#' @examples
#' violinplot_plotly <- create_plotly_violinplot(data_plots, "forest_pc")
create_plotly_violinplot <- function(data, var){

  plot <- plotly::plot_ly(data = data,
                          x = ~scale,
                          y = as.formula(paste0("~`", var, "`")),
                          type = 'violin',
                          meanline = list(visible = TRUE),
                          points = 'all',
                          jitter = 0.1,
                          color = I("black"),
                          alpha = 0.1,
                          scalemode = 'width',
                          marker = list(size = 1, color = ~class_name),
                          spanmode = "hard",
                          hoverinfo = 'y',
                          source = 'V') %>%
    plotly::layout(
      xaxis = list(title = "", showgrid = FALSE),
      showlegend = FALSE
    )

  return(plot)
}


#' Create an overview table of the value-distribution of a variable for each scale (region and axis)
#'
#' @param data classified network data with entries for regional and axis dgos
#' @param var variable for which table should be created
#'
#' @importFrom rlang sym
#' @importFrom DT datatable
#'
#' @return DT-object with stats calculated for one variable for each scale-group
#'
#' @examples
#' classification_scales_overview_table(data_plots, "forest_pc")
classification_scales_overview_table <- function(data, var){

  # create overview dataframe
  summarised_data <-
    data %>%
    group_by(scale) %>%
    summarise(
      n = n(),
      mean = round(mean(!!rlang::sym(var)), 2),
      median = round(median(!!rlang::sym(var)), 2),
      qs_25 = round(quantile(!!rlang::sym(var), 0.25, na.rm = TRUE), 2),
      qs_75 = round(quantile(!!rlang::sym(var), 0.75, na.rm = TRUE), 2)
    )

  # convert df in DT-datatable
  table <-
    DT::datatable(
      summarised_data,
      rownames = scale,
      colnames = c('', 'No. segments', 'Moyenne', 'Médian', '25%-Qantile', '75%-Quantile'),
      # caption = 'Synthèse de la distribution des valeurs',
      options = list(dom = '',
                     columnDefs = list(list(orderable = FALSE, targets = "_all"))
      )
    )

  return(table)
}


#' Plot variable series with categorical variable in background
#'
#' @param data input data table
#' @param y var used for geom_line()-function
#' @param var_cat categorical var to be plotted in background
#' @param colors vector of colors used for printing category
#'
#' @importFrom ggplot2 ggplot geom_rect geom_line scale_fill_manual theme_minimal labs aes
#' @importFrom dplyr arrange if_else lead
#' @importFrom rlang sym
#' @importFrom plotly ggplotly
#'
#' @return ggplot()-graph of dataseries plotted as continuous line and categorical variable in background
#' @export
#'
#' @examples
#' plot_class_series_plotly(isere, "measure", "PC1", "cluster",
#'                 c("1" = "#bc4749", "2" = "#ffc300", "3" = "#a7c957", "4" = "#1a535c", "5" = "#a9def9"))
plot_class_series_plotly <- function(data, y, cat, colors){

  x <- "measure"

  # limit the x-axis to value range
  lims <- c(min(data[[x]]), max(data[[x]]) )

  data_arranged <- data %>% dplyr::arrange(!!rlang::sym(x))

  # create plot with limits and y-variable
  plot <- ggplot2::ggplot(data_arranged, # sort acc. to x-variable
                         mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
    ggplot2::geom_rect(mapping = ggplot2::aes(xmin = !!rlang::sym(x),
                                    xmax = if_else(!is.na(dplyr::lead(!!rlang::sym(x))),
                                                   dplyr::lead(!!rlang::sym(x)),
                                                   !!rlang::sym(x) + 200), # set length of last segment to 200 if no next boundary is available
                                    ymin = min(!!rlang::sym(y)), ymax = max(!!rlang::sym(y)), fill = !!rlang::sym(cat)),
                      alpha = 0.6, stat = "identity") + # background-coloring according to cluster
    ggplot2::geom_line() + # actual graph
    ggplot2::scale_fill_manual(values = colors) + # manual coloring
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "length [m]") +
    ggplot2::xlim(lims[1], lims[2])

  # convert into plotly-graph
  plot <- plotly::ggplotly(plot, tooltip = c("label", "text"), source = 'L') %>%
    plotly::layout(xaxis = list(range = c(lims[1], lims[2])))

  return(plot)
}
