#' Create interactive stacked barplots of class-distribution for region and axis
#'
#' @param data classified network data with entries for regional and axis dgos
#' @param colors color vector
#'
#' @importFrom dplyr count group_by mutate ungroup filter arrange rowwise left_join summarise
#' @importFrom plotly plot_ly layout event_register
#' @importFrom tidyr unite
#' @importFrom stringr str_extract
#'
#' @return interactive stacked barplot with class distribution in % for each scale-group (region and axis)
#'
analysis_plot_classes_distr <- function(data,
                                        france = NULL, france_strahler = c(6:0),
                                        basin_id = NULL, basin_strahler = 0,
                                        region_id = NULL, region_strahler = 0, region_names = NULL,
                                        axis_data = NULL){

  browser()


  # create filter to select scales ------------------------------------------

  # France-Basin scale stats
  if (!is.null(basin_id) & is.null(region_id) & is.null(axis_id)) {
    filter <- c("France (total)_France_0",
                paste0("Basin (total)_", basin_id, "_0"))
  }

  # France-Basin-Region scale stats
  else if (!is.null(basin_id) & !is.null(region_id) & is.null(axis_id)) {
    filter <- c("France (total)_France_0",
                paste0("Basin (total)_", basin_id, "_0"),
                paste0("Région (total)_", region_id, "_0"))
  }

  # France-Basin-Region-Axis scale stats
  else if (!is.null(basin_id) & !is.null(region_id) & !is.null(axis_data)) {

    filter <- c("France (total)_France_0",
                paste0("Basin (total)_", basin_id, "_", basin_strahler),
                paste0("Basin_", basin_id, "_", basin_strahler),
                paste0("Région (total)_", region_id, "_", region_strahler),
                paste0("Région_", region_id, "_", region_strahler),
                "Axe")

    # create axis stats
    axis_data <- axis_data %>%
      filter(class_name != "unvalid") %>%
      dplyr::count(class_name) %>%
      dplyr::mutate(share = round((n / sum(n) * 100), 2)) %>%
      dplyr::ungroup() %>%
      rename(class_count = n) %>%
      rowwise() %>%
      mutate(scale = "Axe", color = get_color_by_class(class_name, colors_list = params_classes_colors()))
  }

  # Only regions
  else if (is.null(basin_id) & !is.null(region_id) & is.null(axis_data)) {
    filter <- c(paste0("Région (total)_", region_id, "_0"),
                paste0("Région_", region_id, "_", region_strahler))
  }

  # default and only France-scale stats
  else {
    filter <- c("France (total)_France_0",
                paste0("France_France_", france_strahler))
  }

  # prepare data ------------------------------------------------------------
  # get group counts
  group_counts <- data %>%
    filter(class_name != "unvalid") %>%
    group_by(level_type, level_name, strahler) %>%
    summarise(count_tot = sum(class_count)) %>%
    ungroup()

  # create stats
  df <- data %>%
    filter(class_name != "unvalid") %>%
    rowwise() %>%
    mutate(color = get_color_by_class(class_name, colors_list = params_classes_colors())) %>%
    left_join(group_counts, by = join_by(level_type, level_name, strahler)) %>%
    mutate(share = round(class_count/count_tot*100, 2)) %>%
    tidyr::unite(scale, c("level_type", "level_name", "strahler")) %>%
    filter(scale %in% filter) %>%
    arrange(match(scale, filter)) %>%
    select(-count_tot) %>%
    add_row(axis_data)

  # change names according to scale
  if (!is.null(basin_id) & !is.null(region_id) & !is.null(axis_data)) {
    df <- df %>%
      mutate(scale = case_when(
        # Handle the "France" cases
        grepl("France \\(total\\)_France_0", scale) ~ "France",
        grepl("France \\(total\\)_France_\\d+", scale) ~ paste0("France, Ordre ", sub(".*_.*_(\\d+)", "\\1", scale)),

        # Handle the "Basin" cases
        grepl("Basin \\(total\\)_\\d+_0", scale) ~ "Bassin",
        grepl("Basin_\\d+_\\d+", scale) ~ paste0("Bassin, Ordre ", sub(".*_(\\d+)$", "\\1", scale)),

        # Handle the "Région" cases
        grepl("Région \\(total\\)_\\d+_0", scale) ~ "Région",
        grepl("Région_\\d+_\\d+", scale) ~ paste0("Région, Ordre ", sub(".*_(\\d+)$", "\\1", scale)),

        # Default case to keep any other values unchanged
        .default = scale
      ))
  }

  else if (is.null(basin_id) & !is.null(region_id) & !is.null(region_names) & is.null(axis_data)) {
    r_names <- setNames(region_names$lbregionhy, region_names$cdbh)


    df <- df %>%
      mutate(scale = case_when(
        # Handle the "France" cases
        grepl("France \\(total\\)_France_0", scale) ~ "France",
        grepl("France \\(total\\)_France_\\d+", scale) ~ paste0("France, Ordre ", sub(".*_.*_(\\d+)", "\\1", scale)),

        # Handle the "Région" cases
        grepl("Région \\(total\\)_\\d+_0", scale) ~ paste0("Région ", r_names[sub(".*_(\\d+)_0", "\\1", scale)]),
        grepl("Région_\\d+_\\d+", scale) ~ paste0(
          "Région ", r_names[sub(".*_(\\d+)_\\d+", "\\1", scale)],
          ", Ordre ", sub(".*_(\\d+)$", "\\1", scale)),

        # Default case to keep any other values unchanged
        .default = scale
      ))
  }


  # create plot -------------------------------------------------------------

  # Create a named vector for colors to ensure correct mapping
  unique_classes <- unique(df$class_name)
  color_palette <- setNames(unique(df$color), unique_classes)

  # Create the stacked bar plot
  plot <-
    plotly::plot_ly(data = df,
                    x = ~scale,
                    y = ~share,
                    type = 'bar',
                    color = ~class_name,  # This will automatically create a legend
                    colors = color_palette,  # Map colors to class names
                    hovertext = ~paste0(class_name, ": ", share, " % \n (", class_count, " tronçons)"),
                    hoverinfo = 'text',
                    marker = list(line = list(color = 'white', width = 2)),
                    source = 'B'
    ) %>%
    plotly::layout(
      barmode = 'stack',
      bargap = 0.5,
      title = "Proportion de classes",
      xaxis = list(title = "", showgrid = F, categoryorder = "array", categoryarray = df$scale),
      yaxis = list(title = "Pourcentage", showgrid = T, showticklabels = T),
      showlegend = TRUE
    )

  return(plot)
}


# Function to get color by class name
get_color_by_class <- function(class_name, colors_list) {
  for (sublist in colors_list) {
    if (class_name %in% names(sublist)) {
      return(sublist[[class_name]])
    }
  }
  return(NULL)  # Return NULL if class name not found
}
