#' Create interactive stacked bar plots of class distribution for various scales
#'
#' This function generates an interactive stacked bar plot showing the class distribution
#' for various scales (France, Basin, Region, and Axis). The plot is colored based on class names,
#' and the distribution is shown as percentages.
#'
#' @param data A dataframe containing classified network data with regional and axis information.
#' @param france Numeric vector for France Strahler order. Default is NULL.
#' @param france_strahler Numeric vector representing Strahler orders for France. Default is c(6:0).
#' @param basin_id Integer specifying the basin ID. Default is NULL.
#' @param basin_strahler Integer for Strahler order of the basin. Default is 0.
#' @param region_id Integer specifying the region ID. Default is NULL.
#' @param region_strahler Integer for Strahler order of the region. Default is 0.
#' @param region_names Dataframe containing region names and corresponding IDs.
#' @param axis_data Dataframe containing axis data (optional). Default is NULL.
#'
#' @importFrom dplyr filter mutate left_join summarise group_by ungroup arrange
#' @importFrom plotly plot_ly layout
#' @importFrom tidyr unite
#' @importFrom stringr str_extract
#' @return Interactive stacked bar plot with class distribution percentages.
#' @examples
#' \dontrun{
#' analysis_plot_classes_distr(data = classified_data, basin_id = 3, region_id = 1)
#' }
#' @export
analysis_plot_classes_distr <- function(data,
                                        france = NULL, france_strahler = c(6:0),
                                        basin_id = NULL, basin_strahler = 0,
                                        region_id = NULL, region_strahler = 0, region_names = NULL,
                                        axis_data = NULL){

  # create filter to select scales ------------------------------------------

  # France-Basin scale stats
  if (!is.null(basin_id) & is.null(region_id) & is.null(axis_data)) {
    filter <- c("France (total)_France_0",
                paste0("Basin (total)_", basin_id, "_0"))
  }

  # France-Basin-Region scale stats
  else if (!is.null(basin_id) & !is.null(region_id) & is.null(axis_data)) {
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
      count(class_name) %>%
      mutate(share = round((n / sum(n) * 100), 2)) %>%
      ungroup() %>%
      rename(class_count = n) %>%
      rowwise() %>%
      mutate(scale = "Axe", color = get_color_by_class(class_name, colors_list = params_classes_colors()))
  }

  # Only regions
  else if (is.null(basin_id) & !is.null(region_id) & is.null(axis_data)) {

    filter <- c(paste0("Région (total)_", region_id, "_0"))
    for (i in region_strahler[region_strahler > 0]) {
      filter <- c(filter,
                  paste0("Région_", region_id, "_", i))
    }
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
    select(-count_tot)

  # add axis data if available
  if (!is.null(axis_data)) {
    df <- bind_rows(df, axis_data)
  }

  # Rename scale levels -----------------------------------------------------

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
    r_names <- setNames(region_names$lbregionhy, region_names$gid)


    df <- df %>%
      mutate(scale = case_when(
        # Handle the "France" cases
        grepl("France \\(total\\)_France_0", scale) ~ "France",
        grepl("France \\(total\\)_France_\\d+", scale) ~ paste0("France, Ordre ", sub(".*_.*_(\\d+)", "\\1", scale)),

        # Handle the "Région" cases
        grepl("Région \\(total\\)_\\d+_0", scale) ~ r_names[sub(".*_(\\d+)_0", "\\1", scale)],
        grepl("Région_\\d+_\\d+", scale) ~ paste0(r_names[sub(".*_(\\d+)_\\d+", "\\1", scale)],
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
