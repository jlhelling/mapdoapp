#' Prepare selected data for interactive class distribution plot
#'
#' This function processes the network data and prepares it for plotting the class distribution
#' across different scales (France, Basin, Region, and Axis). It applies filters, computes class shares,
#' and generates color mappings.
#'
#' @param data A dataframe containing classified network data with regional and axis information.
#' @param basin_id Integer specifying the basin ID. Default is NULL.
#' @param region_id Integer specifying the region ID. Default is NULL.
#' @param strahler Integer for Strahler order. Default is 0.
#' @param region_names Dataframe containing region names and corresponding IDs.
#' @param axis_data Dataframe containing axis data (optional). Default is NULL.
#'
#' @importFrom dplyr filter mutate group_by summarise ungroup arrange left_join
#' @importFrom tidyr unite
#' @importFrom stringr str_extract
#' @return A prepared dataframe with class distribution percentages and color mappings for plotting.
#' @examples
#' \dontrun{
#' df <- prepare_selact_data_for_plot(data = classified_data, basin_id = 3, region_id = 1)
#' }
#' @export
prepare_selact_data_for_plot <- function(data,
                                         classification_type = "classes",
                                         manual_classes_table = NULL,
                                         basin_id = NULL,
                                         region_id = NULL, strahler = 0, region_names = NULL,
                                         axis_data = NULL) {


  # create filter to select scales ------------------------------------------

  # France-Basin scale stats
  if (!is.null(basin_id) & is.null(region_id) & is.null(axis_data)) {
    filter <- c(paste0("France (total)_France_", strahler),
                paste0("France_France_", strahler),
                paste0("Basin (total)_", basin_id, "_", strahler),
                paste0("Basin_", basin_id, "_", strahler))
  }

  # France-Basin-Region scale stats
  else if (!is.null(basin_id) & !is.null(region_id) & is.null(axis_data)) {
    filter <- c(paste0("France (total)_France_", strahler),
                paste0("France_France_", strahler),
                paste0("Basin (total)_", basin_id, "_", strahler),
                paste0("Basin_", basin_id, "_", strahler),
                paste0("Région (total)_", region_id, "_", strahler),
                paste0("Région_", region_id, "_", strahler))
  }

  # France-Basin-Region-Axis scale stats
  else if (!is.null(basin_id) & !is.null(region_id) & !is.null(axis_data)) {

    filter <- c(paste0("France (total)_France_", strahler),
                paste0("France_France_", strahler),
                paste0("Basin (total)_", basin_id, "_", strahler),
                paste0("Basin_", basin_id, "_", strahler),
                paste0("Région (total)_", region_id, "_", strahler),
                paste0("Région_", region_id, "_", strahler),
                "Axe")

    axis_data <- axis_data %>%
      filter(class_name != "unvalid") %>%
      count(class_name) %>%
      mutate(share = round((n / sum(n) * 100), 2)) %>%
      ungroup() %>%
      rename(class_count = n)

    # create axis stats
    if (classification_type == "classes") {
      axis_data <- axis_data %>%
        rowwise() %>%
        mutate(scale = "Axe", color = get_color_by_class(class_name, colors_list = params_classes_colors()))
    }

    else if(classification_type == "manual") {
      axis_data <- axis_data %>%
        rowwise() %>%
        mutate(scale = "Axe", color = manual_classes_table %>% filter(class == class_name) %>% pull(color))
    }

  }

  # default and only France-scale stats
  else {
    filter <- c("France (total)_France_0",
                paste0("France_France_", strahler))
  }

  # prepare data ------------------------------------------------------------
  # get group counts
  group_counts <- data %>%
    filter(class_name != "unvalid") %>%
    group_by(level_type, level_name, strahler) %>%
    summarise(count_tot = sum(class_count)) %>%
    ungroup()

  # create stats

  if (classification_type == "classes") {
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
  }

  # manual classification
  else if (classification_type == "manual") {
    df <- data %>%
      filter(class_name != "unvalid") %>%
      rowwise() %>%
      mutate(color = manual_classes_table %>% filter(class == class_name) %>% pull(color)) %>%
      left_join(group_counts, by = join_by(level_type, level_name, strahler)) %>%
      mutate(share = round(class_count/count_tot*100, 2)) %>%
      tidyr::unite(scale, c("level_type", "level_name", "strahler")) %>%
      filter(scale %in% filter) %>%
      arrange(match(scale, filter)) %>%
      select(-count_tot)
  }

  # add axis data if available
  if (!is.null(axis_data)) {
    df <- bind_rows(df, axis_data)
  }

  # Rename scale levels -----------------------------------------------------

  # change names according to scale
  df <- df %>%
    mutate(scale = case_when(
      # Handle the "France" cases
      grepl("France \\(total\\)_France_0", scale) ~ "France",
      grepl("France_France_\\d+", scale) ~ paste0("France, Ordre ", sub(".*_.*_(\\d+)", "\\1", scale)),

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

#' Prepare region-level data for interactive class distribution plot
#'
#' This function processes the network data at the region level, preparing it for plotting the class distribution
#' for specific regions. It filters data, calculates shares, and renames region scales based on the provided region names.
#'
#' @param data A dataframe containing classified network data.
#' @param region_id Integer specifying the region ID. Default is NULL.
#' @param region_strahler Integer for Strahler order of the region. Default is 0.
#' @param region_names Dataframe containing region names and corresponding IDs.
#'
#' @importFrom dplyr filter mutate group_by summarise ungroup arrange left_join
#' @importFrom tidyr unite
#' @importFrom stringr str_extract
#' @return A prepared dataframe with class distribution percentages and color mappings for plotting.
#' @examples
#' \dontrun{
#' df <- prepare_regions_data_for_plot(data = classified_data, region_id = 1, region_names = region_names_df)
#' }
#' @export
prepare_regions_data_for_plot <- function(data,
                                          classification_type = "classes",
                                          manual_classes_table = NULL,
                                          region_id = NULL, region_strahler = 0, region_names = NULL) {

  # create filter to select scales ------------------------------------------
  if (0 %in% region_strahler) {
    filter <- c(paste0("Région (total)_", region_id, "_0"))
  } else {
    filter <- c()
  }

  for (i in region_strahler[region_strahler > 0]) {
    filter <- c(filter,
                paste0("Région_", region_id, "_", i))
  }

  # prepare data ------------------------------------------------------------
  # get group counts
  group_counts <- data %>%
    filter(class_name != "unvalid") %>%
    group_by(level_type, level_name, strahler) %>%
    summarise(count_tot = sum(class_count)) %>%
    ungroup()

  # create stats
  # create stats

  if (classification_type == "classes") {
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
  }

  # manual classification
  else if (classification_type == "manual") {
    df <- data %>%
      filter(class_name != "unvalid") %>%
      rowwise() %>%
      mutate(color = manual_classes_table %>% filter(class == class_name) %>% pull(color)) %>%
      left_join(group_counts, by = join_by(level_type, level_name, strahler)) %>%
      mutate(share = round(class_count/count_tot*100, 2)) %>%
      tidyr::unite(scale, c("level_type", "level_name", "strahler")) %>%
      filter(scale %in% filter) %>%
      arrange(match(scale, filter)) %>%
      select(-count_tot)
  }

  # Rename scale levels -----------------------------------------------------

  r_names <- setNames(region_names$lbregionhy, region_names$gid)

  df <- df %>%
    mutate(scale = case_when(
      grepl("Région \\(total\\)_\\d+_0", scale) ~ r_names[sub(".*_(\\d+)_0", "\\1", scale)],
      grepl("Région_\\d+_\\d+", scale) ~ paste0(r_names[sub(".*_(\\d+)_\\d+", "\\1", scale)],
                                                ", Ordre ", sub(".*_(\\d+)$", "\\1", scale)),
      # Default case to keep any other values unchanged
      .default = scale
    ))
}











#' Create interactive stacked bar plots of class distribution for various scales
#'
#' This function generates an interactive stacked bar plot showing the class distribution
#' for various scales (France, Basin, Region, and Axis). The plot is colored based on class names,
#' and the distribution is shown as percentages.
#'
#' @param df A dataframe prepared by the `prepare_selact_data_for_plot` or `prepare_regions_data_for_plot` functions,
#' containing the processed data ready for plotting.
#'
#' @importFrom plotly plot_ly layout
#' @return Interactive stacked bar plot with class distribution percentages.
#' @examples
#' \dontrun{
#' plot <- analysis_plot_classes_distr(df)
#' }
#' @export
analysis_plot_classes_distr <- function(df){

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
      title = "Proportions des classes",
      xaxis = list(title = "", showgrid = F, categoryorder = "array", categoryarray = df$scale),
      yaxis = list(title = "Pourcentage", showgrid = T, showticklabels = T),
      showlegend = TRUE
      # margin = list(b = 70, autoexpand = FALSE)
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


#' Create a Biplot with Linear Regression and Statistical Annotations
#'
#' This function creates a biplot using the `echarts4r` library that displays a scatterplot
#' of two metrics from a given dataframe, fits a linear regression line, and shows key
#' statistical measures (correlation, R², p-value) in a floating text box.
#'
#' @param df A data frame containing the metrics to be plotted.
#' @param metric_x A string specifying the name of the x-axis metric (column in the dataframe).
#' @param metric_y A string specifying the name of the y-axis metric (column in the dataframe).
#'
#' @importFrom echarts4r e_charts_ e_scatter_ e_line e_axis_labels e_x_axis e_y_axis e_legend e_tooltip e_text_g e_toolbox_feature e_show_loading
#' @return An `echarts4r` plot object showing the biplot, regression line, and statistical annotations.
#' @examples
#' create_analysis_biplot(df = mtcars, metric_x = "mpg", metric_y = "wt")
#' @export
create_analysis_biplot <- function(df, metric_x, metric_y) {

  # Get metric titles
  metric_x_title <- globals$metrics_params |> filter(metric_name == metric_x) |> pull(metric_title)
  metric_y_title <- globals$metrics_params |> filter(metric_name == metric_y) |> pull(metric_title)

  # removing any rows with missing values (NA)
  data <- df %>% na.omit()

  # Compute linear regression
  lm_model <- lm(data[[metric_y]] ~ data[[metric_x]], data = data)
  data$lm <- predict(lm_model)

  # Compute correlation, R², and p-value
  correlation <- cor.test(data[[metric_x]], data[[metric_y]])
  r_value <- round(correlation$estimate, 2)
  p_value <- round(correlation$p.value, 4)
  r_squared <- round(r_value^2, 2)

  # Extract the coefficients for the linear model and create the equation
  coefficients <- coef(lm_model)
  intercept <- round(coefficients[1], 2)  # Intercept (b)
  slope <- round(coefficients[2], 2)      # Slope (m)

  # Format the formula as "y = mx + b"
  formula_text <- sprintf("y = %sx + %s", slope, intercept)
  linear_dependency_text <- sprintf("%s, R = %s, R² = %s, p-value = %s",
                                    formula_text, r_value, r_squared, p_value)

  # Create the biplot using echarts4r
  plot <- data %>%
    e_charts_(metric_x) %>%  # Initialize the plot and specify the x-axis metric
    e_scatter_(metric_y, symbol_size = 6, itemStyle = list(color = "#1b263b"), legend = FALSE) %>%  # Add scatter plot with points
    e_line(lm, name = "Modèle linéaire", lineStyle = list(color = "red"), symbol = 'none') %>%  # Add the linear regression line
    e_axis_labels(x = metric_x_title, y = metric_y_title) %>%  # Set axis labels using metric titles
    e_x_axis(nameLocation = "middle", nameGap = 30) %>%  # Center the x-axis title and move it below the axis
    e_y_axis(nameLocation = "middle", nameGap = 50) %>%  # Center the y-axis title and move it to the left
    e_legend(show = TRUE, itemStyle = list(color = "transparent")) %>%  # Show legend (transparent)
    # Configure tooltips that display data points info on hover
    e_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(
        sprintf("function(params) {
                return('%s: ' + params.value[0].toFixed(2) + '<br/>' +
                '%s: ' + params.value[1].toFixed(2)
                );
              }", metric_x_title, metric_y_title)
      )
    ) %>%
    # Add a floating text box with the statistical summary (R, R², p-value)
    e_text_g(
      left = "10%",               # Position text horizontally
      top = "9%",                # Position text vertically
      style = list(
        text = linear_dependency_text,  # Display correlation text
        fontSize = 12,                  # Font size for the text
        z = 1000,                       # Set z-index to bring the text to the foreground
        backgroundColor = "#ffccd5",    # Set background color of the text box
        borderRadius = 5,               # Add rounded corners to the text box
        padding = 5                     # Add padding around the text within the box
      )
    ) %>%
    #Add toolbox features (e.g., zooming, saving the plot)
    e_toolbox_feature(feature = c("dataZoom", "saveAsImage")) %>%
    # Show a loading animation while the plot is rendered
    e_show_loading()

  return(plot)
}
