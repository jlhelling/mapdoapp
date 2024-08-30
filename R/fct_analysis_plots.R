#' Create interactive stacked barplots of class-distribution for region and axis
#'
#' @param data classified network data with entries for regional and axis dgos
#' @param colors color vector
#'
#' @importFrom dplyr count group_by mutate ungroup filter arrange rowwise left_join summarise
#' @importFrom plotly plot_ly layout event_register
#' @importFrom tidyr unite
#'
#' @return interactive stacked barplot with class distribution in % for each scale-group (region and axis)
#'
analysis_plot_classes_distr <- function(data){

  group_counts <- data |>
    filter(class_name != "unvalid") |>
    group_by(level_type, level_name, strahler) |>
    summarise(count_tot = sum(class_count)) |>
    ungroup()

  data2 <- data |>
    filter(class_name != "unvalid") |>
    rowwise() |>
    mutate(color = get_color_by_class(class_name, colors_list = params_classes_colors())) |>
    left_join(group_counts, by = join_by(level_type, level_name, strahler)) |>
    mutate(share = round(class_count/count_tot*100, 2)) |>
    tidyr::unite(scale, c("level_type", "level_name", "strahler")) |>
    filter(scale %in% c("France (total)_France_0", "Basin (total)_04_0", "Basin (total)_06_0")) |>
    arrange(scale, desc(share))

  # Create a named vector for colors to ensure correct mapping
  unique_classes <- unique(data2$class_name)
  color_palette <- setNames(unique(data2$color), unique_classes)


  # Create the stacked bar plot
  plot <-
    plotly::plot_ly(data = data2,
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
      xaxis = list(title = "", showgrid = F),
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
