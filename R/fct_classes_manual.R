#' Create initial dataframe for 1-variable classification, to be displayed on the UI table
#'
#' @param variable_name name of variable for which the classification should be undertaken
#' @param q_0025 2.5%-quantile value of selected metric
#' @param q_0975 97.5%-quantile value of selected metric
#' @param quantile size of quantile which provides value-range of classification
#' @param no_classes number of classes to be generated
#'
#' @return dataframe with 4 columns: class (name of each class, here automatically set from A-Z),
#'         variable (variable chosen for classification), greaterthan (values defining the threshold of each class),
#'         and color (defining the coloring for the map)
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#'df <- create_df_input(
#'       variable_name = "crops_pc",
#'       q_0025 = 5,
#'       q_0975 = 346,
#'       quantile = 75,
#'       no_classes = 4
#'       )
#'
create_df_input <- function(variable_name, q_0025, q_0975, quantile = 95, no_classes = 4){



  # 95%-Quantile Range
  q_tot <- q_0975 - q_0025

  # Desired Quantile Range
  q_effective = q_tot*(quantile/100/0.95)

  # Set upper and lower boundaries of quantile interval
  q_min <- q_0025 + q_effective*((0.95-quantile/100)/2)
  q_max <- q_0975 - q_effective*((0.95-quantile/100)/2)

  # Calculate quantile values (min, max) and steps
  q_steps <- (q_max - q_min) / (no_classes)

  # Create class thresholds with yero as first threshold and penultimate as last
  classes <- c(0, round(seq(q_min, q_max, by = q_steps), 2)[2:no_classes])


  # Create reversed RdBu color palette
  color_palette <- if (no_classes == 2) {
    c("#2166AC", "#B2182B")
  } else {
    rev(RColorBrewer::brewer.pal(no_classes, "RdBu"))
  }

  # Create dataframe
  df <- data.frame(
    class = LETTERS[1:no_classes],
    variable = variable_name,
    greaterthan = round(classes, 2),
    color = color_palette,
    stringsAsFactors = FALSE
  )

  return(df)
}


#' Assign classes to network dgos
#'
#' @param data dataframe or sf object which contains dgos of axis or region
#' @param classes df containing columns variables, greater_thans, class_names, colors which define the classification of the network
#'
#' @return classified dataframe/sf object with additional variables: class_name and color
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
assign_classes_manual <- function(data, classes) {

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
