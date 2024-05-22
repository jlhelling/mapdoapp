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
                   color = brewer.pal(no_classes, "RdBu"),
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
#'
#' @examples
#' classified_network <- network_dgo %>%
#'     assign_classes(variables = as.character(r_val$grouping_table_data$variable),
#'     greater_thans = r_val$grouping_table_data$greaterthan,
#'     class_names = r_val$grouping_table_data$class)
#'
assign_classes <- function(data, variables, greater_thans, class_names){
  data %>%
    mutate(
      class_name = case_when(
        !!!parse_exprs(
          paste0(variables, ' >= ', greater_thans, ' ~ "', class_names, '"')
        )
      )
    )}
