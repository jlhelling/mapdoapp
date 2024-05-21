#' Create initial dataframe for classification of a variable
#'
#' @param axis_data
#' @param variable_name
#' @param no_classes
#' @param quantile
#'
#' @return
#' @export
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
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
    classes[i] <- q_steps*(no_classes-i)
  }

  # create dataframe
  df <- data.frame(class = LETTERS[1:no_classes],
                   variable = variable_name,
                   greaterthan = classes,
                   color = brewer.pal(no_classes, "RdBu"),
                   stringsAsFactors = FALSE)
}

#' Assign classes to network dgos
#'
#' @param db
#' @param variables
#' @param greater_thans
#' @param class_names
#'
#' @return
#' @export
#' @importFrom rlang parse_exprs
#'
#' @examples
assign_classes <- function(db, variables, greater_thans, class_names){
  db %>%
    mutate(
      class_name = case_when(
        !!!parse_exprs(
          paste0(variables, ' >= ', greater_thans, ' ~ "', class_names, '"')
        )
      )
    )}
