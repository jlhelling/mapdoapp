#' Get the category name of a selected metric
#'
#' This function takes a selected metric and returns the category name to which
#' the metric belongs. It does this by comparing the selected metric to the
#' metric names in the categories provided by the `params_metrics_choice` function.
#'
#' @param selected_metric The name of the metric for which you want to find the
#'                       category name.
#'
#' @return The category name to which the selected metric belongs.
#'
#' @examples
#' selected_metric <- "some_metric"
#' category_name <- utile_get_category_name(selected_metric)
#'
#' @export
utile_get_category_name <- function(selected_metric) {
  category_name <- names(params_metrics_choice())[
    sapply(params_metrics_choice(), function(x) selected_metric %in% names(x))
  ]
  return(category_name)
}

#' Get the name of a selected metric
#'
#' This function takes a selected metric and returns its corresponding name.
#' It first determines the category name of the metric using the
#' `utile_get_category_name` function and then retrieves the metric name
#' from the category's metrics list provided by the `params_metrics_choice` function.
#'
#' @param selected_metric The name of the metric for which you want to retrieve
#'                       the metric name.
#'
#' @return The name of the selected metric.
#'
#' @examples
#' # get name label from metric name
#' utile_get_metric_name("active_channel_width")
#'
#' @export
utile_get_metric_name <- function(selected_metric) {
  category_name <- utile_get_category_name(selected_metric = selected_metric)
  metric_name <- params_metrics_choice()[[category_name]][[selected_metric]]
  return(metric_name)
}

#' Normalize a string by removing spaces, accents, and special characters.
#'
#' This function takes an input string and normalizes it by removing spaces, accents, diacritics,
#' and special characters. It then converts the string to lowercase.
#'
#' @param input_string The input string to be normalized.
#'
#' @return The normalized string with spaces, accents, and special characters removed, and in lowercase.
#'
#' @examples
#' original_string <- "Thïs is à sâmplè strîng with spèciál chàracters!"
#' normalized_string <- normalize_string(original_string)
#' cat(normalized_string)  # "thisisasamplestringwithspecialcharacters"
#'
#' @export
normalize_string <- function(input_string) {
  # Remove accents and diacritics
  input_string <- chartr("ÀÁÂÃÄÅàáâãäåÒÓÔÕÖØòóôõöøÈÉÊËèéêëÌÍÎÏìíîïÙÚÛÜùúûüÝýÑñÇç",
                         "AAAAAAaaaaaaOOOOOOooooooEEEEeeeeIIIIiiiiUUUUuuuuYyNnCc",
                         input_string)

  # Remove spaces and special characters
  input_string <- gsub("[^a-zA-Z0-9]", "", input_string)

  # Convert to lowercase
  normalized_string <- tolower(input_string)

  return(normalized_string)
}
