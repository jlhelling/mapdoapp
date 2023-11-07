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
#' normalized_string <- utile_normalize_string(original_string)
#' cat(normalized_string)  # "thisisasamplestringwithspecialcharacters"
#'
#' @export
utile_normalize_string <- function(input_string) {
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

#' Get a named vector of metric types from the params metric list.
#'
#' This function extracts metric types from a list of objects and returns a named
#' vector with metric type titles as names and metric type values as values.
#'
#' @param input_list A list of objects where each object has a 'metric_type_title' field.
#'
#' @importFrom stats setNames
#'
#' @return A named character vector with metric type titles as names and metric type values as values.
#'
#' @examples
#' metric_types <- utile_get_metric_type(params_metrics_choice())
#'
#' @export
utile_get_metric_type <- function(input_list) {
  metric_type <- setNames(names(sapply(input_list, function(x) x$metric_type_title)), sapply(input_list, function(x) x$metric_type_title))
  return(metric_type)
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
#' metrics <- utile_get_metric_name_value("largeur")
#'
#' @export
utile_get_metric_name_value <- function(metric_type){
  metric_name <- sapply(params_metrics_choice()[[metric_type]]$metric_type_value, function(x) x$metric_title)
  return(metric_name)
}
