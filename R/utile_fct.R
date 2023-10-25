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

