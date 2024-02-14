#' Track input user session.
#'
#' @param input reactivesValues Shiny input.
#'
#' @return message with the list avec the input values tracked.
#' @export
tracks <- function(input = input){
  inputs_tracked <- c("exploremap_shape_click", "strahler", "metricfilter", "metric_type",
                      "metric", "unit_area", "roe_profile", "profile_metric_type",
                      "profile_metric", "profile_unit_area", "roe_profile",
                      "remove_profile_axe")

  values <- list()
  for (name in names(input)) {
    if (grepl(paste(inputs_tracked, collapse = "|"), name)) {
      values[[name]] <- input[[name]]
    }
  }
  return(message(list(values)))
}
