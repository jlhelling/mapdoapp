#' create a table to compare characteristics of region and segment
#'
#' @description A fct function
#'
#' @import dplyr
#' @importFrom reactable reactable colDef
#' @importFrom sparkline sparkline
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr pivot_longer
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
create_reactable_characteristics <- function(region_network, dgo){

  # get data
  region_df <- region_network %>% sf::st_drop_geometry()
  # axis_df <- axis_network %>% sf::st_drop_geometry()
  dgo_df <- dgo %>% sf::st_drop_geometry()

  # convert data
  network_pivot <- region_df %>%
    tidyr::pivot_longer(-c(fid, axis, measure, toponyme, strahler, gid_region),
                        names_to = "variable", values_to = "value") %>%
    dplyr::mutate(value = round(value, 2)) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarize(
      region_mean = mean(value),
      region_distr = list(value)
    )

  dgo_pivot <- dgo_df %>%
    dplyr::select(-c(fid, axis, measure, toponyme, strahler, gid_region)) %>%
    tidyr::pivot_longer(-c(), names_to = "variable", values_to = "segment") %>%
    dplyr::mutate(segment = round(segment, 2))

  # merge datasets
  merged <- left_join(network_pivot, dgo_pivot, by = join_by(variable)) %>%
    dplyr::left_join(params_metrics() %>% select(metric_name, metric_title, metric_type_title),
                     by = dplyr::join_by(variable == metric_name)) %>%
    dplyr::select(!variable) %>%
    dplyr::relocate(metric_type_title, metric_title, region_mean, region_distr, segment) %>%
    # na.omit() %>%
    dplyr::arrange(factor(metric_type_title, levels = unique(params_metrics()$metric_type_title))) %>%
    dplyr::group_by(metric_title)

  # create table
  table <- reactable(
    data = merged,
    columns = list(
      metric_type_title = colDef(name = "Groupe"),
      metric_title = colDef(name = "Métrique"),
      region_mean = colDef(
        name = "Région (moyenne)",
        style = function(value, index) {
          reactable_style_function(value, merged$segment[index])
        }
        ),
      region_distr = colDef(
        name = "Distribution",
        cell = function(value, index) {
          sparkline(merged$region_distr[[index]], type = "box")
        }),
      segment = colDef(
        name = "Tronçon",
        style = function(value, index) {
          reactable_style_function(value, merged$region_mean[index])
        })
    ),
    groupBy = "metric_type_title",
    paginateSubRows = TRUE,
    defaultExpanded = TRUE, # categories expanded rather than closed
    defaultPageSize = 10, # set rows per page
    highlight = TRUE, # highlight rows on hover
    compact = TRUE
  )

  return(table)
}



#' Helper function to style reactable table
#'
#' @param value actual value which should be coloured
#' @param other_value value to which the actual value should be compared
#'
#' @return styling for reactable row
#' @export
#'
#' @examples
#' # inside reactable function:
#' colDef(
#' name = "Tronçon",
#' style = function(value, index) {
#'   reactable_style_function(value, merged$region_mean[index])
#' })
reactable_style_function <- function(value, other_value) {
  if (is.na(value) || is.na(other_value)) {
    color <- "#212529"  # Default color for NA values
  } else if (value > other_value) {
    color <- "#283618"
  } else if (value < other_value) {
    color <- "#780000"
  } else {
    color <- "#212529"
  }
  list(color = color, fontWeight = "bold")
}
