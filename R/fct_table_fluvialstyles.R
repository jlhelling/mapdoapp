#' table_fluvialstyles
#'
#' @description A fct function
#'
#' @importFrom reactable reactable colDef
#' @importFrom htmltools div
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
create_table_fluvialstyles <- function(params_styles) {

  table <- reactable(params_styles,
                     columns = list(
                       class = colDef(name = "Style", sortable = FALSE),
                       description = colDef(show = FALSE)  # Hide the detail column in the main view
                     ),
                     details = function(index) {
                       htmltools::div(
                         style = "padding: 10px; margin-left: 84px; white-space: pre-wrap;",  # Add text indentation
                         # htmltools::strong("Details: "),
                         params_styles$description[index]  # Display the detail column for the specific row
                       )
                     }, selection = "single", defaultSelected = 1, onClick = "select")
}
