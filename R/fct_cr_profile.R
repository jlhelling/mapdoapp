#' Create an empty cross section plot.
#'
#' This function generates an empty cross section plot using the 'plot_ly'
#' function from the 'plotly' package.
#'
#' @return An empty longitudinal profile plot with a specified title.
#'
#' @importFrom plotly plot_ly layout
#'
#' @examples
#' # Create an empty longitudinal profile plot
#' empty_plot <- lg_profile_empty()
#' empty_plot
#'
#' @export
cr_profile_empty <- function() {
  temp <- data.frame()
  plot <- plot_ly(data = temp, source = "plot_pg") %>%
    layout(
      title = list(
        text = "Sélectionnez un cours d'eau sur la carte, une métrique et un tronçon pour afficher la section",
        y = 0.80,  # y title position
        x = 0.3,   # x title position
        font = list(size = 15)
      ),
      xaxis = list(
        zeroline = FALSE
      ),
      yaxis = list(
        zeroline = FALSE
      ))
  return(plot)
}
