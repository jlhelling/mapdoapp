#' Create an empty longitudinal profile plot.
#'
#' This function generates an empty longitudinal profile plot using the 'plot_ly'
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
lg_profile_empty <- function() {
  temp <- data.frame()
  plot <- plot_ly(data = temp, source = "plot_pg", type = 'scatter', mode = 'lines') %>%
    layout(
      title = list(
        text = "Sélectionnez un cours d'eau sur la carte pour afficher le graphique",
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

#' Create a longitudinal profile plot for selected axis data.
#'
#' This function generates a longitudinal profile plot using the 'plot_ly'
#' function from the 'plotly' package. It allows you to visualize a specific
#' metric along the selected axis.
#'
#' @param data data frame containing the selected axis data.
#' @param y text metric to be plotted on the y-axis.
#' @param y_label text name of the metric plotted.
#' @param y_label_category text metric category name.
#'
#' @return plotly A longitudinal profile plot with the specified metric.
#'
#' @importFrom plotly plot_ly layout
#'
#' @examples
#' # Create a longitudinal profile plot for active channel width
#' selected_axis_df <- as.data.frame(network_dgo)
# profile_plot <- lg_profile_main(data = selected_axis_df, y = "active_channel_width",
#                                  y_label = "Chenal actif",
#                                  y_label_category = "Largeurs")
# profile_plot
#'
#' @export
lg_profile_main <- function(data, y, y_label, y_label_category) {

  plot <- plot_ly(x = data$measure, y = y, yaxis = 'y1',
                  key = data$fid,  # the "id" column for hover text
                  type = 'scatter', mode = 'lines', name = y_label,
                  source = 'L', line = list(color = "#22223b")) %>%
    layout(
      xaxis = lg_xaxis_layout(data),
      yaxis = lg_yaxis_layout(y_label_category, y_label),
      # river name
      annotations = lg_annotations_layout(data),
      showlegend = TRUE,
      legend = list(orientation = 'h'),
      hovermode = "x unified",
      # shapes = list(shapes = NULL),
      margin = list(t = 20, b = 10, l = 50, r = 80)  # create space for the second y-axis title
    )
  return(plot)
}

#' plotly xaxis layout.
#'
#' @param data data.frame dgo from axis.
#'
#' @return list
#' @export
lg_xaxis_layout <- function(data){
  xaxis <- list(
    title = 'Distance depuis l\'exutoire (km)',
    range = c(0, max(data$measure)),
    zeroline = FALSE)
  return(xaxis)
}

#' plotly yaxis layout.
#'
#' @param y_label text name of the metric plotted.
#' @param y_label_category text metric category name.
#'
#' @return list
#' @export
lg_yaxis_layout <- function(y_label_category, y_label){
  yaxis <- list(
    title = paste0(y_label_category, " - ", y_label),
    side = 'left',
    zeroline = FALSE
  )
  return(yaxis)
}

#' plotly annotations layout.
#'
#' @param data data.frame dgo from axis.
#'
#' @return list
#' @export
lg_annotations_layout <- function(data){
  annotations = list(
    text = unique(data$toponyme),
    x = 1,  # x-coordinate (0 to 1, where 0 is left and 1 is right)
    y = -0.18,  # y-coordinate (0 to 1, where 0 is bottom and 1 is top)
    xref = "paper",  # "paper" to specify coordinates relative to the entire plot
    yref = "paper",
    showarrow = FALSE,  # Don't show the arrow
    font = list(
      # family = "Open Sans",
      size = 14,
      # color = "black"
      weight = "bold"
    )
  )
  return(annotations)
}
