#' Create an empty longitudinal profile plot
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
  plot <- plot_ly(data = temp) %>%
    layout(title = list(
      text = "Sélectionnez un cours d'eau sur la carte et une métrique pour afficher le graphique",
      y = 0.80,  # y title position
      x = 0.3,   # x title position
      font = list(size = 15)
    ))
  return(plot)
}


#' Create a vertical dashed line annotation for longitudinal profile plots
#'
#' This function generates a vertical dashed line annotation for longitudinal profile
#' plots using the 'plotly' package.
#'
#' @param x The x-coordinate where the vertical line should be positioned.
#' @param color The color of the vertical dashed line (default is "green").
#'
#' @return A list object representing a vertical dashed line annotation.
#'
#' @examples
#' # see lg_profile_main() function to use it in a plotly graph
#' # Create a vertical dashed line annotation at x = 10 with a red color
#' vertical_line_annotation <- lg_vertical_line(x = 10, color = "red")
#'
#' @export
lg_vertical_line <- function(x = 0, color = "green") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot")
  )
}

#' Create a longitudinal profile plot for selected axis data
#'
#' This function generates a longitudinal profile plot using the 'plot_ly'
#' function from the 'plotly' package. It allows you to visualize a specific
#' metric along the selected axis.
#'
#' @param data A data frame containing the selected axis data.
#' @param y The metric to be plotted on the y-axis.
#' @param y_label The name of the metric plotted.
#' @param y_label_category The metric category name.
#'
#' @return A longitudinal profile plot with the specified metric.
#'
#' @importFrom plotly plot_ly layout
#' @importFrom stats as.formula
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
lg_profile_main <- function(data = selected_axis_df, y = "active_channel_width",
                            y_label = "Chenal actif", y_label_category = "Largeurs") {
  plot <- plot_ly(data = data, x = ~measure, y = as.formula(paste0("~", y)), yaxis = 'y1',
                  key = ~fid,  # the "id" column for hover text
                  type = 'scatter', mode = 'lines', name = y_label) %>%
    layout(
      xaxis = list(title = 'Distance depuis l\'exutoire (km)'),
      yaxis = list(
        title = paste0(y_label_category, " - ", y_label),
        side = 'left'
      ),
      legend = list(orientation = 'h'),
      hovermode = "x unified",
      shapes = list(lg_vertical_line(2.5))
    )
  return(plot)
}


#' Create a dual-axis longitudinal profile plot for selected axis data
#'
#' This function generates a dual-axis longitudinal profile plot using the 'plot_ly'
#' function from the 'plotly' package. It allows you to visualize two different
#' metrics along the selected axis.
#'
#' @param data A data frame containing the selected axis data.
#' @param y The primary metric to be plotted on the left y-axis.
#' @param y_label The name of the metric plotted.
#' @param y_label_category The metric category name.
#' @param y2 The secondary metric to be plotted on the right y-axis.
#' @param y2_label The name of the secondary metric plotted.
#' @param y2_label_category The metric category name of the secondary metric plotted.
#'
#' @return A dual-axis longitudinal profile plot with the specified metrics.
#'
#' @importFrom plotly plot_ly layout add_trace
#' @importFrom stats as.formula
#'
#' @examples
#' selected_axis_df <- as.data.frame(network_dgo)
#'
#' dual_axis_plot <- lg_profile_second(data = selected_axis_df,
#'                                    y = "active_channel_width",
#'                                    y_label = "Chenal actif",
#'                                    y_label_category = "Largeurs",
#'                                    y2 = "talweg_elevation_min",
#'                                    y2_label = "Chenal actif",
#'                                    y2_label_category = "Pentes")
#' dual_axis_plot
#'
#' @export
lg_profile_second <- function(data = selected_axis_df, y = "active_channel_width", y_label = "Chenal actif",
                              y_label_category = "Largeurs", y2 = "talweg_elevation_min", y2_label = "Chenal actif",
                              y2_label_category = "Pentes"){
  plot <- lg_profile_main(data = data,
                          y = y,
                          y_label = y_label,
                          y_label_category = y_label_category) %>%
    add_trace(data = data, x = ~measure, y = as.formula(paste0("~", y2),),
              key = ~fid,  # the "id" column for hover text
              type = 'scatter', mode = 'lines', name = y2_label,
              yaxis = 'y2') %>%
    layout(
      yaxis2 = list(
        title = list(text = paste0(y2_label_category, " - ",
                       y2_label),
                     standoff = 15  # control the distance between the title and the graph
        ),
        overlaying = 'y',
        side = 'right',
        showgrid = FALSE,  # Hide the gridlines for the second y-axis
        showline = FALSE  # Hide the axis line for the second y-axis
      ),
      margin = list(
        r = 80  # create space for the second y-axis title
      ),
      legend = list(orientation = 'h'),
      hovermode = "x unified"
    )
  return(plot)
}
