lg_profile_main <- function(data = selected_axis_df, y = "active_channel_width"){
  plot <- plot_ly(data = data, x = ~measure, y = as.formula(paste0("~", y)), yaxis = 'y1',
                  key = ~fid,  # Specify the "id" column for hover text
                  type = 'scatter', mode = 'lines', name = utile_get_metric_name(y))%>%
    layout(
      xaxis = list(title = 'Distance depuis la source (km)'),
      yaxis = list(
        title = paste0(utile_get_category_name(y), " - ",
                       utile_get_metric_name(y)),
        side = 'left'
      ),
      legend = list(orientation = 'h'),
      hovermode = "x unified"
    )
  return(plot)
}


lg_profile_second <- function(data = selected_axis_df, y = "active_channel_width", y2 = "talweg_elevation_min"){
  plot <- lg_profile_main(data = data,
                          y = y) %>%
    add_trace(data = data, x = ~measure, y = as.formula(paste0("~", y2),),
              key = ~fid,  # Specify the "id" column for hover text
              type = 'scatter', mode = 'lines', name = utile_get_metric_name(y2),
              yaxis = 'y2') %>%
    layout(
      yaxis2 = list(
        title = list(text = paste0(utile_get_category_name(y2), " - ",
                       utile_get_metric_name(y2)),
                     standoff = 15  # Adjust this value to control the distance between the title and the graph
        ),
        overlaying = 'y',
        side = 'right',
        showgrid = FALSE,  # Hide the gridlines for the second y-axis
        showline = FALSE  # Hide the axis line for the second y-axis
      ),
      margin = list(
        r = 80  # Adjust this value to create space for the second y-axis title
      ),
      legend = list(orientation = 'h'),
      hovermode = "x unified"
    )
  return(plot)
}
