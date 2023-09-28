lg_profile_main <- function(data = selected_axis_df, y = "active_channel_width",
                            y_axe_label = "active channel width"){
  plot <- plot_ly(data = data, x = ~measure, y = as.formula(paste0("~", y)),
                  key = ~fid,  # Specify the "id" column for hover text
                  type = 'scatter', mode = 'lines', name = 'Ligne1')%>%
    layout(
      xaxis = list(title = 'Distance depuis la source'),
      yaxis = list(
        title = y_axe_label,
        side = 'left'
      )
    )
  return(plot)
}


lg_profile_second <- function(data = selected_axis_df, y = "active_channel_width",
                            y_axe_label = "active channel width", y2 = "talweg_elevation_min", y2_axe_label = "talweg elevation min"){
  plot <- lg_profile_main(data = data,
                          y = y,
                          y_axe_label = y_axe_label
  ) %>%
    add_trace(data = data, x = ~measure, y = as.formula(paste0("~", y2),),
              key = ~fid,  # Specify the "id" column for hover text
              type = 'scatter', mode = 'lines', name = 'Ligne2', yaxis = 'y2') %>%
    layout(
      yaxis2 = list(
        title = y2_axe_label,
        overlaying = 'y',
        side = 'right'
      )
    )
  return(plot)
}
