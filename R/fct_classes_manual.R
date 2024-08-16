#' Create initial dataframe for 1-variable classification, to be displayed on the UI table
#'
#' @param variable_name name of variable for which the classification should be undertaken
#' @param q_0025 2.5%-quantile value of selected metric
#' @param q_0975 97.5%-quantile value of selected metric
#' @param quantile size of quantile which provides value-range of classification
#' @param no_classes number of classes to be generated
#'
#' @return dataframe with 4 columns: class (name of each class, here automatically set from A-Z),
#'         variable (variable chosen for classification), greaterthan (values defining the threshold of each class),
#'         and color (defining the coloring for the map)
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#'df <- create_df_input(
#'       variable_name = "crops_pc",
#'       q_0025 = 5,
#'       q_0975 = 346,
#'       quantile = 75,
#'       no_classes = 4
#'       )
#'
create_df_input <- function(variable_name, q_0025, q_0975, quantile = 95, no_classes = 4){



  # 95%-Quantile Range
  q_tot <- q_0975 - q_0025

  # Desired Quantile Range
  q_effective = q_tot*(quantile/100/0.95)

  # Set upper and lower boundaries of quantile interval
  q_min <- q_0025 + q_effective*((0.95-quantile/100)/2)
  q_max <- q_0975 - q_effective*((0.95-quantile/100)/2)

  # Calculate quantile values (min, max) and steps
  q_steps <- (q_max - q_min) / (no_classes)

  # Create class thresholds with yero as first threshold and penultimate as last
  classes <- c(0, round(seq(q_min, q_max, by = q_steps), 2)[2:no_classes])


  # Create reversed RdBu color palette
  color_palette <- if (no_classes == 2) {
    c("#2166AC", "#B2182B")
  } else {
    rev(RColorBrewer::brewer.pal(no_classes, "RdBu"))
  }

  # Create dataframe
  df <- data.frame(
    class = LETTERS[1:no_classes],
    variable = variable_name,
    greaterthan = round(classes, 2),
    color = color_palette,
    stringsAsFactors = FALSE
  )

  return(df)
}


#' Assign classes to network dgos
#'
#' @param data dataframe or sf object which contains dgos of axis or region
#' @param classes df containing columns variables, greater_thans, class_names, colors which define the classification of the network
#'
#' @return classified dataframe/sf object with additional variables: class_name and color
#' @importFrom rlang parse_exprs
#' @importFrom dplyr mutate case_when left_join join_by
#' @importFrom sf st_as_sf
#'
#' @examples
#' classified_network <- network_dgo %>%
#'     assign_classes(variables = as.character(r_val$grouping_table_data$variable),
#'     greater_thans = r_val$grouping_table_data$greaterthan,
#'     class_names = r_val$grouping_table_data$class)
#'
assign_classes_manual <- function(data, classes) {

  variables <- as.character(classes$variable)
  greater_thans <- classes$greaterthan
  class_names <- classes$class
  colors <- classes %>% select(class, color)

  df <-
    data %>%
    mutate(
      class_name = case_when(
        !!!parse_exprs(paste0(variables, ' >= ', greater_thans, ' ~ "', class_names, '"')
        )
      )
    ) %>%
    left_join(colors, by = join_by(class_name == class))

  return(df)
}

#' Get Styled Layer Descriptor (SLD) for network metric layer
#'
#' This function generates a Styled Layer Descriptor (SLD) XML for styling network metric layer based on quantile breaks colors.
#'
#' @param breaks A numeric vector containing quantile breaks.
#' @param colors A character vector of colors generated based on quantile breaks.
#' @param metric The name of the selected metric.
#'
#' @return A character string containing the SLD XML for styling data visualization.
#'
#' @examples
#' con <- db_con()
#' # get quantiles from active_channel_width metric
#' quantile_metrics <- sld_get_quantile_metric(selected_region_id = 11,
#'                                             selected_metric = "active_channel_width",
#'                                             con = con)
#' DBI::dbDisconnect(con)
#'
#' # get color from quantile
#' quantile_colors <- sld_get_quantile_colors(quantile_breaks = quantile_metrics)
#' # create sld style
#' sld_style <- sld_get_style(breaks = quantile_metrics,
#'                            colors = quantile_colors,
#'                            metric = "active_channel_width")
#' sld_style
#'
#' @importFrom glue glue
#'
#' @export
sld_get_style <- function(breaks, colors, metric) {
  sld_begin <- glue::glue('<?xml version="1.0" encoding="UTF-8"?>
    <StyledLayerDescriptor xmlns="http://www.opengis.net/sld" version="1.1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.1.0/StyledLayerDescriptor.xsd" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:se="http://www.opengis.net/se" xmlns:ogc="http://www.opengis.net/ogc">
      <NamedLayer>
      <se:Name>network_metrics</se:Name>
        <UserStyle>
        <se:Name>network_metrics</se:Name>
        <se:FeatureTypeStyle>')

  sld_end <- '
          </se:FeatureTypeStyle>
        </UserStyle>
      </NamedLayer>
    </StyledLayerDescriptor>'

  sld_rules <- character(0)  # Initialize an empty list to store rules



  for (i in 1:(length(breaks))) {
    if (i < length(breaks)) {
      sld_rule <- glue::glue('
      <se:Rule>
          <se:Name>{breaks[i]} - {breaks[i+1]}</se:Name>
          <se:Description>
            <se:Title>{breaks[i]} - {breaks[i+1]}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>{metric}</ogc:PropertyName>
                <ogc:Literal>{breaks[i]}</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>{metric}</ogc:PropertyName>
                <ogc:Literal>{breaks[i+1]}</ogc:Literal>
              </ogc:PropertyIsLessThan>
            </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors[i]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors[i]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')
    } else {
      sld_rule <- glue::glue('
      <se:Rule>
          <se:Name> >= {breaks[i]}</se:Name>
          <se:Description>
            <se:Title> >= {breaks[i]}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>{metric}</ogc:PropertyName>
                <ogc:Literal>{breaks[i]}</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors[i]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors[i]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')
    }
    sld_rules <- c(sld_rules, sld_rule)
  }

  sld <- paste0(sld_begin, paste(sld_rules, collapse = "\n"), sld_end)
  return(sld)
}
