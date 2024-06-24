#' Get Quantile Metrics for a Selected Region
#'
#' This function calculates quantile metrics (Q1, Q2, Q3, Q4, Q5) for a selected metric within a specified region.
#'
#' @param selected_region_id The ID of the selected region.
#' @param selected_metric The name of the selected metric.
#' @param con PqConnection to Postgresql database.
#'
#' @return A numeric vector containing quantile metrics (Q1, Q2, Q3, Q4, Q5) for the selected metric within the specified region.
#'
#' @examples
#' con <- db_con()
#' # get quantiles from active_channel_width metric
#' quantile_metrics <- sld_get_quantile_metric(selected_region_id = 11,
#'                                             selected_metric = "active_channel_width",
#'                                             con = con)
#' quantile_metrics
#' DBI::dbDisconnect(con)
#'
#' @importFrom DBI dbGetQuery sqlInterpolate dbQuoteIdentifier SQL
#'
#' @export
sld_get_quantile_metric <- function(selected_region_id, selected_metric, con) {
  sql <- "
      SELECT
        ROUND(percentile_cont(0) WITHIN GROUP (ORDER BY ?selected_metric ASC)::numeric, 1) AS q1,
        ROUND(percentile_cont(0.25) WITHIN GROUP (ORDER BY ?selected_metric ASC)::numeric, 1) AS q2,
        ROUND(percentile_cont(0.50) WITHIN GROUP (ORDER BY ?selected_metric ASC)::numeric, 1) AS q3,
        ROUND(percentile_cont(0.75) WITHIN GROUP (ORDER BY ?selected_metric ASC)::numeric, 1) AS q4,
        ROUND(percentile_cont(1) WITHIN GROUP (ORDER BY ?selected_metric ASC)::numeric, 1) AS q5
    FROM public.network_metrics
    WHERE gid_region = ?selected_region_id"

  query <- sqlInterpolate(con, sql,
                          selected_metric = DBI::dbQuoteIdentifier(con, selected_metric),
                          selected_region_id = DBI::SQL(selected_region_id))

  data <- DBI::dbGetQuery(conn = con, statement = query)
  vector <- c(data$q1, data$q2, data$q3, data$q3, data$q4, data$q5)

  return(vector)
}


#' Get Quantile Colors
#'
#' This function generates a color palette based on quantile breaks for data mapping.
#'
#' @param quantile_breaks A numeric vector containing quantile breaks.
#'
#' @return A character vector of colors generated based on quantile breaks.
#'
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' con <- db_con()
#' # get quantiles from active_channel_width metric
#' quantile_metrics <- sld_get_quantile_metric(selected_region_id = 11,
#'                                             selected_metric = "active_channel_width",
#'                                             con = con)
#' DBI::dbDisconnect(con)
#' # get color from quantile
#' quantile_colors <- sld_get_quantile_colors(quantile_breaks = quantile_metrics)
#' quantile_colors
#'
#' @export
sld_get_quantile_colors <- function(quantile_breaks) {
  colors_palette <- colorRampPalette(c("#03045e", "#90e0ef"))(length(quantile_breaks))
  return(colors_palette)
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

  sld_end <- '</se:FeatureTypeStyle>
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


sld_get_fluvialstyles <- function() {

  # classes_tbl$sld <- ""

  sld_begin <- glue::glue('<?xml version="1.0" encoding="UTF-8"?>
    <StyledLayerDescriptor xmlns="http://www.opengis.net/sld" version="1.1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.1.0/StyledLayerDescriptor.xsd" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:se="http://www.opengis.net/se" xmlns:ogc="http://www.opengis.net/ogc">
      <NamedLayer>
      <se:Name>network_metrics</se:Name>
      <UserStyle>
      <se:Name>network_metrics</se:Name>
      <se:FeatureTypeStyle>')

  sld_end <- '</se:FeatureTypeStyle>
        </UserStyle>
      </NamedLayer>
    </StyledLayerDescriptor>'


# strahler ----------------------------------------------------------------

  colors_strahler <- colorRampPalette(c("#90e0ef", "#03045e"))(5)
  strahler_sld_rules <- character(0)

  for (i in 1:5) {
    strahler_sld_rule <- glue::glue('
      <se:Rule>
          <se:Name>{i}</se:Name>
          <se:Description>
            <se:Title>{i}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:PropertyIsEqualTo>
                <ogc:PropertyName>strahler</ogc:PropertyName>
                <ogc:Literal>{i}</ogc:Literal>
              </ogc:PropertyIsEqualTo>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_strahler[i]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_strahler[i]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')
    strahler_sld_rules <- c(strahler_sld_rules, strahler_sld_rule)
  }
  sld_strahler <- paste(strahler_sld_rules, collapse = "\n")

# Topographie -------------------------------------------------------------

  sld_topographie <- NULL


# Dominant Land use -------------------------------------------------------


  sld_lu_dominante  <- NULL


# Urban landuse -----------------------------------------------------------


  sld_urban  <- NULL


# Agricultural landuse ----------------------------------------------------


  sld_agriculture  <- NULL


# Natural landuse ---------------------------------------------------------


  sld_nature  <- NULL


# Confinement -------------------------------------------------------------


  sld_confinement  <- NULL


# Habitat -----------------------------------------------------------------


  sld_habitat  <- NULL


# Gravel bars -------------------------------------------------------------


  sld_gravel  <- NULL


# Channel evolution -------------------------------------------------------


  sld_channelevolution  <- NULL


# join all together -------------------------------------------------------

  sld_final <- tibble(
    class_name = c("class_strahler", "class_topographie", "class_lu_dominante", "class_urban", "class_agriculture",
                   "class_nature", "class_confinement", "class_habitat", "class_gravel", "class_channelevolution"),
    class_sld = c(paste0(sld_begin, sld_strahler, sld_end),
                  # "", "", "", ""
                  paste0(sld_begin, sld_topographie, sld_end),
                  paste0(sld_begin, sld_lu_dominante, sld_end),
                  paste0(sld_begin, sld_urban, sld_end),
                  paste0(sld_begin, sld_agriculture, sld_end),
                  paste0(sld_begin, sld_nature, sld_end),
                  paste0(sld_begin, sld_confinement, sld_end),
                  paste0(sld_begin, sld_habitat, sld_end),
                  paste0(sld_begin, sld_gravel, sld_end),
                  paste0(sld_begin, sld_channelevolution, sld_end))
  )

  return(sld_final)
}
