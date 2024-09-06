#' Prepare metrics-statistics dataframe for reactable table
#'
#' @param data metrics-statistics dataframe
#' @param type type of level_type to be included in the dataframe, default is c("Région (total)", "Région")
#'
#' @import dplyr
prepare_selact_stats_for_table <- function(data,
                                           basin_id = NULL, region_id = NULL,
                                           axis_data = NULL) {

  # create filter to select scales ------------------------------------------

  # France-Basin scale stats
  if (!is.null(basin_id)) {
    filter <- c("France (total)_France", paste0("Basin (total)_", basin_id))
  }

  # France-Basin-Region scale stats
  if (!is.null(region_id)) {
    filter <- c(filter, paste0("Région (total)_", region_id))
  }

  # France-Basin-Region-Axis scale stats
  if (!is.null(axis_data)) {

    filter <- c(filter, "France_France",
                paste0("Basin_", basin_id),
                paste0("Région_", region_id),
                "Axe")


    # Select the numeric variables to compute stats
    numeric_vars <- axis_data %>%
      select(where(is.numeric), -c(fid:strahler,gid_region, sum_area)) %>%
      sf::st_drop_geometry() %>%
      na.omit()



    # Function to compute the quantiles for each variable
    compute_quantiles <- function(x) {
      quantiles <- quantile(x, probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), na.rm = TRUE)
      as.list(round(as.numeric(quantiles), 2))  # Return only the values, rounded to 2 decimals
    }

    # Compute the average and quantiles for each numeric variable
    axis_stats <- numeric_vars %>%
      summarise(across(everything(),
                       list(
                         avg = ~ mean(.x, na.rm = TRUE),
                         distr = ~ list(compute_quantiles(.x))
                       ),
                       .names = "{.col}_{.fn}")) %>%
      mutate(scale = "Axe",
             strahler = max(axis_data$strahler))  # Add the scale and strahler column
  }

  # default and only France-scale stats
  if (is.null(basin_id) & is.null(region_id) & is.null(axis_data)) {
    filter <- c("France (total)_France", "France_France")
  }


  # Prepare dataset ---------------------------------------------------------

  suffixes <- c("_min", "_0025", "_025", "_05", "_075", "_0975", "_max")



  df <- data %>%
    select(-ends_with(suffixes)) %>%
    tidyr::unite(scale, c("level_type", "level_name")) %>%
    filter(scale %in% filter) %>%
    arrange(match(scale, filter))

  # add axis data if available
  if (!is.null(axis_data)) {
    df <- bind_rows(df, axis_stats )
  }

  # Rename scale levels -----------------------------------------------------

  # change names according to scale
  df <- df %>%
    mutate(name = case_when(
      # Handle the "France" cases
      grepl("France \\(total\\)_France_0", scale) ~ "France",
      grepl("France_France_\\d+", scale) ~ paste0("France, Ordre ", sub(".*_.*_(\\d+)", "\\1", scale)),

      # Handle the "Basin" cases
      grepl("Basin \\(total\\)_\\d+_0", scale) ~ "Bassin",
      grepl("Basin_\\d+_\\d+", scale) ~ paste0("Bassin, Ordre ", sub(".*_(\\d+)$", "\\1", scale)),

      # Handle the "Région" cases
      grepl("Région \\(total\\)_\\d+_0", scale) ~ "Région",
      grepl("Région_\\d+_\\d+", scale) ~ paste0("Région, Ordre ", sub(".*_(\\d+)$", "\\1", scale)),

      # Default case to keep any other values unchanged
      .default = scale
    )) %>%
    select(-scale)
}

#' Prepare metrics-statistics dataframe for reactable table for regions
#'
#' @param data metrics-statistics dataframe
#' @param region_names df with ids and names of regions
#'
#' @import dplyr
prepare_regions_stats_for_table <- function(data, region_names = NULL) {
  r_names <- setNames(region_names$lbregionhy, region_names$gid)
  suffixes <- c("_min", "_0025", "_025", "_05", "_075", "_0975", "_max")

  df <- data %>%
    select(-ends_with(suffixes)) %>%
    filter(level_type %in% c("Région (total)", "Région")) %>%
    mutate(name = r_names[level_name])

  return(df)
}


#' Create reactable table for metrics-statistics
#'
#' @param df metric-statistics dataframe with *_distr-columns
#' @param vars metric names to be included in table
#' @param strahler_sel selected Strahler order, default is 0 which represents an aggregate of the whole entity
#'
#' @importFrom reactable colDef reactable
#' @importFrom sparkline sparkline
#'
#' @return reactable table with variables and sparklines
#'
#' @examples
#' create_table(df, vars = c("crops_pc", "dense_urban_pc", "dense_urban"))
create_analysis_table <- function(df, vars, strahler_sel = 0, scale_name = "") {

  # extract column names from metric variables
  col_names <- c(paste0(vars, "_avg"), paste0(vars, "_distr")) %>%
    sort()

  # filter the columns and rename metrics
  df <- df %>%
    select(name, strahler, col_names) %>%
    filter(strahler %in% strahler_sel) %>%
    mutate(strahler = if_else(strahler == 0, "tous", as.character(strahler)))

  # Initialize the list of column definitions
  columns_list <- list(
    name = colDef(name = scale_name, width = 140, style = list(fontWeight = "bold"), sticky = "left"),
    strahler = colDef(name = "Ordre Strahler", width = 85, style = list(fontWeight = "bold"), sticky = "left")
  )

  # deactivate showing of column when only whole region-aggregated values are selected (strahler==0)
  if (length(strahler_sel) == 1) {
    if (strahler_sel == 0) {
      columns_list$strahler$show = FALSE
    }
  }


  metric_names <- setNames(params_metrics()$metric_title, params_metrics()$metric_name)

  # Add column definitions dynamically based on the selected metrics (vars)
  for (var in vars) {

    # Define the _avg column
    avg_col_name <- paste0(var, "_avg")
    distr_col_name <- paste0(var, "_distr")

    columns_list[[avg_col_name]] <- colDef(
      name = metric_names[[var]],  # replace column names
      minWidth = 100
    )

    columns_list[[distr_col_name]] <- colDef(
      name = "",  # No name for the sparkline column
      minWidth = 80,
      cell = function(value, index, distr_col_name) {
        sparkline(df[[distr_col_name]][[index]], type = "box",
                  chartRangeMin = range(df[[distr_col_name]], na.rm = TRUE)[1],
                  chartRangeMax = range(df[[distr_col_name]], na.rm = TRUE)[2],
                  raw = TRUE)
      }
    )
  }

  # Create the reactable
  table <- reactable(
    data = df,
    columns = columns_list,
    height = 420,
    defaultPageSize = 9,
    highlight = TRUE,  # highlight rows on hover
    compact = TRUE,
    pagination = FALSE,
    striped = TRUE
  )

  return(table)
}
