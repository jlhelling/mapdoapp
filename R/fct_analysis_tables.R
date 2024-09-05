#' Prepare metrics-statistics dataframe for reactable table
#'
#' @param data metrics-statistics dataframe
#' @param type type of level_type to be included in the dataframe, default is c("Région (total)", "Région")
#'
#' @import dplyr
#' @importFrom purrr map
#'
#' @return metrics-statistics dataframe with *_distr-columns
prepare_stats_df <- function(data, type = c("Région (total)", "Région"), region_names) {

  r_names <- setNames(region_names$lbregionhy, region_names$gid)

  suffixes <- c("_min", "_0025", "_025", "_05", "_075", "_0975", "_max")

  df <- data %>%
    filter(level_type %in% type) %>%
    rowwise() %>%
    mutate(across(ends_with("_avg"),
                  ~ list(map(suffixes, ~ round(get(paste0(sub("_avg$", "", cur_column()), .))), 2)),
                  .names = "{.col}_distr")) %>%
    rename_with(~ sub("_avg_distr$", "_distr", .), ends_with("_avg_distr")) %>%
    ungroup() %>%
    mutate(name = r_names[level_name],
           across(ends_with("_avg"), ~ round(., 2)))
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
create_table <- function(df, vars, strahler_sel = 0) {

  # extract column names from metric variables
  col_names <- c(paste0(vars, "_avg"), paste0(vars, "_distr")) %>%
    sort()

  # filter the columns
  df <- df %>%
    select(name, strahler, col_names) %>%
    filter(strahler %in% strahler_sel)

  # Initialize the list of column definitions
  columns_list <- list(
    name = colDef(name = "Région", width = 140),
    strahler = colDef(name = "Ordre Strahler", width = 85)
  )

  # deactivate showing of column when only whole region-aggregated values are selected (strahler==0)
  if (length(strahler_sel) == 1) {
    if (strahler_sel == 0) {
      columns_list$strahler$show = FALSE
    }
  }


  # Add column definitions dynamically based on the selected metrics (vars)
  for (var in vars) {

    # Define the _avg column
    avg_col_name <- paste0(var, "_avg")
    distr_col_name <- paste0(var, "_distr")

    columns_list[[avg_col_name]] <- colDef(
      name = gsub("_", " ", var),  # Replace _ with space for column names
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
