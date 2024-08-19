#' Prepare metrics-statistics dataframe for reactable table
#'
#' @param data metrics-statistics dataframe
#'
#' @return metrics-statistics dataframe with *_distr-columns
prepare_stats_df <- function(data) {
  suffixes <- c("_min", "_0025", "_025", "_05", "_075", "_0975", "_max")

  df <- data %>%
    filter(level_type %in% c("Région (total)", "Région")) |>
    rowwise() %>%
    mutate(across(ends_with("_avg"),
                  ~ list(map(suffixes, ~ round(get(paste0(sub("_avg$", "", cur_column()), .))), 2)),
                  .names = "{.col}_distr")) %>%
    rename_with(~ sub("_avg_distr$", "_distr", .), ends_with("_avg_distr")) %>%
    ungroup() |>
    mutate(across(ends_with("_avg"), ~ round(., 2)))
}



#' Create reactable table for metrics-statistics
#'
#' @param df metric-statistics dataframe with *_distr-columns
#' @param vars metric names to be included in table
#'
#' @importFrom reactable colDef reactable
#' @importFrom sparkline sparkline
#'
#' @return reactable table with variables and sparklines
#'
#' @examples
#' create_table(df, vars = c("crops_pc", "dense_urban_pc", "dense_urban"))
create_table <- function(df, vars) {

  # extract column names from metric variables
  col_names <- c(paste0(vars, "_avg"), paste0(vars, "_distr")) %>%
    sort()

  # filter the columns
  df <- df %>%
    select(level_name, strahler, col_names)

  # Initialize the list of column definitions
  columns_list <- list(
    level_name = colDef(name = "Région", width = 60),
    strahler = colDef(name = "No. Strahler", width = 60)
  )

  # Add column definitions dynamically based on the selected metrics (vars)
  for (var in vars) {

    # Define the _avg column
    avg_col_name <- paste0(var, "_avg")
    distr_col_name <- paste0(var, "_distr")

    columns_list[[avg_col_name]] <- colDef(
      name = gsub("_", " ", var),  # Replace _ with space for column names
      width = 100
    )

    columns_list[[distr_col_name]] <- colDef(
      name = "",  # No name for the sparkline column
      width = 80,
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
    compact = TRUE
  )

  return(table)
}
