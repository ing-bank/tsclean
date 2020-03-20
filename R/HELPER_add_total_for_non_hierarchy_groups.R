#' Add total for non-hierarchy groups
#'
#' \code{add_total_for_non_hierarchy_groups} is a function to add the total for
#' each group within grouping, excluding the hierarchical group.
#'
#' @param data A tibble containing several columns of data required for time
#'   series forecasting, which has been created using the
#'   \code{initialize_ts_forecast_data} function.
#' @param hierarchical_cols A character string indicating which column corresponds
#'   to the hierarchical group, for which no total has to be calculated (because
#'   this is done hierarchically).
#'
#' @return A tibble extended with additional groupings, corresponding to totals
#'   per group.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom tibble as_tibble
#' @import dplyr
#'
#' @examples
#' add_total_for_non_hierarchy_groups(dummy_anomaly_data)
add_total_for_non_hierarchy_groups <- function(data, hierarchical_cols = "") {
  # Check data
  check_data_format(
    data = data,
    func_name = "add_total_for_non_hierarchy_groups",
    req_cols = c("period", "col_of_interest", "grouping")
  )
  # Keep original column order
  col_order <- colnames(data)
  # Remove previous totals
  data <- data %>% 
    dplyr::filter(!grepl(" = ALL", grouping))
  # Extract information on each group and its values
  groups <- data %>% 
    dplyr::select(grouping) %>% 
    dplyr::distinct() %>% 
    split_grouping_column() %>% 
    dplyr::select(-grouping) %>% 
    as.list()
  # Check hierarchical_cols
  if (any(hierarchical_cols != "") && !all(hierarchical_cols %in% names(groups))) {
    message <- paste0("The specified hierarchical_cols ('", paste0(hierarchical_cols, collapse = ", "), "') are not available in the data, only the following groups are:\n\t", paste0(names(groups), collapse = ", "))
    stop(message)
  }
  # Keep only unique values per group and add total group if required
  for (group in names(groups)) {
    values <- unique(groups[[group]])
    if (length(values) > 1 && !group %in% hierarchical_cols) {
      values <- c("ALL", values)
    }
    groups[[group]] <- values
  }
  # Now make all possible combinations
  groups <- expand.grid(
      groups,
      stringsAsFactors = F
    ) %>% 
    tibble::as_tibble()
  # Get group column names
  group_cols <- colnames(groups)
  # Split grouping column
  data <- data %>% 
    split_grouping_column()
  # Filter out existing rows (which do not require aggregation)
  groups <- dplyr::anti_join(
    x = groups,
    y = data,
    by = group_cols
  )
  # Define summarise quosures
  summary_quosures <- quos(col_of_interest = sum(col_of_interest, na.rm = T))
  if ("original_col_of_interest" %in% colnames(data)) {
    new_quosure <- quos(original_col_of_interest = sum(original_col_of_interest, na.rm = T))
    summary_quosures <- c(summary_quosures, new_quosure)
  }
  if ("changepoint_delta" %in% colnames(data)) {
    new_quosure <- quos(changepoint_delta = sum(changepoint_delta, na.rm = T))
    summary_quosures <- c(summary_quosures, new_quosure)
  }
  if ("is_anomaly" %in% colnames(data)) {
    new_quosure <- quos(is_anomaly = max(0, is_anomaly, na.rm = T))
    summary_quosures <- c(summary_quosures, new_quosure)
  }
  # Create empty results table
  total_data <- tibble::tibble()
  # If there are groups left to aggregate
  if (nrow(groups) > 0) {
    # Loop over each combination of groups
    for (i in 1:nrow(groups)) {
      # Determine what to do with each group
      groups_to_agg <- colnames(groups)[groups[i,] == "ALL"]
      groups_to_join <- colnames(groups)[groups[i,] != "ALL"]
      # Join if there are available groups to join
      if (length(groups_to_join) > 0) {
        group_data <- dplyr::inner_join(
          x = groups[i,],
          y = data %>% 
            dplyr::select(-groups_to_agg),
          by = groups_to_join
        )
      } else {
        group_data <- data %>% 
          dplyr::select(-groups_to_agg) %>% 
          dplyr::mutate(!!! groups[i,])
      }
      # Execute aggregation and add to total_data
      if (nrow(group_data) > 0) {
        total_data <- group_data %>% 
          dplyr::select(-grouping) %>% 
          add_grouping_column(group_cols = group_cols) %>% 
          dplyr::group_by(period, grouping) %>% 
          dplyr::summarise(!!! summary_quosures) %>% 
          dplyr::ungroup() %>% 
          dplyr::bind_rows(total_data, .)
      }
    }
  }
  # Add to the rest of the data and return
  data %>% 
    dplyr::bind_rows(total_data, .) %>% 
    dplyr::select(col_order) %>% 
    return()
}