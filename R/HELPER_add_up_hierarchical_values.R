#' Add up hierarchical values
#'
#' \code{add_up_hierarchical_values} is a function to make the hierarchical
#' dataset consistent within itself.
#'
#' @param data A tibble containing the data to be used for time series
#'   forecasting, which has been created using the
#'   \code{initialize_ts_forecast_data} function and which is hierarchical.
#' @param hierarchy A list containing the hierarchy matrix and data showing the
#'   levels for each hierarchical group. It is assumed to be created using the
#'   \code{create_hierarchy_list} function.
#'
#' @return A tibble which is similar to input data, except that all the parent
#'   hierarchical groups are re-created by adding up their respective
#'   hierarchical children.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#'
#' @examples
#' data <- dummy_hierarchical_gasprice %>%
#'    initialize_ts_forecast_data(
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = "currency",
#'       hierarchical_cols = c("location", "oil_company")
#'    ) %>%
#'    dplyr::mutate(original_col_of_interest = col_of_interest)
#' hierarchy <- create_hierarchy_list(data)
#' add_up_hierarchical_values(
#'    data = data,
#'    hierarchy = hierarchy
#' )
add_up_hierarchical_values <- function(data, hierarchy) {
  # Check inputs
  check_data_format(
    data = data,
    func_name = "add_up_hierarchical_values",
    req_cols = c("col_of_interest", "original_col_of_interest", "grouping", "period") 
  )
  # Check hierarchy
  if (!is.list(hierarchy)) {
    stop("The input hierarchy needs to be a list")
  }
  if (!"matrix" %in% names(hierarchy)) {
    stop("A matrix must be in the hierarchy list")
  }
  if (!all(comment(hierarchy$matrix) %in% colnames(data %>% split_grouping_column()))) {
    stop("Not all hierarchical group are available in the input data")
  }
  # Get hierarchical group
  hierarchical_groups <- comment(hierarchy$matrix)
  # Get non-hierarchical groups
  non_hierarchical_groups <- data %>% 
    split_grouping_column() %>% 
    colnames() %>% 
    setdiff(colnames(data))
  non_hierarchical_groups <- non_hierarchical_groups[!non_hierarchical_groups %in% hierarchical_groups]
  # Filter previous results
  data <- data %>% 
    dplyr::filter(!grepl(" = ALL", grouping))
  # Make sure the full grid of data is available
  full_data <- expand.grid(
      period = unique(data$period),
      grouping = unique(data$grouping),
      stringsAsFactors = F
    ) %>% 
    dplyr::full_join(
      x = .,
      y = data %>% 
        dplyr::select(period, grouping, col_of_interest),
      by = c("period", "grouping")
    ) %>% 
    dplyr::mutate(
      col_of_interest = dplyr::case_when(
        is.na(col_of_interest) ~ 0,
        TRUE ~ col_of_interest
      )
    )
  # Create working_data, where data is thinned
  working_data <- full_data %>% 
    split_grouping_column() %>% 
    dplyr::select("period", "col_of_interest", hierarchical_groups, non_hierarchical_groups) %>%
    dplyr::distinct() %>% 
    # Arrange hierarchical groups in proper order
    add_grouping_column(group_cols = hierarchical_groups) %>% 
    dplyr::arrange(match(grouping, rownames(hierarchy$matrix))) %>% 
    # Group across everything
    dplyr::group_by(period, !!! syms(non_hierarchical_groups)) %>% 
    # Create column where only the leaves have their original values, others are NA
    dplyr::mutate(
      leaves = dplyr::case_when(
        grouping %in% colnames(hierarchy$matrix) ~ col_of_interest,
        TRUE ~ NA_real_
      ),
      # Add up values
      added_up_col_of_interest = hierarchy$matrix %*% leaves[!is.na(leaves)]
    ) %>% 
    # Get rid of the leaves column
    dplyr::select(-grouping, -leaves) %>%   
    dplyr::ungroup()
  # Merge added up values into data, "col_of_interest" and return
  data %>% 
    split_grouping_column() %>% 
    dplyr::left_join(
      x = .,
      y = working_data,
      by = c("period", "col_of_interest", hierarchical_groups, non_hierarchical_groups)
    ) %>% 
    dplyr::select(colnames(data)[colnames(data) != "col_of_interest"], "added_up_col_of_interest") %>% 
    dplyr::rename(col_of_interest = added_up_col_of_interest) %>% 
    dplyr::select(colnames(data)) %>% 
    return()
}