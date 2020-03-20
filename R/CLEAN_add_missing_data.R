#' Add missing data for all groupings
#'
#' \code{add_missing_data} is a function to add missing data points within the
#' range of dates available in a dataset to be used for time series forecasting,
#' for every available grouping. The function creates additional dummy data for
#' historic data that is missing. It will impute the missing months with a
#' specified (non-zero) value, so that the time series forecasting models can
#' handle the data.
#'
#' @param data A tibble containing the data to be used for time series
#'   forecasting, which has been created using the
#'   \code{initialize_ts_forecast_data} function.
#'
#' @return A tibble which has been extended with previously missing data points
#'   for every grouping.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom tibble tibble
#' @import dplyr
#'
#' @examples
#' initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    dplyr::filter(!period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31"))) %>%
#'    add_missing_data()
add_missing_data <- function(data) {
  # Check data
  check_data_format(
    data = data,
    func_name = "add_missing_data",
    req_cols = c("period", "col_of_interest", "grouping")
  )
  # Create empty results set
  results <- tibble::tibble()
  # Call function for every grouping
  for (group in unique(data$grouping)) {
    results <- data %>% 
      dplyr::filter(grouping == group) %>% 
      add_missing_data_for_grouping() %>% 
      dplyr::bind_rows(
        results,
        .
      )
  }
  # Return results
  return(results)
}