#' Add missing data for a single grouping
#'
#' \code{add_missing_data_for_grouping_forecast_package} is a function to add missing data points
#' within the range of dates available in a dataset to be used for time series
#' forecasting, for only a single available grouping. The function creates
#' additional dummy data for historic data that is missing. It will impute the
#' missing months with a specified (non-zero) value, so that the time series
#' forecasting models can handle the data.
#'
#' @param data A tibble containing the data to be used for time series
#'   forecasting, which has been created using the
#'   \code{initialize_ts_forecast_data} function.
#'
#' @return A tibble which has been extended with previously missing data points
#'   for a single grouping.
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @importFrom forecast na.interp
#'
#' @examples
#' initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'    dplyr::mutate(
#'       col_of_interest = ifelse(
#'          test = period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31")),
#'          yes = NA,
#'          no = col_of_interest
#'       )
#'    ) %>%
#'    add_missing_data_for_grouping()
add_missing_data_for_grouping <- function(data) {
  # Check data
  check_data_format(
    data = data,
    func_name = "add_missing_data_for_grouping",
    req_cols = c("period", "col_of_interest", "grouping"),
    unique_value_cols = "grouping"
  )
  # Leverage on a function from the forecast package
  data %>%
    dplyr::mutate(
      col_of_interest = forecast::na.interp(col_of_interest),
      col_of_interest = as.numeric(col_of_interest)
    ) %>% 
    return()
}