#' Removes anomalies from data frame
#'
#' \code{remove_saved_anomalies} is a function that takes a prepared dataframe
#' and imputes Nan to rows from a separate dataframe containin the rows with
#' anomalies. The function can be applied to already cleaned data where the
#' function will only impute additional rows from the anomalies data frame.
#'
#' @param data A tibble containing the data to be cleaned before it is used for
#'   time series forecasting, which has been created using the
#'   \code{initialize_ts_forecast_data} function.
#' @param anomalies A tibble containing the data with the anomalies. Can be
#'   loaded by using the \code{load_saved_anomalies} function.
#'
#' @return A tibble possibly extended with is_anomaly column and anomality rows
#'   imputed with NaN.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @importFrom tidyr replace_na
remove_saved_anomalies <- function(data, anomalies) {
  # Check data
  check_data_format(
    data = data,
    func_name = "remove_saved_anomalies",
    req_cols = c("period", "col_of_interest", "grouping")
  )
  # Check anomalies
  check_data_format(
    data = anomalies,
    func_name = "remove_saved_anomalies",
    req_cols = c("period", "grouping", "is_anomaly", "changepoint_delta")
  )
  # Rename columns before joining
  anomalies <- anomalies %>% 
    dplyr::rename(
      loaded_is_anomaly = is_anomaly,
      loaded_changepoint_delta = changepoint_delta
    )
  # Join the tables
  data <- dplyr::left_join(
      x = data,
      y = anomalies,
      by = c("period", "grouping")
    ) %>% 
    tidyr::replace_na(
      list(
        loaded_is_anomaly = 0,
        loaded_changepoint_delta = 0
      )
    )
  # If comments are in data, then remove them
  if ("comment" %in% colnames(data)) {
    data <- data %>% 
      dplyr::select(-comment)
  }
  # Sets the is_anomaly flag
  if ("is_anomaly" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(is_anomaly = as.numeric(is_anomaly | loaded_is_anomaly)) %>%
      dplyr::select(-loaded_is_anomaly)
  } else {
    data <- dplyr::rename(data, is_anomaly = loaded_is_anomaly)
  }
  # Sets the changepoint_delta value
  if ("changepoint_delta" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(changepoint_delta = changepoint_delta + loaded_changepoint_delta) %>%
      dplyr::select(-loaded_changepoint_delta)
  } else {
    data <- dplyr::rename(data, changepoint_delta = loaded_changepoint_delta)
  }
  # Adds original_col_of_interest if not in data
  if (!("original_col_of_interest" %in% colnames(data))) {
    data <- dplyr::mutate(data, original_col_of_interest = col_of_interest)
  }
  # Sets col_interest to NA if anomaly and returns data
  data %>%
    dplyr::mutate(
      col_of_interest = original_col_of_interest + changepoint_delta,
      col_of_interest = ifelse(
        test = is_anomaly == 1,
        yes = NA,
        no = col_of_interest
      )
    ) %>%
    return()
}