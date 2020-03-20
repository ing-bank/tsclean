#' Detect anomalies using the tsoutlier function
#'
#' \code{detect_anomalies_using_ts_outlier} is a function to detect potential
#' anomalies using the tsoutliers function from the forecast package.
#'
#' @param data A tibble containing the data to be cleaned before it is used for
#'   time series forecasting, which has been created using the
#'   \code{initialize_ts_forecast_data} function.
#'
#' @return A tibble extended with several columns related to anomaly detection.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @importFrom forecast tsoutliers
#'
#' @examples
#' data <- detect_anomalies_using_ts_outlier(dummy_anomaly_data)
detect_anomalies_using_ts_outlier <- function(data) {
  # Check data
  check_data_format(
    data = data,
    func_name = "detect_anomalies_using_ts_outlier",
    req_cols = c("period", "col_of_interest", "grouping")
  )
  # Initialize fields to do anomaly detection
  data <- data %>% 
    dplyr::mutate(
      anomaly_det_alg = "ts_outlier",
      anomaly_lower_bound = col_of_interest,
      anomaly_upper_bound = col_of_interest,
      anomaly_flag = 0L
    )
  # Detect anomalies for each group
  for (group in unique(data$grouping)) {
    # Extract time series data
    ts <- data %>% 
      dplyr::filter(grouping == group) %>% 
      dplyr::select(period, col_of_interest)
    # Detect outliers
    outliers <- forecast::tsoutliers(ts$col_of_interest)
    outlier_periods <- ts %>% 
      dplyr::slice(outliers$index) %>% 
      dplyr::pull(period)
    # Fill as outliers
    data <- data %>% 
      dplyr::mutate(
        anomaly_flag = dplyr::case_when(
          (grouping == group & period %in% outlier_periods) ~ 1L,
          TRUE ~ anomaly_flag
        )
      )
  }
  # Return results
  return(data)
}