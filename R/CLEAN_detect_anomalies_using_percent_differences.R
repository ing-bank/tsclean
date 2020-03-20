#' Detect anomalies using a percentage difference
#'
#' \code{detect_anomalies_using_percent_differences} is a function to detect
#' potential anomalies by calculating a percentage difference of every point
#' compared to the moving average at the previous timestep. Combined with a 
#' specified cut-off point this will flag individual points as potential anomalies.
#'
#' @param data A tibble containing the data to be cleaned before it is used for 
#' time series forecasting, which has been created using the 
#' \code{initialize_ts_forecast_data} function.
#' @param max_pct_diff A scalar between 0 and 1, which defines the maximum 
#' percentage difference between two consecutive values of the time series for 
#' it to be considered a potential anomaly.
#' @param m_width A positive integer value indicating the number of data point 
#' to use when calculating the moving average flag outliers.
#'   
#' @return A tibble extended with several columns related to anomaly detection.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @importFrom tidyr replace_na
#' @importFrom zoo rollapply na.locf
#'
#' @examples
#' detect_anomalies_using_percent_differences(dummy_anomaly_data)
detect_anomalies_using_percent_differences <- function(data, max_pct_diff = 0.3,
                                                       m_width = 3) {
  # Check data
  check_data_format(
    data = data,
    func_name = "detect_anomalies_using_percent_differences",
    req_cols = c("period", "col_of_interest", "grouping")
  )
  # Check to make sure m_width is a non-negative whole number
  if (!(is.numeric(max_pct_diff) & (max_pct_diff > 0))) {
    message <- paste0("The parameter 'max_pct_diff' should be a positive scalar, instead of '", max_pct_diff, "' ... ")
    stop(message)
  }
  # Calculate fields to do anomaly detection and return
  data %>%
    dplyr::group_by(grouping) %>%
    # Sorts by grouping and then period
    dplyr::arrange(period, .by_group = T) %>% 
    dplyr::mutate(
      anomaly_det_alg = "percent_difference",
      # Calculate moving average with certain window size
      moving_average_ = zoo::rollapply(col_of_interest, width = m_width,
                                       align = "right", FUN = mean, fill = NA),
      # Move Ahead
      moving_average_ = dplyr::lag(moving_average_),
      # Back fill moving average
      moving_average_ = zoo::na.locf(moving_average_, fromLast = T),

      # Sets the anomaly flag
      pct_diff_ = col_of_interest / moving_average_ - 1,
      pct_diff_ = tidyr::replace_na(pct_diff_, 0),
      abs_pct_diff_ = abs(pct_diff_),
      
      # Possible that moving average is 0 which makes abs_pct_diff to Inf
      diff_ = ifelse(
        test = abs_pct_diff_ == Inf,
        yes = 10, # Heuristic
        no = abs_pct_diff_
      ),
      
      # Set anomaly flag
      anomaly_flag = as.integer(abs_pct_diff_ >= max_pct_diff),
      
      # Computes the upper and lower bound based on moving average
      anomaly_lower_bound = moving_average_ * (1 - max_pct_diff),
      anomaly_upper_bound = moving_average_ * (1 + max_pct_diff)
      
    ) %>%
    dplyr::ungroup() %>%
    # Removes temporary columns.
    dplyr::select(-ends_with('_')) %>% 
    return()
}
