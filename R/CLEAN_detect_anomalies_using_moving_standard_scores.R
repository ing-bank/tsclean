#' Detect anomalies using a moving standard score
#'
#' \code{detect_anomalies_using_moving_standard_scores} is a function to detect
#' potential anomalies by calculating a moving average and moving standard
#' deviation, as input for the calculation of a standard score. Combined with a
#' specified cut-off point this will flag individual points as potential
#' anomalies.
#'
#' @param data A tibble containing the data to be cleaned before it is used for
#'   time series forecasting, which has been created using the
#'   \code{initialize_ts_forecast_data} function.
#' @param m_width A positive integer value indicating the number of data point
#'   to use when calculating the moving average and moving standard deviation
#'   that are used to flag outliers.
#' @param sd_cutoff A positive value indicating the number of standard
#'   deviations to be used as cutoff point for flagging potential outliers.
#'
#' @return A tibble extended with several columns related to anomaly detection.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @importFrom zoo rollapply na.locf
#'
#' @examples
#' detect_anomalies_using_moving_standard_scores(dummy_anomaly_data)
detect_anomalies_using_moving_standard_scores <- function(data, m_width = 12, sd_cutoff = 2.0) {
  # Check data
  check_data_format(
    data = data,
    func_name = "detect_anomalies_using_moving_standard_scores",
    req_cols = c("period", "col_of_interest", "grouping")
  )
  # Check to make sure m_width is a non-negative whole number
  if (!(is.numeric(m_width) & m_width > 0 & m_width == suppressWarnings(as.integer(m_width)))) {
    message <- paste0("The parameter 'm_width' should be a positive integer value, instead of '",m_width,"' ... ")
    stop(message)
  }
  # Check to make sure sd_cutoff is a non-negative number
  if (!(is.numeric(sd_cutoff) & sd_cutoff >= 0)) {
    message <- paste0("The parameter 'sd_cutoff' should be a positive numeric value, instead of '",sd_cutoff,"' ... ")
    stop(message)
  }
  # Calculate fields to do anomaly detection and return
  data %>%
    dplyr::group_by(grouping) %>% 
    dplyr::mutate(
      anomaly_det_alg = "moving_standard_score",
      moving_average_ = zoo::rollapply(col_of_interest, width = m_width, FUN = mean, fill = NA),
      moving_average_ = zoo::na.locf(moving_average_, na.rm = F),
      moving_average_ = zoo::na.locf(moving_average_, na.rm = F, fromLast = T),
      moving_sd_ = zoo::rollapply(col_of_interest, width = m_width, FUN = sd, fill = NA),
      moving_sd_ = zoo::na.locf(moving_sd_, na.rm = F),
      moving_sd_ = zoo::na.locf(moving_sd_, na.rm = F, fromLast = T),
      anomaly_lower_bound = (moving_average_ - sd_cutoff * moving_sd_),
      anomaly_upper_bound = (moving_average_ + sd_cutoff * moving_sd_),
      standard_score_ = ifelse(
        test = is.na(moving_sd_) | moving_sd_ == 0,
        yes = 0,
        no = abs((col_of_interest - moving_average_) / moving_sd_)
      ),
      anomaly_flag = as.integer(standard_score_ >= sd_cutoff)
    ) %>% 
    dplyr::ungroup() %>% 
    # Removes temporary columns.
    select(-ends_with('_')) %>% 
    return()
}