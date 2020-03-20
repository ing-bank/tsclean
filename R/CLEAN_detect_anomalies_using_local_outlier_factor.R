#' Detect anomalies using local outlier factor
#'
#' \code{detect_anomalies_using_local_outlier_factor} is a function to detect
#' potential anomalies by using a machine learning technique called local
#' outlier factor. The technique attempts to find anomalies based on the
#' absolute percentage difference to the previous time steps moving average, as
#' well as information on which month and quarter it is. The local outlier
#' factor gives an anomaly score which is used to determine if a data point is
#' considered a anomaly using a cut-off point.
#'
#' @param data A tibble containing the data to be cleaned before it is used for
#'   time series forecasting, which has been created using the
#'   \code{initialize_ts_forecast_data} function.
#' @param threshold A positive numeric value to label datapoints as anomaly or
#'   not. Higher values correspond to a more conservative threshold, making
#'   points less likely to be labeled as anomalies.
#' @param s A positive integer value corresponding to the size of the
#'   neighborhood of data points to be considered when running the algorithm.
#' @param m_width A positive integer value indicating the number of data point
#'   to use when calculating the moving average and moving standard deviation
#'   that are used to flag outliers.
#' @param per_group Boolean, which is set to TRUE to calculate the local outlier
#'   factor separately for each group in the data, or set to FALSE to calculate
#'   the local outlier factor using all per_group in the data together.
#'
#' @return A tibble extended with several columns related to anomaly detection.
#' @export
#'
#' @import DDoutlier
#' @import dplyr
#' @importFrom tidyr replace_na
#' @importFrom zoo rollapply na.locf
#'
#' @examples
#' detect_anomalies_using_local_outlier_factor(dummy_anomaly_data)
detect_anomalies_using_local_outlier_factor <- function(data, threshold = 10, s = 9, m_width = 3, per_group = FALSE){
  
  # This function is to calculate local outlier factor for datasets which have only 1 group
  one_group_lof <- function(data, s){
    # Select only columns: year, month, quarter, diff_ as features
    data_t <- data %>% 
      dplyr::select(year_, month_,  quarter_,  diff_)
    # Using lof in the packadge Rlof to caculate local outlier factor
    scores <- DDoutlier::LOF(data_t, s)
    return(scores)
  }
  
  # Check data format
  check_data_format(data)
  
  # Check to make sure threshold is larget than 1
  if (!(is.numeric(threshold) && (threshold > 1))) {
    message <- paste0("The parameter 'threshold' should be a numeric value larger than 1, instead of '", threshold,"' ... ")
    stop(message)
  }
  # Check to make sure s is a non-negative whole number
  if (!(is.numeric(s) && s > 0 && s == suppressWarnings(as.integer(s)))) {
    message <- paste0("The parameter 's' should be a positive integer value, instead of '",s,"' ... ")
    stop(message)
  }
  # Check to make sure m_width is a non-negative whole number
  if (!(is.numeric(m_width) && m_width > 0 && m_width == suppressWarnings(as.integer(m_width)))) {
    message <- paste0("The parameter 'm_width' should be a positive integer value, instead of '",m_width,"' ... ")
    stop(message)
  }
  
  # if data contains only one group, assign per_group to be FALSE
  if (per_group && (length(unique(data$grouping)) == 1)) {
    per_group = FALSE
  }
  
  # Calculate absolute percentage difference between moving average
  data <- data %>% 
    # Calculate for each grouping
    dplyr::group_by(grouping) %>% 
    # Sorts by group and the period
    dplyr::arrange(period, .by_group = T) %>% 
    dplyr::mutate(
      # Calculate moving average with certain window size
      moving_average_ = zoo::rollapply(
        data = col_of_interest,
        width = m_width,
        align = "right", 
        FUN = mean, 
        fill = NA
      ),
      # Move Ahead
      moving_average_ = dplyr::lag(moving_average_),
      # Back fill moving average
      moving_average_ = zoo::na.locf(moving_average_, fromLast = T),
      # Calculate absolute percentage difference
      diff_ = col_of_interest/moving_average_ - 1,
      diff_ = tidyr::replace_na(diff_, 0),
      diff_ = abs(diff_),
      # Possible that moving average is 0 and then the diff_ will be Inf
      diff_ = ifelse(
        test = (diff_ == Inf),
        yes = 10, # Heuristic
        no = diff_
      )
    ) %>% 
    dplyr::ungroup() %>% 
    # Add month, quarter and year 
    dplyr::mutate(
      month_ = lubridate::month(period),
      quarter_ = lubridate::quarter(period),
      year_ = lubridate::year(period)
    )
  
  # If specified, calculate local outlier factor per group
  if (per_group) {
    
    # Create table with data for each grouping
    grouping_data <- data %>% 
      dplyr::select(group = grouping) %>% 
      dplyr::distinct(.keep_all = T) %>% 
      dplyr::group_by(group) %>% 
      dplyr::mutate(
        data = data %>%
          dplyr::filter(grouping == group) %>%
          list()
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::rename(grouping = group)
    
    # Calculate local outlier factor for each group of data
    data_x <- grouping_data %>% 
      dplyr::select(data) %>% 
      dplyr::mutate(
        score_ = purrr::pmap(
          .l = list(
            "data" = data
          ),
          .f = one_group_lof,
          s = s
        )
      ) %>% 
      dplyr::select(score_)
    
    # Add grouping for each scores and unnest
    data_x_t <- grouping_data %>% 
      dplyr::select(grouping) %>% 
      dplyr::bind_cols(data_x) %>% 
      tidyr::unnest(score_) 
    
    # Add the column score_ in the table data
    data$score_ <-  data_x_t$score_
    
  } else {
    
    # Treat all data as one group
    data$score_ <- one_group_lof(data, s)
    
  }

  # Add required anomaly information and return
  data <- data %>% 
    # Use col_of_interest as dummy for bounds
    dplyr::mutate(
      anomaly_lower_bound = col_of_interest,
      anomaly_upper_bound = col_of_interest
    ) %>% 
    # Add anomaly flag and algorithm name
    dplyr::mutate(
      anomaly_flag = as.integer(score_ > threshold),
      anomaly_det_alg = "loc_out_fact"
    ) %>% 
    # Remove all temporary columns and return
    dplyr::select(-ends_with('_')) %>% 
    return()
}