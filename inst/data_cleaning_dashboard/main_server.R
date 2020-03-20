
#### DATA UPDATES ####

# Update all available inputs
MAIN_update_all_inputs <- reactive({
  reactiveValuesToList(input)
})

# Initialize reactive dataset for anomaly data
MAIN_anomaly_data <- reactiveValues(
  data = app_data$data %>% 
    dplyr::mutate(
      original_col_of_interest = col_of_interest,
      changepoint_delta = 0,
      is_anomaly = 0
    )
)

# Initialize reactive dataset for anomaly changepoint data
MAIN_anomaly_changepoint_data <- reactiveValues(
  data = app_data$data %>% 
    dplyr::transmute(
      from = period,
      to = period,
      grouping = grouping,
      delta = 0
    ) %>% 
    dplyr::filter(FALSE)
)

# Initialize reactive data for anomaly detection settings
MAIN_anomaly_detection_settings <- reactiveValues(
  parameters = list(
    algorithms = c(
      "moving_standard_score",
      "percent_difference",
      "ts_outlier",
      "loc_out_fact"
    ),
    m_width = app_data$default_m_width,
    sd_cutoff = app_data$default_sd_cutoff,
    max_pct_diff = app_data$default_max_pct_diff,
    pct_m_width = app_data$default_pct_m_width,
    lof_threshold = app_data$default_lof_threshold,
    lof_s = app_data$default_lof_s,
    lof_m_width = app_data$default_lof_m_width
  )
)

# Initialize reactive dataset for comments data
MAIN_comment_data <- reactiveValues(
  data = tibble::tibble(
    grouping = app_data$groupings$grouping,
    comment = "Insert comment here..."
  )
)


#### DASHBOARD FUNCTIONS ####

# Wrapper function to detect anomalies
detect_anomalies_within_dashboard <- function(data, algorithms, parameters) {
  # Set progress depending on number of selected algorithms
  progress <- 1/length(algorithms)
  # Create empty results data
  anomaly_detection_data <- tibble::tibble()
  # Add the required anomaly detection algorithms
  if ("moving_standard_score" %in% algorithms) {
    incProgress(progress, detail = "Moving standard scores")
    anomaly_detection_data <- data %>% 
      detect_anomalies_using_moving_standard_scores(
        m_width = parameters$m_width,
        sd_cutoff = parameters$sd_cutoff
      ) %>% 
      dplyr::bind_rows(anomaly_detection_data, .)
  }
  if ("percent_difference" %in% algorithms) {
    incProgress(progress, detail = "Percent differences")
    anomaly_detection_data <- data %>% 
      detect_anomalies_using_percent_differences(
        max_pct_diff = parameters$max_pct_diff,
        m_width = parameters$pct_m_width
      ) %>% 
      dplyr::bind_rows(anomaly_detection_data, .)
  }
  if ("ts_outlier" %in% algorithms) {
    incProgress(progress, detail = "TS Outlier")
    anomaly_detection_data <- data %>% 
      detect_anomalies_using_ts_outlier() %>% 
      dplyr::bind_rows(anomaly_detection_data, .)
  }
  if ("loc_out_fact" %in% algorithms) {
    incProgress(progress, detail = "Local outlier factor")
    anomaly_detection_data <- data %>% 
      detect_anomalies_using_local_outlier_factor(
        threshold = parameters$lof_threshold,
        s = parameters$lof_s,
        m_width = parameters$lof_m_width
      ) %>% 
      dplyr::bind_rows(anomaly_detection_data, .)
  }
  # Return results
  return(anomaly_detection_data)
}

# Wrapper function to add totals for (non-)hierarchy groups
add_up_required_group_totals <- function(data) {
  # Add hierarchical if required
  if (app_data$is_hierarchical) {
    data <- data %>% 
      add_up_hierarchical_values(
        hierarchy = app_data$hierarchy
      )
  }
  # Add non-hierarchy totals and return
  data %>% 
    add_total_for_non_hierarchy_groups(
      hierarchical_cols = app_data$hierarchical_cols
    ) %>% 
    return()
}
