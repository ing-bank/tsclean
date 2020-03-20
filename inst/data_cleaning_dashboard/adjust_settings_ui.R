fluidPage(
  includeScript(path = "source.js"),
  shinydashboard::box(
    title = "Select Algorithm",
    width = 12,
    collapsible = TRUE,
    checkboxGroupInput(
      inputId = "ADJ_SET_algo",
      label = "Choose an algorithm:",
      choices = c(
        "Rolling Standard Deviation: Uses the rolling mean and standard deviation to find outliers" = "moving_standard_score", 
        "Percentage Difference: Uses the percentage difference to find outliers" = "percent_difference",
        "Using Friedman's SuperSmoother to find outliers" = "ts_outlier",
        "Local Outlier Factor: Uses the machine learning mathod Local Outlier Factor to find outliers.
        The features are the absolute percentage difference from the previous moving average, 
        the month, quarter and year." = "loc_out_fact"
      ),
      selected = c(
        "moving_standard_score",
        "percent_difference",
        "iso_for",
        "ts_outlier",
        "loc_out_fact"
      )
    )
  ),
  conditionalPanel(
    condition = "input.ADJ_SET_algo.includes('moving_standard_score')",
    shinydashboard::box(
      title = "Rolling Standard Deviation - Outlier detection",
      collapsible = TRUE,
      sliderInput(
        inputId = "ADJ_SET_m_width",
        label = "The number of data point to use when calculating the moving average and moving standard deviation",
        min = 2,
        max = 60,
        value = app_data$default_m_width,
        step = 1
      ),
      sliderInput(
        inputId = "ADJ_SET_sd_cutoff",
        label = "The number of standard deviations to be used as cut-off point for flagging potential outliers",
        min = 0,
        max = 5,
        value = app_data$default_sd_cutoff,
        step = 0.1
      )
    )
  ),
  conditionalPanel(
    condition = "input.ADJ_SET_algo.includes('percent_difference')",
    shinydashboard::box(
      title = "Percentage Difference - Outlier detection",
      collapsible = TRUE,
      sliderInput(
        inputId = "ADJ_SET_max_pct_diff",
        label = "The maximum percentage diff between points",
        min = 0.1,
        max = 0.9,
        value = app_data$default_max_pct_diff,
        step = 0.01
      ),
      sliderInput(
        inputId = "ADJ_SET_pct_m_width",
        label = "The number of data point to use when calculating the moving average",
        min = 2,
        max = 10,
        value = app_data$default_pct_m_width,
        step = 1
      )
    )
  ),
  conditionalPanel(
    condition = "input.ADJ_SET_algo.includes('loc_out_fact')",
    shinydashboard::box(
      title = "Local Outlier Factor - Outlier detection",
      collapsible = TRUE,
      sliderInput(
        inputId = "ADJ_SET_lof_threshold",
        label = "The threshold for the anomaly score",
        min = 1.01,
        max = 20.01,
        value = app_data$default_lof_threshold,
        step = 0.01
      ),
      sliderInput(
        inputId = "ADJ_SET_lof_s",
        label = "The size of the neighborhood of data points to be considered",
        min = 2,
        max = 10,
        value = app_data$default_lof_s,
        step = 1
      ),
      sliderInput(
        inputId = "ADJ_SET_lof_m_width",
        label = "The number of data point to use when calculating the moving average",
        min = 2,
        max = 10,
        value = app_data$default_lof_m_width,
        step = 1
      )
    )
  )
)
