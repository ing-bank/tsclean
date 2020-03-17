
context("detect_anomalies_using_moving_standard_scores")

test_that("check detect_anomalies_using_moving_standard_scores output with default specifications", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    )
  function_output <- detect_anomalies_using_moving_standard_scores(function_input)
  expect_is(function_output, "data.frame")
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 9)
  expected_columns <- c(
    "period", "col_of_interest", "grouping", "spotprice", "gemprice", 
    "anomaly_det_alg", "anomaly_lower_bound", "anomaly_upper_bound", 
    "anomaly_flag"
  )
  expect_named(function_output, expected_columns)
  expect_is(function_output$period, "Date")
  expect_is(function_output$grouping, "character")
  expect_is(function_output$anomaly_det_alg, "character")
  expect_is(function_output$anomaly_flag, "integer")
  for (col in expected_columns[!expected_columns %in% c("period", "grouping", "anomaly_det_alg", "anomaly_flag")]) {
    expect_is(function_output[[col]], "numeric")
  }
  expect_equal(length(unique(function_output$period)), 191)
  expect_equal(min(function_output$period), as.Date("1991-01-31"))
  expect_equal(max(function_output$period), as.Date("2006-11-30"))
  expect_equal(round(min(function_output$col_of_interest), 2), 0.78)
  expect_equal(round(max(function_output$col_of_interest), 2), 3.46)
  expect_equal(unique(function_output$grouping), c(
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB", 
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB"
  ))
  expect_equal(round(min(function_output$spotprice), 2), 9.64)
  expect_equal(round(max(function_output$spotprice), 2), 86.61)
  expect_equal(round(min(function_output$gemprice), 2), 19.06)
  expect_equal(round(max(function_output$gemprice), 2), 81.52)
  expect_equal(unique(function_output$anomaly_det_alg), "moving_standard_score")
  expect_equal(round(min(function_output$anomaly_lower_bound), 2), 0.54)
  expect_equal(round(max(function_output$anomaly_lower_bound), 2), 1.86)
  expect_equal(round(min(function_output$anomaly_upper_bound), 2), 1.14)
  expect_equal(round(max(function_output$anomaly_upper_bound), 2), 3.61)
  expect_equal(min(function_output$anomaly_flag), 0)
  expect_equal(max(function_output$anomaly_flag), 1)
  expect_equal(sum(function_output$anomaly_flag), 9)
})

test_that("check detect_anomalies_using_moving_standard_scores output with varying specifications", {
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice, 
    date_col = "year_month", 
    col_of_interest = "gasprice", 
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice", "gemprice")
  )
  function_output <- detect_anomalies_using_moving_standard_scores(
    data = function_input,
    m_width = 6,
    sd_cutoff = 1.5
  )
  expect_is(function_output, "data.frame")
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 9)
  expected_columns <- c(
    "period", "col_of_interest", "grouping", "spotprice", "gemprice", 
    "anomaly_det_alg", "anomaly_lower_bound", "anomaly_upper_bound", 
    "anomaly_flag"
  )
  expect_named(function_output, expected_columns)
  expect_is(function_output$period, "Date")
  expect_is(function_output$grouping, "character")
  expect_is(function_output$anomaly_det_alg, "character")
  expect_is(function_output$anomaly_flag, "integer")
  for (col in expected_columns[!expected_columns %in% c("period", "grouping", "anomaly_det_alg", "anomaly_flag")]) {
    expect_is(function_output[[col]], "numeric")
  }
  expect_equal(length(unique(function_output$period)), 191)
  expect_equal(min(function_output$period), as.Date("1991-01-31"))
  expect_equal(max(function_output$period), as.Date("2006-11-30"))
  expect_equal(round(min(function_output$col_of_interest), 2), 0.78)
  expect_equal(round(max(function_output$col_of_interest), 2), 3.46)
  expect_equal(unique(function_output$grouping), c(
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB", 
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB"
  ))
  expect_equal(round(min(function_output$spotprice), 2), 9.64)
  expect_equal(round(max(function_output$spotprice), 2), 86.61)
  expect_equal(round(min(function_output$gemprice), 2), 19.06)
  expect_equal(round(max(function_output$gemprice), 2), 81.52)
  expect_equal(unique(function_output$anomaly_det_alg), "moving_standard_score")
  expect_equal(round(min(function_output$anomaly_lower_bound), 2), 0.70)
  expect_equal(round(max(function_output$anomaly_lower_bound), 2), 2.39)
  expect_equal(round(min(function_output$anomaly_upper_bound), 2), 0.94)
  expect_equal(round(max(function_output$anomaly_upper_bound), 2), 3.70)
  expect_equal(min(function_output$anomaly_flag), 0)
  expect_equal(max(function_output$anomaly_flag), 1)
  expect_equal(sum(function_output$anomaly_flag), 47)
})

test_that("check detect_anomalies_using_moving_standard_scores output with invalid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    )
  expect_error(
    detect_anomalies_using_moving_standard_scores(
      data = "potato"
    )
  )
  expect_error(
    detect_anomalies_using_moving_standard_scores(
      data = dummy_gasprice
    )
  )
  expect_error(
    detect_anomalies_using_moving_standard_scores(
      data = function_input %>% 
        dplyr::select(-grouping)
    )
  )
  expect_error(
    detect_anomalies_using_moving_standard_scores(
      data = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    detect_anomalies_using_moving_standard_scores(
      data = function_input,
      m_width = 1.5
    )
  )
  expect_error(
    detect_anomalies_using_moving_standard_scores(
      data = function_input,
      m_width = -3
    )
  )
  expect_error(
    detect_anomalies_using_moving_standard_scores(
      data = function_input,
      m_width = 12,
      sd_cutoff = -3
    )
  )
})
