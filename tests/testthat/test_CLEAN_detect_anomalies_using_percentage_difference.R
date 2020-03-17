
context("detect_anomalies_using_percentage_difference")

test_that("check detect_anomalies_using_percentage_difference output with default specifications", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    )
  function_output <- detect_anomalies_using_percent_differences(function_input)
  expect_is(function_output, "data.frame")
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 9)
  expected_columns <- c(
    "period", "col_of_interest", "grouping", "spotprice", "gemprice", 
    "anomaly_det_alg", "anomaly_flag", "anomaly_lower_bound", 
    "anomaly_upper_bound"
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
    "state = Indiana   &   oil_company = CompanyA",
    "state = Indiana   &   oil_company = CompanyB",
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB"
  ))
  expect_equal(round(min(function_output$spotprice), 2), 9.64)
  expect_equal(round(max(function_output$spotprice), 2), 86.61)
  expect_equal(round(min(function_output$gemprice), 2), 19.06)
  expect_equal(round(max(function_output$gemprice), 2), 81.52)
  expect_equal(round(min(function_output$anomaly_lower_bound), 2), 0.56)
  expect_equal(round(max(function_output$anomaly_lower_bound), 2), 2.25)
  expect_equal(round(min(function_output$anomaly_upper_bound), 2), 1.04)
  expect_equal(round(max(function_output$anomaly_upper_bound), 2), 4.17)
  expect_equal(min(function_output$anomaly_flag), 0)
  expect_equal(max(function_output$anomaly_flag), 1)
  expect_equal(sum(function_output$anomaly_flag), 39)
})

test_that("check detect_anomalies_using_percent_differences output with varying specifications", {
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice, 
    date_col = "year_month", 
    col_of_interest = "gasprice", 
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice", "gemprice")
  )
  function_output <- detect_anomalies_using_percent_differences(
    data = function_input,
    max_pct_diff = 0.1
  )
  expect_is(function_output, "data.frame")
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 9)
  expected_columns <- c(
    "period", "col_of_interest", "grouping", "spotprice", "gemprice", 
    "anomaly_det_alg", "anomaly_flag","anomaly_lower_bound", "anomaly_upper_bound"
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
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB",
    "state = New York   &   oil_company = CompanyA",
    "state = New York   &   oil_company = CompanyB"
  ))
  expect_equal(round(min(function_output$spotprice), 2), 9.64)
  expect_equal(round(max(function_output$spotprice), 2), 86.61)
  expect_equal(round(min(function_output$gemprice), 2), 19.06)
  expect_equal(round(max(function_output$gemprice), 2), 81.52)
  expect_equal(round(min(function_output$anomaly_lower_bound), 2), 0.72)
  expect_equal(round(max(function_output$anomaly_lower_bound), 2), 2.89)
  expect_equal(round(min(function_output$anomaly_upper_bound), 2), 0.88)
  expect_equal(round(max(function_output$anomaly_upper_bound), 2), 3.53)
  expect_equal(min(function_output$anomaly_flag), 0)
  expect_equal(max(function_output$anomaly_flag), 1)
  expect_equal(sum(function_output$anomaly_flag), 400)
})

test_that("check detect_anomalies_using_percent_differences output with invalid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    )
  expect_error(
    detect_anomalies_using_percent_differences(
      data = "potato"
    )
  )
  expect_error(
    detect_anomalies_using_percent_differences(
      data = dummy_gasprice
    )
  )
  expect_error(
    detect_anomalies_using_percent_differences(
      data = function_input %>% 
        dplyr::select(-grouping)
    )
  )
  expect_error(
    detect_anomalies_using_percent_differences(
      data = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    detect_anomalies_using_percent_differences(
      data = function_input,
      max_pct_diff = -1
    )
  )
})
