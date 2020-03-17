
context("detect_anomalies_using_local_outlier_factor")

test_that("check detect_anomalies_using_local_outlier_factor output with default specifications", {
  function_input <- dummy_anomaly_data
  function_output <- detect_anomalies_using_local_outlier_factor(function_input)
  
  expect_is(function_output, "data.frame")
  expect_equal(nrow(function_output), 1026)
  expect_equal(ncol(function_output), 7)
  expected_columns <- c(
    "period", "col_of_interest", "grouping",
    "anomaly_lower_bound", "anomaly_upper_bound", 
    "anomaly_flag", "anomaly_det_alg" 
  )
  expect_named(function_output, expected_columns)
  expect_is(function_output$period, "Date")
  expect_is(function_output$grouping, "character")
  expect_is(function_output$anomaly_det_alg, "character")
  expect_is(function_output$anomaly_flag, "integer")
  for (col in expected_columns[!expected_columns %in% c("period", "grouping", "anomaly_det_alg", "anomaly_flag")]) {
    expect_is(function_output[[col]], "numeric")
  }
  expect_equal(length(unique(function_output$period)), 54)
  expect_equal(min(function_output$period), as.Date("2014-01-31"))
  expect_equal(max(function_output$period), as.Date("2018-06-30"))
  expect_equal(round(min(function_output$col_of_interest), 2), -658285.11)
  expect_equal(round(max(function_output$col_of_interest), 2), 7308309.89)
  expect_equal(unique(function_output$grouping), c(
    "location = Imagination Land   &   product = Product Alfa", 
    "location = Imagination Land   &   product = Product Bravo", 
    "location = Imagination Land   &   product = Product Charlie", 
    "location = Imagination Land   &   product = Product Delta", 
    "location = Imagination Land   &   product = Product Echo", 
    "location = Imagination Land   &   product = Product Foxtrot", 
    "location = Imagination Land   &   product = Product Golf", 
    "location = Imagination Land   &   product = Product Hotel", 
    "location = Imagination Land   &   product = Product India", 
    "location = Imagination Land   &   product = Product Juliett", 
    "location = Imagination Land   &   product = Product Kilo", 
    "location = Imagination Land   &   product = Product Lima", 
    "location = Imagination Land   &   product = Product Mike", 
    "location = Imagination Land   &   product = Product November", 
    "location = Imagination Land   &   product = Product Oscar", 
    "location = Imagination Land   &   product = Product Papa", 
    "location = Imagination Land   &   product = Product Quebec", 
    "location = Imagination Land   &   product = Product Romeo", 
    "location = Imagination Land   &   product = Product Sierra"
  ))
  expect_equal(round(min(function_output$anomaly_lower_bound), 2), -658285.11) # Same as col_of_interest
  expect_equal(round(max(function_output$anomaly_lower_bound), 2), 7308309.89) # Same as col_of_interest
  expect_equal(round(min(function_output$anomaly_upper_bound), 2), -658285.11) # Same as col_of_interest
  expect_equal(round(max(function_output$anomaly_upper_bound), 2), 7308309.89) # Same as col_of_interest
  expect_equal(min(function_output$anomaly_flag), 0)
  expect_equal(max(function_output$anomaly_flag), 1)
  expect_equal(sum(function_output$anomaly_flag), 26)
  expect_equal(unique(function_output$anomaly_det_alg), "loc_out_fact")
})

test_that("check detect_anomalies_using_local_outlier_factor output with varying specifications", {
  function_input <- dummy_anomaly_data
  function_output <- detect_anomalies_using_local_outlier_factor(
    data = function_input,
    threshold = 2, 
    per_group = TRUE
  )
  expect_is(function_output, "data.frame")
  expect_equal(nrow(function_output), 1026)
  expect_equal(ncol(function_output), 7)
  expected_columns <- c(
    "period", "col_of_interest", "grouping",
    "anomaly_lower_bound", "anomaly_upper_bound", 
    "anomaly_flag", "anomaly_det_alg" 
  )
  expect_named(function_output, expected_columns)
  expect_is(function_output$period, "Date")
  expect_is(function_output$grouping, "character")
  expect_is(function_output$anomaly_det_alg, "character")
  expect_is(function_output$anomaly_flag, "integer")
  for (col in expected_columns[!expected_columns %in% c("period", "grouping", "anomaly_det_alg", "anomaly_flag")]) {
    expect_is(function_output[[col]], "numeric")
  }
  expect_equal(length(unique(function_output$period)), 54)
  expect_equal(min(function_output$period), as.Date("2014-01-31"))
  expect_equal(max(function_output$period), as.Date("2018-06-30"))
  expect_equal(round(min(function_output$col_of_interest), 2), -658285.11)
  expect_equal(round(max(function_output$col_of_interest), 2), 7308309.89)
  expect_equal(unique(function_output$grouping), c(
    "location = Imagination Land   &   product = Product Alfa", 
    "location = Imagination Land   &   product = Product Bravo", 
    "location = Imagination Land   &   product = Product Charlie", 
    "location = Imagination Land   &   product = Product Delta", 
    "location = Imagination Land   &   product = Product Echo", 
    "location = Imagination Land   &   product = Product Foxtrot", 
    "location = Imagination Land   &   product = Product Golf", 
    "location = Imagination Land   &   product = Product Hotel", 
    "location = Imagination Land   &   product = Product India", 
    "location = Imagination Land   &   product = Product Juliett", 
    "location = Imagination Land   &   product = Product Kilo", 
    "location = Imagination Land   &   product = Product Lima", 
    "location = Imagination Land   &   product = Product Mike", 
    "location = Imagination Land   &   product = Product November", 
    "location = Imagination Land   &   product = Product Oscar", 
    "location = Imagination Land   &   product = Product Papa", 
    "location = Imagination Land   &   product = Product Quebec", 
    "location = Imagination Land   &   product = Product Romeo", 
    "location = Imagination Land   &   product = Product Sierra"
  ))
  expect_equal(round(min(function_output$anomaly_lower_bound), 2), -658285.11) # Same as col_of_interest
  expect_equal(round(max(function_output$anomaly_lower_bound), 2), 7308309.89) # Same as col_of_interest
  expect_equal(round(min(function_output$anomaly_upper_bound), 2), -658285.11) # Same as col_of_interest
  expect_equal(round(max(function_output$anomaly_upper_bound), 2), 7308309.89) # Same as col_of_interest
  expect_equal(min(function_output$anomaly_flag), 0)
  expect_equal(max(function_output$anomaly_flag), 1)
  expect_equal(sum(function_output$anomaly_flag), 13)
  expect_equal(unique(function_output$anomaly_det_alg), "loc_out_fact")
  
  function_input <- dummy_anomaly_data
  function_output <- detect_anomalies_using_local_outlier_factor(
    data = function_input,
    s = 5, 
    per_group = FALSE
  )
  expect_is(function_output, "data.frame")
  expect_equal(nrow(function_output), 1026)
  expect_equal(ncol(function_output), 7)
  expected_columns <- c(
    "period", "col_of_interest", "grouping",
    "anomaly_lower_bound", "anomaly_upper_bound", 
    "anomaly_flag", "anomaly_det_alg" 
  )
  expect_named(function_output, expected_columns)
  expect_is(function_output$period, "Date")
  expect_is(function_output$grouping, "character")
  expect_is(function_output$anomaly_det_alg, "character")
  expect_is(function_output$anomaly_flag, "integer")
  for (col in expected_columns[!expected_columns %in% c("period", "grouping", "anomaly_det_alg", "anomaly_flag")]) {
    expect_is(function_output[[col]], "numeric")
  }
  expect_equal(length(unique(function_output$period)), 54)
  expect_equal(min(function_output$period), as.Date("2014-01-31"))
  expect_equal(max(function_output$period), as.Date("2018-06-30"))
  expect_equal(round(min(function_output$col_of_interest), 2), -658285.11)
  expect_equal(round(max(function_output$col_of_interest), 2), 7308309.89)
  expect_equal(unique(function_output$grouping), c(
    "location = Imagination Land   &   product = Product Alfa", 
    "location = Imagination Land   &   product = Product Bravo", 
    "location = Imagination Land   &   product = Product Charlie", 
    "location = Imagination Land   &   product = Product Delta", 
    "location = Imagination Land   &   product = Product Echo", 
    "location = Imagination Land   &   product = Product Foxtrot", 
    "location = Imagination Land   &   product = Product Golf", 
    "location = Imagination Land   &   product = Product Hotel", 
    "location = Imagination Land   &   product = Product India", 
    "location = Imagination Land   &   product = Product Juliett", 
    "location = Imagination Land   &   product = Product Kilo", 
    "location = Imagination Land   &   product = Product Lima", 
    "location = Imagination Land   &   product = Product Mike", 
    "location = Imagination Land   &   product = Product November", 
    "location = Imagination Land   &   product = Product Oscar", 
    "location = Imagination Land   &   product = Product Papa", 
    "location = Imagination Land   &   product = Product Quebec", 
    "location = Imagination Land   &   product = Product Romeo", 
    "location = Imagination Land   &   product = Product Sierra"
  ))
  expect_equal(round(min(function_output$anomaly_lower_bound), 2), -658285.11) # Same as col_of_interest
  expect_equal(round(max(function_output$anomaly_lower_bound), 2), 7308309.89) # Same as col_of_interest
  expect_equal(round(min(function_output$anomaly_upper_bound), 2), -658285.11) # Same as col_of_interest
  expect_equal(round(max(function_output$anomaly_upper_bound), 2), 7308309.89) # Same as col_of_interest
  expect_equal(min(function_output$anomaly_flag), 0)
  expect_equal(max(function_output$anomaly_flag), 1)
  expect_equal(sum(function_output$anomaly_flag), 30)
  expect_equal(unique(function_output$anomaly_det_alg), "loc_out_fact")
  
  function_input <- dummy_anomaly_data
  function_output <- detect_anomalies_using_local_outlier_factor(
    data = function_input,
    m_width = 7, 
    per_group = TRUE
  )
  expect_is(function_output, "data.frame")
  expect_equal(nrow(function_output), 1026)
  expect_equal(ncol(function_output), 7)
  expected_columns <- c(
    "period", "col_of_interest", "grouping",
    "anomaly_lower_bound", "anomaly_upper_bound", 
    "anomaly_flag", "anomaly_det_alg" 
  )
  expect_named(function_output, expected_columns)
  expect_is(function_output$period, "Date")
  expect_is(function_output$grouping, "character")
  expect_is(function_output$anomaly_det_alg, "character")
  expect_is(function_output$anomaly_flag, "integer")
  for (col in expected_columns[!expected_columns %in% c("period", "grouping", "anomaly_det_alg", "anomaly_flag")]) {
    expect_is(function_output[[col]], "numeric")
  }
  expect_equal(length(unique(function_output$period)), 54)
  expect_equal(min(function_output$period), as.Date("2014-01-31"))
  expect_equal(max(function_output$period), as.Date("2018-06-30"))
  expect_equal(round(min(function_output$col_of_interest), 2), -658285.11)
  expect_equal(round(max(function_output$col_of_interest), 2), 7308309.89)
  expect_equal(unique(function_output$grouping), c(
    "location = Imagination Land   &   product = Product Alfa", 
    "location = Imagination Land   &   product = Product Bravo", 
    "location = Imagination Land   &   product = Product Charlie", 
    "location = Imagination Land   &   product = Product Delta", 
    "location = Imagination Land   &   product = Product Echo", 
    "location = Imagination Land   &   product = Product Foxtrot", 
    "location = Imagination Land   &   product = Product Golf", 
    "location = Imagination Land   &   product = Product Hotel", 
    "location = Imagination Land   &   product = Product India", 
    "location = Imagination Land   &   product = Product Juliett", 
    "location = Imagination Land   &   product = Product Kilo", 
    "location = Imagination Land   &   product = Product Lima", 
    "location = Imagination Land   &   product = Product Mike", 
    "location = Imagination Land   &   product = Product November", 
    "location = Imagination Land   &   product = Product Oscar", 
    "location = Imagination Land   &   product = Product Papa", 
    "location = Imagination Land   &   product = Product Quebec", 
    "location = Imagination Land   &   product = Product Romeo", 
    "location = Imagination Land   &   product = Product Sierra"
  ))
  expect_equal(round(min(function_output$anomaly_lower_bound), 2), -658285.11) # Same as col_of_interest
  expect_equal(round(max(function_output$anomaly_lower_bound), 2), 7308309.89) # Same as col_of_interest
  expect_equal(round(min(function_output$anomaly_upper_bound), 2), -658285.11) # Same as col_of_interest
  expect_equal(round(max(function_output$anomaly_upper_bound), 2), 7308309.89) # Same as col_of_interest
  expect_equal(min(function_output$anomaly_flag), 0)
  expect_equal(max(function_output$anomaly_flag), 1)
  expect_equal(sum(function_output$anomaly_flag), 1)
  expect_equal(unique(function_output$anomaly_det_alg), "loc_out_fact")
})

test_that("check detect_anomalies_using_local_outlier_factor output with invalid inputs", {
  function_input <- dummy_anomaly_data
  expect_error(
    detect_anomalies_using_local_outlier_factor(
      data = "potato"
    )
  )
  expect_error(
    detect_anomalies_using_local_outlier_factor(
      data = dummy_gasprice
    )
  )
  expect_error(
    detect_anomalies_using_local_outlier_factor(
      data = function_input %>% 
        dplyr::select(-grouping)
    )
  )
  expect_error(
    detect_anomalies_using_local_outlier_factor(
      data = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    detect_anomalies_uing_local_outlier_factor(
      data = function_input,
      threshold = 0.4
    )
  )
  expect_error(
    detect_anomalies_using_local_outlier_factor(
      data = function_input,
      s = 0.4
    )
  )
  expect_error(
    detect_anomalies_using_local_outlier_factor(
      data = function_input,
      m_width = 0.4
    )
  )
})
