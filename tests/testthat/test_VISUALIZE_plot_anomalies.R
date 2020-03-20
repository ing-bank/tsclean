
context("plot_anomalies")

test_that("check plot_anomalies with valid input for single grouping", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    detect_anomalies_using_moving_standard_scores() %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  function_output <- plot_anomalies(function_input)
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(length(function_output$layers), 6)
  expect_equal(function_output$labels$x, "period")
  expect_equal(function_output$labels$y, "col_of_interest")
  expect_equal(function_output$labels$linetype, "linetype")
  expect_equal(function_output$labels$ymin, "anomaly_lower_bound")
  expect_equal(function_output$labels$ymax, "anomaly_upper_bound")
  expect_equal(function_output$labels$colour, "anomaly_det_alg")
  expect_equal(function_output$labels$fill, "anomaly_det_alg")
  expect_equal(function_output$labels$text, "paste0(\"<br>Period: \", format.Date(period, \"%Y-%m\"), \"<br>Value: \", ...")
  expect_equal(length(function_output$layers), 6)
  expect_equal(nrow(function_output$layers[[1]]$data), 191)
  expect_equal(ncol(function_output$layers[[1]]$data), 5)
  expect_named(function_output$layers[[1]]$data, c(
    "period", "grouping", "col_of_interest", "anomaly_flags", "detected_by"
  ))
  expect_equal(nrow(function_output$layers[[2]]$data), 191)
  expect_equal(ncol(function_output$layers[[2]]$data), 9)
  expect_named(function_output$layers[[2]]$data, c(
    "period", "col_of_interest", "grouping", "spotprice", "gemprice", 
    "anomaly_det_alg", "anomaly_lower_bound", "anomaly_upper_bound", "anomaly_flag"
  ))
  expect_equal(nrow(function_output$layers[[3]]$data), 2)
  expect_equal(ncol(function_output$layers[[3]]$data), 4)
  expect_named(function_output$layers[[3]]$data, c(
    "period", "col_of_interest", "anomaly_det_alg", "anomaly_flag"
  ))
  expect_equal(nrow(function_output$layers[[4]]$data), 191)
  expect_equal(ncol(function_output$layers[[4]]$data), 5)
  expect_named(function_output$layers[[4]]$data, c(
    "period", "grouping", "col_of_interest", "anomaly_flags", "detected_by"
  ))
})

test_that("check plot_anomalies with valid input for multiple groupings", {
  function_input <- dplyr::bind_rows(
    detect_anomalies_using_percent_differences(dummy_anomaly_data),
    detect_anomalies_using_moving_standard_scores(dummy_anomaly_data)
  )
  function_output <- plot_anomalies(function_input)
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(length(function_output$layers), 4)
  expect_equal(function_output$labels$x, "period")
  expect_equal(function_output$labels$y, "col_of_interest")
  expect_equal(function_output$labels$linetype, "linetype")
  expect_equal(function_output$labels$ymin, "anomaly_lower_bound")
  expect_equal(function_output$labels$ymax, "anomaly_upper_bound")
  expect_equal(function_output$labels$colour, "anomaly_det_alg")
  expect_equal(function_output$labels$fill, "anomaly_det_alg")
  expect_equal(function_output$labels$text, "paste0(\"<br>Period: \", format.Date(period, \"%Y-%m\"), \"<br>Value: \", ...")
  expect_equal(length(function_output$layers), 4)
  expect_equal(nrow(function_output$layers[[1]]$data), 1026)
  expect_equal(ncol(function_output$layers[[1]]$data), 5)
  expect_named(function_output$layers[[1]]$data, c(
    "period", "grouping", "col_of_interest", "anomaly_flags", "detected_by"
  ))
  expect_equal(nrow(function_output$layers[[2]]$data), 2052)
  expect_equal(ncol(function_output$layers[[2]]$data), 7)
  expect_named(function_output$layers[[2]]$data, c(
    "period", "col_of_interest", "grouping", "anomaly_det_alg", 
    "anomaly_flag", "anomaly_lower_bound", "anomaly_upper_bound" 
  ))
  expect_equal(nrow(function_output$layers[[3]]$data), 134)
  expect_equal(ncol(function_output$layers[[3]]$data), 5)
  expect_named(function_output$layers[[3]]$data, c(
    "period", "col_of_interest", "anomaly_det_alg", "anomaly_flag", "grouping"
  ))
  expect_equal(nrow(function_output$layers[[4]]$data), 1026)
  expect_equal(ncol(function_output$layers[[4]]$data), 5)
  expect_named(function_output$layers[[4]]$data, c(
    "period", "grouping", "col_of_interest", "anomaly_flags", "detected_by"
  ))
})

test_that("check plot_anomalies when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    detect_anomalies_using_percent_differences()
  expect_error(
    plot_anomalies(
      data = "potato"
    )
  )
  expect_error(
    plot_anomalies(
      data = dummy_gasprice
    )
  )
  expect_error(
    plot_anomalies(
      data = function_input %>% 
        dplyr::select(-col_of_interest)
    )
  )
  expect_error(
    plot_anomalies(
      data = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
})
