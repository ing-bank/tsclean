
context("add_total_for_non_hierarchy_groups")

test_that("check add_total_for_non_hierarchy_groups with valid inputs without hierarchy", {
  # With original_col_of_interest available
  function_input <- dummy_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::mutate(original_col_of_interest = col_of_interest)
  function_output <- add_total_for_non_hierarchy_groups(
    data = function_input
  )
  expect_equal(colnames(function_output), colnames(function_input))
  expect_is(function_output$period, "Date")
  expect_is(function_output$col_of_interest, "numeric")
  expect_is(function_output$grouping, "character")
  expect_is(function_output$original_col_of_interest, "numeric")
  expect_equal(unique(function_output$period), unique(function_input$period))
  expect_gt(mean(function_output$col_of_interest), mean(function_input$col_of_interest))
  expect_equal(round(mean(function_output$col_of_interest), 3), 2.478)
  expect_equal(round(min(function_output$col_of_interest), 3), 0.775)
  expect_equal(round(max(function_output$col_of_interest), 3), 12.886)
  expect_equal(unique(function_output$grouping), c(
    "state = ALL   &   oil_company = ALL", 
    "state = New York   &   oil_company = ALL", 
    "state = Indiana   &   oil_company = ALL", 
    "state = ALL   &   oil_company = CompanyA", 
    "state = ALL   &   oil_company = CompanyB", 
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB", 
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB"
  ))
  expect_equal(round(mean(function_output$original_col_of_interest), 3), 2.478)
  expect_equal(round(min(function_output$original_col_of_interest), 3), 0.775)
  expect_equal(round(max(function_output$original_col_of_interest), 3), 12.886)
  repeated_function_output <- add_total_for_non_hierarchy_groups(
    data = function_output
  )
  expect_equal(repeated_function_output, function_output)
  # With changepoint_delta and is_anomaly available
  function_input <- dummy_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::mutate(
      changepoint_delta = 100,
      is_anomaly = 1
    )
  function_output <- add_total_for_non_hierarchy_groups(
    data = function_input
  )
  expect_equal(colnames(function_output), colnames(function_input))
  expect_is(function_output$period, "Date")
  expect_is(function_output$col_of_interest, "numeric")
  expect_is(function_output$grouping, "character")
  expect_is(function_output$spotprice, "numeric")
  expect_is(function_output$gemprice, "numeric")
  expect_is(function_output$changepoint_delta, "numeric")
  expect_is(function_output$is_anomaly, "numeric")
  expect_equal(unique(function_output$period), unique(function_input$period))
  expect_gt(mean(function_output$col_of_interest), mean(function_input$col_of_interest))
  expect_equal(round(mean(function_output$col_of_interest), 3), 2.478)
  expect_equal(round(min(function_output$col_of_interest), 3), 0.775)
  expect_equal(round(max(function_output$col_of_interest), 3), 12.886)
  expect_equal(unique(function_output$grouping), c(
    "state = ALL   &   oil_company = ALL", 
    "state = New York   &   oil_company = ALL", 
    "state = Indiana   &   oil_company = ALL", 
    "state = ALL   &   oil_company = CompanyA", 
    "state = ALL   &   oil_company = CompanyB", 
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB", 
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB"
  ))
  expect_equal(round(mean(function_output$spotprice), 3), NA_real_)
  expect_equal(round(min(function_output$spotprice), 3), NA_real_)
  expect_equal(round(max(function_output$spotprice), 3), NA_real_)
  expect_equal(round(mean(function_output$spotprice, na.rm = T), 3), 27.892)
  expect_equal(round(min(function_output$spotprice, na.rm = T), 3), 9.638)
  expect_equal(round(max(function_output$spotprice, na.rm = T), 3), 86.610)
  expect_equal(round(mean(function_output$gemprice), 3), NA_real_)
  expect_equal(round(min(function_output$gemprice), 3), NA_real_)
  expect_equal(round(max(function_output$gemprice), 3), NA_real_)
  expect_equal(round(mean(function_output$gemprice, na.rm = T), 3), 45.230)
  expect_equal(round(min(function_output$gemprice, na.rm = T), 3), 19.055)
  expect_equal(round(max(function_output$gemprice, na.rm = T), 3), 81.520)
  expect_equal(round(mean(function_output$changepoint_delta), 3), 177.778)
  expect_equal(round(min(function_output$changepoint_delta), 3), 100)
  expect_equal(round(max(function_output$changepoint_delta), 3), 400)
  expect_equal(mean(function_output$is_anomaly), 1)
  expect_equal(min(function_output$is_anomaly), 1)
  expect_equal(max(function_output$is_anomaly), 1)
  repeated_function_output <- add_total_for_non_hierarchy_groups(
    data = function_output
  )
  expect_equal(repeated_function_output, function_output)
  # With complex grouping across four variables
  function_input <- dplyr::bind_rows(
      dummy_gasprice %>% 
        dplyr::mutate(fake = "news") %>% 
        dplyr::mutate(pig = "no"),
      dummy_gasprice %>% 
        dplyr::mutate(fake = "people") %>% 
        dplyr::mutate(pig = "no")
    ) %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company", "fake", "pig")
    )
  function_output <- add_total_for_non_hierarchy_groups(
    data = function_input
  )
  expect_equal(colnames(function_output), colnames(function_input))
  expect_is(function_output$period, "Date")
  expect_is(function_output$col_of_interest, "numeric")
  expect_is(function_output$grouping, "character")
  expect_equal(unique(function_output$period), unique(function_input$period))
  expect_gt(mean(function_output$col_of_interest), mean(function_input$col_of_interest))
  expect_equal(round(mean(function_output$col_of_interest), 3), 3.305)
  expect_equal(round(min(function_output$col_of_interest), 3), 0.775)
  expect_equal(round(max(function_output$col_of_interest), 3), 25.772)
  expect_equal(unique(function_output$grouping), c(
    "state = ALL   &   oil_company = ALL   &   fake = ALL   &   pig = no", 
    "state = New York   &   oil_company = ALL   &   fake = ALL   &   pig = no", 
    "state = Indiana   &   oil_company = ALL   &   fake = ALL   &   pig = no", 
    "state = ALL   &   oil_company = CompanyA   &   fake = ALL   &   pig = no", 
    "state = New York   &   oil_company = CompanyA   &   fake = ALL   &   pig = no", 
    "state = Indiana   &   oil_company = CompanyA   &   fake = ALL   &   pig = no", 
    "state = ALL   &   oil_company = CompanyB   &   fake = ALL   &   pig = no", 
    "state = New York   &   oil_company = CompanyB   &   fake = ALL   &   pig = no", 
    "state = Indiana   &   oil_company = CompanyB   &   fake = ALL   &   pig = no", 
    "state = ALL   &   oil_company = ALL   &   fake = news   &   pig = no", 
    "state = New York   &   oil_company = ALL   &   fake = news   &   pig = no", 
    "state = Indiana   &   oil_company = ALL   &   fake = news   &   pig = no", 
    "state = ALL   &   oil_company = CompanyA   &   fake = news   &   pig = no", 
    "state = ALL   &   oil_company = CompanyB   &   fake = news   &   pig = no", 
    "state = ALL   &   oil_company = ALL   &   fake = people   &   pig = no", 
    "state = New York   &   oil_company = ALL   &   fake = people   &   pig = no", 
    "state = Indiana   &   oil_company = ALL   &   fake = people   &   pig = no", 
    "state = ALL   &   oil_company = CompanyA   &   fake = people   &   pig = no", 
    "state = ALL   &   oil_company = CompanyB   &   fake = people   &   pig = no", 
    "state = New York   &   oil_company = CompanyA   &   fake = news   &   pig = no", 
    "state = New York   &   oil_company = CompanyB   &   fake = news   &   pig = no", 
    "state = Indiana   &   oil_company = CompanyA   &   fake = news   &   pig = no", 
    "state = Indiana   &   oil_company = CompanyB   &   fake = news   &   pig = no", 
    "state = New York   &   oil_company = CompanyA   &   fake = people   &   pig = no", 
    "state = New York   &   oil_company = CompanyB   &   fake = people   &   pig = no", 
    "state = Indiana   &   oil_company = CompanyA   &   fake = people   &   pig = no", 
    "state = Indiana   &   oil_company = CompanyB   &   fake = people   &   pig = no"
  ))
  repeated_function_output <- add_total_for_non_hierarchy_groups(
    data = function_output
  )
  expect_equal(repeated_function_output, function_output)
})

test_that("check add_total_for_non_hierarchy_groups with valid inputs with hierarchy", {
  function_input <- dummy_hierarchical_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_cols = c("location", "oil_company")
    ) %>% 
    dplyr::mutate(
      original_col_of_interest = col_of_interest,
      changepoint_delta = 100,
      is_anomaly = 1
    )
  function_output <- add_total_for_non_hierarchy_groups(
    data = function_input,
    hierarchical_cols = "location"
  )
  expect_equal(colnames(function_output), colnames(function_input))
  expect_is(function_output$period, "Date")
  expect_is(function_output$col_of_interest, "numeric")
  expect_is(function_output$grouping, "character")
  expect_is(function_output$level_location, "numeric")
  expect_is(function_output$level_oil_company, "numeric")
  expect_is(function_output$original_col_of_interest, "numeric")
  expect_is(function_output$changepoint_delta, "numeric")
  expect_is(function_output$is_anomaly, "numeric")
  expect_equal(unique(function_output$period), unique(function_input$period))
  expect_gt(mean(function_output$col_of_interest), mean(function_input$col_of_interest))
  expect_equal(round(mean(function_output$col_of_interest), 3), 7.846)
  expect_equal(round(min(function_output$col_of_interest), 3), 0.812)
  expect_equal(round(max(function_output$col_of_interest), 3), 142.527)
  expect_equal(unique(function_output$grouping), c(
    "location = USA   &   oil_company = ALL   &   currency = ALL", 
    "location = New York   &   oil_company = ALL   &   currency = ALL", 
    "location = North New York   &   oil_company = ALL   &   currency = ALL", 
    "location = South New York   &   oil_company = ALL   &   currency = ALL", 
    "location = Bronx   &   oil_company = ALL   &   currency = ALL", 
    "location = Queens   &   oil_company = ALL   &   currency = ALL", 
    "location = Indiana   &   oil_company = ALL   &   currency = ALL", 
    "location = North Indiana   &   oil_company = ALL   &   currency = ALL", 
    "location = South Indiana   &   oil_company = ALL   &   currency = ALL", 
    "location = USA   &   oil_company = CompanyC   &   currency = ALL", 
    "location = New York   &   oil_company = CompanyC   &   currency = ALL", 
    "location = North New York   &   oil_company = CompanyC   &   currency = ALL", 
    "location = South New York   &   oil_company = CompanyC   &   currency = ALL", 
    "location = Bronx   &   oil_company = CompanyC   &   currency = ALL", 
    "location = Queens   &   oil_company = CompanyC   &   currency = ALL", 
    "location = Indiana   &   oil_company = CompanyC   &   currency = ALL", 
    "location = North Indiana   &   oil_company = CompanyC   &   currency = ALL", 
    "location = South Indiana   &   oil_company = CompanyC   &   currency = ALL", 
    "location = USA   &   oil_company = CompanyA   &   currency = ALL", 
    "location = New York   &   oil_company = CompanyA   &   currency = ALL", 
    "location = North New York   &   oil_company = CompanyA   &   currency = ALL", 
    "location = South New York   &   oil_company = CompanyA   &   currency = ALL", 
    "location = Bronx   &   oil_company = CompanyA   &   currency = ALL", 
    "location = Queens   &   oil_company = CompanyA   &   currency = ALL", 
    "location = Indiana   &   oil_company = CompanyA   &   currency = ALL", 
    "location = North Indiana   &   oil_company = CompanyA   &   currency = ALL", 
    "location = South Indiana   &   oil_company = CompanyA   &   currency = ALL", 
    "location = USA   &   oil_company = CompanyB   &   currency = ALL", 
    "location = New York   &   oil_company = CompanyB   &   currency = ALL", 
    "location = North New York   &   oil_company = CompanyB   &   currency = ALL", 
    "location = South New York   &   oil_company = CompanyB   &   currency = ALL", 
    "location = Bronx   &   oil_company = CompanyB   &   currency = ALL", 
    "location = Queens   &   oil_company = CompanyB   &   currency = ALL", 
    "location = Indiana   &   oil_company = CompanyB   &   currency = ALL", 
    "location = North Indiana   &   oil_company = CompanyB   &   currency = ALL", 
    "location = South Indiana   &   oil_company = CompanyB   &   currency = ALL", 
    "location = USA   &   oil_company = ALL   &   currency = EUR", 
    "location = New York   &   oil_company = ALL   &   currency = EUR", 
    "location = North New York   &   oil_company = ALL   &   currency = EUR", 
    "location = South New York   &   oil_company = ALL   &   currency = EUR", 
    "location = Bronx   &   oil_company = ALL   &   currency = EUR", 
    "location = Queens   &   oil_company = ALL   &   currency = EUR", 
    "location = Indiana   &   oil_company = ALL   &   currency = EUR", 
    "location = North Indiana   &   oil_company = ALL   &   currency = EUR", 
    "location = South Indiana   &   oil_company = ALL   &   currency = EUR", 
    "location = USA   &   oil_company = ALL   &   currency = USD", 
    "location = New York   &   oil_company = ALL   &   currency = USD", 
    "location = North New York   &   oil_company = ALL   &   currency = USD", 
    "location = South New York   &   oil_company = ALL   &   currency = USD", 
    "location = Bronx   &   oil_company = ALL   &   currency = USD", 
    "location = Queens   &   oil_company = ALL   &   currency = USD", 
    "location = Indiana   &   oil_company = ALL   &   currency = USD", 
    "location = North Indiana   &   oil_company = ALL   &   currency = USD", 
    "location = South Indiana   &   oil_company = ALL   &   currency = USD", 
    "location = USA   &   oil_company = CompanyC   &   currency = EUR", 
    "location = USA   &   oil_company = CompanyC   &   currency = USD", 
    "location = USA   &   oil_company = CompanyA   &   currency = EUR", 
    "location = USA   &   oil_company = CompanyA   &   currency = USD", 
    "location = USA   &   oil_company = CompanyB   &   currency = EUR", 
    "location = USA   &   oil_company = CompanyB   &   currency = USD", 
    "location = New York   &   oil_company = CompanyC   &   currency = EUR", 
    "location = New York   &   oil_company = CompanyC   &   currency = USD", 
    "location = New York   &   oil_company = CompanyA   &   currency = EUR", 
    "location = New York   &   oil_company = CompanyA   &   currency = USD", 
    "location = New York   &   oil_company = CompanyB   &   currency = EUR", 
    "location = New York   &   oil_company = CompanyB   &   currency = USD", 
    "location = North New York   &   oil_company = CompanyC   &   currency = EUR", 
    "location = North New York   &   oil_company = CompanyC   &   currency = USD", 
    "location = North New York   &   oil_company = CompanyA   &   currency = EUR", 
    "location = North New York   &   oil_company = CompanyA   &   currency = USD", 
    "location = North New York   &   oil_company = CompanyB   &   currency = EUR", 
    "location = North New York   &   oil_company = CompanyB   &   currency = USD", 
    "location = South New York   &   oil_company = CompanyC   &   currency = EUR", 
    "location = South New York   &   oil_company = CompanyC   &   currency = USD", 
    "location = South New York   &   oil_company = CompanyA   &   currency = EUR", 
    "location = South New York   &   oil_company = CompanyA   &   currency = USD", 
    "location = South New York   &   oil_company = CompanyB   &   currency = EUR", 
    "location = South New York   &   oil_company = CompanyB   &   currency = USD", 
    "location = Bronx   &   oil_company = CompanyC   &   currency = EUR", 
    "location = Bronx   &   oil_company = CompanyC   &   currency = USD", 
    "location = Bronx   &   oil_company = CompanyA   &   currency = EUR", 
    "location = Bronx   &   oil_company = CompanyA   &   currency = USD", 
    "location = Bronx   &   oil_company = CompanyB   &   currency = EUR", 
    "location = Bronx   &   oil_company = CompanyB   &   currency = USD", 
    "location = Queens   &   oil_company = CompanyC   &   currency = EUR", 
    "location = Queens   &   oil_company = CompanyC   &   currency = USD", 
    "location = Queens   &   oil_company = CompanyA   &   currency = EUR", 
    "location = Queens   &   oil_company = CompanyA   &   currency = USD", 
    "location = Queens   &   oil_company = CompanyB   &   currency = EUR", 
    "location = Queens   &   oil_company = CompanyB   &   currency = USD", 
    "location = Indiana   &   oil_company = CompanyC   &   currency = EUR", 
    "location = Indiana   &   oil_company = CompanyC   &   currency = USD", 
    "location = Indiana   &   oil_company = CompanyA   &   currency = EUR", 
    "location = Indiana   &   oil_company = CompanyA   &   currency = USD", 
    "location = Indiana   &   oil_company = CompanyB   &   currency = EUR", 
    "location = Indiana   &   oil_company = CompanyB   &   currency = USD", 
    "location = North Indiana   &   oil_company = CompanyC   &   currency = EUR", 
    "location = North Indiana   &   oil_company = CompanyC   &   currency = USD", 
    "location = North Indiana   &   oil_company = CompanyA   &   currency = EUR", 
    "location = North Indiana   &   oil_company = CompanyA   &   currency = USD", 
    "location = North Indiana   &   oil_company = CompanyB   &   currency = EUR", 
    "location = North Indiana   &   oil_company = CompanyB   &   currency = USD", 
    "location = South Indiana   &   oil_company = CompanyC   &   currency = EUR", 
    "location = South Indiana   &   oil_company = CompanyC   &   currency = USD", 
    "location = South Indiana   &   oil_company = CompanyA   &   currency = EUR", 
    "location = South Indiana   &   oil_company = CompanyA   &   currency = USD", 
    "location = South Indiana   &   oil_company = CompanyB   &   currency = EUR", 
    "location = South Indiana   &   oil_company = CompanyB   &   currency = USD"
  ))
  expect_equal(round(mean(function_output$level_location), 3), NA_real_)
  expect_equal(round(min(function_output$level_location), 3), NA_real_)
  expect_equal(round(max(function_output$level_location), 3), NA_real_)
  expect_equal(round(mean(function_output$level_location, na.rm = T), 3), 2.778)
  expect_equal(round(min(function_output$level_location, na.rm = T), 3), 1)
  expect_equal(round(max(function_output$level_location, na.rm = T), 3), 4)
  expect_equal(round(mean(function_output$level_oil_company), 3), NA_real_)
  expect_equal(round(min(function_output$level_oil_company), 3), NA_real_)
  expect_equal(round(max(function_output$level_oil_company), 3), NA_real_)
  expect_equal(round(mean(function_output$level_oil_company, na.rm = T), 3), 1.667)
  expect_equal(round(min(function_output$level_oil_company, na.rm = T), 3), 1)
  expect_equal(round(max(function_output$level_oil_company, na.rm = T), 3), 2)
  expect_equal(round(mean(function_output$original_col_of_interest), 3), 7.846)
  expect_equal(round(min(function_output$original_col_of_interest), 3), 0.812)
  expect_equal(round(max(function_output$original_col_of_interest), 3), 142.527)
  expect_equal(round(mean(function_output$changepoint_delta), 3), 200)
  expect_equal(round(min(function_output$changepoint_delta), 3), 100)
  expect_equal(round(max(function_output$changepoint_delta), 3), 600)
  expect_equal(mean(function_output$is_anomaly), 1)
  expect_equal(min(function_output$is_anomaly), 1)
  expect_equal(max(function_output$is_anomaly), 1)
  repeated_function_output <- add_total_for_non_hierarchy_groups(
    data = function_output,
    hierarchical_cols = "location"
  )
  expect_equal(repeated_function_output, function_output)
})

test_that("check add_total_for_non_hierarchy_groups with valid inputs with single group column", {
  function_input <- dummy_gasprice %>% 
    dplyr::filter(state == "New York") %>% 
    dplyr::select(-state) %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "oil_company"
    )
  function_output <- add_total_for_non_hierarchy_groups(
    data = function_input
  )
  expect_equal(colnames(function_output), colnames(function_input))
  expect_is(function_output$period, "Date")
  expect_is(function_output$col_of_interest, "numeric")
  expect_is(function_output$grouping, "character")
  expect_equal(unique(function_output$period), unique(function_input$period))
  expect_gt(mean(function_output$col_of_interest), mean(function_input$col_of_interest))
  expect_equal(round(mean(function_output$col_of_interest), 3), 1.868)
  expect_equal(round(min(function_output$col_of_interest), 3), 0.781)
  expect_equal(round(max(function_output$col_of_interest), 3), 6.102)
  expect_equal(function_output$grouping, c(
    rep("oil_company = ALL", 191),
    function_input$grouping
  ))
  repeated_function_output <- add_total_for_non_hierarchy_groups(
    data = function_output
  )
  expect_equal(repeated_function_output, function_output)
})

test_that("check add_total_for_non_hierarchy_groups with invalid inputs", {
  valid_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company")
  )
  expect_error(
    add_total_for_non_hierarchy_groups(
      data = "potato"
    )
  )
  expect_error(
    add_total_for_non_hierarchy_groups(
      data = 42
    )
  )
  expect_error(
    add_total_for_non_hierarchy_groups(
      data = list()
    )
  )
  expect_error(
    add_total_for_non_hierarchy_groups(
      data = valid_input %>% 
        dplyr::select(-grouping)
    )
  )
  expect_error(
    add_total_for_non_hierarchy_groups(
      data = valid_input %>% 
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    add_total_for_non_hierarchy_groups(
      data = valid_input,
      hierarchical_cols = 42
    )
  )
  expect_error(
    add_total_for_non_hierarchy_groups(
      data = valid_input,
      hierarchical_cols = "potato"
    )
  )
})
