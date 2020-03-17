
context("add_missing_data_for_grouping")

test_that("check add_missing_data_for_grouping output with default specifications", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    dplyr::mutate(
      col_of_interest = ifelse(
        test = period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31", "2000-09-30")),
        yes = NA,
        no = col_of_interest
      )
    )
  function_output <- add_missing_data_for_grouping(function_input)
  expect_is(function_output, "data.frame")
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 5)
  expect_equal(nrow(function_input), nrow(function_output))
  expect_equal(ncol(function_input), ncol(function_output))
  expected_columns <- c("period", "col_of_interest", "grouping", "spotprice", "gemprice")
  expect_named(function_output, expected_columns)
  expect_is(function_output$period, "Date")
  expect_is(function_output$col_of_interest, "numeric")
  expect_is(function_output$grouping, "character")
  expect_is(function_output$spotprice, "numeric")
  expect_is(function_output$gemprice, "numeric")
  expect_equal(length(unique(function_output$period)), 191)
  expect_equal(min(function_output$period), as.Date("1991-01-31"))
  expect_equal(max(function_output$period), as.Date("2006-11-30"))
  expect_equal(round(min(function_output$col_of_interest), 2), 0.81)
  expect_equal(round(max(function_output$col_of_interest), 2), 3.46)
  expect_equal(unique(function_output$grouping), "state = New York   &   oil_company = CompanyA")
  expect_equal(round(min(function_output$spotprice), 2), 9.83)
  expect_equal(round(max(function_output$spotprice), 2), 86.61)
  expect_equal(round(min(function_output$gemprice), 2), 19.06)
  expect_equal(round(max(function_output$gemprice), 2), 67.24)
  check_missing <- function_input %>% 
    dplyr::filter(period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31", "2000-09-30"))) %>% 
    dplyr::pull(col_of_interest)
  expect_equal(check_missing, rep(as.numeric(NA), 4))
  check_filled <- function_output %>% 
    dplyr::filter(period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31", "2000-09-30"))) %>% 
    dplyr::pull(col_of_interest) %>% 
    round(3)
  expect_equal(check_filled, c(1.006, 1.212, 1.8, 1.768))
})

test_that("check add_missing_data_for_grouping output without missing data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  function_output <- add_missing_data_for_grouping(function_input)
  expect_identical(function_input, function_output)
})
  
test_that("check add_missing_data_for_grouping output with very limited data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    dplyr::mutate(
      col_of_interest = ifelse(
        test = !period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31", "2000-09-30")),
        yes = NA,
        no = col_of_interest
      )
    )
  function_output <- add_missing_data_for_grouping(function_input)
  expect_is(function_output, "data.frame")
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 5)
  expect_equal(nrow(function_input), nrow(function_output))
  expect_equal(ncol(function_input), ncol(function_output))
  expected_columns <- c("period", "col_of_interest", "grouping", "spotprice", "gemprice")
  expect_named(function_output, expected_columns)
  expect_is(function_output$period, "Date")
  expect_is(function_output$col_of_interest, "numeric")
  expect_is(function_output$grouping, "character")
  expect_is(function_output$spotprice, "numeric")
  expect_is(function_output$gemprice, "numeric")
  expect_equal(length(unique(function_output$period)), 191)
  expect_equal(min(function_output$period), as.Date("1991-01-31"))
  expect_equal(max(function_output$period), as.Date("2006-11-30"))
  expect_equal(round(min(function_output$col_of_interest), 2), 1.03)
  expect_equal(round(max(function_output$col_of_interest), 2), 1.65)
  expect_equal(unique(function_output$grouping), "state = New York   &   oil_company = CompanyA")
  expect_equal(round(min(function_output$spotprice), 2), 9.83)
  expect_equal(round(max(function_output$spotprice), 2), 86.61)
  expect_equal(round(min(function_output$gemprice), 2), 19.06)
  expect_equal(round(max(function_output$gemprice), 2), 67.24)
  check_missing <- function_input %>% 
    dplyr::filter(!period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31", "2000-09-30"))) %>% 
    dplyr::pull(col_of_interest)
  expect_equal(check_missing, rep(as.numeric(NA), 187))
  check_filled <- function_output %>% 
    dplyr::filter(!period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31", "2000-09-30"))) %>% 
    dplyr::pull(col_of_interest) %>% 
    round(3)
  expect_equal(check_filled, c(rep(1.030, 48), c(1.047, 1.063, 1.079, 1.095, 1.111, 
    1.127, 1.143, 1.159, 1.175, 1.191, 1.207, 1.223, 1.239, 1.26, 
    1.264, 1.269, 1.273, 1.278, 1.282, 1.286, 1.291, 1.295, 1.3, 
    1.304, 1.309, 1.313, 1.317, 1.322, 1.326, 1.331, 1.335, 1.34, 
    1.344, 1.349, 1.353, 1.357, 1.362, 1.366, 1.371, 1.375, 1.38, 
    1.384, 1.388, 1.393, 1.397, 1.402, 1.406, 1.411, 1.415, 1.42, 
    1.424, 1.428, 1.433, 1.437, 1.442, 1.446, 1.451, 1.455, 1.46, 
    1.464, 1.468, 1.473, 1.477, 1.482, 1.486), rep(1.655, 74)))
})

test_that("check add_missing_data_for_grouping output with invalid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice, 
    date_col = "year_month", 
    col_of_interest = "gasprice", 
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice", "gemprice")
  )
  expect_error(
    add_missing_data_for_grouping(
      data = "potato"
    )
  )
  expect_error(
    add_missing_data_for_grouping(
      data = dummy_gasprice
    )
  )
  expect_error(
    add_missing_data_for_grouping(
      data = function_input %>% 
        dplyr::select(-grouping)
    )
  )
  expect_error(
    add_missing_data_for_grouping(
      data = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    add_missing_data_for_grouping(
      data = function_input
    )
  )
})
