
context("add_missing_data")

test_that("check add_missing_data output with default specifications", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::mutate(
      col_of_interest = ifelse(
        test = period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31", "2000-09-30")),
        yes = NA,
        no = col_of_interest
      )
    )
  function_output <- add_missing_data(function_input)
  expect_is(function_output, "data.frame")
  expect_equal(nrow(function_output), 764)
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
  expect_equal(round(min(function_output$col_of_interest), 2), 0.78)
  expect_equal(round(max(function_output$col_of_interest), 2), 3.46)
  expect_equal(unique(function_output$grouping), c(
    "state = New York   &   oil_company = CompanyA", "state = New York   &   oil_company = CompanyB", 
    "state = Indiana   &   oil_company = CompanyA", "state = Indiana   &   oil_company = CompanyB"
  ))
  expect_equal(round(min(function_output$spotprice), 2), 9.64)
  expect_equal(round(max(function_output$spotprice), 2), 86.61)
  expect_equal(round(min(function_output$gemprice), 2), 19.06)
  expect_equal(round(max(function_output$gemprice), 2), 81.52)
  check_missing <- function_input %>% 
    dplyr::filter(period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31", "2000-09-30"))) %>% 
    dplyr::pull(col_of_interest)
  expect_equal(check_missing, rep(as.numeric(NA), 4*4))
  check_filled <- function_output %>% 
    dplyr::filter(period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31", "2000-09-30"))) %>% 
    dplyr::pull(col_of_interest) %>% 
    round(3)
  expect_equal(check_filled, c(
    1.006, 1.212, 1.8, 1.768, 1.13, 1.199, 1.759, 1.754, 1.194, 
    1.084, 1.689, 1.672, 0.976, 1.148, 1.382, 1.326))
})

test_that("check add_missing_data output without missing data", {
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice, 
    date_col = "year_month", 
    col_of_interest = "gasprice", 
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice", "gemprice")
  )
  function_output <- add_missing_data(function_input)
  expect_identical(function_input, function_output)
})
  
test_that("check add_missing_data output with very limited data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::mutate(
      col_of_interest = ifelse(
        test = !period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31", "2000-09-30")),
        yes = NA,
        no = col_of_interest
      )
    )
  function_output <- add_missing_data(function_input)
  expect_is(function_output, "data.frame")
  expect_equal(nrow(function_output), 764)
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
  expect_equal(round(min(function_output$col_of_interest), 2), 0.97)
  expect_equal(round(max(function_output$col_of_interest), 2), 1.84)
  expect_equal(unique(function_output$grouping), c(
    "state = New York   &   oil_company = CompanyA", "state = New York   &   oil_company = CompanyB", 
    "state = Indiana   &   oil_company = CompanyA", "state = Indiana   &   oil_company = CompanyB"
  ))
  expect_equal(round(min(function_output$spotprice), 2), 9.64)
  expect_equal(round(max(function_output$spotprice), 2), 86.61)
  expect_equal(round(min(function_output$gemprice), 2), 19.06)
  expect_equal(round(max(function_output$gemprice), 2), 81.52)
  check_missing <- function_input %>% 
    dplyr::filter(!period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31", "2000-09-30"))) %>% 
    dplyr::pull(col_of_interest)
  expect_equal(check_missing, rep(as.numeric(NA), 187*4))
  check_filled <- function_output %>% 
    dplyr::filter(!period %in% as.Date(c("1995-01-31", "1996-03-31", "2000-08-31", "2000-09-30"))) %>% 
    dplyr::pull(col_of_interest) %>% 
    round(1)
  expect_equal(unique(check_filled), c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.7, 1.8, 1.6))
})

test_that("check add_missing_data output with invalid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice, 
    date_col = "year_month", 
    col_of_interest = "gasprice", 
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice", "gemprice")
  )
  expect_error(
    add_missing_data(
      data = "potato"
    )
  )
  expect_error(
    add_missing_data(
      data = dummy_gasprice
    )
  )
  expect_error(
    add_missing_data(
      data = function_input %>% 
        dplyr::select(-grouping)
    )
  )
  expect_error(
    add_missing_data(
      data = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
})
