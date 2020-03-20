
context("add_up_hierarchical_values")

test_that("check add_up_hierarchical_values with valid univariate inputs", {
  # Where col_of_interest in a leaf is increased
  function_input <- dummy_hierarchical_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_col = c("location", "oil_company")
    ) %>% 
    dplyr::mutate(original_col_of_interest = col_of_interest) %>% 
    dplyr::mutate(
      col_of_interest = case_when(
        grouping == "location = Queens   &   oil_company = CompanyB   &   currency = EUR" & period <= as.Date("1996-12-31") ~ col_of_interest + 4,
        TRUE ~ col_of_interest
      )
    )
  hierarchy <- tstools::create_hierarchy_list(function_input)  
  function_output <- add_up_hierarchical_values(
    data = function_input,
    hierarchy = hierarchy
  )
  expect_equal(colnames(function_output), colnames(function_input))
  expect_equal(function_output$period, function_input$period)
  expect_equal(function_output$grouping, function_input$grouping)
  expect_equal(function_output$level_location, function_input$level_location)
  expect_equal(function_output$level_oil_company, function_input$level_oil_company)
  expect_equal(function_output$original_col_of_interest, function_input$original_col_of_interest)
  expect_gt(mean(function_output$col_of_interest), mean(function_input$col_of_interest))
  expect_equal(max(function_output$col_of_interest - function_input$col_of_interest), 4)
  expect_is(function_output$col_of_interest, "numeric")
  expect_equal(round(mean(function_output$col_of_interest), 3), 4.146)
  expect_equal(round(min(function_output$col_of_interest), 3), 0.812)
  expect_equal(round(max(function_output$col_of_interest), 3), 37.328)
  
  # Where col_of_interest in a parent is increased (should make no difference)
  function_input <- dummy_hierarchical_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_col = c("location", "oil_company")
    ) %>% 
    dplyr::mutate(original_col_of_interest = col_of_interest) %>% 
    dplyr::mutate(
      col_of_interest = case_when(
        grouping == "location = USA   &   oil_company = CompanyB   &   currency = USD" & period <= as.Date("1996-12-31") ~ col_of_interest + 4,
        TRUE ~ col_of_interest
      )
    )
  hierarchy <- tstools::create_hierarchy_list(function_input)  
  function_output <- add_up_hierarchical_values(
    data = function_input,
    hierarchy = hierarchy
  )
  expect_equal(colnames(function_output), colnames(function_input))
  expect_equal(function_output$period, function_input$period)
  expect_equal(function_output$grouping, function_input$grouping)
  expect_equal(function_output$level_location, function_input$level_location)
  expect_equal(function_output$level_oil_company, function_input$level_oil_company)
  expect_equal(function_output$original_col_of_interest, function_input$original_col_of_interest)
  expect_equal(function_output$original_col_of_interest, function_output$col_of_interest)
  expect_gt(mean(function_input$col_of_interest), mean(function_output$col_of_interest))
  expect_equal(max(function_input$col_of_interest - function_output$col_of_interest), 4)
  expect_is(function_output$col_of_interest, "numeric")
  expect_equal(round(mean(function_output$col_of_interest), 3), 3.923)
  expect_equal(round(min(function_output$col_of_interest), 3), 0.812)
  expect_equal(round(max(function_output$col_of_interest), 3), 37.328)
})

test_that("check add_up_hierarchical_values with valid multivariate inputs", {
  # Where col_of_interest in a leaf is increased
  function_input <- dummy_hierarchical_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      xreg_cols = c("gemprice", "spotprice"),
      hierarchical_col = c("location", "oil_company")
    ) %>% 
    dplyr::mutate(original_col_of_interest = col_of_interest) %>% 
    dplyr::mutate(
      col_of_interest = case_when(
        grouping == "location = Bronx   &   oil_company = CompanyA   &   currency = USD" & period <= as.Date("1996-12-31") ~ col_of_interest + 4,
        TRUE ~ col_of_interest
      )
    )
  hierarchy <- tstools::create_hierarchy_list(function_input)  
  function_output <- add_up_hierarchical_values(
    data = function_input,
    hierarchy = hierarchy
  )
  expect_equal(colnames(function_output), colnames(function_input))
  expect_equal(function_output$period, function_input$period)
  expect_equal(function_output$grouping, function_input$grouping)
  expect_equal(function_output$level_location, function_input$level_location)
  expect_equal(function_output$level_oil_company, function_input$level_oil_company)
  expect_equal(function_output$original_col_of_interest, function_input$original_col_of_interest)
  expect_gt(mean(function_output$col_of_interest), mean(function_input$col_of_interest))
  expect_equal(max(function_output$col_of_interest - function_input$col_of_interest), 4)
  expect_is(function_output$col_of_interest, "numeric")
  expect_equal(round(mean(function_output$col_of_interest), 3), 4.146)
  expect_equal(round(min(function_output$col_of_interest), 3), 0.812)
  expect_equal(round(max(function_output$col_of_interest), 3), 37.328)
  
  # Where col_of_interest in a parent is increased (should make no difference)
  function_input <- dummy_hierarchical_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      xreg_cols = c("gemprice", "spotprice"),
      hierarchical_col = c("location", "oil_company")
    ) %>% 
    dplyr::mutate(original_col_of_interest = col_of_interest) %>% 
    dplyr::mutate(
      col_of_interest = case_when(
        grouping == "location = Indiana   &   oil_company = CompanyC   &   currency = EUR" & period <= as.Date("1996-12-31") ~ col_of_interest + 4,
        TRUE ~ col_of_interest
      )
    )
  hierarchy <- tstools::create_hierarchy_list(function_input)  
  function_output <- add_up_hierarchical_values(
    data = function_input,
    hierarchy = hierarchy
  )
  expect_equal(colnames(function_output), colnames(function_input))
  expect_equal(function_output$period, function_input$period)
  expect_equal(function_output$grouping, function_input$grouping)
  expect_equal(function_output$level_location, function_input$level_location)
  expect_equal(function_output$level_oil_company, function_input$level_oil_company)
  expect_equal(function_output$original_col_of_interest, function_input$original_col_of_interest)
  expect_equal(function_output$original_col_of_interest, function_output$col_of_interest)
  expect_gt(mean(function_input$col_of_interest), mean(function_output$col_of_interest))
  expect_equal(max(function_input$col_of_interest - function_output$col_of_interest), 4)
  expect_is(function_output$col_of_interest, "numeric")
  expect_equal(round(mean(function_output$col_of_interest), 3), 3.923)
  expect_equal(round(min(function_output$col_of_interest), 3), 0.812)
  expect_equal(round(max(function_output$col_of_interest), 3), 37.328)
})

test_that("check add_up_hierarchical_values with invalid inputs", {
  valid_hierarchy <-  dummy_hierarchical_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_col = c("location", "oil_company")
    ) %>% 
    create_hierarchy_list()
  valid_input <-  dummy_hierarchical_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_col = c("location", "oil_company")
    ) %>% 
    dplyr::mutate(original_col_of_interest = col_of_interest) %>% 
    dplyr::mutate(
      col_of_interest = case_when(
        grouping == "location = Bronx   &   oil_company = CompanyB   &   currency = EUR" & period <= as.Date("1996-12-31") ~ col_of_interest + 4,
        TRUE ~ col_of_interest
      )
    )
  # Create non-valid data inputs
  non_hierarchical_data <- dummy_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    )
  missing_original_col <- dummy_hierarchical_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_col = c("location", "oil_company")
    )
  invalid_data <- list(non_hierarchical_data, missing_original_col, 42, NA, NULL, dummy_gasprice, dummy_hierarchical_gasprice)
  for (input in invalid_data) {
    expect_error(
      add_up_hierarchical_values(
        data = input,
        hierarchy = valid_hierarchy
      )
    )
  }
  # Create non-valid hierarchy inputs
  only_matrix <- valid_hierarchy$matrix
  only_data <- valid_hierarchy$data
  invalid_hierarchy <- list(only_matrix, only_data, 42, NA, NULL, dummy_gasprice, dummy_hierarchical_gasprice)
  for (input in invalid_hierarchy) {
    expect_error(
      suppressWarnings(
        add_up_hierarchical_values(
          data = valid_input,
          hierarchy = input
        ) 
      )
    )
  }
  for (data in invalid_data) {
    for (hierarchy in invalid_hierarchy) {
      suppressWarnings(
        expect_error(
          add_up_hierarchical_values(
            data = data,
            hierarchy = hierarchy
          )
        )
      )
    }
  }
})
