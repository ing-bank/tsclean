
context("save_load_anomalies")

test_that("check load_saved_anomalies and remove_saved_anomalies together", {
  # Loads data and applies anomality detection
  # At least one point will be detected as outlier here.
  data <- dummy_anomaly_data %>% 
    detect_anomalies_using_percent_differences(max_pct_diff = 0.3) %>% 
    dplyr::rename(is_anomaly = anomaly_flag) %>% 
    dplyr::mutate(
      original_col_of_interest = col_of_interest,
      changepoint_delta = 0
    )
    
  # Define temporary file path
  file_path <- file.path(tempdir(), "test_anomalies.csv")
  
  # Save anomalies
  data %>%
    dplyr::filter(is_anomaly == 1 | changepoint_delta != 0) %>% 
    dplyr::transmute(
      period = period,
      grouping = grouping,
      new_value = col_of_interest,
      changepoint_delta = changepoint_delta,
      is_anomaly = is_anomaly,
      anomaly_delta = round((new_value - original_col_of_interest - changepoint_delta), 2),
      original_value = original_col_of_interest,
      comment = "this ain't no pony"
    ) %>% 
    tstools::split_grouping_column() %>% 
    dplyr::select(-grouping) %>% 
    write.csv2(
      file = file_path,
      row.names = F
    )
  
  # Load anomalies
  saved_anoms <- load_saved_anomalies(file_path)
  
  # Remove anomalies
  new_data <- remove_saved_anomalies(dummy_anomaly_data, saved_anoms)
  
  # Compare so that the original and the new data is the same
  expect_true(sum(data$is_anomaly) == 84)
  expect_true(sum(new_data$is_anomaly) == 84)
  expect_true(sum(data$is_anomaly != new_data$is_anomaly) == 0)
})

test_that("check load_saved_anomalies with invalid inputs", {
  fake_data <- data.frame(magic = 42)
  fake_xlsx_file <- file.path(tempdir(), "fake.xlsx")
  saveRDS(fake_data, fake_xlsx_file)
  fake_csv_file <- file.path(tempdir(), "fake.csv")
  write.csv2(fake_data, fake_csv_file)
  expect_error(
    load_saved_anomalies(
      file_path = "potato"
    )
  )
  expect_error(
    load_saved_anomalies(
      file_path = fake_xlsx_file
    )
  )
  expect_error(
    load_saved_anomalies(
      file_path = fake_csv_file
    )
  )
})

test_that("check remove_saved_anomalies with invalid inputs", {
  expect_error(
    remove_saved_anomalies(
      data = "potato"
    )
  )
  expect_error(
    remove_saved_anomalies(
      data = dummy_anomaly_data %>% 
        dplyr::select(-period)
    )
  )
  expect_error(
    remove_saved_anomalies(
      data = dummy_anomaly_data %>% 
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    remove_saved_anomalies(
      data = dummy_anomaly_data,
      anomalies = "potato"
    )
  )
  expect_error(
    remove_saved_anomalies(
      data = dummy_anomaly_data,
      anomalies = dummy_anomaly_data
    )
  )
})
