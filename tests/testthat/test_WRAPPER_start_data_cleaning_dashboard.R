
context("start_data_cleaning_dashboard")

test_that("check start_data_cleaning_dashboard directly, with non-hierarchical data", {
  # Start shiny app in the background
  lines_to_run <- c(
    paste0("setwd('",getwd(),"')"),
    "devtools::load_all('.')",
    "options(shiny.port = 9518)",
    "data = tstools::initialize_ts_forecast_data(data = tstools::dummy_gasprice, date_col = 'year_month', col_of_interest = 'gasprice', group_cols = c('state', 'oil_company'))",
    "start_data_cleaning_dashboard(data)"
  )
  invisible(
    system(
      command = paste0("R -e ", paste0(gsub(" ","",lines_to_run), collapse = ";")),
      ignore.stdout = T,
      ignore.stderr = T,
      show.output.on.console = F,
      wait = F
    )
  )
  # Wait a few seconds and check if app is running
  shiny_url <- "http://127.0.0.1:9518/"
  start <- Sys.time()
  scraped <- FALSE
  # Get shiny_html
  while (difftime(Sys.time(), start, units = "sec") <= 60 & !scraped) {
    shiny_html <- try(
      expr = xml2::read_html(shiny_url),
      silent = T
    )
    if (any(class(shiny_html) != 'try-error')) {
      scraped <- TRUE
    } else {
      Sys.sleep(5)
    }
  }
  # Get html
  expect_is(shiny_html, "xml_document")
  # Temp function to parse html
  parse_html <- function(html, css) {
    shiny_html %>% 
      rvest::html_nodes(css) %>% 
      rvest::html_children() %>% 
      rvest::html_text() %>% 
      gsub("\n", "", .) %>% 
      gsub("\t", "", .) %>% 
      trimws() %>% 
      stringr::str_squish() %>% 
      magrittr::extract(. != "")
  }
  
  # Check header
  header <- parse_html(
    html = shiny_html,
    css = ".navbar-static-top"
  )
  expect_equal(header, "Toggle navigation")
  # Check sidebar
  sidebar <- parse_html(
    html = shiny_html,
    css = ".sidebar-menu"
  )
  expect_equal(sidebar, c(
    "Select tabs", 
    "Adjust settings", 
    "Anomaly detection", 
    "All Detections", 
    "Adjust settings Apply adjusted settings Reset to default settings", 
    "Select group", 
    "Store results Download cleaned data Deselect all anomalies Upload anomaly data Browse... Download anomalies"
  ))
  # Check body
  body <- parse_html(
    html = shiny_html,
    css = ".content"
  )
  expect_equal(body, "$(document).ready(function(){ $('input[name=ADJ_SET_algo]').on('click', function(event){ if($('input[name=ADJ_SET_algo]:checked').length == 0){ $(this).prop('checked', true); } });}); Select Algorithm Choose an algorithm: Rolling Standard Deviation: Uses the rolling mean and standard deviation to find outliers Percentage Difference: Uses the percentage difference to find outliers Using Friedman's SuperSmoother to find outliers Local Outlier Factor: Uses the machine learning mathod Local Outlier Factor to find outliers. The features are the absolute percentage difference from the previous moving average, the month, quarter and year. Rolling Standard Deviation - Outlier detection The number of data point to use when calculating the moving average and moving standard deviation The number of standard deviations to be used as cut-off point for flagging potential outliers Percentage Difference - Outlier detection The maximum percentage diff between points The number of data point to use when calculating the moving average Local Outlier Factor - Outlier detection The threshold for the anomaly score The size of the neighborhood of data points to be considered The number of data point to use when calculating the moving average Loading... Increase/decrease the value for a range of dates, based on changepoints in the series Select range of dates to change Specify value to increase/decrease the above date range by: Apply change Revert selected Loading... $('#ANOM_DET_changepoints').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('ANOM_DET_changepoints_is_collapsed', true);}) $('#ANOM_DET_changepoints').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('ANOM_DET_changepoints_is_collapsed', false);}) Remove individual points as anomalies, to be replaced using seasonally adjusted interpolation Loading... Remove all Remove selected Return all Return selected Loading... $('#ANOM_DET_anomalies').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('ANOM_DET_anomalies_is_collapsed', true);}) $('#ANOM_DET_anomalies').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('ANOM_DET_anomalies_is_collapsed', false);}) Comments {\"id\":\"ANOM_DET_comment\",\"vimKeyBinding\":false,\"readOnly\":false,\"wordWrap\":false,\"showLineNumbers\":true,\"highlightActiveLine\":true,\"cursorId\":\"cursor\",\"autoComplete\":\"disabled\",\"tabSize\":4,\"useSoftTabs\":true,\"showInvisibles\":false,\"setBehavioursEnabled\":true,\"autoScrollEditorIntoView\":false,\"autoCompleters\":[\"snippet\",\"text\",\"keyword\"],\"fontSize\":16,\"debounce\":1000} Save comment Clear comment $('#ANOM_DET_comments').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('ANOM_DET_comments_is_collapsed', true);}) $('#ANOM_DET_comments').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('ANOM_DET_comments_is_collapsed', false);}) #shiny-notification-error_loading_file {font-size: 20px;} Loading...")
})

test_that("check start_data_cleaning_dashboard directly, with hierarchical data", {
  # Start shiny app in the background
  lines_to_run <- c(
    paste0("setwd('",getwd(),"')"),
    "devtools::load_all('.')",
    "options(shiny.port = 9519)",
    "data = tstools::initialize_ts_forecast_data(data = tstools::dummy_hierarchical_gasprice, date_col = 'year_month', col_of_interest = 'gasprice', group_cols = 'currency', hierarchical_cols = c('location', 'oil_company'))",
    "start_data_cleaning_dashboard(data, add_total_per_group = F)"
  )
  invisible(
    system(
      command = paste0("R -e ", paste0(gsub(" ","",lines_to_run), collapse = ";")),
      ignore.stdout = T,
      ignore.stderr = T,
      show.output.on.console = F,
      wait = F
    )
  )
  # Wait a few seconds and check if app is running
  shiny_url <- "http://127.0.0.1:9519/"
  start <- Sys.time()
  scraped <- FALSE
  # Get shiny_html
  while (difftime(Sys.time(), start, units = "sec") <= 60 & !scraped) {
    shiny_html <- try(
      expr = xml2::read_html(shiny_url),
      silent = T
    )
    if (any(class(shiny_html) != 'try-error')) {
      scraped <- TRUE
    } else {
      Sys.sleep(5)
    }
  }
  # Get html
  expect_is(shiny_html, "xml_document")
  # Temp function to parse html
  parse_html <- function(html, css) {
    shiny_html %>% 
      rvest::html_nodes(css) %>% 
      rvest::html_children() %>% 
      rvest::html_text() %>% 
      gsub("\n", "", .) %>% 
      gsub("\t", "", .) %>% 
      trimws() %>% 
      stringr::str_squish() %>% 
      magrittr::extract(. != "")
  }
  
  # Check header
  header <- parse_html(
    html = shiny_html,
    css = ".navbar-static-top"
  )
  expect_equal(header, "Toggle navigation")
  # Check sidebar
  sidebar <- parse_html(
    html = shiny_html,
    css = ".sidebar-menu"
  )
  expect_equal(sidebar, c(
    "Select tabs", 
    "Adjust settings", 
    "View Hierarchy", 
    "Anomaly detection", 
    "All Detections", 
    "Adjust settings Apply adjusted settings Reset to default settings", 
    "Select group", 
    "Store results Download cleaned data Deselect all anomalies Upload anomaly data Browse... Download anomalies"
  ))
  # Check body
  body <- parse_html(
    html = shiny_html,
    css = ".content"
  )
  expect_equal(body, "$(document).ready(function(){ $('input[name=ADJ_SET_algo]').on('click', function(event){ if($('input[name=ADJ_SET_algo]:checked').length == 0){ $(this).prop('checked', true); } });}); Select Algorithm Choose an algorithm: Rolling Standard Deviation: Uses the rolling mean and standard deviation to find outliers Percentage Difference: Uses the percentage difference to find outliers Using Friedman's SuperSmoother to find outliers Local Outlier Factor: Uses the machine learning mathod Local Outlier Factor to find outliers. The features are the absolute percentage difference from the previous moving average, the month, quarter and year. Rolling Standard Deviation - Outlier detection The number of data point to use when calculating the moving average and moving standard deviation The number of standard deviations to be used as cut-off point for flagging potential outliers Percentage Difference - Outlier detection The maximum percentage diff between points The number of data point to use when calculating the moving average Local Outlier Factor - Outlier detection The threshold for the anomaly score The size of the neighborhood of data points to be considered The number of data point to use when calculating the moving average Loading... Increase/decrease the value for a range of dates, based on changepoints in the series Select range of dates to change Specify value to increase/decrease the above date range by: Apply change Revert selected Loading... $('#ANOM_DET_changepoints').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('ANOM_DET_changepoints_is_collapsed', true);}) $('#ANOM_DET_changepoints').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('ANOM_DET_changepoints_is_collapsed', false);}) Remove individual points as anomalies, to be replaced using seasonally adjusted interpolation Loading... Remove all Remove selected Return all Return selected Loading... $('#ANOM_DET_anomalies').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('ANOM_DET_anomalies_is_collapsed', true);}) $('#ANOM_DET_anomalies').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('ANOM_DET_anomalies_is_collapsed', false);}) Comments {\"id\":\"ANOM_DET_comment\",\"vimKeyBinding\":false,\"readOnly\":false,\"wordWrap\":false,\"showLineNumbers\":true,\"highlightActiveLine\":true,\"cursorId\":\"cursor\",\"autoComplete\":\"disabled\",\"tabSize\":4,\"useSoftTabs\":true,\"showInvisibles\":false,\"setBehavioursEnabled\":true,\"autoScrollEditorIntoView\":false,\"autoCompleters\":[\"snippet\",\"text\",\"keyword\"],\"fontSize\":16,\"debounce\":1000} Save comment Clear comment $('#ANOM_DET_comments').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('ANOM_DET_comments_is_collapsed', true);}) $('#ANOM_DET_comments').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('ANOM_DET_comments_is_collapsed', false);}) #shiny-notification-error_loading_file {font-size: 20px;} Loading...")
})

test_that("check start_data_cleaning_dashboard with invalid input", {
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company")
  )
  expect_error(
    start_data_cleaning_dashboard(
      data = "potato"
    )
  )
  expect_error(
    start_data_cleaning_dashboard(
      data = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    start_data_cleaning_dashboard(
      data = dummy_gasprice
    )
  )
  expect_error(
    start_data_cleaning_dashboard(
      data = function_input %>% 
        dplyr::select(-grouping)
    )
  )
  expect_error(
    start_data_cleaning_dashboard(
      data = function_input %>% 
        dplyr::mutate(
          period = period %>% 
            date_to_period() %>% 
            period_to_first_day()
        )
    )
  )
})
