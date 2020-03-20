#' Start a local data cleaning dashboard
#'
#' \code{start_data_cleaning_dashboard} is a wrapper function around several
#' visualization functions to start a local Shiny dashboard to execute the
#' cleaning of the available data.
#'
#' @param data A tibble containing several columns of data required for time
#'   series forecasting, which has been created using the
#'   \code{initialize_ts_forecast_data} function.
#' @param add_total_per_group Boolean, which is set to TRUE if for every group
#'   in the data (with more than one level) a total should be added that is
#'   shown in the dashboard, or set to FALSE if not.
#'
#' @return Starts a local Shiny dashboard to execute data cleaning.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools get_browser_to_use_for_shiny
#' @import dplyr
#' @rawNamespace import(shiny, except = c(renderDataTable, dataTableOutput))
#' @import V8
#' @importFrom shinyjs useShinyjs extendShinyjs js enable disable
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinyAce
#' @importFrom plotly renderPlotly plotlyOutput
#' @importFrom visNetwork renderVisNetwork visNetworkOutput visOptions visEvents
#'   visNetworkProxy visSelectNodes
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom rvest html_nodes html_children html_text
#'
#' @examples
#' start_data_cleaning_dashboard(dummy_anomaly_data)
start_data_cleaning_dashboard <- function(data, add_total_per_group = T) {
  # Check data
  check_data_format(
    data = data,
    func_name = "start_data_cleaning_dashboard",
    req_cols = c("period", "col_of_interest", "grouping")
  )
  # Check to make sure period is last day of the month
  periods <- data %>% 
    dplyr::pull(period)
  check_periods <- periods %>% 
    date_to_period() %>% 
    period_to_last_day()
  invalid_periods <- periods[periods != check_periods]
  if (!all(periods == check_periods)) {
    message <- paste0("The following period values within the specified data do not correspond to the last day of that month (as is required):\n", paste0(invalid_periods, collapse = "; "))
    stop(message)
  }
  # Determine location of shiny dashboard
  appDir <- system.file("data_cleaning_dashboard", package = "tsclean")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `tsclean`.")
  }
  # If applicable, get hierarchy information
  if (any(grepl("level_", colnames(data)))) {
    hierarchy <- create_hierarchy_list(data)
    is_hierarchical <- TRUE
    hierarchical_cols <- comment(hierarchy$matrix)
    # Take out level from data
    data <- data %>%
      dplyr::select(-dplyr::starts_with("level_"))
  } else {
    hierarchy <- NA
    is_hierarchical <- FALSE
    hierarchical_cols <- ""
  }
  # Initialize data if required
  if (add_total_per_group) {
    data <- add_total_for_non_hierarchy_groups(
      data = data,
      hierarchical_cols = hierarchical_cols
    )
  }
  # Determine group filter names
  groupings <- data %>% 
    dplyr::select(grouping) %>% 
    dplyr::distinct() %>% 
    tstools::split_grouping_column()
  # Define app data
  app_data <<- list(
    # Data
    data = data,
    # Variables
    groupings = groupings,
    # Hierarchy information
    is_hierarchical = is_hierarchical,
    hierarchy = hierarchy,
    hierarchical_cols = hierarchical_cols,
    # Periods
    periods = sort(unique(data$period)),
    # Default values for anomaly detection
    default_m_width = 12,
    default_sd_cutoff = 2,
    default_max_pct_diff = 0.3,
    default_pct_m_width = 3,
    default_lof_threshold = 2,
    default_lof_s = 9,
    default_lof_m_width = 3,
    # Other information
    add_total_per_group = add_total_per_group,
    latest_split_date = max(data$period),
    documentation = list()
  )
  # Determine best browser to use
  browser_to_use <- tstools::get_browser_to_use_for_shiny()
  # Function to launch the best browser to use
  launch.browser = function(appUrl, browser.path = browser_to_use) {
    message('Browser path: ', browser.path)
    browseURL(url = appUrl, browser = browser.path, encodeIfNeeded = TRUE)
  }
  # Run the data cleaning dashboard
  shiny::runApp(
    appDir = appDir, 
    launch.browser = launch.browser
  )
}