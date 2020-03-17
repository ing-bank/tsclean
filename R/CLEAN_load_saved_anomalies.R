#' Loads saved anomalies from a csv file from a file_path.
#'
#' \code{load_saved_anomalies} is a function that returns saved anomalies from a
#' csv file.
#'
#' @param file_path The file_path to the csv file with the anomalies.
#'
#' @return A tibble containing the anomalies
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools add_grouping_column parse_date_from_vector
#' @import dplyr
#'
#' @examples
#' # REPLACE network_location_path/file_name WITH A VALID FILE ON A NETWORK LOCATION
#' load_saved_anomalies(
#'   file_path = "network_location_path/file_name.csv"
#' )
load_saved_anomalies <- function(file_path) {
  # Checks
  if (!file.exists(file_path)) {
    message <- paste0("The specified file_path does not seem to exist, please have a look!\n\t", file_path)
    stop(message)
  }
  if (!grepl(".csv", basename(file_path))) {
    message <- paste0("The specified file_path should direct to a CSV file, please adjust!\n\t", file_path)
    stop(message)
  }
  # Read the anomalies data
  data <- read.csv2(
    file = file_path,
    stringsAsFactors = F
  )
  # Check for required columns
  cols_to_check <- c("period", "new_value", "changepoint_delta", "is_anomaly", "anomaly_delta", "original_value", "comment")
  for (col_to_check in cols_to_check) {
    if (!col_to_check %in% colnames(data)) {
      message <- paste0("The required column '",col_to_check,"' is not available in the data, are you sure this the csv file that was exported from the data cleaning dashboard?\n")
      stop(message)
    }
  }
  # Create cols_to_save vector
  cols_to_save <- c("period", "grouping", "is_anomaly", "changepoint_delta")
  # If comment column exists, add into cols_to_check and cols_to_save
  if ("comment" %in% colnames(data)) {
    cols_to_check <- c(cols_to_check, "comment")
    cols_to_save <- c(cols_to_save, "comment")
  }
  # Get grouping columns
  grouping_cols <- colnames(data)[!colnames(data) %in% cols_to_check]
  # Transform anomalies and period to correct format
  data %>% 
    dplyr::mutate(period = parse_date_from_vector(period)) %>% 
    tstools::add_grouping_column(group_cols = grouping_cols) %>%
    dplyr::select(!!! syms(cols_to_save)) %>% 
    return()
}