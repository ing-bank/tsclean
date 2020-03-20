#' Convert scenario string into a forecast date
#'
#' \code{get_fc_date} is a function to convert a character string containing
#' information on the scenario (as retrieved from MISOB using the
#' \code{retrieve_misob_data} function) into a period (numeric - yyyymm format).
#'
#' @param misob_year A four digit integer/numeric value indicating the MISOB
#'   year which was used to retreive the MISOB data.
#' @param scenario A character string coming from the scenario column in a MISOB
#'   dataset as retrieved from MISOB.
#'
#' @return An object in yyyymm numeric format
#' @export
#'
#' @importFrom stringi stri_trim
#' @importFrom lubridate ymd
#'
#' @examples
#' get_fc_date(
#'    misob_year = 2018,
#'    scenario = "Forecast May"
#' )
get_fc_date <- function(misob_year, scenario) {
  # Check inputs
  if (!is.numeric(misob_year) || nchar(misob_year) != 4) {
    stop(paste0("The specified parameter 'misob_year' should be a four digit integer/numeric value indicating a year!"))
  }
  if (!is.character(scenario) || nchar(scenario) == 0) {
    stop(paste0("The specified parameter 'scenario' should be a non-empty character string coming from a misob dataset gathered using the retrieve_misob_data() function!"))
  }
  # Get month from scenario
  month <- scenario %>% 
    stringi::stri_trim() %>% 
    gsub("^Forecast ([a-zA-Z]{1,9})$","\\1", .)
  # Return NA if month is not valid
  if (!all(month %in% month.name)) {
    return(NA_real_)
  }
  # Parse forecast date from scenario
  month %>% 
    paste0(misob_year, " - ", ., " - 1") %>% 
    lubridate::ymd(.) %>% 
    tstools::date_to_period() %>% 
    return()
}