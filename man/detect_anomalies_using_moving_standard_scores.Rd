% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/CLEAN_detect_anomalies_using_moving_standard_scores.R
\name{detect_anomalies_using_moving_standard_scores}
\alias{detect_anomalies_using_moving_standard_scores}
\title{Detect anomalies using a moving standard score}
\usage{
detect_anomalies_using_moving_standard_scores(
  data,
  m_width = 12,
  sd_cutoff = 2
)
}
\arguments{
\item{data}{A tibble containing the data to be cleaned before it is used for
time series forecasting, which has been created using the
\code{initialize_ts_forecast_data} function.}

\item{m_width}{A positive integer value indicating the number of data point
to use when calculating the moving average and moving standard deviation
that are used to flag outliers.}

\item{sd_cutoff}{A positive value indicating the number of standard
deviations to be used as cutoff point for flagging potential outliers.}
}
\value{
A tibble extended with several columns related to anomaly detection.
}
\description{
\code{detect_anomalies_using_moving_standard_scores} is a function to detect
potential anomalies by calculating a moving average and moving standard
deviation, as input for the calculation of a standard score. Combined with a
specified cut-off point this will flag individual points as potential
anomalies.
}
\examples{
detect_anomalies_using_moving_standard_scores(dummy_anomaly_data)
}
