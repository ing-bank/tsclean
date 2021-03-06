% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HELPER_add_total_for_non_hierarchy_groups.R
\name{add_total_for_non_hierarchy_groups}
\alias{add_total_for_non_hierarchy_groups}
\title{Add total for non-hierarchy groups}
\usage{
add_total_for_non_hierarchy_groups(data, hierarchical_cols = "")
}
\arguments{
\item{data}{A tibble containing several columns of data required for time
series forecasting, which has been created using the
\code{initialize_ts_forecast_data} function.}

\item{hierarchical_cols}{A character string indicating which column corresponds
to the hierarchical group, for which no total has to be calculated (because
this is done hierarchically).}
}
\value{
A tibble extended with additional groupings, corresponding to totals
  per group.
}
\description{
\code{add_total_for_non_hierarchy_groups} is a function to add the total for
each group within grouping, excluding the hierarchical group.
}
\examples{
add_total_for_non_hierarchy_groups(dummy_anomaly_data)
}
