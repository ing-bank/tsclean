#' Plot anomalies for visual inspection
#'
#' \code{plot_anomalies} is a function to create a plot which displays the
#' development of data over time, in which potential anomalies that have been
#' detected are highlighted for visual inspection.
#'
#' @param data A tibble containing the data on which anomaly detection has been
#'   run, containing a single grouping or multiple.
#'
#' @return A plotly object highlighting points as potential anomalies. If the data 
#'   multiple groupings. It will return a facet plot with simpler features.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @import ggplot2
#' @importFrom tstools get_plot_colors
#' @importFrom scales pretty_breaks
#' @importFrom ggthemes theme_calc
#' @importFrom plotly ggplotly layout
#' @importFrom forecastHybrid hybridModel
#' @importFrom forecast forecast
#'
#' @examples
#' initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    detect_anomalies_using_moving_standard_scores() %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'    plot_anomalies()
plot_anomalies <- function(data) {
  # Check data
  check_data_format(
    data = data,
    func_name = "plot_anomalies",
    req_cols = c(
      "period", "col_of_interest", "grouping", 
      "anomaly_det_alg", "anomaly_lower_bound", "anomaly_upper_bound", "anomaly_flag"
    )
  )
  
  # Check if the data has multiple groupings
  multiple_groupings <- length(unique(data$grouping)) > 1

  # Determine format function
  format_as <- function(x) format(round(x, 2), nsmall = 2, big.mark = ",", scientific = F)
  format_as_axis <- function(x) format(round(x, 0), nsmall = 0, big.mark = ",", scientific = F)
  
  # Check if original_col_of_interest is available
  group_base_data_by <- c("period", "grouping", "col_of_interest")
  if ("original_col_of_interest" %in% colnames(data)) {
    group_base_data_by <- c(group_base_data_by, "original_col_of_interest")
  }
  
  # Remove col_of_interest if original_col_of_interest is NA
  if ("original_col_of_interest" %in% colnames(data)) {
    data <- data %>% 
      dplyr::mutate(
        col_of_interest = ifelse(
          test = is.na(original_col_of_interest),
          yes = NA,
          no = col_of_interest
        )
      )
  }
  
  # Aggregate original data
  base_data <- data %>%
    dplyr::mutate(
      anomaly_det_alg = ifelse(
        test = anomaly_flag == 1,
        yes = paste0("- ", anomaly_det_alg),
        no = NA
      )
    ) %>% 
    dplyr::group_by(!!! syms(group_base_data_by)) %>% 
    dplyr::summarise(
      anomaly_flags = sum(anomaly_flag),
      detected_by = paste0(na.omit(anomaly_det_alg), collapse = "<br>")
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::distinct()

  # Filter outlier data
  if (multiple_groupings) {
    outlier_data <- data %>% 
      dplyr::select(period, col_of_interest, anomaly_det_alg, anomaly_flag, grouping) %>% 
      dplyr::filter(anomaly_flag > 0)
  } else {
    outlier_data <- data %>% 
      dplyr::select(period, col_of_interest, anomaly_det_alg, anomaly_flag) %>% 
      dplyr::filter(anomaly_flag > 0)
  }

  # Create linetypes for plot
  linetypes <- setNames(
    object = c("dotted", "solid", "dashed"),
    nm = c("original_value", "cleaned_value", "forecast")
  )
  
  # Create colours for plot
  algorithms <- sort(unique(data$anomaly_det_alg))
  colors <- tstools::get_plot_colors(n_colors = length(algorithms))
  colors <- setNames(c(colors, "#767676"), c(algorithms, "forecast confidence"))
  
  # Tooltip
  tooltip_text <- paste0("
    paste0(
      '<br>Period: ', format.Date(period, '%Y-%m'),
      '<br>Value: ', format_as(col_of_interest),
      '<br>',
      '<br>', ifelse(anomaly_flags > 0, 'Flagged by:', ''),
      '<br>', ifelse(anomaly_flags > 0, detected_by, '')
    )
  ")
  
  # Create emtpy plot
  plot <- ggplot2::ggplot()
  # Add original data if available
  if ("original_col_of_interest" %in% colnames(data)) {
    plot <- plot +
      ggplot2::geom_line(
        data = base_data,
        mapping = ggplot2::aes(
          x = period,
          y = original_col_of_interest,
          linetype = "original_value"
        )
      )
  }
  # Add cleaned data
  plot <- plot +
    ggplot2::geom_line(
      data = base_data,
      mapping = ggplot2::aes(
        x = period,
        y = col_of_interest,
        linetype = "cleaned_value"
      )
    )
  # Add lower and upper bounds
  plot <- plot +
    ggplot2::geom_ribbon(
      data = data %>% 
        dplyr::filter(!is.na(anomaly_det_alg)), 
      mapping = ggplot2::aes(
        x = period, 
        ymin = anomaly_lower_bound, 
        ymax = anomaly_upper_bound,
        color = anomaly_det_alg,
        fill = anomaly_det_alg
      ), 
      linetype = 2, 
      alpha = 0.2
    ) +
    ggplot2::scale_color_manual(values = colors) + 
    ggplot2::scale_fill_manual(values = colors)
  
  # Add highlighted points
  if (multiple_groupings) {
    point_size <- 5
  } else {
    point_size <- 10
  }
  plot <- plot +
    ggplot2::geom_point(
      data = outlier_data,
      mapping = ggplot2::aes(
        x = period,
        y = col_of_interest,
        color = anomaly_det_alg
      ), 
      size = point_size, 
      alpha = 0.5
    )

  # Add style
  plot <- plot +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(),
      labels = format_as_axis
    ) +
    ggthemes::theme_calc() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank()
    )
  
  # Add tooltip
  plot <- suppressWarnings(
    plot +
      ggplot2::geom_point(
        data = base_data,
        ggplot2::aes_string(
          x = "period",
          y = "col_of_interest",
          text = tooltip_text
        ), 
        size = 0.5
      )
  )

  # Add forecast for single group
  if (!multiple_groupings) {
    # Define fc_length
    fc_length <- 12
    # Filter time series data
    ts_data <- data %>%
      dplyr::select(period, col_of_interest, grouping) %>% 
      dplyr::distinct() %>% 
      tstools::transform_data_to_ts_object()
    # Create forecast dates
    fc_dates <- max(data$period) %>% 
      date_to_period() %>% 
      period_delta(2) %>% 
      period_to_first_day() %>% 
      seq(by = "month", length.out = fc_length) - 1
    # Create forecast using only fast hybrid models
    forecasts <- ts_data %>% 
      forecastHybrid::hybridModel(
        models = "sz", # a, n & t are slow ... :(
        verbose = F
      ) %>% 
      forecast::forecast(h = fc_length) %>% 
      tibble::as_tibble() %>%
      dplyr::mutate(period = fc_dates)
    # Add forecast data to plot
    plot <- plot +
      ggplot2::geom_ribbon(
        data = forecasts,
        ggplot2::aes_string(
          x = "period", 
          ymin = "`Lo 95`",
          ymax = "`Hi 95`", 
          alpha = 0.4321,
          fill = "'forecast confidence'"
        )
      ) +
      ggplot2::geom_line(
        data = forecasts,
        ggplot2::aes_string(
          x = "period",
          y = "`Point Forecast`",
          linetype = "'forecast'"
        )
      ) +
      ggplot2::scale_linetype_manual(values = linetypes) 
  }

  # Adds more features to a plot with only one grouping, but does not use them for multiple groupings.
  # With multiple groupings, tooltips and legends will be too detailed and slow to generate.
  if (!multiple_groupings) { 
    # Get required y-axis range
    values <- base_data[["col_of_interest"]]
    if (exists("forecasts")) {
      values <- c(values, forecasts[["Lo 95"]], forecasts[["Hi 95"]])
    }
    # Determine y-axis range and values
    y_axis_range <- max(values) - min(values)
    y_axis_min <- min(values) - (0.1 * y_axis_range)
    y_axis_max <- max(values) + (0.1 * y_axis_range)
    # Adds the title
    plot <- plot + ggplot2::ggtitle(unique(data$grouping))
    # Transform to plotly
    plot <- plot %>% 
      plotly::ggplotly(tooltip = "text") %>% 
      plotly::layout(
        xaxis = list(
          fixedrange = T
        ),
        yaxis = list(
          range = list(
            y_axis_min, 
            y_axis_max
          ),
          tickmode = "auto",
          nticks = 5
        ),
        legend = list(
          x = 100, 
          y = 0.5
        )
      )
    # Overwrite legend names
    for (i in 1:length(plot$x$data)) {
      # Clean the legend name
      new_name <- plot$x$data[[i]]$name %>% 
        gsub(",NA)", "", .) %>% 
        gsub("_", " ", .) %>% 
        gsub("[^A-Za-z ]", "", .)
      # Overwrite the legend name
      plot$x$data[[i]]$legendgroup <- new_name
      plot$x$data[[i]]$name <- new_name
    }
  } else {
    # Number of columns to use with the facet grid
    n_cols <- 2
    # Remove legend from plots
    plot <- plot +
      ggplot2::theme(
        legend.position = "none"
      ) 
    # Adds the facets and corrects spacing.
    plot <- plot + 
      ggplot2::facet_wrap(
        facets = vars(grouping), 
        ncol = n_cols, 
        scales = 'free'
      )
    # Each row gets 150px height
    n_groups <- length(unique(data$grouping))
    height <- 150 * ceiling(n_groups/n_cols) 
    # Transform to plotly
    plot <- plot %>% 
      plotly::ggplotly(
        height = height, 
        tooltip = "text"
      )
  }
  
  # Return plot
  return(plot)
}