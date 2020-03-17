
#### DATA UPDATES ####

# Get all selected anomalies
ANOM_ALL_detected_anomaly_data <- reactive({
  # Get anomaly data
  data <- MAIN_anomaly_data$data
  # Requirement
  req(nrow(data) > 0)
  # Filter only anomalies
  data <- data %>% 
    dplyr::filter(is_anomaly == 1 | changepoint_delta != 0) %>% 
    return()
})

# Prepare anomaly data to save
ANOM_ALL_detected_anomaly_data_to_save <- reactive({
  # Get filtered data
  data <- ANOM_ALL_detected_anomaly_data()
  # Requirement
  req(nrow(data) > 0)
  # Get rid of aggregation groups
  data <- data %>% 
    dplyr::filter(!grepl(" = ALL", grouping))
  # Get comment data
  comment_data <- MAIN_comment_data$data
  # Reformat data and return
  data %>% 
    dplyr::transmute(
      period = period,
      grouping = grouping,
      new_value = col_of_interest,
      changepoint_delta = changepoint_delta,
      is_anomaly = is_anomaly,
      anomaly_delta = round((new_value - original_col_of_interest - changepoint_delta), 2),
      original_value = original_col_of_interest
    ) %>% 
    tstools::split_grouping_column() %>% 
    dplyr::left_join(
      x = .,
      y = comment_data,
      by = "grouping"
    ) %>% 
    dplyr::select(-grouping) %>% 
    return()
})

# Prepare cleaned data to save
ANOM_ALL_cleaned_data_to_save <- reactive({
  # Get cleaned data
  data <- MAIN_anomaly_data$data
  # Requirement
  req(nrow(data) > 0)
  # Define columns to retain
  cols_to_keep <- c("period", "col_of_interest", "grouping", "original_col_of_interest")
  # Get rid of aggregation groups
  data <- data %>% 
    dplyr::filter(!grepl(" = ALL", grouping))
  # If data is hierarchical, then sum up
  if (app_data$is_hierarchical) {
    data <- data %>%
      add_up_hierarchical_values(hierarchy = app_data$hierarchy) %>%
      tstools::split_grouping_column() %>%
      dplyr::left_join(
        x = .,
        y = app_data$hierarchy$data %>% 
          dplyr::select(-grouping),
        by = comment(app_data$hierarchy$matrix)
      )
    # Extend columns to retain
    level_cols <- colnames(data)[grepl("level_", colnames(data))]
    cols_to_keep <- c(cols_to_keep, level_cols)
  }
  # Reformat data and return
  data %>% 
    dplyr::select(cols_to_keep) %>% 
    return()
})

# Filter data for selected grouping
ANOM_ALL_anomaly_detection_data <- reactive({
  withProgress(
    message = "Calculating anomalies",
    detail = "Get required data",
    expr = {
      # Get anomaly detection data
      anomaly_data <- MAIN_anomaly_data$data
      # Requirements
      req(nrow(anomaly_data) > 0)
      # Get latest parameters
      parameters <- MAIN_anomaly_detection_settings$parameters
      # Use wrapper to detect anomalies
      detect_anomalies_within_dashboard(
          data = anomaly_data,
          algorithms = parameters$algorithms,
          parameters = list(
            m_width = parameters$m_width,
            sd_cutoff = parameters$sd_cutoff,
            max_pct_diff = parameters$max_pct_diff,
            pct_m_width = parameters$pct_m_width,
            iso_cut_off = parameters$iso_cut_off,
            iso_m_width = parameters$iso_m_width,
            lof_threshold = parameters$lof_threshold,
            lof_s = parameters$lof_s,
            lof_m_width = parameters$lof_m_width
          )
        ) %>% 
        return()
    }
  )
})


#### BUTTON EVENTS ####

# Button to deselect all current selected anomalies
observeEvent(input$MAIN_deselect_all_anomalies, {
  showNotification(
    ui = "Are you sure you want to deselect all anomalies?",
    action = actionButton('MAIN_certain_deselect_anomalies', 'Yes'),
    id = "no_MAIN_deselect_notification",
    duration = NULL,
    type = "warning"
  )
})

# Remove all current selected anomalies
observeEvent(
  eventExpr = input$MAIN_certain_deselect_anomalies,
  handlerExpr = {
    # Remove notification
    removeNotification(id = "no_MAIN_deselect_notification")
    # Remove all anomalies
    anomaly_data <- MAIN_anomaly_data$data %>% 
      dplyr::mutate(
        col_of_interest = original_col_of_interest,
        changepoint_delta = 0,
        is_anomaly = 0
      )
    # Overwrite reactive data
    MAIN_anomaly_data$data <- anomaly_data
    # Remove all changepoints
    changepoint_data <- MAIN_anomaly_changepoint_data$data %>% 
      dplyr::filter(FALSE)
    # Overwrite reactive data
    MAIN_anomaly_changepoint_data$data <- changepoint_data
    # Fire trigger to recalculate
    shinyjs::click(id = "MAIN_recalculate")
  }
)


#### (DE)ACTIVATE DOWNLOAD BUTTONS ####

# Anomaly data download button
observe({
  # Get data
  data <- ANOM_ALL_detected_anomaly_data()
  # Only create button if data is available
  if (nrow(data) == 0) {
    shinyjs::disable("MAIN_download_anomaly_data")
  } else {
    shinyjs::enable("MAIN_download_anomaly_data")
  }
})

      
#### SAVE AND LOAD DATA ####

# Download file handler for anomaly data
output$MAIN_download_anomaly_data <- downloadHandler(
  filename = function() {
    'saved_anomalies.csv'
  },
  content = function(file) {
    write.csv2(
      x = ANOM_ALL_detected_anomaly_data_to_save(), 
      file = file, 
      row.names = FALSE
    )
  }
)

# Download file handler for cleaned data
output$MAIN_download_cleaned_data <- downloadHandler(
  filename = function() {
    'cleaned_data.csv'
  },
  content = function(file) {
    write.csv2(
      x = ANOM_ALL_cleaned_data_to_save(), 
      file = file, 
      row.names = FALSE
    )
  }
)

# Read csv and assign anomalies from it
observeEvent(input$MAIN_upload_anomaly_data, {
  withProgress(
    message = "Uploading anomaly data",
    detail = "Specify csv file",
    expr = {
      # Requirement
      req(input$MAIN_upload_anomaly_data)
      # Try to read and parse the file
      tryCatch(
        {
          # Read anomalies and transform them to correct format
          incProgress(0.2, detail = "Load data from file")
          saved_anomalies <- load_saved_anomalies(input$MAIN_upload_anomaly_data$datapath)
          # Remove loaded anomalies and fill missing data
          incProgress(0.2, detail = "Remove anomalies")
          anomaly_data <- remove_saved_anomalies(
              data = MAIN_anomaly_data$data,
              anomalies = saved_anomalies
            ) %>% 
            add_missing_data()
          # Apply calculations
          incProgress(0.2, detail = "Recalculate")
          # Filter changepoint data
          changepoint_data <- anomaly_data %>% 
            dplyr::filter(changepoint_delta != 0)
          # Get changepoint data, if they exist
          if (nrow(changepoint_data) > 0) {
            changepoint_data <- changepoint_data %>% 
              dplyr::group_by(grouping) %>% 
              dplyr::arrange(period) %>% 
              dplyr::mutate(
                prev_changepoint_delta = dplyr::lag(changepoint_delta),
                episode = ifelse(
                  test = (changepoint_delta != prev_changepoint_delta | is.na(prev_changepoint_delta)),
                  yes = 1,
                  no = 0
                ),
                episode = cumsum(episode)
              ) %>% 
              dplyr::group_by(grouping, episode) %>% 
              dplyr::summarise(
                from = min(period),
                to = max(period),
                delta = unique(changepoint_delta)
              ) %>% 
              dplyr::ungroup() %>% 
              dplyr::select(-episode) %>% 
              dplyr::bind_rows(
                MAIN_anomaly_changepoint_data$data
              ) %>% 
              dplyr::group_by(grouping, from, to) %>% 
              dplyr::summarise(
                delta = sum(delta)
              ) %>% 
              dplyr::ungroup()
          } else {
            changepoint_data <- MAIN_anomaly_changepoint_data$data
          }
          # Get comment data, if they exist
          if ("comment" %in% colnames(saved_anomalies)) {
            comment_data <- saved_anomalies %>% 
              dplyr::select(grouping, comment) %>% 
              dplyr::distinct() %>% 
              dplyr::rename(new_comment = comment)
            # Add into existing comment data
            comment_data <- MAIN_comment_data$data %>% 
              # New comments
              dplyr::left_join(
                x = .,
                y = comment_data,
                by = "grouping"
              ) %>% 
              # Replace comment by new_comment where appropriate
              dplyr::mutate(
                comment = dplyr::case_when(
                  !is.na(new_comment) ~ new_comment,
                  TRUE ~ comment
                )
              ) %>% 
              dplyr::select(grouping, comment)
            # Overwrite
            MAIN_comment_data$data <- comment_data 
          }
          # Add totals for non-hierarchy and non-unique groups
          anomaly_data <- anomaly_data %>% 
            add_up_required_group_totals()
          # Overwrite reactive data
          incProgress(0.2, detail = "Finalize")
          MAIN_anomaly_changepoint_data$data <- changepoint_data
          MAIN_anomaly_data$data <- anomaly_data
          # Fire trigger to recalculate
          shinyjs::click(id = "MAIN_recalculate")
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          showNotification(
            id = "error_loading_file",
            ui = toString(e),
            duration = 15,
            type = "error"
          )
        }
      )
    }
  )
})


#### OUTPUT UPDATES ####

# Create plot to view all plots
output$ANOM_DET_all_plot <- plotly::renderPlotly({
  withProgress(
    message = "Prepare all plots",
    detail = "Get required data",
    expr = {
      # Update anomaly detection data for plotting
      plot_data <- ANOM_ALL_anomaly_detection_data()
      # Requirements
      req(nrow(plot_data) > 0)
      # Create plot
      incProgress(0.5, detail = "Create plots")
      plot <- plot_anomalies(
        data = plot_data
      )
      # Return plot
      incProgress(0.1, detail = "Finalize plots")
      plot
    }
  )
})
