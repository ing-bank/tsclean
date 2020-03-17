
#### DATA UPDATES ####

# Update selected grouping
ANOM_DET_selected_grouping <- reactive({
  # Get groups
  groups <- app_data$groupings %>% 
    dplyr::select(-grouping) %>% 
    colnames() %>% 
    paste0("ANOM_DET_select_single_grouping_", .)
  # Initialize grouping
  grouping <- c()
  # Only start when inputs are available
  for (group in groups) {
    # Requirement
    req(input[[group]])
    # Add filter
    grouping <- c(grouping, input[[group]])
  }
  # Parse grouping together and return
  grouping %>% 
    paste0(., collapse = "   &   ") %>% 
    return()
})

# Filter data for selected grouping
ANOM_DET_anomaly_data <- reactive({
  # Get anomaly data
  data <- MAIN_anomaly_data$data
  # Parse filters together
  selected_grouping <- ANOM_DET_selected_grouping()
  # Requirements
  req(nchar(selected_grouping) > 1)
  # Filter data based on grouping(s) and return
  data %>%
    dplyr::filter(grouping == selected_grouping) %>% 
    return()
})

# Filter data for selected grouping
ANOM_DET_anomaly_changepoint_data <- reactive({
  # Get anomaly data
  data <- MAIN_anomaly_changepoint_data$data
  # Parse filters together
  selected_grouping <- ANOM_DET_selected_grouping()
  # Requirements
  req(nchar(selected_grouping) > 1)
  # Filter data based on grouping(s) and return
  data %>%
    dplyr::filter(grouping == selected_grouping) %>% 
    dplyr::select(-grouping) %>% 
    dplyr::arrange(desc(from), desc(to), desc(delta)) %>% 
    return()
})

# Filter data for selected grouping
ANOM_DET_anomaly_detection_data <- reactive({
  withProgress(
    message = "Calculating anomalies",
    detail = "Get required data",
    expr = {
      # Get anomaly detection data
      anomaly_data <- ANOM_DET_anomaly_data()
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
            lof_threshold = parameters$lof_threshold,
            lof_s = parameters$lof_s,
            lof_m_width = parameters$lof_m_width
          )
        ) %>% 
        return()
    }
  )
})

# Update data on removed anomalies
ANOM_DET_removed_anomalies <- reactive({
  # Get anomaly data
  data <- ANOM_DET_anomaly_data()
  # Filter rows with removed anomalies
  data %>% 
    dplyr::filter(is_anomaly == 1) %>% 
    dplyr::transmute(
      period = period,
      value = round(col_of_interest, 2),
      original_value = round(original_col_of_interest, 2)
    ) %>% 
    dplyr::arrange(desc(period)) %>% 
    return()
})

# Update data on potential anomalies
ANOM_DET_potential_anomalies <- reactive({
  # Get outlier data
  data <- ANOM_DET_anomaly_detection_data()
  # Filter rows with potential anomalies
  data %>% 
    dplyr::filter(is_anomaly == 0) %>% 
    dplyr::group_by(period) %>% 
    dplyr::summarise(
      value = unique(round(col_of_interest, 2)),
      anomaly_flags = sum(anomaly_flag)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(period) %>% 
    return()
})


#### INITIALIZE SELECTIZES ####

# Update available selectInputs for groupings
output$MAIN_uiOutput_select_single_grouping <- renderUI({
  # Create tagList to put selectInputs in
  TagList_for_groupings <- tagList()
  # Get groups
  groups <- app_data$groupings %>% 
    dplyr::select(-grouping)
  # Requirement
  req(nrow(groups) > 0)
  # Add selectInput for every group
  for (group in colnames(groups)) {
    # Create clean choices
    clean_choices <- groups %>% 
      dplyr::pull(group) %>% 
      unique() %>%
      paste0(group, " = ", .)
    # Check if hierarchical dataset
    if (app_data$is_hierarchical) {
      if (group %in% comment(app_data$hierarchy$matrix)) {
        # Determine level column
        level_col <- paste0("level_", group)
        # Add dashes to the front of the choices
        hierarchical_choices <- groups %>% 
          dplyr::select(group) %>% 
          dplyr::distinct() %>% 
          # Re-arrange
          dplyr::arrange(match(!! sym(group), rownames(app_data$hierarchy$matrix))) %>% 
          # Put in level information
          dplyr::left_join(
            x = .,
            y = app_data$hierarchy$data,
            by = group
          ) %>% 
          dplyr::rowwise() %>% 
          # Get dashes
          dplyr::mutate(dashes := paste0(rep('-', !! dplyr::sym(level_col)), collapse = '')) %>% 
          # Put dashes and hierarchy groups together
          dplyr::mutate(!! sym(group) := paste0(dashes, !! sym(group))) %>% 
          # Pull and organize
          dplyr::pull(group) %>% 
          unique() %>%
          paste0(group, " = ", .)
        # Overwrite clean choices after re-arrange
        clean_choices <- groups %>% 
          dplyr::arrange(match(!! sym(group), rownames(app_data$hierarchy$matrix))) %>% 
          dplyr::pull(group) %>% 
          unique() %>%
          paste0(group, " = ", .)
        # Use setNames for the selectInput
        clean_choices <- setNames(
          object = clean_choices,
          nm = hierarchical_choices
        )
      }
    }
    # Define new selectInput
    new_selectInput <- selectInput(
      inputId = paste0("ANOM_DET_select_single_grouping_", group),
      label = NULL,
      choices = clean_choices,
      width = "100%"
    )
    # Append new selectInput to list of existing inputs
    TagList_for_groupings <- tagAppendChild(
      TagList_for_groupings,
      new_selectInput
    )
  }
  # Return updated list of inputs
  TagList_for_groupings
})


#### BUTTON EVENTS ####

# Apply current changepoint
observeEvent(
  eventExpr = input$ANOM_DET_apply_change,
  handlerExpr = {
    withProgress(
      message = "Applying changepoint delta",
      detail = "Get required data",
      expr = {
        # Requirements
        req(input$ANOM_DET_changepoint_dates)
        req(input$ANOM_DET_changepoint_delta)
        # Try to parse the value
        suppressWarnings(
          delta_value <- as.numeric(input$ANOM_DET_changepoint_delta)
        )
        # Try to eval a formula for simpler delta calculation
        if (is.na(delta_value)) {
          suppressWarnings(
            delta_value <- eval(parse(text = input$ANOM_DET_changepoint_delta))
          )
        }
        # Test if inserted value is numeric
        if (is.na(delta_value)  || (!is.numeric(delta_value))) {
          # Show notification
          showNotification(
            id = "ANOM_DET_invalid_value",
            ui = "The specified value is invalid, please specify a number or a formula! :(",
            duration = 5,
            closeButton = TRUE,
            type = "error"
          )
        } else {
          # Apply delta
          incProgress(0.5, detail = "Apply specified value")
          selected_dates <- as.numeric(gsub(" - ", "", input$ANOM_DET_changepoint_dates))
          changepoint_data <- tibble::tibble(
            period = seq.Date(
                from = tstools::period_to_first_day(min(selected_dates)),
                to = tstools::period_to_first_day(max(selected_dates)),
                by = "month"
              ) %>% 
              tstools::date_to_period() %>% 
              tstools::period_to_last_day(),
            grouping = ANOM_DET_selected_grouping(),
            delta = delta_value
          )
          # Add delta to MAIN_anomaly_changepoint_data
          new_changepoint_data <- changepoint_data %>% 
            dplyr::summarise(
              grouping = unique(grouping),
              from = min(period),
              to = max(period),
              delta = unique(delta)
            ) %>% 
            dplyr::bind_rows(
              MAIN_anomaly_changepoint_data$data
            ) %>% 
            dplyr::group_by(grouping, from, to) %>% 
            dplyr::summarise(
              delta = sum(delta)
            ) %>% 
            dplyr::ungroup()
          # Add delta to col_of_interest
          new_anomaly_data <- dplyr::left_join(
              x = MAIN_anomaly_data$data,
              y = changepoint_data,
              by = c("period", "grouping")
            ) %>% 
            dplyr::mutate(
              delta = ifelse(is.na(delta), 0, delta),
              col_of_interest = col_of_interest + delta,
              col_of_interest = ifelse(is_anomaly == 1, NA, col_of_interest),
              changepoint_delta = changepoint_delta + delta
            ) %>% 
            dplyr::select(-delta) %>% 
            add_missing_data() %>% 
            add_up_required_group_totals()
          # Overwrite reactive data
          incProgress(0.4, detail = "Finalize data")
          MAIN_anomaly_changepoint_data$data <- new_changepoint_data
          MAIN_anomaly_data$data <- new_anomaly_data
        }
      }
    )
  }
)

# Revert selected changepoints
observeEvent(
  eventExpr = input$ANOM_DET_revert_current,
  handlerExpr = {
    withProgress(
      message = "Reverting selected changepoints",
      detail = "Get required data",
      expr = {
        # Get applied changepoints data
        applied_changepoints <- ANOM_DET_anomaly_changepoint_data()
        # Requirements
        req(nrow(applied_changepoints) > 0)
        req(input$ANOM_DET_table_changepoints_rows_selected)
        
        # Filter based on selected rows
        incProgress(0.5, detail = "Replace adjusted data")
        # Get previous data
        anomaly_data <- MAIN_anomaly_data$data
        changepoint_data <- MAIN_anomaly_changepoint_data$data
        # Revert delta for every row
        for (i in input$ANOM_DET_table_changepoints_rows_selected) {
          # Filter data for selected row
          row_data <- applied_changepoints %>% 
            dplyr::slice(i)
          # Remove reverted changepoints
          changepoint_data <- row_data %>% 
            dplyr::mutate(
              grouping = ANOM_DET_selected_grouping()
            ) %>% 
            dplyr::anti_join(
              x = changepoint_data,
              y = .,
              by = c("grouping", "from", "to")
            )
          # Create data to revert changepoint
          revert_changepoint <- tibble::tibble(
            period = seq.Date(
              from = tstools::period_to_first_day(tstools::date_to_period(row_data$from)),
              to = tstools::period_to_first_day(tstools::date_to_period(row_data$to)),
              by = "month"
            ) %>% 
              tstools::date_to_period() %>% 
              tstools::period_to_last_day(),
            grouping = ANOM_DET_selected_grouping(),
            delta = -row_data$delta
          )
          # Revert delta from col_of_interest
          anomaly_data <- dplyr::left_join(
              x = anomaly_data,
              y = revert_changepoint,
              by = c("period", "grouping")
            ) %>% 
            dplyr::mutate(
              delta = ifelse(is.na(delta), 0, delta),
              col_of_interest = col_of_interest + delta,
              col_of_interest = ifelse(is_anomaly == 1, NA, col_of_interest),
              changepoint_delta = changepoint_delta + delta
            ) %>% 
            dplyr::select(-delta) %>% 
            add_missing_data()
        }
        # Add totals for non-hierarchy and non-unique groups
        anomaly_data <- anomaly_data %>% 
          add_up_required_group_totals()
        # Overwrite reactive data
        incProgress(0.4, detail = "Finalize data")
        MAIN_anomaly_changepoint_data$data <- changepoint_data
        MAIN_anomaly_data$data <- anomaly_data
      }
    )
  }
)

# Remove all potential anomalies
observeEvent(
  eventExpr = input$ANOM_DET_remove_all,
  handlerExpr = {
    withProgress(
      message = "Removing all anomalies",
      detail = "Get required data",
      expr = {
        # Get anomalies data
        anomalies <- ANOM_DET_potential_anomalies()
        # Requirements
        req(nrow(anomalies) > 0)
        # Filter based on selected rows
        incProgress(0.5, detail = "Fill missing data")
        new_data <- anomalies %>% 
          dplyr::filter(anomaly_flags > 0) %>% 
          dplyr::transmute(
            period = period,
            grouping = ANOM_DET_selected_grouping(),
            is_new_anomaly = 1
          ) %>% 
          dplyr::left_join(
            x = MAIN_anomaly_data$data,
            y = .,
            by = c("period", "grouping")
          ) %>% 
          dplyr::mutate(
            is_anomaly = ifelse(is.na(is_new_anomaly), is_anomaly, is_new_anomaly),
            col_of_interest = ifelse(is.na(is_new_anomaly), col_of_interest, NA)
          ) %>% 
          dplyr::select(-is_new_anomaly) %>% 
          add_missing_data() %>% 
          add_up_required_group_totals()
        # Overwrite reactive data
        incProgress(0.4, detail = "Finalize data")
        MAIN_anomaly_data$data <- new_data
      }
    )
  }
)

# Remove selected potential anomalies
observeEvent(
  eventExpr = input$ANOM_DET_remove_current,
  handlerExpr = {
    withProgress(
      message = "Removing selected anomalies",
      detail = "Get required data",
      expr = {
        # Get anomalies data
        anomalies <- ANOM_DET_potential_anomalies()
        # Requirements
        req(nrow(anomalies) > 0)
        req(input$ANOM_DET_table_potential_rows_selected)
        # Filter based on selected rows
        incProgress(0.5, detail = "Fill missing data")
        new_data <- anomalies %>% 
          dplyr::slice(input$ANOM_DET_table_potential_rows_selected) %>% 
          dplyr::transmute(
            period = period,
            grouping = ANOM_DET_selected_grouping(),
            is_new_anomaly = 1
          ) %>% 
          dplyr::left_join(
            x = MAIN_anomaly_data$data,
            y = .,
            by = c("period", "grouping")
          ) %>% 
          dplyr::mutate(
            is_anomaly = ifelse(is.na(is_new_anomaly), is_anomaly, is_new_anomaly),
            col_of_interest = ifelse(is.na(is_new_anomaly), col_of_interest, NA)
          ) %>% 
          dplyr::select(-is_new_anomaly) %>% 
          add_missing_data() %>% 
          add_up_required_group_totals()
        # Overwrite reactive data
        incProgress(0.4, detail = "Finalize data")
        MAIN_anomaly_data$data <- new_data
      }
    )
  }
)

# Return all potential anomalies
observeEvent(
  eventExpr = input$ANOM_DET_return_all,
  handlerExpr = {
    withProgress(
      message = "Returning all anomalies",
      detail = "Get required data",
      expr = {
        # Get removed anomalies data
        removed_anomalies <- ANOM_DET_removed_anomalies()
        # Requirements
        req(nrow(removed_anomalies) > 0)
        # Filter based on selected rows
        incProgress(0.5, detail = "Replace removed data")
        new_data <- removed_anomalies %>% 
          dplyr::transmute(
            period = period,
            grouping = ANOM_DET_selected_grouping(),
            is_new_anomaly = 0
          ) %>% 
          dplyr::left_join(
            x = MAIN_anomaly_data$data,
            y = .,
            by = c("period", "grouping")
          ) %>% 
          dplyr::mutate(
            is_anomaly = ifelse(is.na(is_new_anomaly), is_anomaly, is_new_anomaly),
            col_of_interest = ifelse(is.na(is_new_anomaly), col_of_interest, original_col_of_interest + changepoint_delta)
          ) %>% 
          dplyr::select(-is_new_anomaly) %>% 
          add_up_required_group_totals()
        # Overwrite reactive data
        incProgress(0.4, detail = "Finalize data")
        MAIN_anomaly_data$data <- new_data
      }
    )
  }
)

# Return selected potential anomalies
observeEvent(
  eventExpr = input$ANOM_DET_return_current,
  handlerExpr = {
    withProgress(
      message = "Returning selected anomalies",
      detail = "Get required data",
      expr = {
        # Get removed anomalies data
        removed_anomalies <- ANOM_DET_removed_anomalies()
        # Requirements
        req(nrow(removed_anomalies) > 0)
        req(input$ANOM_DET_table_removed_rows_selected)
        # Filter based on selected rows
        incProgress(0.5, detail = "Replace removed data")
        new_data <- removed_anomalies %>% 
          dplyr::arrange(period) %>% 
          dplyr::slice(input$ANOM_DET_table_removed_rows_selected) %>% 
          dplyr::transmute(
            period = period,
            grouping = ANOM_DET_selected_grouping(),
            is_new_anomaly = 0
          ) %>% 
          dplyr::left_join(
            x = MAIN_anomaly_data$data,
            y = .,
            by = c("period", "grouping")
          ) %>% 
          dplyr::mutate(
            is_anomaly = ifelse(is.na(is_new_anomaly), is_anomaly, is_new_anomaly),
            col_of_interest = ifelse(is.na(is_new_anomaly), col_of_interest, original_col_of_interest + changepoint_delta)
          ) %>% 
          dplyr::select(-is_new_anomaly) %>% 
          add_up_required_group_totals()
        # Overwrite reactive data
        incProgress(0.4, detail = "Finalize data")
        MAIN_anomaly_data$data <- new_data
      }
    )
  }
)

# Save current comment
observeEvent(
  eventExpr = input$ANOM_DET_save_comment,
  handlerExpr = {
    withProgress(
      message = "Saving comment",
      expr = {
        # Get comment data and grouping data
        comment_data <- MAIN_comment_data$data
        selected_grouping <- ANOM_DET_selected_grouping()
        selected_comment <- input$ANOM_DET_comment
        # Add in current comment
        comment_data <- comment_data %>% 
          dplyr::mutate(
            comment = dplyr::case_when(
              grouping == selected_grouping ~ selected_comment,
              TRUE ~ comment
            )
          )
        # Overwrite reactive data
        MAIN_comment_data$data <- comment_data
      }
    )
  }
)

# Clear current comment
observeEvent(
  eventExpr = input$ANOM_DET_clear_comment,
  handlerExpr = {
    withProgress(
      message = "Clearing comment",
      expr = {
        # Update
        shinyAce::updateAceEditor(
          session = session,
          editorId = "ANOM_DET_comment",
          value = "Insert comment here..."
        )
        # Get comment data and grouping data
        comment_data <- MAIN_comment_data$data
        selected_grouping <- ANOM_DET_selected_grouping()
        # Add in current comment
        comment_data <- comment_data %>% 
          dplyr::mutate(
            comment = dplyr::case_when(
              grouping == selected_grouping ~ "Insert comment here...",
              TRUE ~ comment
            )
          )
        # Overwrite reactive data
        MAIN_comment_data$data <- comment_data
      }
    )
  }
)


#### NOTIFICATIONS ####

# Show notifications if there is no data available
observe({
  # Update data
  data <- ANOM_DET_anomaly_data()
  # If current tab is selected and no data is available
  if (input$MAIN_menu == "ANOM_DET" & nrow(data) == 0) {
    # Show notification
    showNotification(
      id = "ANOM_DET_no_data_warning",
      ui = "The current combination of grouping(s) and model(s) does not have any data! :(",
      duration = NULL,
      closeButton = FALSE,
      type = "error"
    )
  } else {
    # Remove notification
    removeNotification(id = "ANOM_DET_no_data_warning")
  }
})

# Show notifications for non-hierarchical leaves
observe({
  # Check that there is hierarchical data
  if (app_data$is_hierarchical) {
    # Update data
    data <- ANOM_DET_anomaly_data()
    # Extract hierarchical columns
    hierarchical_cols <- comment(app_data$hierarchy$matrix)
    # Requirement
    req(nrow(data) > 0)
    # Extract the hierarchical group
    hierarchical_group <- data %>% 
      tstools::split_grouping_column() %>% 
      dplyr::select(hierarchical_cols) %>% 
      tstools::add_grouping_column(group_cols = hierarchical_cols) %>% 
      dplyr::pull(grouping)
    # Is it a parent?
    if (input$MAIN_menu == "ANOM_DET" && !hierarchical_group %in% colnames(app_data$hierarchy$matrix)) {
      # Show notification
      showNotification(
        id = "ANOM_DET_forbidden_hierarchical_group",
        ui = "
          The specified group does not correspond to an hierarchical leaf!
          Please note that anomalies can only be fixed at the lowest levels!
        ",
        duration = NULL,
        closeButton = FALSE,
        type = "warning"
      )
      # Overwrite settings for hierarchical group
      ANOM_DET_freeze_buttons$settings$hierarchical_group <- TRUE
    } else {
      # Remove notification
      removeNotification(id = "ANOM_DET_forbidden_hierarchical_group")
      # Overwrite settings for hierarchical group
      ANOM_DET_freeze_buttons$settings$hierarchical_group <- FALSE
    }
  }
})

# Show notifications for aggregation groups
observe({
  # Get aggregation group
  selected_grouping <- ANOM_DET_selected_grouping()
  # Check that there is an aggregation group
  if (input$MAIN_menu == "ANOM_DET" && grepl(" = ALL", selected_grouping)) {
    # Show nofitication
    showNotification(
      id = "ANOM_DET_forbidden_aggregated_group",
      ui = paste0("
        The specified group corresponds to an aggregation across group(s)!
        Please note that anomalies can only be fixed at non-aggregated levels!
      "),
      duration = NULL,
      closeButton = FALSE,
      type = "warning"
    )
    # Overwrite settings for aggregated group
    ANOM_DET_freeze_buttons$settings$aggregated_group <- TRUE
  } else {
    # Remove notification
    removeNotification(id = "ANOM_DET_forbidden_aggregated_group")
    # Overwrite settings for aggregated group
    ANOM_DET_freeze_buttons$settings$aggregated_group <- FALSE
  }
})


#### COLLAPSE BOXES ####

# Initialize reactive data for (un)collapsing boxes
ANOM_DET_reverted_boxes <- reactiveValues(
  boxes = list(
    ANOM_DET_changepoints = FALSE,
    ANOM_DET_anomalies = TRUE,
    ANOM_DET_comments = FALSE
  )
)
# Function to collapse box
collapse_box <- function(id) {
  is_collapsed <- input[[paste0(id, "_is_collapsed")]]
  if (!is.null(is_collapsed)) {
    if (!is_collapsed) {
      # Overwrite information
      ANOM_DET_reverted_boxes$boxes[[id]] <- TRUE
      # Collapse box
      shinyjs::js$collapse(id)
    }
  }
}
# Function to uncollapse box
uncollapse_box <- function(id) {
  to_revert <- ANOM_DET_reverted_boxes$boxes[[id]]
  if (to_revert) {
    # Overwrite information
    ANOM_DET_reverted_boxes$boxes[[id]] <- FALSE
    # Uncollapse box
    shinyjs::js$collapse(id)
  }
}


#### FREEZE INPUTS ####

# Initialize reactive data for freezing buttons
ANOM_DET_freeze_buttons <- reactiveValues(
  settings = list(
    hierarchical_group = FALSE,
    aggregated_group = FALSE
  )
)

# Update data on button inputs to (un)freeze
ANOM_DET_button_inputs_to_freeze <- reactive({
  # Get all inputs
  all_inputs <- isolate(MAIN_update_all_inputs())
  all_inputs <- names(all_inputs)
  # Filter buttons to freeze
  button_inputs <- all_inputs[grepl("ANOM_DET_", all_inputs)]
  button_inputs <- button_inputs[!grepl("_select_single_grouping_", button_inputs)]
  button_inputs <- button_inputs[!grepl("_table_", button_inputs)]
  # Return selected buttons
  return(button_inputs)
})

# Freeze inputs for non-hierarchical leaves and/or aggregation groups
observe({
  # Get vector of all buttons to (un)freeze
  button_inputs <- ANOM_DET_button_inputs_to_freeze()
  # Get current grouping settings
  settings <- ANOM_DET_freeze_buttons$settings
  # Check if either non-hierarchical leaves or aggregation group
  if (settings$hierarchical_group | settings$aggregated_group) {
    # Freeze buttons
    for (button in button_inputs) shinyjs::disable(button)
    # Collapse boxes, if uncollapsed
    collapse_box("ANOM_DET_changepoints")
    collapse_box("ANOM_DET_anomalies")
    collapse_box("ANOM_DET_comments")
  } else {
    # Unfreeze buttons
    for (button in button_inputs) shinyjs::enable(button)
    # Uncollapse boxes, if previously uncollapsed
    uncollapse_box("ANOM_DET_changepoints")
    uncollapse_box("ANOM_DET_anomalies")
    uncollapse_box("ANOM_DET_comments")
  }
})


#### OUTPUT UPDATES ####

# Create plot to detect anomalies
output$ANOM_DET_plot <- plotly::renderPlotly({
  # Update anomaly detection data for plotting
  plot_data <- ANOM_DET_anomaly_detection_data()
  # Requirements
  req(nrow(plot_data) > 0)
  # Create plot
  plot_anomalies(
    data = plot_data
  )
})

# Create DataTable with anomaly info
output$ANOM_DET_table_changepoints <- DT::renderDataTable({
  # Get removed anomalies data
  changepoints <- ANOM_DET_anomaly_changepoint_data() %>%
    dplyr::transmute(
      from = format(from, "%Y-%m"),
      to = format.Date(to, "%Y-%m"),
      delta = round(delta, 2)
    )
  # Use DT package for table
  DT::datatable(
    data = changepoints,
    extensions = c('FixedHeader','KeyTable','Scroller'),
    options = list(
      dom = 't',
      fixedHeader = TRUE,
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = 110,
      scroller = TRUE
    ),
    filter = 'none',
    rownames = FALSE
  )
})

# Create DataTable with anomaly info
output$ANOM_DET_table_potential <- DT::renderDataTable({
  # Get potential anomalies data
  anomalies <- ANOM_DET_potential_anomalies() %>% 
    dplyr::arrange(period) %>% 
    dplyr::mutate(
      period = format.Date(period, "%Y-%m")
    )
  # Use DT package for table
  DT::datatable(
    data = anomalies,
    extensions = c('FixedHeader','KeyTable','Scroller'),
    options = list(
      dom = 't',
      fixedHeader = TRUE,
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = 275,
      scroller = TRUE
    ),
    filter = 'none',
    rownames = FALSE
  )
})

# Create DataTable with anomaly info
output$ANOM_DET_table_removed <- DT::renderDataTable({
  # Get removed anomalies data
  removed_anomalies <- ANOM_DET_removed_anomalies() %>%
    dplyr::arrange(period) %>% 
    dplyr::mutate(
      period = format.Date(period, "%Y-%m")
    )
  # Use DT package for table
  DT::datatable(
    data = removed_anomalies,
    extensions = c('FixedHeader','KeyTable','Scroller'),
    options = list(
      dom = 't',
      fixedHeader = TRUE,
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = 275,
      scroller = TRUE
    ),
    filter = 'none',
    rownames = FALSE
  )
})

# Update comment box
observe(
  x = {
    # Take out current comment
    current_comment <- MAIN_comment_data$data %>% 
      dplyr::filter(grouping == ANOM_DET_selected_grouping()) %>% 
      dplyr::pull(comment)
    # Requirement
    req(nchar(current_comment) > 0)
    # Update
    shinyAce::updateAceEditor(
      session = session,
      editorId = "ANOM_DET_comment",
      value = current_comment 
    )
  }
)
