
#### BUTTON EVENTS ####

# Observe event of recalculation button
observeEvent(
  eventExpr = input$MAIN_recalculate,
  handlerExpr = {
    # Overwrite anomaly detection parameters
    MAIN_anomaly_detection_settings$parameters <- list(
      algorithms = input$ADJ_SET_algo,
      m_width = input$ADJ_SET_m_width,
      sd_cutoff = input$ADJ_SET_sd_cutoff,
      max_pct_diff = input$ADJ_SET_max_pct_diff,
      pct_m_width = input$ADJ_SET_pct_m_width,
      lof_threshold = input$ADJ_SET_lof_threshold,
      lof_s = input$ADJ_SET_lof_s,
      lof_m_width = input$ADJ_SET_lof_m_width
    )
  },
  priority = 1
)

# Reset settings to default values
observeEvent(
  eventExpr = input$MAIN_reset_to_default,
  handlerExpr = {
    # Reset ADJ_SET_m_width
    updateSliderInput(
      session = session,
      inputId = "ADJ_SET_m_width",
      value = app_data$default_m_width
    )
    # Reset ADJ_SET_sd_cutoff
    updateSliderInput(
      session = session,
      inputId = "ADJ_SET_sd_cutoff",
      value = app_data$default_sd_cutoff
    )
    # Reset ADJ_SET_max_pct_diff
    updateSliderInput(
      session = session,
      inputId = "ADJ_SET_max_pct_diff",
      value = app_data$default_max_pct_diff
    )
    # Reset ADJ_SET_pct_m_width
    updateSliderInput(
      session = session,
      inputId = "ADJ_SET_pct_m_width",
      value = app_data$default_pct_m_width
    )
    # Reset ADJ_SET_lof_threshold
    updateSliderInput(
      session = session,
      inputId = "ADJ_SET_lof_threshold",
      value = app_data$default_lof_threshold
    )
    # Reset ADJ_SET_lof_s
    updateSliderInput(
      session = session,
      inputId = "ADJ_SET_lof_s",
      value = app_data$default_lof_s
    )
    # Reset ADJ_SET_lof_m_width
    updateSliderInput(
      session = session,
      inputId = "ADJ_SET_lof_m_width",
      value = app_data$default_lof_m_width
    )
    # Fire trigger to recalculate
    shinyjs::click(id = "MAIN_recalculate")
  }
)
