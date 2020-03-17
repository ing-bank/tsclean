
fluidPage(

  br(),
  shinycssloaders::withSpinner(
    plotly::plotlyOutput(
      outputId = 'ANOM_DET_plot', 
      height = "400px"
    ),
    type = 6,
    color = "#FF6200"
  ),
  br(),
  
  shinydashboard::box(
    id = "ANOM_DET_changepoints",
    title = "Increase/decrease the value for a range of dates, based on changepoints in the series",
    collapsible = TRUE,
    collapsed = TRUE,
    width = 12,
    fluidRow(
      # Show date range and value input
      column(
        width = 6,
        fluidRow(
          column(
            width = 12,
            h5("Select range of dates to change", align = "center"),
            shinyWidgets::sliderTextInput(
              inputId = "ANOM_DET_changepoint_dates",
              label = NULL,
              choices = format.Date(app_data$periods, "%Y - %m"), 
              selected = c(
                format.Date(min(app_data$periods), "%Y - %m"),
                format.Date(max(app_data$periods), "%Y - %m")
              ),
              width = "100%",
              dragRange = T
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 8,
            h5("Specify value to increase/decrease the above date range by: ", align = "right")
          ),
          column(
            width = 4,
            textInput(
              inputId = "ANOM_DET_changepoint_delta",
              label = NULL,
              width = "80%",
              value = 0,
              placeholder = "Insert value"
            )
          )
        )
      ),
      # Column with buttons
      column(
        width = 2,
        h5("Apply change", align = "center"),
        actionButton(
          inputId = "ANOM_DET_apply_change",
          label = NULL,
          icon = icon("arrow-right",  lib = "glyphicon"),
          width = "80%"
        ),
        h5("Revert selected", align = "center"),
        actionButton(
          inputId = "ANOM_DET_revert_current",
          label = NULL,
          icon = icon("arrow-left",  lib = "glyphicon"),
          width = "80%"
        ),
        align = "center"
      ),
      # Show table with already applied deltas
      column(
        width = 4,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(
            outputId = "ANOM_DET_table_changepoints",
            width = "100%"
          ),
          type = 6,
          color = "#FF6200"
        )
      )
    )
  ),
  is_box_collapsed(inputId = "ANOM_DET_changepoints_is_collapsed", boxId = "ANOM_DET_changepoints"),
  
  shinydashboard::box(
    id = "ANOM_DET_anomalies",
    title = "Remove individual points as anomalies, to be replaced using seasonally adjusted interpolation",
    collapsible = TRUE,
    collapsed = TRUE,
    width = 12,
    # Show table with potential anomalies in box
    fluidRow(
      column(
        width = 6,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(
            outputId = "ANOM_DET_table_potential",
            width = "100%"
          ),
          type = 6,
          color = "#FF6200"
        )
      ),
      # Column with buttons
      column(
        width = 2,
        h5("Remove all", align = "center"),
        actionButton(
          inputId = "ANOM_DET_remove_all",
          label = NULL,
          icon = icon("ok-circle", lib = "glyphicon"),
          width = "80%"
        ),
        h5("Remove selected", align = "center"),
        actionButton(
          inputId = "ANOM_DET_remove_current",
          label = NULL,
          icon = icon("arrow-right",  lib = "glyphicon"),
          width = "80%"
        ),
        hr(),
        h5("Return all", align = "center"),
        actionButton(
          inputId = "ANOM_DET_return_all",
          label = NULL,
          icon = icon("remove-circle", lib = "glyphicon"),
          width = "80%"
        ),
        h5("Return selected", align = "center"),
        actionButton(
          inputId = "ANOM_DET_return_current",
          label = NULL,
          icon = icon("arrow-left",  lib = "glyphicon"),
          width = "80%"
        ),
        align = "center"
      ),
      # Show table with already removed anomalies in box
      column(
        width = 4,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(
            outputId = "ANOM_DET_table_removed",
            width = "100%"
          ),
          type = 6,
          color = "#FF6200"
        )
      )
    )
  ),
  is_box_collapsed(inputId = "ANOM_DET_anomalies_is_collapsed", boxId = "ANOM_DET_anomalies"),

  shinydashboard::box(
    id = "ANOM_DET_comments",
    title = "Comments",
    collapsible = TRUE,
    collapsed = TRUE,
    width = 12,
    # Show text box
    fluidRow(
      column(
        width = 6,
        shinyAce::aceEditor(
          outputId = "ANOM_DET_comment",
          fontSize = 16,
          cursorId = "cursor",
          height = "180px",
          highlightActiveLine = T
        )
      ),
      # Column with saving button
      column(
        width = 2,
        h5("Save comment", align = "center"),
        actionButton(
          inputId = "ANOM_DET_save_comment",
          label = NULL,
          icon = icon("ok-circle", lib = "glyphicon"),
          width = "80%"
        ),
        h5("Clear comment", align = "center"),
        actionButton(
          inputId = "ANOM_DET_clear_comment",
          label = NULL,
          icon = icon("remove-circle", lib = "glyphicon"),
          width = "80%"
        ),
        align = "center"
      ),
      column(
        width = 4
      )
    )
  ),
  is_box_collapsed(inputId = "ANOM_DET_comments_is_collapsed", boxId = "ANOM_DET_comments")
)
