
fluidPage(
  tags$style("#shiny-notification-error_loading_file {font-size: 20px;}"),
  shinycssloaders::withSpinner(
    plotly::plotlyOutput(
      outputId = 'ANOM_DET_all_plot'
    ),
    type = 6,
    color = "#FF6200"
  )
)
