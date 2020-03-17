
# Function that defines the shiny server
shiny::shinyServer(function(input, output, session) {
  
  # Main server functions
  source('main_server.R',local = TRUE)
  
  # Server functions for the first tab
  source('adjust_settings_server.R',local = TRUE)
  # Server functions for the second tab
  source('view_hierarchy_server.R',local = TRUE)
  # Server functions for the third tab
  source('anomaly_detection_server.R',local = TRUE)
  # Server functions for the fourth tab
  source('anomaly_all_server.R',local = TRUE)
  
  # Stop server upon closing the browser tab
  session$onSessionEnded(stopApp)
  
})
