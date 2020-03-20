
#### DEFINE DYNAMIC INPUTS ####

# Define which menu items to show
DYNAMIC_menu_items <- list()
# Add ui for the first tab
DYNAMIC_menu_items[[length(DYNAMIC_menu_items) + 1]] <- shinydashboard::menuItem(
  text = "Adjust settings", 
  tabName = "ADJ_SET", 
  icon = icon("cog")
)
# Add ui for the second item, if data is hierarchical
if (app_data$is_hierarchical) {
  DYNAMIC_menu_items[[length(DYNAMIC_menu_items) + 1]] <- shinydashboard::menuItem(
    text = "View Hierarchy", 
    tabName = "VIEW_HY", 
    icon = icon("sitemap")
  )
}
# Add uit for the third tab
DYNAMIC_menu_items[[length(DYNAMIC_menu_items) + 1]] <- shinydashboard::menuItem(
  text = "Anomaly detection", 
  tabName = "ANOM_DET", 
  icon = icon("line-chart"),
  selected = TRUE
)
# Add uit for the fourth tab
DYNAMIC_menu_items[[length(DYNAMIC_menu_items) + 1]] <- shinydashboard::menuItem(
  text = "All Detections", 
  tabName = "ANOM_ALL", 
  icon = icon("stream")
)


#### CREATE SIDEBAR MENU ####

# Using the dynamic inputs
shinydashboard::sidebarMenu(
  id = "MAIN_menu",
  
  # Add main title
  fluidRow(
    column(
      width = 12,
      align = "center",
      img(
        src = 'tsclean_name_logo.png', 
        height = "125px"
      )
    )
  ),

  # Line break
  hr(class = "thick"),
  # Indicitate tabs below
  h4("Select tabs", align = "center"),
  # Line break
  hr(class = "thick"),
  # Source the ui for each tab dynamically
  DYNAMIC_menu_items,

  # Line break
  hr(class = "thick"),
  
  # If tab is 'Adjust settings'
  conditionalPanel(
    condition = "input.MAIN_menu == 'ADJ_SET'",
    # Indicate tabs below
    h4("Adjust settings", align = "center"),
    # Line break
    hr(class = "thick"),
    # Row with selectInputs
    fluidRow(
      column(
        width = 12,
        align = "center",
        # Button to start recalculation
        actionButton(
          inputId = "MAIN_recalculate",
          label = " Apply adjusted settings",
          icon = icon("check-square"),
          style = "display: block; margin: 0 auto; width: 300px;color: black;"
        ),
        br(),
        # Button to reset settings to default values
        actionButton(
          inputId = "MAIN_reset_to_default",
          label = " Reset to default settings",
          icon = icon("refresh"),
          style = "display: block; margin: 0 auto; width: 300px;color: black;"
        )
      )
    )
  ),
  
  # If tab is 'Anomaly detection'
  conditionalPanel(
    condition = "input.MAIN_menu == 'ANOM_DET' | input.MAIN_menu == 'VIEW_HY'",
    # Indicate tabs below
    h4("Select group", align = "center"),
    # Line break
    hr(class = "thick"),
    # Row with selectInputs
    fluidRow(
      column(
        width = 10,
        align = "center",
        offset = 1,
        # Select single grouping to view
        uiOutput("MAIN_uiOutput_select_single_grouping")
      )
    )
  ),
  
  # If tab is 'All Anomalities'
  conditionalPanel(
    condition = "input.MAIN_menu == 'ANOM_ALL'",
    # Indicate tabs below
    h4("Store results", align = "center"),
    # Line break
    hr(class = "thick"),
    # Row with buttons
    fluidRow(
      column(
        width = 12,
        align = 'center',
        # Button to download cleaned data
        downloadButton(
          outputId = "MAIN_download_cleaned_data",
          label = "Download cleaned data", 
          style = "display: block; margin: 0 auto; width: 300px;color: black;"
        ),
        br(),
        # Button to deselect all anomalies
        actionButton(
          inputId = "MAIN_deselect_all_anomalies", 
          label = "Deselect all anomalies",
          icon = icon("remove"), 
          style = "display: block; margin: 0 auto; width: 300px;color: black;"
        ),
        br(),
        h5("Upload anomaly data"),
        # Button to upload anomaly data
        fileInput(
          inputId = "MAIN_upload_anomaly_data", 
          label = NULL, 
          multiple = FALSE, 
          accept = c(".csv"),
          width = "300px",
          placeholder = "Upload anomaly data"
        ),
        br(),
        # Button to download anomaly data
        downloadButton(
          outputId = "MAIN_download_anomaly_data",
          label = "Download anomalies",
          style = "display: block; margin: 0 auto; width: 300px;color: black;"
        )
      )      
    )
  )
  
)
