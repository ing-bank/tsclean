
# Code for (un)collapsing boxes
js_collapse_box <- "
  shinyjs.collapse = function(boxid) {
    $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
  }
"
is_box_collapsed <- function(inputId, boxId) {
  tags$script(
    sprintf(
      "$('#%s').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('%s', true);})",
      boxId, inputId
    ),
    sprintf(
      "$('#%s').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('%s', false);})",
      boxId, inputId
    )
  )
}

# Create header
header <- shinydashboard::dashboardHeader(
  title = div(
    img(
      src = "white_space.png",
      height = 35,
      width = 110,
      style = "
        margin-top: -10px; 
        padding: 5px 5px 5px 5px; 
      "
    )
  ),
  titleWidth = 350
)

# Create sidebar
sidebar <- shinydashboard::dashboardSidebar(
  width = 350,
  # Source ui for dashboard sidebar
  source('sidebar_ui.R', local = T)[1]
)

# Create body
body <- shinydashboard::dashboardBody(
  # Makes the side bar stay on the page and the scrolling with all the plots stay only for the plots.
  tags$head(
    tags$style(
      HTML('
        .content-wrapper {
          overflow: auto;
        }
      ')
    )
  ),
  # Use style from css file
  tags$head(
    tags$link(
      rel = "stylesheet", type = "text/css", href = "style.css"
    )
  ),
  # Enable shinyjs
  shinyjs::useShinyjs(),
  # Add in js code for collapsing boxes
  shinyjs::extendShinyjs(text = js_collapse_box),
  # Source ui for dashboard body per tab
  shinydashboard::tabItems(
    shinydashboard::tabItem(
      tabName = "ADJ_SET",
      source('adjust_settings_ui.R', local = T)[1]
    ),
    shinydashboard::tabItem(
      tabName = "VIEW_HY",
      source('view_hierarchy_ui.R', local = T)[1]
    ),
    shinydashboard::tabItem(
      tabName = "ANOM_DET",
      source('anomaly_detection_ui.R', local = T)[1]
    ),
    shinydashboard::tabItem(
      tabName = "ANOM_ALL",
      source('anomaly_all_ui.R', local = T)[1]
    )
  )
)

# Create dashboard
shiny::shinyUI(
  shinydashboard::dashboardPage(
    header = header, 
    sidebar = sidebar, 
    body = body,
    title = "Data Cleaning Dashboard",
    skin = "blue"
  )
)
