
#### NODE SELECTION UPDATES ####

# Update selected nodes when grouping is updated
observe({
  # Get groups
  groups <- app_data$groupings %>% 
    dplyr::select(-grouping) %>% 
    colnames()
  # Update selected node based on groups
  for (group in groups) {
    # Define selectize name
    selectize_name <- paste0("ANOM_DET_select_single_grouping_", group)
    # Extract value
    selectize_value <- input[[selectize_name]] %>% 
      gsub(group, "", .) %>% 
      gsub(" = ", "", .)
    # Define network name
    network_name <- paste0("VIEW_HY_", group)
    # Change selected node
    network_name %>% 
      visNetwork::visNetworkProxy() %>% 
      visNetwork::visSelectNodes(selectize_value)
  }
})

# Update grouping when a node is selected
observe({
  # Show each selected node
  for (hierarchical_col in app_data$hierarchical_cols) {
    # Get node value
    node_value <- input[[paste0("VIEW_HY_",hierarchical_col,"_node")]]
    # Continue if a node is selected
    if (!is.null(node_value)) {
      # Group to update
      group_to_update <- paste0("ANOM_DET_select_single_grouping_", hierarchical_col)
      # Get value to set
      value_to_set <- paste0(
        hierarchical_col, " = ", input[[paste0("VIEW_HY_",hierarchical_col,"_node")]]
      )
      # Update selectInput
      updateSelectInput(
        session = session,
        inputId = group_to_update,
        selected = value_to_set
      )
    }
  }
})


#### OUTPUT UPDATES ####

# Create visnetwork outputs for each hierarchical column
output$VIEW_HY_uiOutput_hierarchy <- renderUI({
  # Create tagList to put visnetworks in
  TagList_for_hierarchy <- tagList()
  # Loop over hierarchical columns
  for (hierarchical_col in app_data$hierarchical_cols) {
    # Determine network name
    name <- paste0("VIEW_HY_", hierarchical_col)
    # Define new selectInput
    new_visnetwork <- shinydashboard::box(
      title = hierarchical_col,
      visNetwork::visNetworkOutput(
        outputId = name,
        height = "350px"
      )
    )
    # Append new selectInput to list of existing inputs
    TagList_for_hierarchy <- tagAppendChild(
      TagList_for_hierarchy,
      new_visnetwork
    )
  }
  # Return updated list of inputs
  TagList_for_hierarchy
})

# Create visnetwork to render for each hierarchical column
for (hierarchical_col in app_data$hierarchical_cols) {
  # Use local because of this: https://gist.github.com/wch/5436415/
  local({
    # Copy it first, to make local work
    col <- hierarchical_col 
    # Determine network name
    name <- paste0("VIEW_HY_", col)
    # Overwrite visnetwork output
    output[[name]] <- visNetwork::renderVisNetwork(
      tstools::plot_hierarchy(
          hierarchy = app_data$hierarchy,
          hierarchical_col = col,
          interact = T
        ) %>% 
        visNetwork::visEvents(
          select = paste0("
            function(nodes) {
              Shiny.onInputChange('",name,"_node', nodes.nodes);
            ;}
          ")
        )
    )
  })
}
