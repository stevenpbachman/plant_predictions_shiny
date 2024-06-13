# Load necessary libraries
library(shiny)
library(DT)
library(dplyr)
library(bslib)

# Load your data
predictions <- read.csv("predictions.csv")
predictions <- predictions[, -1]

ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,  # Use Bootstrap 5
    primary = "darkgreen",#007bff",  # Custom primary color
    lightness = 0.9  # Adjust lightness of primary color
  ),
  titlePanel("Plant predictions filter"),
  sidebarLayout(
    sidebarPanel(
      selectInput('redlist_filter', 'Red List status:', choices = c("All", unique(predictions$red_list))),
      selectInput('prediction_filter', 'Predicted threat:', choices = c("All", unique(predictions$prediction))),
      selectInput('confidence_filter', 'Prediction confidence:', choices = c("All", unique(predictions$confidence))),
      selectInput('family_filter', 'Family:', choices = c("All", unique(predictions$family))),
      selectInput('genus_filter', 'Genus:', choices = c("All", unique(predictions$genus))),
      selectInput('continent_filter', 'Continent:', choices = c("All", unique(predictions$continent))),
      selectInput('region_filter', 'Region:', choices = c("All", unique(predictions$region))),
      selectInput('area_filter', 'Botanical country:', choices = c("All", unique(predictions$area))),
      selectInput('lf_filter', 'Life form:', choices = c("All", unique(predictions$lifeform))),
      
      downloadButton('downloadFiltered', 'Download Filtered Table')
      #actionButton('reset_filters', 'Reset Filters')  # Add reset button
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  # ReactiveValues to store the filtered data
  rv <- reactiveValues(filtered_data = predictions)
  
  # Helper function to update selectInput choices
  update_select_input <- function(input_id, choices, selected_value) {
    updateSelectInput(session, input_id, choices = choices, selected = selected_value)
  }
  
  # Reactive expression to filter the data based on user inputs
  filtered_data <- reactive({
    df <- predictions
    
    # Filter based on red_list
    if (input$redlist_filter != "All") {
      df <- df %>% filter(red_list == input$redlist_filter)
    }
    
    # Filter based on prediction
    if (input$prediction_filter != "All") {
      df <- df %>% filter(prediction == input$prediction_filter)
    }
    
    # Filter based on confidence
    if (input$confidence_filter != "All") {
      df <- df %>% filter(confidence == input$confidence_filter)
    }
    
    # Filter based on family
    if (input$family_filter != "All") {
      df <- df %>% filter(family == input$family_filter)
    }
    
    # Filter based on genus (dependent on family)
    if (input$genus_filter != "All") {
      df <- df %>% filter(genus == input$genus_filter)
    } else if (input$family_filter != "All") {
      df <- df %>% filter(family == input$family_filter)
    }
    
    # Filter based on continent
    if (input$continent_filter != "All") {
      df <- df %>% filter(continent == input$continent_filter)
    }
    
    # Filter based on region (dependent on continent)
    if (input$region_filter != "All") {
      df <- df %>% filter(region == input$region_filter)
    } else if (input$continent_filter != "All") {
      df <- df %>% filter(continent == input$continent_filter)
    }
    
    # Filter based on area (dependent on region)
    if (input$area_filter != "All") {
      df <- df %>% filter(area == input$area_filter)
    } else if (input$region_filter != "All") {
      df <- df %>% filter(region == input$region_filter)
    }
    
    # Filter based on lifeform
    if (input$lf_filter != "All") {
      df <- df %>% filter(lifeform == input$lf_filter)
    }
    
    df
  })
  
  observe({
    rv$filtered_data <- filtered_data()
    
    # Update select inputs based on current filtered data
    df <- rv$filtered_data
    
    update_select_input("redlist_filter", c("All", unique(df$red_list)), input$redlist_filter)
    update_select_input("prediction_filter", c("All", unique(df$prediction)), input$prediction_filter)
    update_select_input("confidence_filter", c("All", unique(df$confidence)), input$confidence_filter)
    update_select_input("family_filter", c("All", unique(df$family)), input$family_filter)
    update_select_input("genus_filter", c("All", unique(df$genus)), input$genus_filter)
    update_select_input("continent_filter", c("All", unique(df$continent)), input$continent_filter)
    update_select_input("region_filter", c("All", unique(df$region)), input$region_filter)
    update_select_input("area_filter", c("All", unique(df$area)), input$area_filter)
    update_select_input("lf_filter", c("All", unique(df$lifeform)), input$lf_filter)
  })
  
  # # Reset button logic
  # observeEvent(input$reset_filters, {
  #   # Reset all select inputs to "All"
  #   update_select_input("redlist_filter", c("All", unique(predictions$red_list)), "All")
  #   update_select_input("prediction_filter", c("All", unique(predictions$prediction)), "All")
  #   update_select_input("confidence_filter", c("All", unique(predictions$confidence)), "All")
  #   update_select_input("family_filter", c("All", unique(predictions$family)), "All")
  #   update_select_input("genus_filter", c("All", unique(predictions$genus)), "All")
  #   update_select_input("continent_filter", c("All", unique(predictions$continent)), "All")
  #   update_select_input("region_filter", c("All", unique(predictions$region)), "All")
  #   update_select_input("area_filter", c("All", unique(predictions$area)), "All")
  #   update_select_input("lf_filter", c("All", unique(predictions$lifeform)), "All")
  #   
  #   # Reset filtered data to original predictions
  #   rv$filtered_data <- predictions
  # })
  
  output$table <- renderDT({
    datatable(rv$filtered_data, filter = 'none', options = list(
      dom = 'rtip',  # Remove the 'B' from 'Bfrtip' to exclude buttons
      rownames = FALSE
    ))
  }, server = TRUE)
  
  output$downloadFiltered <- downloadHandler(
    filename = function() {
      paste("filtered_table", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$filtered_data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
