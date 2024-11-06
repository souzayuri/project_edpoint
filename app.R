library(shiny)
library(leaflet)
library(sf)
library(dplyr)

ui <- fluidPage(
  titlePanel("Coordinate Editor"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = ".csv"),
      downloadButton("download", "Download Updated Data")
    ),
    mainPanel(
      leafletOutput("map"),
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data object for storing the original sf data and updated locations
  data <- reactiveVal()
  
  # Observe file upload and read data
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath)
    
    # Convert to an sf object
    sf_data <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
    data(sf_data)
    
    # Render the initial leaflet map
    output$map <- renderLeaflet({
      leaflet(sf_data) %>%
        addTiles() %>%
        addMarkers(
          layerId = ~Species,
          label = ~paste(Species, Place),
          options = leaflet::markerOptions(draggable = TRUE)
        )
    })
  })
  
  # Update coordinates based on map_marker_dragend input
  observeEvent(input$map_marker_dragend, {
    event <- input$map_marker_dragend
    req(event)  # Ensure event is not NULL
    sf_data <- data()
    
    # Check if the Species ID matches the dragged point
    sf_data <- sf_data %>%
      mutate(
        Longitude = if_else(Species == event$id, event$lng, Longitude),
        Latitude = if_else(Species == event$id, event$lat, Latitude),
        geometry = if_else(Species == event$id, st_sfc(st_point(c(event$lng, event$lat)), crs = 4326), geometry)
      )
    
    # Update the reactive value to store the modified sf_data with new coordinates
    data(sf_data)
    
    # Re-render the map with updated coordinates
    leafletProxy("map", data = sf_data) %>%
      clearMarkers() %>%
      addMarkers(
        layerId = ~Species,
        label = ~paste(Species, Place),
        options = leaflet::markerOptions(draggable = TRUE)
      )
    
    # Output message to confirm update
    print(paste("Updated", event$id, "to new coordinates:", event$lat, event$lng))
  })
  
  # Show the updated table
  output$table <- renderTable({
    data() %>% st_drop_geometry()  # Drop geometry for display
  })
  
  # Download the updated data
  output$download <- downloadHandler(
    filename = function() {
      paste("updated_coordinates", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data() %>% st_drop_geometry(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
