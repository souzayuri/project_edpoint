
# packages -------------------------------------------------------------------------


if (!require(shiny)) install.packages("shiny", dependencies = TRUE) 
if (!require(shinythemes)) install.packages("shinythemes", dependencies = TRUE) 
if (!require(leaflet)) install.packages("leaflet", dependencies = TRUE) 
if (!require(sf)) install.packages("sf", dependencies = TRUE) 
if (!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE) 
if (!require(janitor)) install.packages("janitor", dependencies = TRUE) 
if (!require(DT)) install.packages("DT", dependencies = TRUE)


# ui ----------------------------------------------------------------------


# Set maximum upload size to 1GB
options(shiny.maxRequestSize = 1000 * 1024^2)

ui <- shiny::fluidPage(
  shiny::tags$style(shiny::HTML("body { background-color: #eee9e3; }")),
  
  shiny::titlePanel("Coordinate Editor"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput("file", "Upload CSV", accept = ".csv"),
      shiny::downloadButton("download", "Download Updated Data")
    ),
    shiny::mainPanel(
      leaflet::leafletOutput("map"),
      DT::dataTableOutput("table")  # Use DT output here for a prettier table
    )
  )
)


# server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  # Load the shapefile from 06_biomas folder
  biome_shape <- sf::st_read("biomas/biomes.shp")
  palette <- leaflet::colorFactor(palette = "Set3", domain = biome_shape$name_biome)
  
  # Define reactive value for the data
  data <- shiny::reactiveVal()
  
  # Render initial leaflet map with shapefile polygons
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::addPolygons(
        data = biome_shape,
        color = ~palette(name_biome),
        weight = 1,
        fillOpacity = 0.5,
        label = ~name_biome
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        pal = palette,
        values = biome_shape$name_biome,
        title = "Biomes",
        opacity = 0.7
      )
  })
  
# Handle CSV upload and add markers to map
shiny::observeEvent(input$file, {
  shiny::req(input$file)
  
  # Read the first line to detect the separator
  first_line <- readLines(input$file$datapath, n = 1)
  
  # Choose separator based on presence of comma or semicolon
  sep <- if (grepl(";", first_line)) ";" else ","
  
  # Load the data with the correct separator
  df <- if (sep == ";") {
    utils::read.csv2(input$file$datapath, encoding = "UTF-8", stringsAsFactors = FALSE)
  } else {
    utils::read.csv(input$file$datapath, encoding = "UTF-8", stringsAsFactors = FALSE)
  }
  
  
  df[] <- lapply(df, function(col) base::iconv(col, from = "UTF-8", to = "UTF-8", sub = ""))
  
  df <- df %>%
    janitor::clean_names() %>% 
    dplyr::rename_with(~ tools::toTitleCase(.))
  
  # Add ID column only if it doesn't already exist
  if (!"ID" %in% colnames(df)) {
    df <- df %>% dplyr::mutate(ID = dplyr::row_number())
  }
  
  # Reorder to have ID as the first column
  df <- df %>% dplyr::select(ID, everything())
  
  sf_data <- sf::st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
  data(sf_data)
  
  # Prepare labels
  labels <- sf_data %>%
    dplyr::select(-Latitude, -Longitude) %>%
    apply(1, function(row) paste(names(row), row, sep = ": ", collapse = "<br>"))
  
  leaflet::leafletProxy("map", data = sf_data) %>%
    leaflet::clearMarkers() %>%
    leaflet::addMarkers(
      layerId = ~ID,
      label = ~lapply(labels, shiny::HTML),
      options = leaflet::markerOptions(draggable = TRUE),
      popup = ~paste("<strong>ID: </strong>", ID,
                     "<br><strong>Order: </strong>", Order,
                     "<br><strong>Family: </strong>", Family,
                     "<br><strong>Genus: </strong>", Genus,
                     "<br><strong>Species: </strong>", Species,
                     "<br><strong>Site: </strong>", Site,
                     "<br><strong>Reference: </strong>", Reference)
    )
})

  
  # Update coordinates when markers are dragged
  shiny::observeEvent(input$map_marker_dragend, {
    event <- input$map_marker_dragend
    shiny::req(event)
    sf_data <- data()
    
    sf_data <- sf_data |>
      dplyr::mutate(
        Longitude = as.numeric(Longitude),
        Latitude = as.numeric(Latitude)
      ) |>
      dplyr::mutate(
        Longitude = dplyr::if_else(ID == event$id, as.numeric(event$lng), Longitude),
        Latitude = dplyr::if_else(ID == event$id, as.numeric(event$lat), Latitude),
        geometry = dplyr::if_else(ID == event$id, sf::st_sfc(sf::st_point(c(as.numeric(event$lng), as.numeric(event$lat))), crs = 4326), geometry)
      )
    
    data(sf_data)
    
    labels <- sf_data |>
      dplyr::select(-Latitude, -Longitude) |>
      apply(1, function(row) paste(names(row), row, sep = ": ", collapse = "<br>"))
    
    leaflet::leafletProxy("map", data = sf_data) |>
      leaflet::clearMarkers() |>
      leaflet::addMarkers(
        layerId = ~ID,
        label = ~lapply(labels, shiny::HTML),
        options = leaflet::markerOptions(draggable = TRUE),
        popup = ~paste("<strong>ID: </strong>", ID,
                       "<br><strong>Order: </strong>", Order,
                       "<br><strong>Family: </strong>", Family,
                       "<br><strong>Genus: </strong>", Genus,
                       "<br><strong>Species: </strong>", Species,
                       "<br><strong>Site: </strong>", Site,
                       "<br><strong>Reference: </strong>", Reference)
      )
  })
  
  # Display updated table with DT for better styling
  output$table <- DT::renderDataTable({
    data() |> sf::st_drop_geometry()}, 
    options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
        "}"),
      pageLength = -1,
      autoWidth = TRUE,
      scrollX = TRUE,
      style = "bootstrap",
      dom = 'ftip'
      ),
    filter = "top" # Adds a filter row at the top of each column
    )
  
  # Download updated data
  output$download <- shiny::downloadHandler(
    filename = function() {
      paste("updated_coordinates", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(data() |> sf::st_drop_geometry(), file, row.names = FALSE)
    }
  )
}

shiny::shinyApp(ui, server)
