library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(jsonlite)
library(st)

require(sf)
require(leaflet)
require(leaflet.extras)
require(dplyr)
require(readxl)
require(stringr)


# weddingGuests <- st_read("https://raw.githubusercontent.com/mgermaine93/wedding-guest-map/master/constants/guests.geojson")

weddingGuests <- st_read("data/guests.geojson")

ui <- navbarPage(
  
  "Wedding Guest Map",
  
  mainPanel(
    leafletOutput("leaflet")
  )
  
)

server <- function(input, output) {
  
  output$leaflet <- renderLeaflet(
    
    map <- leaflet(data = weddingGuests) %>%
      addTiles() %>%
      setView(
        lng = -98.583,
        lat = 39.833,
        zoom = 3
      ) %>%
      addMarkers(
        weddingGuests$coords,
        popup = ~as.character(popupContent),
        clusterOptions = markerClusterOptions()
      )
  
  )
  
}

shinyApp(ui = ui, server = server)