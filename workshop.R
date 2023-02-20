library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(jsonlite)


weddingGuests <- rgdal::readOGR("https://raw.githubusercontent.com/mgermaine93/wedding-guest-map/master/constants/guests.geojson")

leaflet(data = weddingGuests) %>%
  addTiles() %>%
  setView() %>%
  addMarkers(
    lng = weddingGuests$coords.x1,
    lat = weddingGuests$coords.x2,
    clusterOptions = markerClusterOptions()
  )
  
  





# addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))

# data <- fromJSON("data/guests.geojson")
# weddingGuests <- cbind(data$features$properties, data$features$geometry)


# r_colors <- rgb(t(col2rgb(colors()) / 255))
# names(r_colors) <- colors()
# 
# ui <- fluidPage(
#   
#   # this is basically equivalent to "plotOutput" in the UI
#   leafletOutput("mymap"),
#   p(),
#   actionButton("recalc", "New points")
# )
# 
# server <- function(input, output, session) {
#   
#   points <- eventReactive(
#     # when the "recalc" button is clicked...
#     input$recalc, {
#     
#     cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
#   }, ignoreNULL = FALSE)
#   
#   # this is basically equivalent to "renderPlot" in the server
#   output$mymap <- renderLeaflet({
#     leaflet() %>%
#       addProviderTiles(
#         providers$Stamen.TonerLite,
#         options = providerTileOptions(
#           noWrap = TRUE
#           )
#       ) %>%
#       addMarkers(
#         data = points()
#       )
#   })
# }
# 
# shinyApp(ui, server)
