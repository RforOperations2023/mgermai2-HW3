library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(jsonlite)
library(st)
library(shinydashboard)
require(sf)
require(leaflet)
require(leaflet.extras)
require(dplyr)
require(readxl)
require(stringr)
require(shinythemes)
library(shinyjs)
library(rgeos)


# weddingGuests <- st_read("https://raw.githubusercontent.com/mgermaine93/wedding-guest-map/master/constants/guests.geojson")

weddingGuests <- st_read("data/guests.geojson")





# ui part begins here
ui <- dashboardPage(
  
  skin = "green",
  
  # sets the theme/coloring of the app
  # theme = shinytheme("cerulean"),
  
  
  # sets the title of the app
  dashboardHeader(
    title = "Meredith and Matt's Wedding Guest Map",
    titleWidth = 400
  ),
  
  
  
  # user input part
  dashboardSidebar(
    
    width = 400,
    
    sidebarMenu(
      id = "tabs",
      # the first "page" of the app, seen in the upper left of the app
      # it corresponds with the tabItem that has the same tabName value as this one.
      menuItem(
        "Map of Guests", 
        tabName = "leaflet",
        icon = icon("map")
      ),
      # the second "page" of the app, seen in the upper left of the app
      # it corresponds with the tabItem that has the same tabName value as this one.
      menuItem(
        "Wedding Guests by Generational Age", 
        tabName = "plotByAge",
        icon = icon("chart-column")
      ),
      # the third "page" of the app, seen in the upper left of the app
      # it corresponds with the tabItem that has the same tabName value as this one
      menuItem(
        "Wedding Guests by State", 
        tabName = "plotByState",
        icon = icon("chart-column")
      ),
      
      selectizeInput(
        inputId = "guestTypeSelect",
        label = "Guest Type",
        choices = c(
          "Everyone",
          "Couple",
          "Invitees",
          "Attendees",
          "Bride's Side",
          "Groom's Side",
          "Couple's Side",
          "Wedding Party",
          "Bride's Family",
          "Groom's Family",
          "Family",
          "Friends",
          "Family Friends",
          "Pre-College Friends",
          "College Friends",
          "Post-College Friends",
          "Rehearsal Dinner Attendees",
          "Welcome Party Attendees",
          "Vendors",
          "Heard From (but not invited)"
        ),
        selected = c(
          "Couple",
          "Wedding Party"
        ),
        multiple = TRUE
        
      ),
      # as defined here:  https://caregiversofamerica.com/2022-generation-names-explained/
      checkboxGroupInput(
        inputId = "guestGenerationSelect",
        label = "Guest Generation",
        # might rename these labels later...
        choices = c(unique(sort(weddingGuests$generation)))
      ),
      selectizeInput(
        inputId = "guestStateSelect",
        label = "Guest State",
        choices = unique(sort(weddingGuests$state)),
        selected = c(
          "Michigan",
          "Ohio",
          "Pennsylvania"
        ),
        multiple = TRUE
      )
    )
  ),
    
  # this is the part that actually holds the plotted data
  dashboardBody(
      
    tabItems(
        
      tabItem(
          
        tabName = "leaflet",
        fluidRow(
          box(
            width = 12,
            leafletOutput(
              "leaflet",
              width = "100%",
              # might need to change this depending on the DT/tables/graphs, etc
              # https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
              height = "95vh"
            )
          )
        )
      )
    )
  )
)



server <- function(input, output) {
  
  # load in wedding guest filtered data
  output$leaflet <- renderLeaflet({
    
    # this is important!  otherwise, some kind of circuitous loading happens for some reason.
    weddingGuests <- weddingGuestsInputs()
    
    leaflet(data = weddingGuests) %>%
      addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "NatGeo", options = providerTileOptions(noWrap = TRUE)) %>%
      # addProviderTiles(providers$Stamen.TerrainLabels, options = providerTileOptions(noWrap = TRUE)) %>%
      # addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Earth at Night", options = providerTileOptions(minZoom = 1)) %>%
      
      # addProviderTiles(
      #   # http://leaflet-extras.github.io/leaflet-providers/preview/index.html
      #   "Stamen.Toner",
      #   "OpenTopoMap",
      #   "Thunderforest.Neighbourhood",
      #   "Thunderforest.Pioneer",
      #   "Stamen.Watercolor",
      #   "Stamen.Terrain",
      #   "Stamen.TerrainBackground",
      #   "Stamen.TerrainLabels",
      #   "Stamen.TopOSMRelief",
      #   "Esri.NatGeoWorldMap"
      # ) %>%
      addLayersControl(
        baseGroups = c(
          # "Google", 
          # "Stamen.Toner",
          # "OpenTopoMap",
          # "Thunderforest.Neighbourhood",
          # "Thunderforest.Pioneer",
          "Stamen.Watercolor"
          # "Stamen.Terrain",
          # "Stamen.TerrainBackground",
          # "Stamen.TerrainLabels"
          # "Esri.NatGeoWorldMap"
          # "NASAGIBS.ViirsEarthAtNight2012"
          # "Stamen.TopOSMRelief",
          # "Esri.NatGeoWorldMap"
        )
      ) %>%
      setView(
        lng = -98.583,
        lat = 39.833,
        zoom = 5
      ) %>%
      addMarkers(
        weddingGuests$coords,
        popup = ~as.character(popupContent),
        clusterOptions = markerClusterOptions()
      )
    
  })
  
  weddingGuestsInputs <- reactive({
    
    weddingGuests <- weddingGuests
    
    # types of guests
    if (length(input$guestStateSelect > 1)) {
      weddingGuests <- subset(weddingGuests, state %in% input$guestStateSelect)
    } else {
      weddingGuests <- subset(weddingGuests, state == input$guestStateSelect)
    }
    
    return(weddingGuests)
    
  })
  
  
  # not entirely sure why this isn't working...?
  observe({
    weddingGuests <- weddingGuestsInputs()
    # data is guests
    leafletProxy("leaflet", data = weddingGuests) %>%
      # clearGeoJSON("weddingGuests") %>%
      clearGroup(group = "weddingGuests") %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addMarkers(
        weddingGuests$coords,
        popup = ~as.character(popupContent),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # output$leaflet <- renderLeaflet(
  #   
  #   map <- leaflet(data = weddingGuests) %>%
  #     addTiles() %>%
  #     setView(
  #       lng = -98.583,
  #       lat = 39.833,
  #       zoom = 5
  #     ) %>%
  #     addMarkers(
  #       weddingGuests$coords,
  #       popup = ~as.character(popupContent),
  #       clusterOptions = markerClusterOptions()
  #     )
  #   
  # )
  
}

shinyApp(ui = ui, server = server)
