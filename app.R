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
library(geojsonio)


# weddingGuests <- st_read("https://raw.githubusercontent.com/mgermaine93/wedding-guest-map/master/constants/guests.geojson")

weddingGuests <- st_read("data/guests.geojson")

# I wonder if I can load in my static file of states?
states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
class(states)




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
        "Map of Guests (Clusters)", 
        tabName = "leafletClusters",
        icon = icon("map")
      ),
      menuItem(
        "Map of Guests (Heatmap)", 
        tabName = "leafletHeatmap",
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
          "Everyone" = "everyone",
          "Couple" = "is_couple",
          "Invitees" = "invited",
          "Attendees" = "attended_wedding",
          "Bride's Side" = "brides_side",
          "Groom's Side" = "grooms_side",
          "Couple's Side" = "couples_side",
          "Wedding Party" = "wedding_party",
          "Bride's Family" = "brides_family",
          "Groom's Family" = "grooms_family",
          "Family" = "family",
          "Friends" = "friends",
          "Family Friends" = "family_friends",
          "Pre-College Friends" = "pre_college_friends",
          "College Friends" = "college_friends",
          "Post-College Friends" = "post_college_friends",
          "Rehearsal Dinner Attendees" = "attended_rehearsal_dinner",
          "Welcome Party Attendees" = "attended_welcome_party",
          "Heard From (but not invited)" = "heard_from",
          "Vendors" = "is_vendor"
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
        
        tabName = "leafletClusters",
        fluidRow(
          box(
            width = 12,
            leafletOutput(
              "leafletClusters",
              width = "100%",
              # might need to change this depending on the DT/tables/graphs, etc
              # https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
              height = "95vh"
            )
          )
        )
      ),
      
      tabItem(
        
        tabName = "leafletHeatmap",
        fluidRow(
          box(
            width = 12,
            leafletOutput(
              "leafletHeatmap",
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
  
  weddingGuestsInputs <- reactive({
    
    weddingGuests <- weddingGuests
    
    # new stuff here
    
    # https://stackoverflow.com/questions/49851381/empty-a-data-frame-keep-colnames-headers-only
    guests_to_be_plotted <- weddingGuests[FALSE, ]
    
    everyone <- weddingGuests
    couple <- subset(weddingGuests, is_bride == TRUE | is_groom == TRUE)
    invited <- subset(weddingGuests, invited == TRUE)
    attendedWedding <- subset(weddingGuests, attended_wedding == TRUE)
    bridesSide <- subset(weddingGuests, inviter == "Eyre")
    groomsSide <- subset(weddingGuests, inviter == "Germaine")
    couplesSide <- subset(weddingGuests, inviter == "Couple")
    weddingParty <- subset(weddingGuests, in_wedding_party == TRUE)
    bridesFamily <- subset(weddingGuests, is_family == TRUE & inviter == "Eyre")
    groomsFamily <- subset(weddingGuests, is_family == TRUE & inviter == "Germaine")
    family <- subset(weddingGuests, is_family == TRUE)
    friends <- subset(weddingGuests, is_friend == TRUE)
    familyFriends <- subset(weddingGuests, is_family_friend == TRUE)
    preCollegeFriends <- subset(weddingGuests, is_pre_college_friend == TRUE)
    collegeFriends <- subset(weddingGuests, is_college_friend == TRUE)
    postCollegeFriends <- subset(weddingGuests, is_post_college_friend == TRUE)
    attendedRehearsalDinner <- subset(weddingGuests, attended_rehearsal_dinner == TRUE)
    attendedWelcomeParty <- subset(weddingGuests, attended_welcome_party == TRUE)
    # fix this later and correct the data as well ("heard_from" instead of "heardFrom") so that it's more uniform.
    heardFrom <- subset(weddingGuests, heardFrom == TRUE)
    # isOfficiant <- subset(weddingGuests, is_officiant == TRUE)
    isVendor <- subset(weddingGuests, is_vendor == TRUE | is_officiant == TRUE)
    
    
    if ("everyone" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, everyone)
    }
    if ("is_couple" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, couple)
    }
    if ("invited" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, invited)
    }
    if ("attended_wedding" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedWedding)
    }
    if ("brides_side" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, bridesSide)
    }
    if ("grooms_side" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, groomsSide)
    }
    if ("couples_side" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, couplesSide)
    }
    if ("wedding_party" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, weddingParty)
    }
    if ("brides_family" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, bridesFamily)
    }
    if ("grooms_family" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, groomsFamily)
    }
    if ("family" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, family)
    }
    if ("friends" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, friends)
    }
    if ("family_friends" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, familyFriends)
    }
    if ("pre_college_friends" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, preCollegeFriends)
    }
    if ("college_friends" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, collegeFriends)
    }
    if ("post_college_friends" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, postCollegeFriends)
    }
    if ("attended_rehearsal_dinner" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedRehearsalDinner)
    }
    if ("attended_welcome_party" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedWelcomeParty)
    }
    if ("heard_from" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, heardFrom)
    }
    if ("is_vendor" %in% input$guestTypeSelect) {
      guests_to_be_plotted <- rbind(guests_to_be_plotted, isVendor)
    }
    
    # https://stackoverflow.com/questions/13967063/remove-duplicated-rows
    noDuplicateGuests <- guests_to_be_plotted[!duplicated(guests_to_be_plotted), ]
    
    # end of new stuff
    
    return(noDuplicateGuests)
    
  })
  
  bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
  pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
  labels <- sprintf(
    "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
    states$name, states$density
  ) %>% 
    lapply(htmltools::HTML)
  
  
  # load in wedding guest filtered data
  output$leafletClusters <- renderLeaflet({
    
    # this is important!  otherwise, some kind of circuitous loading happens for some reason.
    weddingGuests <- weddingGuestsInputs()
    
    leaflet(data = weddingGuests) %>%
      addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap", options = providerTileOptions(noWrap = TRUE)) %>%
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
    # addLayersControl(
    #   baseGroups = c(
    #     # "Google", 
    #     # "Stamen.Toner",
    #     # "OpenTopoMap",
    #     # "Thunderforest.Neighbourhood",
    #     # "Thunderforest.Pioneer",
    #     "Stamen.Watercolor"
    #     # "Stamen.Terrain",
    #     # "Stamen.TerrainBackground",
    #     # "Stamen.TerrainLabels"
    #     # "Esri.NatGeoWorldMap"
    #     # "NASAGIBS.ViirsEarthAtNight2012"
    #     # "Stamen.TopOSMRelief",
    #     # "Esri.NatGeoWorldMap"
    #   )
    # ) %>%

  
  
  
  
  # HEATMAP
  # https://rstudio.github.io/leaflet/choropleths.html
  # load in wedding guest filtered data
  output$leafletHeatmap <- renderLeaflet({
    
    # # this is important!  otherwise, some kind of circuitous loading happens for some reason.
    # weddingGuests <- weddingGuestsInputs()
    
    leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.Toner, group = "Stamen.Toner", options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(
        # this will need to change so it reflects the wedding data densities
        fillColor = ~pal(density),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, 
        values = ~density, 
        opacity = 0.7, 
        title = NULL,
        position = "bottomright"
      )
  })
  
  
  
  
  
  
  
  
  # # # types of guests
  # # if (length(input$guestStateSelect > 1)) {
  # #   weddingGuests <- subset(weddingGuests, state %in% input$guestStateSelect)
  # # } else {
  # #   weddingGuests <- subset(weddingGuests, state == input$guestStateSelect)
  # # }
  # 
  # # types of guests
  # if (length(input$guestTypeSelect > 1)) {
  #   weddingGuests <- subset(weddingGuests, state %in% input$guestStateSelect)
  #   
  #   # print(class(weddingGuests))
  #   # weddingGuests <- subset(weddingGuests, summary(groups) %in% input$guestTypeSelect)
  # } else {
  #   weddingGuests <- subset(weddingGuests, groups == input$guestStateSelect)
  # }
  
  
  
  
  # # not entirely sure why this isn't working...?
  # observe({
  #   weddingGuests <- weddingGuestsInputs()
  #   # data is guests
  #   leafletProxy("leaflet", data = weddingGuests) %>%
  #     # clearGeoJSON("weddingGuests") %>%
  #     clearGroup(group = "weddingGuests") %>%
  #     clearMarkerClusters() %>%
  #     clearMarkers() %>%
  #     addMarkers(
  #       weddingGuests$coords,
  #       popup = ~as.character(popupContent),
  #       clusterOptions = markerClusterOptions()
  #     )
  # })
  
}

shinyApp(ui = ui, server = server)
