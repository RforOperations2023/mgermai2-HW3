library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(jsonlite)
library(st)
library(shinydashboard)
library(dashboardthemes)
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
library(RColorBrewer)
library(plotly)
library(ggplot2)
library(DT)
library(bslib)
# weddingGuests <- st_read("https://raw.githubusercontent.com/mgermaine93/wedding-guest-map/master/constants/guests.geojson")
# states <- st_read("https://raw.githubusercontent.com/mgermaine93/wedding-guest-map/master/constants/us-states.geojson")
# states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")


# To-Do:
#   * Add datatables
#   * Add download handler
#   * Add observer(s) to main cluster map
#   * Add text to the ABOUT page 
#   * Add in a few more map tiles to each map 
#   * Add comments to code
#   * Fix bar plots (they are no longer working for some reason)
#   * (If time) Add in functionality that factors in guest type for the two bar charts (the fill in this case would be guest type)
 
weddingGuests <- st_read("data/guests.geojson")
states <- st_read("data/us-states.geojson")

# ui part begins here
ui <- dashboardPage(
  
  skin = "black",
  
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
      menuItem(
        "Map of Guests (Clusters)", 
        tabName = "leafletClusters",
        icon = icon("map")
      ),
      menuItem(
        "Map of Guests (Heatmap)", 
        tabName = "leafletHeatmap",
        icon = icon("fire")
      ),
      menuItem(
        "Wedding Guests by Generational Age", 
        tabName = "plotByGeneration",
        icon = icon("person-cane")
      ),
      menuItem(
        "Wedding Guests by State", 
        tabName = "plotByState",
        icon = icon("flag-usa")
      ),
      menuItem(
        "About this Project", 
        tabName = "about",
        icon = icon("question")
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
          "is_couple",
          "wedding_party"
        ),
        multiple = TRUE
        
      ),
      # as defined here:  https://caregiversofamerica.com/2022-generation-names-explained/
      checkboxGroupInput(
        inputId = "guestGenerationSelect",
        label = "Guest Generation",
        # might rename these labels later...
        choices = c(unique(sort(weddingGuests$generation))),
        selected = c(
          "Millennial",
          "Baby Boomer"
        )
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
              height = "90vh"
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
              height = "90vh"
            )
          )
        )
      ),
      
      tabItem(
        
        tabName = "plotByState",
        
        # first row is the name of the page
        h2("Guests by State"),
        
        # second row is the actual plot
        fluidRow(
          box(
            width = 12,
            plotlyOutput("guestsByState")
          )
        )
        
        # # fourth row is the data table corresponding to the plot
        # fluidRow(
        #   box(
        #     width = 12,
        #     # data table stuff
        #     DT::dataTableOutput(
        #       outputId = "licenseTable",
        #     ),
        #   )
        # )
      ),
      
      tabItem(
        
        tabName = "plotByGeneration",
        
        # first row is the name of the page
        h2("Guests by Generation"),
        
        # second row is the actual plot
        fluidRow(
          box(
            width = 12,
            plotlyOutput("guestsByGeneration")
          )
        )
      ),
        
      tabItem(
        
        tabName = "about",
        
        # # first row is the name of the page
        # h2("About this Project"),
        
        # https://stackoverflow.com/questions/44279773/r-shiny-add-picture-to-box-in-fluid-row-with-text
        fluidRow(
          box(
            title = "Meredith and Matt's Wedding",
            status = "primary",
            solidHeader = F,
            collapsible = F,
            width = 12,
            fluidRow(
              column(
                width = 6,
                textOutput( "aboutText" )
              ),
              column(
                width = 6,
                align = "center",
                img(
                  src = "eyre-germaine466.jpg", 
                  width = "100%",
                  height = "100%",
                  align = "center"
                )
              )
            )
          )
        )
      )
        
        # # fourth row is the data table corresponding to the plot
        # fluidRow(
        #   box(
        #     width = 12,
        #     # data table stuff
        #     DT::dataTableOutput(
        #       outputId = "licenseTable",
        #     ),
        #   )
        # )
    
    )
  )
)



server <- function(input, output) {
  
  weddingGuestsInputs <- reactive({
    
    weddingGuests <- weddingGuests

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
    return(noDuplicateGuests)
    # print(guests_to_be_plotted)
    # return(guests_to_be_plotted)
  })
  
  # might adjust this later...
  bins <- c(0, 1, 2, 3, 5, 10, 20, 50, 100)
  # bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
  pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
  labels <- sprintf(
    "<strong>%s</strong><br/>%g guests", states$name, states$density
  ) %>% 
  lapply(htmltools::HTML)
  
  
  # load in wedding guest filtered data
  output$leafletClusters <- renderLeaflet({
    
    # this is important!  otherwise, some kind of circuitous loading happens for some reason.
    weddingGuests <- weddingGuestsInputs()
    # noDuplicateGuests <- weddingGuests[!duplicated(weddingGuests), ]
    
    leaflet(data = weddingGuests) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap.Mapnik", options = providerTileOptions(noWrap = TRUE)) %>%
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
  
  # HEATMAP
  # https://rstudio.github.io/leaflet/choropleths.html
  # load in wedding guest filtered data
  output$leafletHeatmap <- renderLeaflet({
    
    # # this is important!  otherwise, some kind of circuitous loading happens for some reason.
    # weddingGuests <- weddingGuestsInputs()
    
    leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap.Mapnik", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(
        lng = -98.583,
        lat = 39.833,
        zoom = 5
      ) %>%
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
      # https://rstudio.github.io/leaflet/legends.html
      addLegend(
        title = "Number of Guests",
        pal = pal, 
        values = ~density, 
        opacity = 0.7, 
        position = "bottomright"
      )
  })
  
  
  guestsByState <- reactive({
    result = weddingGuestsInputs() %>%
      filter(state %in% input$guestStateSelect) %>%
      group_by(state) %>%
      arrange(state) %>%
      summarize(n = n()) %>%
      rename("num_guests" = n)
    print("---guestStateSelect"); return(result);
  })

  
  guestsByGeneration <- reactive({
    result = weddingGuestsInputs() %>%
      filter(generation %in% input$guestGenerationSelect) %>%
      group_by(generation) %>%
      arrange(generation) %>%
      summarize(n = n()) %>%
      rename("num_guests" = n)
    print("---guestGenerationSelect"); return(result);
  })
  
  
  output$guestsByState <- renderPlotly({
    
    ggplotly(
      guestsByState() %>%
        ggplot(
          mapping = aes(
            x = state,
            y = num_guests,
            fill = state
          )
        ) +
        labs(
          x = "State of Residence",
          y = "Number of Guests",
          fill = "State of Residence"
        ) +
        geom_bar(
          alpha = 0.5,
          stat = "identity",
          position = position_dodge()
        ) +
        scale_fill_brewer(palette = "Set1") +
        ggtitle(paste(str_interp("Wedding Guests by State of Residence at the Time of the Wedding"))) +
        theme(plot.title = element_text(hjust = 0.5))
    )
    
  })
  
  
  output$guestsByGeneration <- renderPlotly({
    
    ggplotly(
      guestsByGeneration() %>%
        ggplot(
          mapping = aes(
            x = generation,
            y = num_guests,
            fill = generation
          )
        ) +
        labs(
          x = "Age Generation of Guests",
          y = "Number of Guests",
          fill = "Generational Age of Guests"
        ) +
        geom_bar(
          alpha = 0.5,
          stat = "identity",
          position = position_dodge()
        ) +
        scale_fill_brewer(palette = "Set1") +
        ggtitle(paste(str_interp("Wedding Guests by Generational Age at the Time of the Wedding"))) +
        theme(plot.title = element_text(hjust = 0.5))
    )
    
  })
  
  output$aboutText <- renderText(
    "This text here will include details about the project and perhaps about our wedding..."
  )
  
  
}

shinyApp(ui = ui, server = server)

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
  