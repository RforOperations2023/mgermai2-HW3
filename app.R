# -------------------------------------------------#
# Matthew Germaine                                 #
# R Shiny for Operations Management                #
# HW #3 / Final Project                            #
# Basic Shiny App                                  #
# -------------------------------------------------#

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
#   * Add datatables == DONE (sort-of)
#   * Add download handler == DONE
#   * Add observer(s) to main cluster map == DONE
#   * Add text to the ABOUT page 
#   * Add in a few more map tiles to each map 
#   * Add comments to code == DONE
#   * Fix bar plots (they are no longer working as expected for some reason)
#   * (If time) Add in functionality that factors in guest type for the two bar charts (the fill in this case would be guest type)
#   * Maybe add more content to the popUpContent of the markers?

# load in the guests data
weddingGuests <- st_read("data/guests.geojson")

# load in the states data
states <- st_read("data/us-states.geojson")

#######################
# ui part begins here #
#######################
ui <- dashboardPage(
  
  # sets the "theme" of the page
  skin = "black",
  
  # sets the title of the app
  dashboardHeader(
    title = "Meredith and Matt's Wedding Guest Map",
    titleWidth = 400
  ),
  
  ############################
  # sidebar part begins here #
  ############################
  
  dashboardSidebar(
    
    width = 400,
    
    sidebarMenu(
      
      id = "tabs",
      
      # this links to the "main" clusters map that dynamically maps different types of guests
      menuItem(
        "Map of Guests (Clusters)", 
        tabName = "leafletClusters",
        icon = icon("map")
      ),
      
      # this links to the heatmap that illustrates how many guests came from each state
      menuItem(
        "Map of Guests (Heatmap)", 
        tabName = "leafletHeatmap",
        icon = icon("fire")
      ),
      
      # this links to the bar plot that dynamically plots guests by their generational age
      menuItem(
        "Wedding Guests by Generational Age", 
        tabName = "guestsByGeneration",
        icon = icon("person-cane")
      ),
      
      # this links to the bar plot that dynamically plots guests by their state of residence at the time of the wedding
      menuItem(
        "Wedding Guests by State", 
        tabName = "guestsByState",
        icon = icon("flag-usa")
      ),
      
      # this links to the page that describes the project :)
      menuItem(
        "About this Project", 
        tabName = "about",
        icon = icon("question")
      ),
      
      ###########################
      # user inputs begins here #
      ###########################
      
      # enables the user to select multiple groups of guests
      selectizeInput(
        inputId = "guestTypeSelect",
        label = "Guest Type",
        choices = c(
          # okay, so the left value is what the user sees, but the right value is how it is in the data itself
          "Everyone" = "everyone",
          "Couple" = "is_couple",
          "Invitees" = "invited",
          "Attendees" = "attended_wedding",
          "Invitees from the Bride's Side" = "brides_side",
          "Invitees from the Groom's Side" = "grooms_side",
          "Invitees from the Couple's Side" = "couples_side",
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
      
      # enables the user to select multiple generational ages
      # generations are defined here:  https://caregiversofamerica.com/2022-generation-names-explained/
      checkboxGroupInput(
        inputId = "guestGenerationSelect",
        label = "Guest Generation",
        # okay, so the left value is what the user sees, but the right value is how it is in the data itself
        choices = c(
          "Baby Boomer (born 1946-1964)" = "Baby Boomer",
          "Generation Alpha (born 2013-Present)" = "Generation Alpha",
          "Generation X (born 1965-1979)" = "Generation X",
          "Generation Z (born 1995-2012)" = "Generation Z",
          "Millennial (born 1980-1994)" = "Millennial",
          "N/A" = "N/A",
          "Silent Generation (born 1925-1945)" = "Silent"
        ),
        selected = c(
          "Millennial",
          "Baby Boomer"
        )
      ),
      
      # enables the user to select multiple states of residence
      selectizeInput(
        inputId = "guestStateSelect",
        label = "Guest State",
        # this specific part simply pulls out all of the unique options that exist for state of residence in the data
        choices = unique(sort(weddingGuests$state)),
        selected = c(
          "Michigan",
          "Ohio",
          "Pennsylvania"
        ),
        multiple = TRUE
      ),
      
      # enables the user to download the data
      # https://shiny.rstudio.com/reference/shiny/1.0.5/downloadbutton
      downloadButton(
        outputId = "downloadData",
        label = "Download Data Table",
        class = "download-button"
      )
      
    )
    
  ),
  
  #############################################################
  # this is the part that actually "holds" the maps and plots #
  #############################################################
  
  dashboardBody(
    
    tabItems(
      
      # this tab holds the main "cluster" map... it's important to refer to th cluster map as "leafletClusters" everywhere else in the app
      tabItem(
        tabName = "leafletClusters",
        fluidRow(
          box(
            width = 12,
            # ... and here's the cluster map itself
            leafletOutput(
              "leafletClusters",
              width = "100%",
              # might need to change this depending on the DT/tables/graphs, etc
              # https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
              height = "90vh"
            )
          )
        )
        # # need a DT here
        # fluidRow(
        #   box(
        #     width = 12,
        #     # data table stuff
        #     DT::dataTableOutput(
        #       outputId = "leafletClustersDataTable",
        #     ),
        #   )
        # )
      ),
      
      # this tab holds the static heatmap... it's important to refer to the heatmap as "leafletHeatmap" everywhere else in the app
      tabItem(
        tabName = "leafletHeatmap",
        fluidRow(
          box(
            width = 12,
            # ... and here's the heatmap itself
            leafletOutput(
              "leafletHeatmap",
              width = "100%",
              # might need to change this depending on the DT/tables/graphs, etc
              # https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
              height = "90vh"
            )
          )
        )
        # # need a DT here
        # fluidRow(
        #   box(
        #     width = 12,
        #     # data table stuff
        #     DT::dataTableOutput(
        #       outputId = "leafletHeatmapDataTable",
        #     ),
        #   )
        # )
      ),
      
      # this tab contains the barplot the dynamically plots guests by their state of residence...
      # it's important to refer to the bar plot as "guestsByState" everywhere else in the app
      tabItem(
        tabName = "guestsByState",
        h2("Guests by State"),
        fluidRow(
          box(
            width = 12,
            # ... and here's the bar plot itself
            plotlyOutput("guestsByState")
          )
        ),
        # need a DT here
        fluidRow(
          box(
            width = 12,
            # data table stuff
            DT::dataTableOutput(
              outputId = "guestsByStateDataTable",
            ),
          )
        )
      ),
      
      # this tab contains the barplot the dynamically plots guests by their generational age...
      # it's important to refer to the bar plot as "guestsByGeneration" everywhere else in the app
      tabItem(
        tabName = "guestsByGeneration",
        h2("Guests by Generation"),
        # second row is the actual plot
        fluidRow(
          box(
            width = 12,
            # ... and here's the bar plot iself
            plotlyOutput("guestsByGeneration")
          )
        ),
        # need a DT here
        fluidRow(
          box(
            width = 12,
            # data table stuff
            DT::dataTableOutput(
              outputId = "guestsByGenerationDataTable",
            ),
          )
        )
      ),
      
      # this tab links to a description of the project
      # it's static, but it's important to refer to it by "about" everywhere else in the app
      tabItem(
        tabName = "about",
        # https://stackoverflow.com/questions/44279773/r-shiny-add-picture-to-box-in-fluid-row-with-text
        fluidRow(
          box(
            title = "Meredith and Matt's Wedding",
            status = "primary",
            solidHeader = F,
            collapsible = F,
            width = 12,
            fluidRow(
              # this is for the text
              column(
                width = 6,
                textOutput( "aboutText" )
              ),
              column(
                # this is for the picture
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
      
    )
  )
)


###########################
# server part begins here #
###########################

server <- function(input, output) {
  
  # okay, so this reactive function is a bit crazy, but it works!
  # since a single individual can belong to many different types of "guest" (wedding party, couple, etc.),
  # this results in the same marker being plotted multiple times when their groups are plotted.
  # I didn't want this feature because it misrepresents the quantities of guests
  # (I wanted each guest to be plotted once and only once regardless of how many of their groups were selected 
  # to be plotted, and then removed only when ALL of the groups to which they belong are unplotted/unselected 
  # from the map), and this function takes care of that.
  
  # this can almost certainly be improved either on its own or by editing the raw data somehow, but the focus here
  # was simply to make it work, then (perhaps someday) make it better.
  weddingGuestsInputs <- reactive({
    
    # load in the guest data
    # doing this in this way because weird things occur if I don't...
    weddingGuests <- weddingGuests
    
    # this creates a NEW variable that is DEVOID of all data,
    # but keeps the structure and column names of the data set, which is important.
    # https://stackoverflow.com/questions/49851381/empty-a-data-frame-keep-colnames-headers-only
    guests_to_be_plotted <- weddingGuests[FALSE, ]
    
    # okay, so each one of the next several lines creates a df (?) for each wedding guest "type" and the individuals
    # that fall into that type
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
    
    # okay, so now what we do is process what the user selected...
    # each one of the next several lines checks to see if a certain wedding guest "type" is one
    # of the types that the user selected...
    # if it is, then it simply adds/concatenates that group onto the empty df defined above
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
    
    # since we now have a sizeable DF that more than likely contains multiple redundant data points,
    # we simply remove duplicate data points
    # https://stackoverflow.com/questions/13967063/remove-duplicated-rows
    noDuplicateGuests <- guests_to_be_plotted[!duplicated(guests_to_be_plotted), ]
    
    # ... and then finally return the df that represents the guests that belong to all of the
    # wedding guest types that the user selected, but without duplicates.
    return(noDuplicateGuests)
    
  })
  
  ######################################
  # heatmap-related constants are here #
  ######################################
  
  # might adjust this later...
  # these are the values that mark the color cutoffs for guest density
  bins <- c(0, 1, 2, 3, 5, 10, 20, 50, 100)
  # bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
  
  # this represents the actual colors for the heatmap and its legend,
  # which correspond to the "bins" defined above
  # MAKE SURE TO UPDATE THE COLOR PALETTE
  pal <- colorBin(
    "YlOrRd", 
    # this means that the color will correspond with the density of guests that came from each state
    domain = states$density, 
    bins = bins
  )
  
  # this represents the label that dynamically appears when the user hovers over each state in the heatmap
  # *** might try to incorporate "stringr" instead... we'll see...
  labels <- sprintf(
    "<strong>%s</strong><br/>%g guests", 
    states$name, 
    states$density
  ) %>% 
    lapply(htmltools::HTML)
  
  
  ######################################
  # actual maps/plots are created here #
  ######################################
  
  ### CLUSTER MAP IS HERE ###
  
  # load in wedding guest filtered data
  output$leafletClusters <- renderLeaflet({
    
    # okay, so here's the "base" map.
    # this will not update unless the user selects a different tile to view
    leaflet() %>%
      addProviderTiles(
        providers$OpenStreetMap.Mapnik, 
        group = "Open Street Map", 
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles(
        providers$OpenTopoMap, 
        group = "Open Topo Map", 
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      # addProviderTiles(
      #   providers$Stadia.AlidadeSmoothDark, 
      #   group = "Smooth Dark", 
      #   options = providerTileOptions(noWrap = TRUE)
      # ) %>%
      setView(
        lng = -98.583,
        lat = 39.833,
        zoom = 5
      ) %>%
      addLayersControl(
        baseGroups = c(
          "Open Street Map", 
          "Open Topo Map", 
          "Smooth Dark"
        ),
        # overlayGroups = c("Quakes", "Outline"),
        options = layersControlOptions(collapsed = TRUE)
      )
    
  })
  
  # okay, so this ensures that only the clusters/data points update on the cluster map above,
  # and NOT the map itself
  observe({
    # this uses the crazy function (defined above) to create the dataset that will be plotted
    weddingGuests <- weddingGuestsInputs()
    # leafletClusters" refers to the OUTPUT MAP created above
    leafletProxy("leafletClusters", data = weddingGuests) %>%
      # clearGeoJSON("weddingGuests") %>%
      clearGroup(group = "weddingGuests") %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      # may have to explore this more?
      addMarkers(
        popup = ~as.character(popupContent),
        clusterOptions = markerClusterOptions()
      )
  })
  
  
  # # outputs the data table corresponding to the first plot
  # output$leafletClusterDataTable = DT::renderDataTable({
  #   DT::datatable(data = leafletClusters())
  # })
  
  ### HEATMAP IS HERE ###
  
  # it is static for now, might change later
  # https://rstudio.github.io/leaflet/choropleths.html
  # load in wedding guest filtered data
  output$leafletHeatmap <- renderLeaflet({
    
    # load in the states data
    leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(
        providers$OpenStreetMap.Mapnik, 
        group = "Open Street Map", 
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles(
        providers$OpenTopoMap, 
        group = "Open Topo Map", 
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      # addProviderTiles(
      #   providers$Stadia.AlidadeSmoothDark, 
      #   group = "Smooth Dark", 
      #   options = providerTileOptions(noWrap = TRUE)
      # ) %>%
      setView(
        lng = -98.583,
        lat = 39.833,
        zoom = 5
      ) %>%
      addLayersControl(
        baseGroups = c(
          "Open Street Map", 
          "Open Topo Map", 
          "Smooth Dark"
        ),
        # overlayGroups = c("Quakes", "Outline"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addPolygons(
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
          style = list(
            "font-weight" = "normal", 
            padding = "3px 8px"
          ),
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
  
  # # outputs the data table corresponding to the first plot
  # output$leatletHeatmapDataTable = DT::renderDataTable({
  #   DT::datatable(data = states)
  # })
  
  
  ### BAR PLOT FOR STATE OF RESIDENCE IS HERE ###
  
  # this filters down the data based on what the user input for state of residence
  guestsByState <- reactive({
    result = weddingGuestsInputs() %>%
      filter(state %in% input$guestStateSelect) %>%
      group_by(state) %>%
      arrange(state) %>%
      summarize(n = n()) %>%
      rename("num_guests" = n)
    print("---guestStateSelect"); return(result);
  })
  
  # this creates the actual barplot for states of residence
  output$guestsByState <- renderPlotly({
    
    ggplotly(
      guestsByState() %>%
        ggplot(
          mapping = aes(
            x = state,
            y = num_guests,
            # the fill color of the bars is determined by the state of residence
            fill = state
          )
        ) +
        # labels for the axes as well as the legend
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
  
  # outputs the data table corresponding to the first plot
  output$guestsByStateDataTable = DT::renderDataTable({
    DT::datatable(data = guestsByState())
  })
  
  
  ### BAR PLOT BY GENERATIONAL AGE IS HERE ###
  
  # this filters down the data based on what the user input for generational ages
  guestsByGeneration <- reactive({
    result = weddingGuestsInputs() %>%
      filter(generation %in% input$guestGenerationSelect) %>%
      group_by(generation) %>%
      arrange(generation) %>%
      summarize(n = n()) %>%
      rename("num_guests" = n)
    print("---guestGenerationSelect"); return(result);
  })
  
  # this creates the actual barplot for generational ages
  output$guestsByGeneration <- renderPlotly({
    
    ggplotly(
      guestsByGeneration() %>%
        ggplot(
          mapping = aes(
            x = generation,
            y = num_guests,
            # the fill color of the bars is determined by the generational age categories
            fill = generation
          )
        ) +
        # labels for the axes and the legend
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
  
  # outputs the data table corresponding to the first plot
  output$guestsByGenerationDataTable = DT::renderDataTable({
    DT::datatable(data = guestsByGeneration())
  })
  
  
  # enables the user to download the datatable created above.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('germaine-eyre-wedding-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(weddingGuestsInputs(), con)
    }
  )
  
  
  
  # this is the text that displays in the "about" page
  # https://stackoverflow.com/questions/23233497/outputting-multiple-lines-of-text-with-rendertext-in-r-shiny
  output$aboutText <- renderText(
    "Something something"
  )
  
  
}

shinyApp(ui = ui, server = server)
