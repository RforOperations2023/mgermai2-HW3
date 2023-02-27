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
require(shinythemes)


# weddingGuests <- st_read("https://raw.githubusercontent.com/mgermaine93/wedding-guest-map/master/constants/guests.geojson")

weddingGuests <- st_read("data/guests.geojson")





# ui part begins here
ui <- fluidPage(
  
  # sets the title of the app
  titlePanel("Wedding Guest Map"),
  
  h5("By Matt Germaine"),
  
  # sets the theme/coloring of the app
  theme = shinytheme("cerulean"),
  
  # displays the title of the app
  titlePanel(
    "Pittsburgh Regional Transit Ridership, 2017-2022",
  ),
  
  
  
  # user input part
  sidebarLayout(
    
    sidebarPanel(
      
      p("
        Wedding Guest Stuff
      "),
      
      p("
        More wedding guest stuff
      "),
      
      br(),
      br(),
      
      selectInput(
        inputId = "guestSelect",
        label = "Guest Type",
        choices = c(
          "Something",
          "Something else"
        ),
        selected = c(
          "Something"
        ),
        selectize = TRUE,
        multiple = TRUE
        
      ),
      
      # as defined here:  https://caregiversofamerica.com/2022-generation-names-explained/
      checkboxGroupInput(
        inputId = "guestGenerationSelect",
        label = "Guest Generation",
        choices = c(
          "The Greatest Generation (born 1901-1924)",
          "The Silent Generation (born 1925-1945)",
          "The Baby Boomer Generation (born 1946-1964)",
          "Generation X (born 1965-1979)",
          "Millennials (born 1980-1994)",
          "Generation Z (born 1995-2012)",
          "Generation Alpha (born 2013-Present)"
        )
      ),
      
      selectizeInput(
        inputId = "guestsState",
        label = "Guest State",
        choices = unique(sort(weddingGuests$state))
      ),
      
      br(),
      
      # enables the user to download the data
      # https://shiny.rstudio.com/reference/shiny/1.0.5/downloadbutton
      downloadButton(
        outputId = "downloadData",
        label = "Download Data Table"
      )
      
    ),
    
    # this is the part that actually holds the plotted data
    mainPanel(
      
      leafletOutput("leaflet"),
      
      br(),
      br(),
      
      # data table stuff
      DT::dataTableOutput(
        outputId = "mytable",
      ),
      
      br(),
      br()
      
    )
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




# ui <- fluidPage(
#   
#   
# 
#   sidebarLayout(
#     
#     sidebarPanel(
#       
#       selectInput(
#         inputId = "guestSelect",
#         label = "Guest Type",
#         choices = c(
#           "Something",
#           "Something else"
#         ),
#         selected = c(
#           "Something"
#         ),
#         selectize = TRUE,
#         multiple = TRUE
# 
#       ),
# 
#       # as defined here:  https://caregiversofamerica.com/2022-generation-names-explained/
#       checkboxGroupInput(
#         inputId = "guestGenerationSelect",
#         label = "Guest Generation",
#         choices = c(
#           "The Greatest Generation (born 1901-1924)",
#           "The Silent Generation (born 1925-1945)",
#           "The Baby Boomer Generation (born 1946-1964)",
#           "Generation X (born 1965-1979)",
#           "Millennials (born 1980-1994)",
#           "Generation Z (born 1995-2012)",
#           "Generation Alpha (born 2013-Present)"
#         )
#       ),
# 
#       selectizeInput(
#         inputId = "guestsState",
#         label = "Guest State",
#         choices = unique(sort(weddingGuests$state))
#       ),
#       
#       br(),
#       
#       downloadButton(
#         outputId = "Download Data",
#         label = "Download Data Table"
#       )
#       
#     ),
#     
#     mainPanel(
#       
#       leafletOutput("leaflet")
#       
#     )
#     
#   )
#   
# )
# 
# 
# 
#   
#   # # Map Panel
#   # sidebarLayout(
#   #   sidebarPanel(
#   #     selectInput(
#   #       inputId = "guestSelect",
#   #       label = "Guest Type",
#   #       choices = c(
#   #         "Something",
#   #         "Something else"
#   #       ),
#   #       selected = c(
#   #         "Something"
#   #       ),
#   #       selectize = TRUE,
#   #       multiple = TRUE
#   #       
#   #     ),
#   #     
#   #     # as defined here:  https://caregiversofamerica.com/2022-generation-names-explained/
#   #     checkboxGroupInput(
#   #       inputId = "guestGenerationSelect",
#   #       label = "Guest Generation",
#   #       choices = c(
#   #         "The Greatest Generation (born 1901-1924)",
#   #         "The Silent Generation (born 1925-1945)",
#   #         "The Baby Boomer Generation (born 1946-1964)",
#   #         "Generation X (born 1965-1979)",
#   #         "Millennials (born 1980-1994)",
#   #         "Generation Z (born 1995-2012)",
#   #         "Generation Alpha (born 2013-Present)"
#   #       )
#   #     ),
#   #     
#   #     selectizeInput(
#   #       inputId = "guestsState",
#   #       label = "Guest State",
#   #       choices = unique(sort(weddingGuests$state))
#   #     )
#   #   ),
#   #   mainPanel(
#   #     leafletOutput("leaflet")
#   #   )
#   # ),
#   
#   
#   
#   
#   
#   
#   
#   
#   
# #   tabPanel(
# #   
# #     "Map",
# #     
# #     sidebarLayout(
# #       sidebarPanel(
# #         
# #         selectInput(
# #           inputId = "guestSelect",
# #           label = "Guest Type",
# #           choices = c(
# #             "Something",
# #             "Something else"
# #           ),
# #           selected = c(
# #             "Something"
# #           ),
# #           selectize = TRUE,
# #           multiple = TRUE
# #           
# #         ),
# #         
# #         # as defined here:  https://caregiversofamerica.com/2022-generation-names-explained/
# #         checkboxGroupInput(
# #           inputId = "guestGenerationSelect",
# #           label = "Guest Generation",
# #           choices = c(
# #             "The Greatest Generation (born 1901-1924)",
# #             "The Silent Generation (born 1925-1945)",
# #             "The Baby Boomer Generation (born 1946-1964)",
# #             "Generation X (born 1965-1979)",
# #             "Millennials (born 1980-1994)",
# #             "Generation Z (born 1995-2012)",
# #             "Generation Alpha (born 2013-Present)"
# #           )
# #         ),
# #         
# #         selectizeInput(
# #           inputId = "guestsState",
# #           label = "Guest State",
# #           choices = unique(sort(weddingGuests$state))
# #         )
# #       )
# #     ),
# #       
# #     mainPanel(
# #       leafletOutput("leaflet")
# #     )
# #       
# #     
# #   # closing bracket of tab panel
# #   )
# # )
# 
# 
#     
#     # "Map",
#     # 
#     # sidebarLayout(
#       
#       
#   #   )
#   # )
#       
#       # select what group the guest falls into (wedding party, etc.)
#       # select what state the guest comes from
#       # select the guests by generation? (Baby Boomer, Millennial, etc.)
#       
#       # for the charts...
#       # bar plot of where people came from
#         # x-xis is state,
#         # y -axis is the number of people
#       
#       # bar plot of the groups of people
#         # x-axis is the group type,
#         # y-axis is the number of people
#       
#       
#       # sidebarPanel(
#       #   
#       #   selectInput(
#       #     inputId = "guestSelect",
#       #     label = "Guest Type",
#       #     choices = c(
#       #       "Something",
#       #       "Something else"
#       #     ),
#       #    selected = c(
#       #       "Something"
#       #     ),
#       #     selectize = TRUE,
#       #     multiple = TRUE
#       #     
#       #   ),
#       #   
#       #   # as defined here:  https://caregiversofamerica.com/2022-generation-names-explained/
#       #   checkboxGroupInput(
#       #     inputId = "guestGenerationSelect",
#       #     label = "Guest Generation",
#       #     choices = c(
#       #       "The Greatest Generation (born 1901-1924)",
#       #       "The Silent Generation (born 1925-1945)",
#       #       "The Baby Boomer Generation (born 1946-1964)",
#       #       "Generation X (born 1965-1979)",
#       #       "Millennials (born 1980-1994)",
#       #       "Generation Z (born 1995-2012)",
#       #       "Generation Alpha (born 2013-Present)"
#       #     )
#       #   ),
#       #   
#       #   selectizeInput(
#       #     inputId = "guestsState",
#       #     label = "Guest State",
#       #     choices = unique(sort(weddingGuests$state))
#       #     # choices = c(
#       #     #   "Alabama",
#       #     #   "Alaska",
#       #     #   "Arizona",
#       #     #   "Arkansas",
#       #     #   "California",
#       #     #   "Colorado",
#       #     #   "Connecticut",
#       #     #   "Delaware",
#       #     #   "District of Columbia",
#       #     #   "Florida",
#       #     #   "Georgia",
#       #     #   "Hawaii",
#       #     #   "Idaho",
#       #     #   "Illinois",
#       #     #   "Indiana",
#       #     #   "Iowa",
#       #     #   "Kansas",
#       #     #   "Kentucky",
#       #     #   "Louisiana",
#       #     #   "Maine",
#       #     #   "Montana",
#       #     #   "Nebraska",
#       #     #   "Nevada",
#       #     #   "New Hampshire",
#       #     #   "New Jersey",
#       #     #   "New Mexico",
#       #     #   "New York",
#       #     #   "North Carolina",
#       #     #   "North Dakota",
#       #     #   "Ohio",
#       #     #   "Oklahoma",
#       #     #   "Oregon",
#       #     #   "Maryland",
#       #     #   "Massachusetts",
#       #     #   "Michigan",
#       #     #   "Minnesota",
#       #     #   "Mississippi",
#       #     #   "Missouri",
#       #     #   "Pennsylvania",
#       #     #   "Rhode Island",
#       #     #   "South Carolina",
#       #     #   "South Dakota",
#       #     #   "Tennessee",
#       #     #   "Texas",
#       #     #   "Utah",
#       #     #   "Vermont",
#       #     #   "Virginia",
#       #     #   "Washington",
#       #     #   "West Virginia",
#       #     #   "Wisconsin",
#       #     #   "Wyoming"
#       #     # )
#       #   )
#       
#        # # Select NYC Borough
#        # radioButtons("boroSelect",
#        #              "Borough Filter:",
#        #              choices = unique(sort(greenInf.load$borough)),
#        #              selected = "Bronx")
#        # ),
#        # # Map Page
#        # mainPanel(
#        #   # Style the background and change the page
#        #   tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
#        #                             body {background-color: #D4EFDF;}"),
#        #   # Map Output
#        #   leafletOutput("leaflet")
#        # )
#   #    )
#   #    )
#   # ),
#   # # # Data Table Pannel
#   # tabPanel("Data",
#   #          fluidPage(
#   #            wellPanel(DT::dataTableOutput("table"))
#   #          )
#   # )
#   
#   
#   
#   
# 