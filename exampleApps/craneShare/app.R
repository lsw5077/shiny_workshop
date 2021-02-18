# Load libraries

library(shiny)
library(shinyWidgets)
library(leaflet)
library(maps)
library(tidyverse)
library(lubridate)

# Load picture list data

picList <- read.csv("data/cranePics.csv")

# Load BBS data 

countRoutes <- read.csv("data/countRoutes.csv") 

# Load ebird data

ecranes <- read.csv("data/ecranes.csv") 

# define user interface using navbar page

ui <- navbarPage("Sandhill Crane app",
                 
# Define the breeding distribution tab panel                 
                 
  tabPanel(title = "Breeding distribution",
    # Add a sidebar layout 
    sidebarLayout(
      # Add a sidebar panel
      sidebarPanel(
        # Add information on cranes and prompt user to use slider
          h4("Sandhill cranes (Antigone canadensis)
               have historically faced
               many conservation challenges including
               over harvest and habitat loss. Modern
               crane populations benefit from conservation
               of their breeding grounds and migratory staging
               areas and stopovers."),
        br(), # linebreak
        sliderInput(inputId = "years", # what shiny sees
                    label = "Use the slider to investigate changes
                             in the number of sandhill cranes observed
                             during the Breeding Bird Survey from 1966
                             to 2019*. Though sandhill cranes still face
                             many challenges, especially east of the
                             Mississippi, the midcontinent populations
                             have increased consistently since the start
                             of the BBS in 1966.", # what the user sees
                    min = 1966, # minimum
                    max = 2019, # maximum
                    value = 1966, # starting value
                    sep = "" # drop the comma b/c these are years
      ), # Close slider input
      # add a little disclaimer b/c we're just plotting raw data.  
      
      "*This map represents raw count datafrom BBS routes.
        For analysis of trends in breeding crane abundance in
        the US and Canada, check out the", 
      
      # Link to the BBS website to direct folks to actual analyses
      
      a("BBS website", href = "https://www.mbr-pwrc.usgs.gov/bbs/bbs.html")
      ), # Close sidebarPanel
    mainPanel( # open main panel
      # Render the crane map
      leafletOutput("craneMap",
                    height = "800px") # Set the plot size
      ) # Close mainPanel
    ) # Close sideBarLayout
  ), # Close the breeding distribution tab panel

# Open the community science tab panel

  tabPanel(title = "Community Science",
         # Add a sidebar layout
    sidebarLayout(
           # Add a sidebar panel
           sidebarPanel(
           # Add a little information about ebird data
          h4("In recent years", 
              a("ebird", href = "https://ebird.org/species/sancra"), 
                "observations have become an important part of research on 
                 how bird use increasingly human-dominated landscapes."), br(), 
          # Add a slider text input to pick from a list of choices using a slider. 
          sliderTextInput(inputId = "season", # what shiny sees
                       label = "Use the slider bar to check out how 
                                ebird observations from 2015-2020 represent the cranes'
                                migration from their breeding grounds in the Northern U.S.
                                and Canada to their wintering grounds in the Southern US and 
                                Mexico.", # what the user sees
                       choices = month.name, # choosing from the built in list of months
                       selected = "January"
           ), # Close slider input
           
             br(), # linebreak
                        
             ), # close the sidebarepanel 
             # Render the ebird map
             mainPanel(
             leafletOutput("ebirdMap",
                           height = "800px") # Set the plot size
           ) # Close mainPanel
         ) # Close sideBarLayout
    ), # Close the community science tab panel

  tabPanel("Crane facts", # Open a second tabPanel for fun crane facts
           
           # Add a fluid row so we can control our layout using rows and columns 
           
           fluidRow(uiOutput("cranePic"), # Render the crane pic from server
                    align = "center" # Center-align the picture
            ), # Close the fluidRow
           br(), # add a line break
           
           # Add a fluid row so we can control our layout using rows and columns 
           
           fluidRow( 
             column( # Begin a column within the fluid row
                 h3(textOutput("craneFact")), # Text output from server
                    width = 6, # Define column width out of 12
                    offset = 3,# The offset out of 12 to either side
                    align = "left" # Left align within our center column, 
                                    # for a center-left appearence.
             ) # Close the column
              ), # Close the fluid row
           br(), # Add a line break
           # Accessibility: use tool tips to make images accessible
           fluidRow( # Create a third row
             # Create an action button for users to click
               actionButton(inputId = "craneButton",# Call it craneButton for shiny
                                 # Label it "Click for a crane fact" for the users
                                 label = "Click for a crane fact!", 
                                 # Label it "Click for a crane fact" for screen readers
                                 title = "Click for a crane fact!"
                             ), # Close the action button
                    align = "center" # Center align the button
                  ) # Close the row
  ), # Close the tab panel

  # Add a References tab panel, linking each Reference to its source.
  tabPanel("References",
           wellPanel(
             h2("References", align = "center"),
             h3("1.", a("All About Birds", href = "https://www.allaboutbirds.org/guide/Sandhill_Crane/id")),
             h3("2.", a("Breeding Bird Survey Data", href = "https://www.mbr-pwrc.usgs.gov/bbs/bbs.html")),
             h3("3.", a("Ebird", href = "https://ebird.org/species/sancra")),
             h3("4.", a("Open Topographic Map", href = "https://opentopomap.org/#map=5/49.000/10.000")),
             h3("5.", a("U.S. Fish and Wildlife Service Sandhill Crane Resources",
                        href = "https://www.fws.gov/birds/surveys-and-data/webless-migratory-game-birds/sandhill-cranes.php")),
            )
          )
        )

server <- function(input, output) {
  
  # Compartmentalizing: break up our reactive elements
  # to process the data in one step to include only the 
  # the user-specified year
  
  countDat <- reactive({
    
    countRoutes <- countRoutes %>%
                   filter(Year == input$years)
    
  })
  
  # filter ebird data to only those dates specified by the slider
  
  ebirdDat <- reactive({

    # filter to the selected month

    ecranes <- ecranes %>%
               filter(Month == input$season)

  })
  
  
  output$ebirdMap <- renderLeaflet({
    
    # bring data into crane map context using contDat from above
    
    ebirdDat <- ebirdDat()
    
    leaflet(options = leafletOptions(nowrap = TRUE)
    ) %>%
      # Add topomap tiles
      addProviderTiles(providers$OpenTopoMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      # Add markers of crane surveys w/ numbers of cranes detected
      # and clustering
      addCircleMarkers(lng = ebirdDat$LONGITUDE,
                       lat = ebirdDat$LATITUDE,
                       fillColor = "black",
                       stroke = FALSE,
                       fillOpacity = 0.75,
                       clusterOptions = markerClusterOptions()
      ) %>%
      # Restrict view
      setView(lng = -100,
              lat = 50,
              zoom = 4) %>%
      # Restrict map bounds
      setMaxBounds(lng1 = -175,
                   lng2 = 20,
                   lat1 = 0,
                   lat2 = 160)
    
    
  })
  
  # and in the next step to render our map
  # with lots of pretty leaflet elements.
  
  output$craneMap <- renderLeaflet({
    
    # bring data into crane map context using contDat from above
    
    countDat <- countDat()

    leaflet(options = leafletOptions(nowrap = TRUE)
            ) %>%
      # Add topomap tiles
      addProviderTiles(providers$OpenTopoMap,
                       options = providerTileOptions(noWrap = TRUE)
                       ) %>%
      # Add markers of crane surveys w/ numbers of cranes detected
      # and clustering
      addMarkers(lng = countDat$Longitude,
                       lat = countDat$Latitude,
                       label = paste0(countDat$SpeciesTotal, " cranes counted, ",
                                     "Longitude = ", 
                                     round(countDat$Longitude, digits = 2), ", ",
                                     "Latitude = ",
                                     round(countDat$Latitude, digits = 2)),
                 clusterOptions = markerClusterOptions()
                ) %>%
      # Restrict view
      setView(lng = -100,
              lat = 50,
              zoom = 4) %>%
      # Restrict map bounds
       setMaxBounds(lng1 = -175,
                    lng2 = 20,
                   lat1 = 0,
                   lat2 = 160)


  })
  
  # observe the button press

    picDF <-  eventReactive(input$craneButton, {
  
  # When the button is pressed, sample one row from the pic list dataframe
        
             picDF <- picList %>%
                      sample_n(1)
    
             })

  # render the corresponding image
  
    output$cranePic <- renderUI({
    
      # Grab the whole one-line dataframe
      
            picDF <- picDF()
            
      # Use the pic column to set an image path
            
            path <- picDF$pic
            
      # Use the alt column to define alt text
            
            alt <- picDF$alt
            
      # Use the fact column to define the fun fact
            
            fact <- picDF$fact
            
      # render the image using the path and facts.
            
            img(src = path,
                width = "500px",
                title = paste0("Image description: ", alt,
                              ". Fun fact: ", fact),
                alt = alt)
      
    })

    # Print the facts as normal text 

    output$craneFact <- renderText({
      
      picDF <- picDF()
      
      picDF$fact
    
    })
    

}

shinyApp(ui, server)