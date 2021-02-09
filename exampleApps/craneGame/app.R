# Load libraries

library(shiny)
library(leaflet)
library(maps)
library(tidyverse)

# Load picture list data

picList <- read.csv("data/cranePics.csv")

# Load BBS data 

countRoutes <- read.csv("data/countRoutes.csv") 

# define user interface using navbar page

ui <- navbarPage("Sandhill Crane app",
                 
# Define the breeding distribution tab panel                 
                 
tabPanel(title = "Breeding distribution",
    
    # Add a sidebar layout 
    
    sidebarLayout(
      # Add a sidebar panel
      sidebarPanel(
        
        h2("Sandhill crane breeding distribution"),
          br(), # linebreak
            h4("Sandhill cranes (Antigone canadensis)
               have historically faced
               many conservation challenges including
               over harvest and habitat loss. Modern
               crane populations benefit from conservation
               of their breeding grounds and migratory staging
               areas and stopovers."), br(),
            h4("Use the slider to investigate changes
               in the number of sandhill cranes observed
               during the Breeding Bird Survey from 1966
               to 2019.*"),
        br(), # linebreak
        sliderInput(inputId = "years", # what shiny sees
                    label = "Suvey years:", # what the user sees
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
      
      a("BBS website", href = "https://www.mbr-pwrc.usgs.gov/bbs/bbs.html")),
    mainPanel( # open main panel
      
      # Render the crane map
      
      leafletOutput("craneMap",
                    height = "800px") # Set the plot size
      ) # Close mainPanel
    ) # Close sideBarLayout
  ), # Close the breeding distribution tab panel

  tabPanel("Crane facts", # Open a second tabPanel for fun crane facts
           
           # Add a fluid row so we can control our layout using rows and columns 
           
           fluidRow(uiOutput("cranePic"), # Render the crane pic from server
                    #width = 6, # Define column width out of 12
                    #offset = 3, 
                    align = "center" # Center-align the picture
            ), # Close the fluidRow
           br(), # add a line break
           
           # Add a fluid row so we can control our layout using rows and columns 
           
           fluidRow( 
             column( # Begin a column within the fluid row
                 h3(
                 textOutput("craneFact" # Text output from server
                 )
                ),
                    width = 6, # Define column width out of 12
                    offset = 3,# The offset out of 12 to either side
                    align = "left" # Left align within our center column, 
                                    # for a center-left appearence.
             ) # Close the column
              ), # Close hte fluid row
           br(), # Add a line break
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
  ),

  # Add a References tab panel, linking each Reference to its source.
  tabPanel("References",
           wellPanel(
             h2("Resources", align = "center"),
             h3("1.", a("All About Birds", href = "https://www.allaboutbirds.org/guide/Sandhill_Crane/id")),
             h3("2.", a("Breeding Bird Survey Data", href = "https://www.mbr-pwrc.usgs.gov/bbs/bbs.html")),
             h3("3.", a("Stamen Map Tiles", href = "http://maps.stamen.com/#watercolor")),
             h3("4.", a("U.S. Fish and Wildlife Service Sandhill Crane Resources",
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