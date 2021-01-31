#Modified from https://rstudio.github.io/leaflet/shiny.html
#Some useful explanation about leaflet: https://rstudio.github.io/leaflet/popups.html

# Load libraries

rm(list=ls())
library(shiny)
library(leaflet)
library(maps)
library(raster)
library(rgdal)
library(tidyverse)

# make a list of cities for choices

unique.cities = c('All', sort(unique(world.cities$country.etc)))

# read in the raster

NDVI <- raster("NDVI_lite.tif")

# Make a list of NDVI-suggestive colors for us to use

r_colors <- rev(colorspace::terrain_hcl(n = length(unique(NDVI))))

# define user interface using fluid page

ui <- fluidPage(
  
  # 1) add a title panel. 
  
  titlePanel('Map of big cities'),
  
  # 4)	Create a side bar that will collect all 
  # the input that we want from the user (e.g., 
  # minimum population size and desired country)
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "pop.thresh",
                   label='Minimum Population size',
                   min = 1000000,
                   max = 20000000,
                   value = 3000000,
                   width = NULL,
                   dragRange = FALSE),
      
      # 2)	Add a drop down list to allow users to subset 
      # only the cities that belong to a particular country
      
      selectInput(inputId = 'country',
                  label = 'Select country',
                  choices = unique.cities,
                  selected = 'None'),
      
      # 3)	Add a radio button that allow users to subset 
      # only the capital cities.
      
      radioButtons(inputId = 'capital',
                   label = 'Just capitals?',
                   choices = c('No','Yes'),
                   selected = 'No')
    ),
    mainPanel(
      leafletOutput("mymap")  
    )
  )
)

server <- function(input, output, session) {

  # build an output object using renderLeaflet({}) a reactive object!
  
  pointDat <- reactive({

    # filter world pop to only those cities with population larger than
    # the user's input in the numeric input
    # If the user selects capitals only, filter to capitals, otherwise keep all rows
    # If the user leaves "all" selected as the company option, keep all rows, otherwise
    # filter to only the country the user selects
    
    points <- world.cities %>%
              filter(pop >= input$pop.thresh) 
    
    
    # Add a radio button that allow users to subset only the capital cities.
    
    if(input$capital == "Yes") {

      points <- points %>%
                filter(capital == 1)

    } else{points <- points}
    
    
    # 2. Add a drop down list to allow users to subset 
    # only the cities that belong to a particular country
    
    if(input$country == "All") {
      
      points <- points 
      
    } else{points <- points %>% filter (country.etc == input$country)}
    

  })
  

  output$mymap <- renderLeaflet({
    
    points <- pointDat()
    
    # make a leaflet map using the stamen tonerlite provider
    # Add NDVI using our custom color pallette. 
    
    my_map <- leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = FALSE)) %>%
      addRasterImage(NDVI,
                     opacity = 0.5,
                     colors = r_colors)
    
    
    # If the user's inputs result in a dataframe with at least one row, 
    # add labels.
    
    if (nrow(points) >= 1) {
      
      my_map <- my_map %>%
                addMarkers(lng = points$long,
                           lat = points$lat,
                           label = points$name)
 
    }

    # print my map
    
    my_map
    
  })
}

shinyApp(ui, server)