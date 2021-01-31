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

# Make a list of colors for us to use
# in the next step.

r_colors <- rev(colorspace::terrain_hcl(n = 254))

# define user interface using fluid page

ui <- fluidPage(
  
  # display object "mymap" from leafletOutput 
  
  leafletOutput("mymap"),
  
  # html tag to make a new paragraph
  
  p(),
  
  # Define a slider input, where numbers are slid up and down.
  
  sliderInput(inputId = "pop.thresh",
              label='Minimum Population size',
              min = 1000000,
              max = 20000000,
              value = 3000000,
              width = NULL,
              dragRange = FALSE)
)

# Define the server, operations that happen on the computer side

server <- function(input, output, session) {

  # build an output object using renderLeaflet({}) a reactive object!
  
  output$mymap <- renderLeaflet({
    
    # filter world pop to only those cities with population larger than
    # the user's input in the numeric input then
    # define the spatial points data from the corresponding cities.
    
    points <- world.cities %>%
              filter(pop >= input$pop.thresh) 
    
    # make a leaflet map using the stamen tonerlite provider

    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(lng = points$long,
                 lat = points$lat,
                 label = points$name) # add our user-specified points 
  })
}

# run the app!

shinyApp(ui, server)