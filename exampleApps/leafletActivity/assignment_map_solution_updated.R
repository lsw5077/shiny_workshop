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


# 

world.cities <- world.cities %>%
                filter(pop >= 500000) %>%
                rbind(data.frame(name = "None",
                                 country.etc = "None",
                                 pop = 30000000,
                                 lat = NA,
                                 long = NA,
                                 capital = 0))


# make a list of cities for choices

unique.cities = c(sort(unique(world.cities$country.etc)))

# read in the raster

NDVI <- raster("NDVI_lite.tif")

# Make a list of NDVI-suggestive colors for us to use

r_colors <- rev(colorspace::terrain_hcl(n = 254))

# legend colors for leaflet map

legend_colors <- r_colors[seq(25, 254, 25)]

# legend labels for leaflet map

legend_labels <- seq(25, 254, 25)

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
                   min = 500000,
                   max = 20000000,
                   value = 3000000,
                   width = NULL,
                   dragRange = FALSE),
      
      # 2)	Add a drop down list to allow users to subset 
      # only the cities that belong to a particular country
      
      selectizeInput(inputId = 'country',
                     label = 'Select country',
                     choices = unique.cities,
                     selected = "None",
                     multiple = TRUE),
      
      # 3)	Add a radio button that allow users to subset 
      # only the capital cities.
      
      radioButtons(inputId = 'capital',
                   label = 'Just capitals?',
                   choices = c('No','Yes'),
                   selected = 'No')
    ),
    # Adjust map size to deal with map reapeating issue
    # You can make this look nicer if you really want
    mainPanel(
      leafletOutput("mymap",
                    width = "150%",
                    height = 600),  
      
      textOutput("countries")
    )
  )
)

server <- function(input, output, session) {

  # build an output object using renderLeaflet({}) a reactive object!

  output$countries <- renderText({(input$country)})
  
  
  output$mymap <- renderLeaflet({

    # make a leaflet map using the stamen tonerlite provider
    # Add NDVI using our custom color pallette. 
    
    my_map <- leaflet() %>%
      # Set the map zoom to pan to the center of the selected points
      setView(lng = 0,
              lat = 0,
              zoom = 2) %>% 
      # Add fun watercolor tiles
      addProviderTiles(providers$Stamen.Watercolor,
                       options = providerTileOptions(noWrap = FALSE)) %>%
      # Add raster
      addRasterImage(NDVI,
                     opacity = 0.5,
                     colors = r_colors) %>%
      # Add legends using our colors and labels from above
      addLegend(colors = legend_colors,
                labels = legend_labels,
                title = 'NDVI')
    
    # filter world pop to only those cities with population larger than
    # the user's input in the numeric input
    # If the user selects capitals only, filter to capitals, otherwise keep all rows
    # If the user leaves "all" selected as the company option, keep all rows, otherwise
    # filter to only the country the user selects
    
    points <- world.cities %>%
              filter(pop >= input$pop.thresh)  %>%
              filter(case_when(input$country == "None" ~ !is.na(pop),
                               input$country != "None" & ~ country.etc %in% list(input$country))) %>%
              filter(case_when(input$capital == "Yes" ~ capital == 1))

    # Add a radio button that allow users to subset only the capital cities.
  
    
    
    # 2. Add a drop down list to allow users to subset 
    # only the cities that belong to a particular country
    
    # if(input$country == "All") {
    #   
    #   points <- points 
    #   
    #   # filter to include dataframe in list of input countries.
    #   
    # } else{points <- points %>%
    #        filter (country.etc %in% input$country)}
    

    # If the user's inputs result in a dataframe with at least one row, 
    # add labels.
    
    if (nrow(points) >= 1) {
      
      my_map <- my_map %>%
                addMarkers(lng = points$long,
                           lat = points$lat,
                           label = points$name) %>%
                setView(lng = mean(points$lon, na.rm = TRUE), 
                        lat = mean(points$lat, na.rm = TRUE),
                        zoom = 2)
 
    } else{my_map <- my_map}

    # print my map
    
    my_map
    
  })
}

shinyApp(ui, server)