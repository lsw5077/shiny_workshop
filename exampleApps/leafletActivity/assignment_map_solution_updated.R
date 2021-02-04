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

# make world.cities a little lighter so our app can run a little faster

world.cities <- world.cities %>%
                filter(pop >= 100000) 

# Make options for selectize input

unique.cities = c("All", sort(unique(world.cities$country.etc)))

# Make a custom "not in" function for filtering

`%nin%` <- negate(`%in%`)

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
                   min = 400000,
                   max = 20000000,
                   value = 3000000,
                   width = NULL,
                   dragRange = FALSE),
      
      # 2)	Add a selectize input so users can select multiple countries
      
      selectizeInput(inputId = 'country',
                     label = 'Select country',
                     choices = unique.cities,
                     selected = "All",
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
                    width = "95%",
                    height = 650)
      
    )
  )
)

server <- function(input, output){
  
  # build an output object using renderLeaflet({}) a reactive object!
  
  output$mymap <- renderLeaflet({
    
    # filter world pop to only those cities with population larger than
    # the user's input in the numeric input
    # If the user selects capitals only, filter to capitals, otherwise keep all rows
    # If the user leaves "all" selected as the company option, keep all rows, otherwise
    # filter to only the country the user selects
    
    points <- world.cities %>%
      rowwise() %>%
      filter(pop >= input$pop.thresh) %>%
      filter(case_when("All" %in% input$country ~ pop >= input$pop.thresh
                       ,length(input$country) == 0 ~ country.etc == "None"
                       ,length(input$country) > 0 & "All" %nin% input$country ~
                         country.etc %in% input$country)) %>%
      filter(case_when(input$capital == "Yes" ~ capital == 1
                       ,input$capital == "No" ~ capital == 1 | capital == 0)) %>%
      as.data.frame()
    
    # Calculate the number of points in the dataframe
    
    npoints <- nrow(points)

    # If the dataframe is empty, overwrite it with a prompt to 
    # choose a better combination
    
    if(npoints == 0) {
      
      points <- data.frame(name = "Select a valid population-country-capital combination to view cities!",
                           country.etc = NA,
                           pop = NA,
                           lat = 36,
                           long = -40,
                           capital = NA)
    }
    
    my_map <- leaflet(options = leafletOptions(nowrap = TRUE)) %>%
      # Add fun watercolor tiles
      addProviderTiles(providers$Stamen.Watercolor,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      #Add raster
      addRasterImage(NDVI,
                     opacity = 0.5,
                     colors = r_colors) %>%
      #Add legends using our colors and labels from above
      addLegend(colors = legend_colors,
                labels = legend_labels,
                title = 'NDVI') %>%
      addMarkers(lng = points$long,
                 lat = points$lat, 
                 label = points$name) %>%
      setView(lng = mean(points$long, na.rm = TRUE),
              lat = mean(points$lat, na.rm = TRUE),
              zoom = 2)

    # print my map
    
    my_map
    
  })
}

shinyApp(ui, server)