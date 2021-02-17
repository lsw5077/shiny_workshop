rm(list=ls())
library(shiny)
library(tidyverse)

# read in fish data
# shiny will be able to find data in its home directory 
# and downstream directories.

fish <- read.csv('data/fishy_data.csv')

# Make a dataframe of fish names

uni.spp <- c('All',sort(unique(fish$Common_Name)))

ui <- navbarPage("A slightly less crappie fish app", # open navbar page
        tabPanel("Age x Length", # open length tab panel
          sidebarLayout( # open sidebarlayout
            sidebarPanel( # open sidebar panel
              # In the sidebarpanel, create a selectize input where
              # users can select multiple fish
              selectizeInput(inputId = "fishChoices",
                             label = "Select a fish species to plot its
                                      length as a function of age.",
                             choices = uni.spp,
                             multiple = TRUE,
                             selected='All'),
              withMathJax(
              'L_{t}')),
            # plot the length plot in the main panel
            mainPanel(plotOutput("lengthPlot"),
                      img(src = "length.png",
                       align = "center"))
          )
        ),
        tabPanel("Age x Mass",
                 sidebarLayout(
                   sidebarPanel(
                     selectizeInput(inputId = "fishChoices2",label = "Select a fish species",choices = uni.spp,multiple=T,selected='All')),
                    mainPanel(plotOutput("weightPlot"),img(src = "mass.png", align = "center"))
                 )
        )
)
server <- function(input, output) {
  output$lengthPlot <- renderPlot({
    cond='All' %in% input$fishChoices
    if (cond) graphFish=fish
    if (!cond) graphFish <- fish %>% filter(Common_Name %in% input$fishChoices)
    ggplot(graphFish) +
      geom_line(aes(x = Age, y = Length, group = Common_Name, color = Common_Name),size=1.5) +
      theme_bw() +
      theme(axis.text=element_text(size=15),
            axis.title=element_text(size=20,face="bold"),
            legend.text=element_text(size=15),
            legend.title=element_text(size=20))
  })
  output$weightPlot <- renderPlot({
    cond='All' %in% input$fishChoices2
    if (cond) graphFish=fish
    if (!cond) graphFish <- fish %>% filter(Common_Name %in% input$fishChoices2)
    ggplot(graphFish) +
      geom_line(aes(x = Age, y = mass, group = Common_Name, color = Common_Name),size=1.5) +
      theme_bw() +
      theme(axis.text=element_text(size=15),
            axis.title=element_text(size=20,face="bold"),
            legend.text=element_text(size=15),
            legend.title=element_text(size=20))
  })
}
shinyApp(ui = ui, server = server)
