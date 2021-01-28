library(shiny)
library(tidyverse)
library(viridis)
library(shinydashboard)

# UI for an app that plots small mammal mass as a function of hindfoot length

ui <- dashboardPage(
  dashboardHeader(title = "Homeranger"),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)
