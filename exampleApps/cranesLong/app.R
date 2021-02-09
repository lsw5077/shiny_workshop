
library(shiny)
library(shinydashboard)
library(shinyWidgets)


ui <- fluidPage(
    navbarPage("Crane game",
    tabPanel("Crane migration",
             mainPanel(
                 img(src = "craneMap.png",
                     width = "150%",
                     title = "From ebird: 
                             Sandhill Crane (Antigone canadensis). 
                             Relative abundance is 
                             depicted for each season along a 
                             color gradient from a light color 
                             indicating lower relative abundance
                             to a dark color indicating a higher
                             relative abundance. Relative abundance
                             is the estimated number of individuals
                             detected by an eBirder during a traveling
                             count at the optimal time of day."),
                       
                       tags$audio(src = "cranes.mp3", 
                            type = "audio/mp3",
                            autoplay = NA)
             )
    ),
    # rank outcomes
    tabPanel("Data input",
         
      )
    )
    )

                  
    


server <- function(input, output) {
    

}

shinyApp(ui, server)