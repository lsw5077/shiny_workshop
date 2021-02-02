
library(shiny)
library(shinydashboard)
library(shinyWidgets)


ui <- fluidPage(
    navbarPage("Crane game",
    tabPanel("Crane life cycle",
             ),
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
         sidebarLayout(
             sidebarPanel(),
             mainPanel(
        "pick your ‘best scenario’ and give it a “1”. 
         Pick your worst and give it a “0”.  Others 
         should fit in between 1 and 0, but you can
         also give other 1’s and 0’s.", br(),
         wellPanel("Liberal harvest (3-6%)", br(),
                    sliderInput(inputId = "outcomeVals1",
                                label = "Population exceeds total",
                                min = 0,
                                max = 1,
                                value = 0.5), br(),
                    sliderInput(inputId = "outcomeVals2",
                                label = "Population within goal",
                                min = 0,
                                max = 1,
                                value = 0.5), br(),
                    sliderInput(inputId = "outcomeVals3",
                                label = "Population below goal",
                                min = 0,
                                max = 1,
                                value = 0.5)),br(),
         wellPanel("No harvest", br(),
                   sliderInput(inputId = "outcomeVals4",
                               label = "Population exceeds total",
                               min = 0,
                               max = 1,
                               value = 0.5), br(),
                   sliderInput(inputId = "outcomeVals5",
                               label = "Population within goal",
                               min = 0,
                               max = 1,
                               value = 0.5), br(),
                   sliderInput(inputId = "outcomeVals6",
                               label = "Population below goal",
                               min = 0,
                               max = 1,
                               value = 0.5))
         )
    
      )
    )
    )
)
                  
    


server <- function(input, output) {
    

}

shinyApp(ui, server)