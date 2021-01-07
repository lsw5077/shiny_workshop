# Load libraries and data
# We only need to do these things once

library(shiny)
library(RColorBrewer)
fishDat <- read_rds("data/fishDat.RData")

# Let's make a custom color palette

fishPal <- c("Walleye" = "#000000",
              "Lake trout" = "#E69F00",
              "Yellow perch" = "#56B4E9",
              "Northern pike" = "#009E73",
              "Muskellunge" = "#0072B2",
              "Burbot" = "#D55E00",
              "Lake sturgeon" = "#CC79A7")

ui <- fluidPage(
  
  # Create a navbar page
  
  navbarPage("Sofishticated growth app"),
  
  # Use a sidebar layout with user inputs on the left and 
  # the reactive plot in the main panel.
  
  sidebarLayout(
    sidebarPanel(h3("Welcome to our most sofishticated app!"),
                 br(), 
                 
     # Add some helpful and informative text.
                 
     "Please select one or more fish species to 
     view their predicted growth trajectory over
     time. These graphs predict growth according to
     an intederminate growth model, per Beverton and 
     Holt (1957).", br(),
     
     # Make a checkboxGroup input where users can select
     # multiple fish species. 
     
      checkboxGroupInput(

      inputId = "fishChoices",
                 label = "",
                 choices = c("Walleye",
                 "Lake trout",
                 "Yellow perch",
                 "Northern pike",
                 "Muskellunge",
                 "Burbot",
                 "Lake sturgeon"))),
    
    mainPanel(plotOutput("lengthPlot"))
    )
  
)

  

server <- function(input, output) {
  
  # Define a reactive value called fishLengths
  # by filtering the fishDat variable we loaded up above
  # to include only records whose common names are in the user selection
  
  
  fishLengths <- reactive({
    
    fishLengths <- fishDat %>% 
                   filter(Common_Name %in% input$fishChoices)
    
  })
  
  
  # Make a plot of user-selected fish species, cleaned up just a little.
  
  output$lengthPlot <- renderPlot({
    
    graphFish <- fishLengths()
    
    # Add some nice long-form text wrapping
    # Graphing fish length as a function of age and species
    # using coord_cartesian to keep the plot size at the maximum
    # and scale_color_manual to show our custom colors
    
    ggplot(graphFish) +
      geom_line(aes(x = Age, y = Length,
                    group = Common_Name,
                    color = Common_Name),
                    size = 2) +
      scale_color_manual(values = fishPal) +
      labs(x = "Age", y = "Length(cm)", color = "Common Name") +
      coord_cartesian(xlim = c(-1, 70),
                      ylim = c(0, 175)) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text = element_text(size = 24, color = "black"),
            axis.title = element_text(size = 36, color = "black"))
    
 
      })
    

  }

shinyApp(ui = ui, server = server)
