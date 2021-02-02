# This app skeleton shows best practices for shiny.
# The reigning logic is: Do things once.

# Load libraries

library(shiny)

# Load data:
## datasets to display
## supporting datasets like palettes and options

# Source functions, remembering that we use
# relative paths

source("bpFunctions.R")


# Define UI 

ui <- fluidPage(
    
    # Define UI, remembering to use accessible interfaces
    # color-blind safe colors
    # screen-readable text, equations
    # tool tips for data.
    # acknowledgements page for citations and funding

   
)

# Define server logic

server <- function(input, output) {

    # compartmentalize server for ready debugging.
    # process data based on user inputs
    
    
    data <- reactive({
        
    })
    
    # Make outputs based on data processed per
    # user inputs
    
    output$plot <- renderPlot({
        
        data <- data()

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
