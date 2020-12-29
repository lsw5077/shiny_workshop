# Load libraries and do other one-time tasks outside main app code.

library(shiny)

# user interface
# ui defines everything the user sees and interacts with
# Fluid page organizes shiny app into rows and columns.


ui <- fluidPage( 
          # SidebarLayout creates a page with 2 panels: side and main. 
                 
                 sidebarLayout(
                   
                   # sidebarPanel makes the container that holds the sidebar content
                   # We'll start by displaying a welcome message
                   
                   sidebarPanel(
                     "Welcome to our Loblolly Pine app!", br(), br(),
                     "Please select the ages of pines whose
                        growth you wish to view", br(), br(),
                     
                     # And then add our reactive element: a slider input. 
                     # The slider controls the ages of the pines that will show up in our growth plot
                     # We give it a name, "ages" for us to pass to server
                     # and a name to display to the user, "Pine Ages"
                     
                     sliderInput("ages",
                                 "Pine Ages", 
                                 min = 1, # We give the slider a minimum value
                                 max = 25, # And a maximum value
                                 value = 25)  # And set the default value for the slider to start at
                   ),
                   
                   mainPanel(
                     
                     # Display pine plot
                     
                     plotOutput("pinePlot")
                     
                   )
               )
            )
                 

# server is where the magic happens
# all the back-end stuff
# manipulating data
# building reactive visuals

server <- function(input, output) {
  
  # Our server has one element in the output object: a plot called pinePlot    
  # We create pinePlot using the renderPlot() function
  # renderPlot() takes input from ui, uses it to refine the data, and 
  # creates the Loblolly growth plot, then passes it back to ui.
  
  output$pinePlot <-  renderPlot({
    
    # We subset our data using the input from the slider in the side panel.            
    
    LoblollySmall <- Loblolly[Loblolly$age <= input$ages,]
    
    # Then make our plot as before!
    
    plot(LoblollySmall$age, LoblollySmall$height,
         xlab = "Age (Years)",
         ylab = "Height (Feet)")
  })
}

# Runs the app

shinyApp(ui = ui, server = server)
