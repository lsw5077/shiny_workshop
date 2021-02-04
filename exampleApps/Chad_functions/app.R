library(shiny)
library(base64enc)

ui <- fluidPage(

    titlePanel("Shiny Input Tools"),
    sidebarLayout(
        sidebarPanel(
                     helpText("inputFunction(inputId = ID for internal Use,
                            label = Label to display to users, input specific parameters)"),
                     
                     #List various input tools
                     numericInput("numeric", "Enter a number", min = 1, max = 100, 
                                  value = 50, step=1),
                    
                     sliderInput("slider", "Select a number", min=1, max=100, 
                                 value=50, step=1),
                    
                     sliderInput("doubleSlider", "Select a numeric range", 
                                 min=1, max=100, value=c(25,75), step=1),
                    
                     dateInput("date", "Enter a date", min="2000-01-01", 
                               max="2021-12-31",  value = "2021-02-02", 
                               format = "mm/dd/yyyy"),
                    
                     dateRangeInput("dateRange", "Enter a start and end date", 
                                    min="2000-01-01", max="2021-12-31", 
                                    start="2021-02-02", end="2021-03-02", format="mm/dd/yyyy"),
                    
                     textInput("text", "Enter your name", placeholder="Enter Text..."),
                    
                     passwordInput("password", "Enter your password", placeholder = "Password..."),
                     
                     fileInput("file", "Upload a file", multiple=FALSE, accept=c("image/png","image/jpeg"),
                               buttonLabel = "Select File...", placeholder = "No file selected"),
                    
                     checkboxInput("checkbox", "Check the box to return a 'True' value"),
                    
                     checkboxGroupInput("groupCheckbox", "Check the boxes to enable the following options",
                                        choices = c("Option 1" = "1", "Option 2" = "2", "Option 3" = "3")),
                     
                     radioButtons("radio", "Choose between the following",
                                  choices = c("Choice 1" = "1", "Choice 2" = "2", "Choice 3" = "3")),
                    
                     selectInput("select", "Select one or more of the following",
                                 c("Choice A" = "A", "Choice B" = "B", "Choice C" = "C", "Choice D" = "D", "Choice E" = "E", "Choice F" = "F"),
                                 multiple=T),
                    
                     actionButton("button", "Click Me")),
        mainPanel(
            textOutput("numeric"),
            textOutput("slider"),
            textOutput("doubleSlider"),
            textOutput("date"),
            textOutput("dateRange"),
            textOutput("text"),
            textOutput("password"),
            uiOutput("file"),
            textOutput("checkbox"),
            textOutput("groupCheckbox"),
            textOutput("radio"),
            textOutput("select")
        )
    )
)

server <- function(input, output) {
    #inputs are called using input$inputId
    #outputs are called using output$outputId
    
    output$numeric<-renderText(c("Your numeric entry is ", input$numeric))
    output$slider<-renderText(c("Your slider number is ", input$slider))
    output$doubleSlider<-renderText(c("Your slider range is ", paste(input$doubleSlider, collapse = " to ")))
    output$date<-renderText(c("The date you selected is ", as.character(input$date, "%m/%d/%Y")))
    output$dateRange<-renderText(c("The date range you selected is ", paste(as.character(input$dateRange, "%m/%d/%Y"), collapse= " to ")))
    output$text<-renderText(c("Your name is ", input$text))
    output$password<-renderText(c("Your password is ", input$password))
    output$file<-renderUI({
        req(input$file)
        base64 <- dataURI(file = input$file$datapath, mime = input$file$type)
        tags$img(src = base64, alt= "error")
        })
    output$checkbox<-renderText({
        if(input$checkbox==T)
            "The box is checked"
        else
            "The box is not checked"
    })
    output$groupCheckbox<-renderText(c("The following options are selected: ", paste(input$groupCheckbox, collapse = ", ")))
    output$radio<-renderText(c("You made choice ", input$radio))
    output$select<-renderText(c("You selected the following choices: ", paste(input$select, collapse = ", ")))
    
    observeEvent(input$button, {
        showModal(modalDialog(
            title = "The button was clicked",
            "The observeEvent() function is set to trigger this message every time the value of input$button changes"))
    })
}

shinyApp(ui = ui, server = server)
