
library(shiny)
library(rfishbase)
library(tidyverse)

ui <- fluidPage(

      navbarPage("A very crappie fish app"),
      sidebarLayout(
             sidebarPanel(checkboxGroupInput(inputId = "fishChoices",
                                      label = "Choose some fish",
                                      choices = c("Walleye","Lake trout","Yellow perch","Northern pike","Muskellunge","Burbot", "Lake sturgeon"))),
               mainPanel(plotOutput("lengthPlot")))
)

server <- function(input, output) {
  List <- data.frame(Species = c("Sander vitreus","Salvelinus namaycush","Perca flavescens", "Esox lucius","Esox masquinongy","Lota lota","Acipenser fulvescens"),
                        Common_Name = c("Walleye","Lake trout","Yellow perch","Northern pike","Muskellunge", "Burbot","Lake sturgeon")) 
  numbers <-  List %>%
              left_join(popgrowth(), by = "Species") %>%
              group_by(Species) %>%
              mutate(lAsym = mean(TLinfinity, na.rm = TRUE),
                     k = mean(K, na.rm = TRUE),
                     t0 = mean(to, na.rm = TRUE),
                     tmax = round(mean(tmax, na.rm = TRUE))) %>%
              select(Species, Common_Name, lAsym, k, t0, tmax) %>% 
              distinct()
            Walleye <- data.frame(Age = seq(0:numbers$tmax[numbers$Common_Name == "Walleye"]), Common_Name = "Walleye")
            Walleye$Length <- numbers$lAsym[numbers$Common_Name == "Walleye"]*(1-exp(-numbers$k[numbers$Common_Name == "Walleye"]*(Walleye$Age - numbers$t0[numbers$Common_Name == "Walleye"])))
            Perch <- data.frame(Age = seq(0:numbers$tmax[numbers$Common_Name == "Yellow perch"]), Common_Name = "Yellow perch")
            Perch$Length <- numbers$lAsym[numbers$Common_Name == "Yellow perch"]*(1-exp(-numbers$k[numbers$Common_Name == "Yellow perch"]*(Perch$Age - numbers$t0[numbers$Common_Name == "Yellow perch"])))
            Trout <- data.frame(Age = seq(0:numbers$tmax[numbers$Common_Name == "Lake trout"]), Common_Name = "Lake trout")
            Trout$Length <- numbers$lAsym[numbers$Common_Name == "Lake trout"]*(1-exp(-numbers$k[numbers$Common_Name == "Lake trout"]*(Trout$Age - numbers$t0[numbers$Common_Name == "Lake trout"])))
            Pike <- data.frame(Age = seq(0:numbers$tmax[numbers$Common_Name == "Northern pike"]), Common_Name = "Northern pike")
            Pike$Length <- numbers$lAsym[numbers$Common_Name == "Northern pike"]*(1-exp(-numbers$k[numbers$Common_Name == "Northern pike"]*(Pike$Age - numbers$t0[numbers$Common_Name == "Northern pike"])))
            Muskellunge <- data.frame(Age = seq(0:numbers$tmax[numbers$Common_Name == "Muskellunge"]), Common_Name = "Muskellunge")
            Muskellunge$Length <- numbers$lAsym[numbers$Common_Name == "Muskellunge"]*(1-exp(-numbers$k[numbers$Common_Name == "Muskellunge"]*(Muskellunge$Age - numbers$t0[numbers$Common_Name == "Muskellunge"])))
            Burbot <- data.frame(Age = seq(0:numbers$tmax[numbers$Common_Name == "Burbot"]), Common_Name = "Burbot")
            Burbot$Length <- numbers$lAsym[numbers$Common_Name == "Burbot"]*(1-exp(-numbers$k[numbers$Common_Name == "Burbot"]*(Burbot$Age - numbers$t0[numbers$Common_Name == "Burbot"])))
            Sturgeon <- data.frame(Age = seq(0:numbers$tmax[numbers$Common_Name == "Lake sturgeon"]), Common_Name = "Lake sturgeon")
            Sturgeon$Length <- numbers$lAsym[numbers$Common_Name == "Lake sturgeon"]*(1-exp(-numbers$k[numbers$Common_Name == "Lake sturgeon"]*(Sturgeon$Age - numbers$t0[numbers$Common_Name == "Lake sturgeon"])))
            fish <- do.call(rbind, list(Walleye, Perch, Trout, Pike, Muskellunge, Burbot, Sturgeon)) 
            fishLengths <- reactive({
                            fishLengths <- fish %>% filter(Common_Name %in% input$fishChoices)
                              })
            output$lengthPlot <- renderPlot({
                                 graphFish <- fishLengths()
                                 ggplot(graphFish) +geom_line(aes(x = Age, y = Length, group = Common_Name, color = Common_Name)) +theme_bw()
                                 })

}

shinyApp(ui = ui, server = server)
