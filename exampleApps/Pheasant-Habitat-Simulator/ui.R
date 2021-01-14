################################################################################################################################################################################
############################################   Web-based Species Distribution Modeling Project, LSW version 11/10/17 #########################################################
##################################### Authors: Lyndsie Wszola, Caitlyn Gillespie, Erica Stuber, Lindsey Messinger, Lucia Corral, Victoria Simonsen  ############################
################################################################################################################################################################################ 



# UI (user interface) controls how the outputs of the operations specified in Server are displayed. Every command in server relates to an object in UI. 
# Shiny UI is a nested list of pages. Sidebar layout is nested in tab panel is nested in navbar page is nested in UI, etc.
# Note: Shiny allows you to mix R syntax with HTML. Some of what follows, especially where outside links are specified, is in HTML. 

  shinyUI(navbarPage("Pheasant Habitat Simulator", theme = "read", # Sets the pagelayout and selects a CSS theme, which controls the colors and appearence
                     tabPanel("Welcome", # Tab name
                                  sidebarLayout(position = "left",
                                    sidebarPanel(
                                    strong("Welcome to the pheasant habitat simulator!"), # make a bold line
                                  
                                     br(), br(), # line break
                                  
                                    "The map to the right shows the predicted relative habitat suitability for ring-necked pheasant", em("(Phasianus colchicus)"), 
                                    "across the state of Nebraska. The map is based on a statewide survey of Nebraska's pheasant population from 
                                    2010-2012 and a model that relates the number of 'crowing' pheasants at a survey stop to land cover in the
                                    surrounding township. To see how changing the land cover of a township in any part of the state would affect 
                                    habitat suitability for pheasants in that region, click the 'Custom Management' tab and select a county. Use 
                                    the slider bars to adjust the proportions of the major habitats that influence pheasant populations in Nebraska and observe how
                                    pheasant habitat suitability would likely change. To get an idea of how much your changes to the landscape 
                                    might cost over a 10-year period, click the 'Economic Module' tab and check out the interactive economic graph. 
                                    If you want to keep your plots to compare pheasant habitat suitability before and after your landscape changes,
                                    click the 'download your plots' button in the 'Economic Module' tab before and after making your changes."), # intro paragraph
                        
                                    
                                  mainPanel(
                                    img(src='sdm_w_county_labels.jpg', height = "40%", width = "75%", align = "center") # Adds pheasant heat map image, location specified by "src"
                                 
                                       )) 
                     ),
          
                      tabPanel("Custom Management", # Makes new tab, specifies name
                              sidebarLayout(position = "left",
                                sidebarPanel(p("Nebraska is a diverse landscape where the availability of major habitats that influence
                                               pheasant populations differs from county to county. Choose a county to view the average land cover
                                               for a township in that county (top graphic) and what it means for pheasant habitat suitability 
                                               (bottom graphic).  Think you can make it better?  Use the sliders to change the availability of each 
                                               habitat type and see what happens to habitat suitability.  Remember that changing the availability of one
                                               habitat generally requires changing the availability of another. The total landcover cannot
                                               exceed 100%, or the plots will not update."),
                                             br(), 
                                                  
                                      # Creates a dropdown input box with county names. 
                                      # Added "select county" dummy row as default
                                                  
                              selectInput('county_id', label = NULL, choices = #'Choose county',
                           c('SELECT COUNTY' = 0, 'Adams'=1,     'Antelope'=2,  'Arthur'=3,     'Banner'=4,     'Blaine'=5,    'Boone'=6,    'Box Butte'=7, 'Boyd'=8,      'Brown'=9,
                             'Buffalo'=10,        'Burt'=11,     'Butler'=12,   'Cass'=13,      'Cedar'=14,     'Chase'=15,    'Cherry'=16,  'Cheyenne'=17, 'Clay'=18,     'Colfax'=19,
                             'Cuming'=20,         'Custer'=21,   'Dakota'=22,   'Dawes'=23,     'Dawson'=24,    'Deuel'=25,    'Dixon'=26,   'Dodge'=27,    'Douglas'=28,  'Dundy'=29,
                             'Fillmore'=30,       'Franklin'=31, 'Frontier'=32, 'Furnas'=33,    'Gage'=34,      'Garden'=35,   'Garfield'=36,'Gosper'=37,   'Grant'=38,    'Greeley'=39,
                             'Hall'=40,           'Hamilton'=41, 'Harlan'=42,   'Hayes'=43,     'Hitchcock'=44, 'Holt'=45,     'Hooker'=46,  'Howard'=47,   'Jefferson'=48,'Johnson'=49,
                             'Kearney'=50,        'Keith'=51,    'Keya Paha'=52,'Kimball'=53,   'Knox'=54,      'Lancaster'=55,'Lincoln'=56, 'Logan'=57,    'Loup'=58,     'Madison'= 59,
                             'Mcpherson'=60,      'Merrick'=61,  'Morrill'=62,  'Nance'=63,     'Nemaha'=64,    'Nuckolls'=65, 'Otoe'=66,    'Pawnee'=67,   'Perkins'=68,  'Phelps'=69,
                             'Pierce'=70,         'Platte'=71,   'Polk'=72,     'Red Willow'=73,'Richardson'=74,'Rock'=75,     'Saline'=76,  'Sarpy'=77,    'Saunders'=78, 'Scotts Bluff'=79,
                             'Seward'=80,         'Sheridan'=81, 'Sherman'=82,  'Sioux'=83,     'Stanton'=84,   'Thayer'=85,   'Thomas'=86,  'Thurston'=87, 'Valley'=88,   'Washington'=89,
                             'Wayne'=90,          'Webster'=91,  'Wheeler'=92,  'York'=93), selectize = TRUE),
                                  
                           
                                                  htmlOutput("GrassUI"),##renders a slider bar (output$GrassUI in the server file), with a dynamic default value. (this allows it to change based on county selection.)  Same for all landcover sliders below.
                                                  htmlOutput("CropUI"), 
                                                  htmlOutput("GrainUI"), 
                                                  htmlOutput("TreeUI"),
                                                  htmlOutput("WetUI"),
                                                  htmlOutput("CRPUI") 
                                                  ), #  total % landcover selected here.),
                                mainPanel(
                                  tags$style(type="text/css",
                                             ".shiny-output-error { visibility: hidden;}",
                                             ".shiny-output-error:before { visibility: hidden; }"), # Suppress error messages on main panel. Warning delivered in sidepanel text.

                                  # h3(textOutput("text1"), align = "center"),
                                  plotOutput("plot1"), # Renders the plot designated plot1 from the server script, showing landcover distribution
                                  # p(h3("Predicted pheasant habitat suitability", align = "center")),
                                  plotOutput("plot2"), # Renders the plots designated plot2 from the server script, showing predicted pheasant habitat suitability/ abundance. 
                                  wellPanel(h3(textOutput("text9"), align = "center"))
                                 ))),
                                                           
                     tabPanel("Economic Module",
                              plotOutput("plot3"),   # Reactively updating bar graph with economic information
                              wellPanel(
                                p("This tab shows the estimated cost of the actions implemented in the 'Custom Management' tab. Wildlife management in agricultural
                                  landscapes can mean incentivizing landowners to change agricultural practices, such as switching from planting corn to enrolling 
                                  in the Conservation Reserve Program (CRP). To estimate the cost to conservation organizations of incentivizing landowners to 
                                  change from one practice to another we used information from the", 
                                  
                                  tags$a(href="https://www.nass.usda.gov/index.php", "National Agricultural Statistics Service "), # links to NASS
                                  
                                  "to get the gross cash value per acre for row crops, small grains, pasture rental, and enrollment in CRP. Differences between counties in production (i.e.,
                                  bushels/acre), crop value (i.e. $/bushel) and rental rates result in a county-specific agricultural value and thus a county-specific 
                                  agricultural cost for changing agricultural practices. Following the CRP model of a 10-year enrollment, we estimated 
                                  the 'rental' cost of changing agricultural practices over a 10-year period from the difference in the agricultural value of the 
                                  township given the change in land-use. In addition to incentivizing desired agricultural practices, wildlife managers also cost-share 
                                  on one-time habitat improvement actions. Using expert opinion of wildlife managers in Nebraska we estimated a Statewide average per-acre 
                                  cost of tree removal ($650/acre) and wetland restoration ($6250/acre) and spread the one-time cost over the 10-year period. The graphs
                                  above display the percent of each land cover before (Original) and after (New) the management actions specified in the 'Custom Management' tab.
                                  The text below updates to reflect the cost of the conservation actions.  Want to save your figures to compare later? Click the
                                               'Download' button to download your current plots."), 
                                 
    
                                 
                                  br(),br(),
                              
                                
                                  
                                  h4(textOutput("text3")), # Reactive text outputs containing estimates of monetary costs of landscape change
                                
                                  h4(textOutput("text5")),
                                  
                                  h4(textOutput("text7"))
                                
                                ),downloadButton('export', label = "Download your figures")),
                     
                    tabPanel("About",
                             wellPanel("The", em("Pheasant Habitat Simulator"),
                                         "is based on a poisson regression model relating the relative habitat suitability of ring-necked pheasants across
                                          the state of Nebraska to the distribution of landcover types. You may notice that similar landcover distributions
                                          can produce slightly different pheasant habitat suitability scenarios on different simulation runs. This occurs 
                                          because the underlying model includes some uncertainty and because different landcover classifications affect 
                                          pheasant habitat suitability at different spatial scales. 
                                          To read more about the model that powers the", em("Pheasant Habitat Simulator"), "click",
                                          tags$a(href="http://journals.plos.org/plosone/article?id=10.1371%2Fjournal.pone.0099339", "here"),
                                          "For questions about the", em("Pheasant Habitat Simulator"), "please contact",
                                          tags$a(href="mailto:jfontaine2@unl.edu?Subject=Pheasant%20habitat%20Simulator","Dr. Joseph J. Fontaine."), "The",
                                           em("Pheasant Habitat Simulator"), "was created by the",
                                          tags$a(href="https://sites.google.com/site/fontainejoseph/", "Fontaine Lab"), "at the",
                                          tags$a(href="http://snr.unl.edu/necoopunit/", "Nebraska Cooperative Fish and Wildlife Research Unit,"),
                                          tags$a(href = "http://snr.unl.edu/", "School of Natural Resources, University of Nebraska"), "in conjunction with the",
                                          tags$a(href = "http://outdoornebraska.gov/",  "Nebraska Game and Parks Commission"), "and the",
                                          tags$a(href = "http://rwbjv.org/", "Rainwater Basin Joint Venture."),
                                       
                                       "If you think you can improve the", em("Pheasant Habitat Simulator"), " or want to build your own 
                                       species habitat simulator, visit us at", tags$a(href = "https://github.com/lsw5077/Pheasant-Habitat-Simulator",
                                       "Github."),
                                       
                                       "Although these data have been processed successfully, no warranty expressed or implied is made regarding
                                       the display or utility of the data on any other system or for general or scientific purposes, nor shall
                                       the act of distribution constitute any such warranty. The USGS or the U.S. Government shall not be held
                                       liable for improper or incorrect use of the data described and (or) contained herein.",

                                  br(), br(),
                                    
                                   strong("Citation:"), br(), br(),
                                  "Wszola, L.S., C.R. Gillespie, E.F. Stuber, L.N. Messinger, L.H. Corral, V.L. Simonsen, C.F. Jorgensen and J.J. Fontaine. 2017. Pheasant Habitat Simulator."
                                  
                                  ),
                              img(src='logos.png', height = "40%", width = "75%", align = "left"))))
                    
