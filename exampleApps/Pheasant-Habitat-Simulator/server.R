# Server directs the shiny app workflow. 



shinyServer(function(input, output, session) {
  
  .libPaths("W:/FontaineGroup/R.packages.0917")
  source("shiny_functions_09192016.R")
  #township<-county_default_raster_values
  #county_id<-50
  library(shiny)
  library(tidyr)
  library(SGP)
  library(grid)
  library(gridExtra)

vals <- reactiveValues(land_default = NULL, phes_default = NULL, land_plot = NULL, phes_plot = NULL, econ_plot = NULL, ag_cost = NULL,
                       management_cost = NULL, total_cost = NULL) # create empty list to hold plots and graphic objects


# Reactive Plot Titles
  
  current_county <- reactive({                    # create a function that gives the name of the current county selected
    capwords(County_defaults_Grass[input$county_id, 5]) 
  })
  
# Landcover slider bar widget 
# Determines default land cover percent value based upon county selected by user

  county_grass<-reactive({                        # creates the value "county_grass" (proportion of the county which is pasture/grasses) to input into slider bar code output$GrassUI
    runif(input$county_id)      
    County_defaults_Grass[input$county_id, 2]     # selects the value in "County_defaults_Grass (from the .dat file) associated with the county_id input on the 'selectInput' drop down menu
  })
  county_crop<-reactive({                         # creates the value "county_crop" to input into slider bar code output$CropUI
    County_defaults_Crop[input$county_id, 2]      # selects the value in "County_defaults_Crop (from the .dat file) associated with the county_id input on the 'selectInput' drop down menu
  })
# Works the same for the rest of the land cover types
  county_grain<-reactive({                        
    County_defaults_Grain[input$county_id, 2]     
  })
  county_tree<-reactive({               
    County_defaults_Tree[input$county_id, 2]      
  })
  county_wet<-reactive({                          
    County_defaults_Wet[input$county_id, 2]      
  })
  county_CRP<-reactive({                        
    County_defaults_CRP[input$county_id, 2]      
  })
  

# Render dynamic UI (a sliderInput) with a default value that changes based on county selection 


  output$GrassUI <- renderUI({ 
    sliderInput("x1", label = h4("% Township in pasture"),                # x1: the value that goes into the sample grid function 
                min = 0, max = 100, value = county_grass()*100, post = "%") # "value" is the default value from the county (as determined in above 'reactive' statements, will be what the slider bar is set to when the UI is rendered)
  })
# works the same for the rest of the land cover types
  output$CropUI <- renderUI({ 
    sliderInput("x2", label = h4("% Township in row crops"),
                min = 0, max = 100, value = county_crop()*100, post = "%")
  })
  output$GrainUI <- renderUI({ 
    sliderInput("x3", label = h4("% Township in small grains"),
                min = 0, max = 100, value = county_grain()*100, post = "%")
  })
  output$TreeUI <- renderUI({ 
    sliderInput("x4", label = h4("% Township in trees"),
                min = 0, max = 100, value = county_tree()*100, post = "%")
  })
  output$WetUI <- renderUI({ 
    sliderInput("x5", label = h4("% Township in wetland"),
                min = 0, max = 100, value = county_wet()*100, post = "%")
  })
  output$CRPUI <- renderUI({ 
    sliderInput("x6", label = h4("% Township in CRP"),
                min = 0, max = 100, value = county_CRP()*100, post = "%")
  })
  

output$text9 = renderText({paste("Total landcover selected = ", 
                                 (input$x1 + input$x2 + input$x3 + input$x4 + input$x5 + input$x6), 
                                 "% of township area.",
                                 ifelse ((input$x1 + input$x2 + input$x3 + input$x4 + input$x5 + input$x6) > 101, 
                                         "You have selected more than 100 % of the total area available. Please 
                                          move one or more slider bars to the left.", ""))}) # works but ugly

# Land Cover Plot 

# Create new grid based on slider bar selections

  dataInput <- reactive({
    sample_grid(county_grid, 1, input$x1*0.01, "Grass", "Predict_next_2")    #start with original county_grid, assign the percentage of rows desingated in first slider bar as predictor 1 (grass), name the new data frame "Grass" and assign all the rest of the rows into a new data frame called "Predict_next_2"
    ## all the rest work the same for each subsequent landcover...
    sample_grid(Predict_next_2, 2, input$x2*0.01, "Crop", "Predict_next_3") 
    sample_grid(Predict_next_3, 3, input$x3*0.01, "Grain", "Predict_next_4") 
    sample_grid(Predict_next_4, 4, input$x4*0.01, "Tree", "Predict_next_5") 
    sample_grid(Predict_next_5, 5, input$x5*0.01, "Wet", "Predict_next_6") 
    sample_grid(Predict_next_6, 6, input$x6*0.01, "CRP", "Other")            # final remaining cells are all designated as "Other"
    
    if ( nrow(Other) >0) {    # If there are any rows in the "Other" data frame, assign them the predictor value for "Other"
      Other[,3]<-7
    }
    
    final_grid<-rbind(Grass, Crop, Grain, Tree, Wet, CRP, Other)%>%    # create one full composite data frame with everything together
      arrange(Var1, Var2)%>%                                           # put everything back in order (so that we can bind it to theoretical_grid later)
      dplyr::select(3)                                                        # select only column 3 (the predictor numbers)

    
    })
 
  
  
  output$plot1<-renderPlot({           # render the low-resolution landcover plot
    final_grid<-dataInput()            # assign an object name for the values created above in the dataInput() reactive expression
    r_county = cbind(theoretical_grid[, 1:2], final_grid)
    
    

    colnames(r_county)<-c('x','y','N')   
    r_county$N <- factor(r_county$N, levels=c('6','1','3','5','2','4','7'), labels=c("CRP      ",
                                                                                       "Grass    ",
                                                                                       "Grain    ",
                                                                                       "Wetland  ",
                                                                                       "Crop     ",
                                                                                       "Tree     ",
                                                                                       "Other    "))   # rename predictor numbers as landcover names
    
    landuse_palette<-c('CRP      '='#548B54','Grass    ' ='#7CCD7C','Grain    ' ='#90EE90',
                       'Wetland  '='#4876FF','Crop     '='#EEEE00','Tree     '='#CD6839','Other    '='darkgray')  # create a new color palette to assign colors to landuse types

# Plot the low-resolution landcover map

vals$land <- ggplot(r_county) + 
      geom_raster(aes(x=x, y=y, fill= N)) +
      scale_fill_manual(values=landuse_palette) +   # use the landuse pallete to manually assign color according to landuse type
      coord_fixed() +
      ggtitle(paste("Land Cover in ", current_county(), "County"))+
      theme_bw()+
      theme(legend.title= element_blank(),
            legend.text = element_text(size = 10),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.title=element_blank(),
            axis.text= element_blank(),
            axis.ticks= element_blank(),
            plot.title=element_text(size= 12, hjust = 0.5))  # make the landcover plot, store in an object called land
     
      vals$land + 
        theme(legend.text = element_text(size = 20),
              plot.title=element_text(size=26, hjust = 0.5)) 
      
      # print land
      # put land into one of our reactive vals
      ## slots. If the counter = 0, ie this is the first plot we've made of this county, store it in our "default" vals slot
      ## if this plot is the result of moving the sliders, store it in our "land_plot" slot. 
      # browser()
        })
  

 
  
    
  dataInput_N <- reactive({
    sample_grid_N(county_grid_N, 1, input$x1*0.01, "Grass", "Predict_next_2")
    sample_grid_N(Predict_next_2, 2, input$x2*0.01, "Crop", "Predict_next_3")
    sample_grid_N(Predict_next_3, 3, input$x3*0.01, "Grain", "Predict_next_4")
    sample_grid_N(Predict_next_4, 4, input$x4*0.01, "Tree", "Predict_next_5")
    sample_grid_N(Predict_next_5, 5, input$x5*0.01, "Wet", "Predict_next_6")
    sample_grid_N(Predict_next_6, 6, input$x6*0.01, "CRP", "Other")
    
    if ( nrow(Other) >0) {
      Other[,3]<-7
    }
    
    final_grid_N<-rbind(Grass, Crop, Grain, Tree, Wet, CRP, Other)%>%
      arrange(Var1, Var2)%>%
      dplyr::select(3)

      
  }) 
  
  
  
  
  # Render plot with predicted pheasant habitat suitability/ abundance
    output$plot2<-renderPlot({
    
    final_grid_N<-dataInput_N()
    
    require(ggplot2)
    r_county_N = cbind(theoretical_grid_N[, 1:2], final_grid_N)
  
  
    theoretical_grid_N$cell_id<-paste("id_", 1:nrow(theoretical_grid_N), sep="")
    
    r_county_Grass_X1 <- ifelse((final_grid_N[,1]) == 1,1,0)
    r_county_Crop_X1 <- ifelse((final_grid_N[,1]) == 2,1,0)
    r_county_Grain_X1 <- ifelse((final_grid_N[,1]) == 3,1,0)
    r_county_Tree_X1 <- ifelse((final_grid_N[,1]) == 4,1,0)
    r_county_Wet_X1 <- ifelse((final_grid_N[,1]) == 5,1,0)
    r_county_CRP_X1 <- ifelse((final_grid_N[,1]) == 6,1,0)
    
    
    r_county_bin_Grass <- rasterFromXYZ(cbind(theoretical_grid_N[, 1:2], r_county_Grass_X1))   #transform matrix to raster
    r_county_bin_Crop <- rasterFromXYZ(cbind(theoretical_grid_N[, 1:2], r_county_Crop_X1))     #transform matrix to raster
    r_county_bin_Grain <- rasterFromXYZ(cbind(theoretical_grid_N[, 1:2], r_county_Grain_X1))   #transform matrix to raster
    r_county_bin_Tree <- rasterFromXYZ(cbind(theoretical_grid_N[, 1:2], r_county_Tree_X1))     #transform matrix to raster
    r_county_bin_Wet <- rasterFromXYZ(cbind(theoretical_grid_N[, 1:2], r_county_Wet_X1))       #transform matrix to raster
    r_county_bin_CRP <- rasterFromXYZ(cbind(theoretical_grid_N[, 1:2], r_county_CRP_X1))       #transform matrix to raster
    
    # Do moving average calculation over raster for model input values (done in background)
    
    r10 <- matrix(1/121,nrow=11,ncol=11)  #for CRP, Grass, Wet. Pixel size reflects scale at which variable affects pheasant abundance. 
    r50 <- matrix(1/2601,nrow=51,ncol=51) #for Crop, Grain, Trees
    unbuffer<-extent(26, 126, 26, 126) # buffer is added for calculations to minimze edge effects, then removed for clean visualization. 
    
    r_county_MA_CRP <- crop(focal(r_county_bin_CRP, w=r10), unbuffer)
    r_county_MA_Grass <- crop(focal(r_county_bin_Grass, w=r10), unbuffer)
    r_county_MA_Wet <- crop(focal(r_county_bin_Wet, w=r10), unbuffer)
    
    r_county_MA_Crop <- crop(focal(r_county_bin_Crop, w=r50), unbuffer)
    r_county_MA_Grain <- crop(focal(r_county_bin_Grain, w=r50), unbuffer)
    r_county_MA_Tree <- crop(focal(r_county_bin_Tree, w=r50), unbuffer)
  
    
    # make raster layers into data.frame for prediction
    r_county_df<-as.data.frame(r_county_MA_CRP,xy=TRUE) #xy would be coordinates
    r_county_df<-cbind(r_county_df,(as.data.frame(r_county_MA_Grass)),(as.data.frame(r_county_MA_Wet)),(as.data.frame(r_county_MA_Crop)),(as.data.frame(r_county_MA_Grain)),(as.data.frame(r_county_MA_Tree)))
    colnames(r_county_df) <- c("x","y","CRP","Grass","Wet","Crop","Grain","Tree")
    
    # add to predict equation
    r_county_df$N = exp(3.0666 + 
                          (((-0.54781 * r_county_df$Tree) - (-0.54781 * 0.06301747)) / (0.053441)) + (((0.131763 * (r_county_df$Tree * r_county_df$Tree)) - (0.131763 * 0.00682374)) / (0.00918277)) + 
                          (((0.511138 * r_county_df$Crop) - (0.511138 * 0.25670848)) / (0.20208898)) + (((-0.05282 * (r_county_df$Crop * r_county_df$Crop)) - (-0.05282 * 0.10669046)) / (0.16528966)) + (-4.6611202 * (r_county_df$Crop * r_county_df$Crop * r_county_df$Crop)) +
                          (((0.133586 * r_county_df$Grass) - (0.133586 * 0.47500357)) / (0.21036192)) +
                          (((0.226327 * r_county_df$CRP) - (0.226327 * 0.11351257)) / (0.14869116)) +
                          (((0.451256 * r_county_df$Grain) - (0.451256 * 0.07708618)) / (0.06580207)) + (((-0.04344 * (r_county_df$Grain * r_county_df$Grain)) - (-0.04344 * 0.01026702)) / (0.016885)) + (-6.849455 * (r_county_df$Grain * r_county_df$Grain * r_county_df$Grain))  +
                          (((-0.10249 * r_county_df$Wet) - (-0.10249 * 0.02997624)) / (0.07880755)) +
                          (-0.15981 * 1))
    
    #Make back into a raster to plot abundance map
    
    N_map_XYZ = data.frame("x" = r_county_df$x, "y" = r_county_df$y, "N" = r_county_df$N)
    
    require(ggplot2)
    
    my.palette<-c('#FFFFFF',
                  '#F0FFFF',
                  '#F0FFFF',
                  '#E0EEEE',
                  '#C1CDCD',
                  '#63B8FF',
                  '#5CACEE',
                  '#4F94CD',
                  '#36648B',
                  '#9AFF9A',
                  '#90EE90',
                  '#7CCD7C',
                  '#548B54',
                  '#9ACD32',
                  '#CDCD00',
                  '#EEEE00',
                  '#FFFF00',
                  '#FFA500',
                  '#EE9A00',
                  '#FF4500',
                  '#EE4000',
                  '#FF0000',
                  '#EE0000',
                  '#CD0000',
                  '#8B0000')
    
    ggplot(data=N_map_XYZ)+
      geom_raster(aes(x=x, y=y, fill=N))+
      scale_fill_gradientn(25, colours = my.palette, limits=c(2,50), breaks=c(2,50), labels = c("Poor", "Excellent"))+
      coord_fixed()+
      ggtitle("Predicted Pheasant Habitat Suitability") + 
      theme_bw()+
      theme(legend.title= element_blank(),
            legend.text = element_text(size = 10, vjust = -1),
            panel.border=element_blank(),
            panel.grid=element_blank(),
            axis.title=element_blank(),
            axis.text= element_blank(),
            axis.ticks= element_blank(),
            plot.title=element_text(size=12, hjust = 0.5)) -> vals$phes # store phes plot
    
    vals$phes +
      theme(legend.text = element_text(size = 20, vjust = -1),
            plot.title=element_text(size=26, hjust = 0.5))# print phes plot
    
})
    

# Economics Module
# Calculating Costs of User Defined Changes


# Extracting the value/acre of each landuse based on county selected

value_acre <- read.csv("Value_per_Acre.csv")
    
    grass_val<-reactive({                     # selecting county based on input, then selecting grassland value/acre
      value_acre[input$county_id, 6]
    })

    crop_val<-reactive({                      # selecting county based on input, then selecting crop value/acre
      value_acre[input$county_id, 4]
    })
    
    grain_val<-reactive({                     # selecting county based on input, then selecting gain value/acre
      value_acre[input$county_id, 3]
    })
    
    CRP_val<-reactive({                       # selecting county based on input, then selecting CRP cost/acre
      value_acre[input$county_id, 5]
    })
    
    tree_val<-reactive({                      # selecting county based on input, then selecting tree removal cost/acre
      value_acre[input$county_id, 7]
    })
    
    wet_val<-reactive({                       # selecting county based on input, then selecting wetland restoration cost/acre
      value_acre[input$county_id, 8]
    })
    

# Calculating the original agriculture value for the county selected 
     
  grass_worth_orig <- reactive({              # grasslands original value 
    (county_grass() * 23040 * grass_val())    # bringing in the proportion grass in the county selected, then multiplying by total acres in a township and value/acre in that county
  })

  crop_worth_orig <- reactive({               # cropland original value
    (county_crop() * 23040 * crop_val())      # bringing in the proportion crop in the county selected, then multiplying by total acres in a township and value/acre in that county
  })

  grain_worth_orig <- reactive({              # grain original value
    (county_grain() * 23040 * grain_val())    # bringing in the proportion grain in the county selected, then multiplying by total acres in a township and value/acre in that county
  })


  ag_worth_orig <- reactive({                 # Total origianl value of the township in the county selected
    (grass_worth_orig() + crop_worth_orig() + grain_worth_orig())
  })


# calculating the new agriculture value based on user-selected landcover 

  grass_worth_new <- reactive({            # user defined value of grassland
    ((input$x1*0.01) * 23040 * grass_val())     # bringing in the user defined % grass in the county selected, then multiplying by total acres in a township and value/acre in that county
  })


  crop_worth_new <- reactive({             # user defined value of crop
    ((input$x2*0.01) * 23040 * crop_val())      # bringing in the user defined % crop in the county selected, then multiplying by total acres in a township and value/acre in that county
  })


  grain_worth_new <- reactive({            # user defined value of grain
    ((input$x3*0.01) * 23040 * grain_val())     # bringing in the user defined % grain in the county selected, then multiplying by total acres in a township and value/acre in that county
  })

 
  ag_worth_new <- reactive({               # user defined total agricultural value in county selected
    (grass_worth_new() + crop_worth_new() + grain_worth_new())  
  })


# calculating CRP and management costs based on user selected land use cover
  
  crp_cost <- reactive({                         # CRP cost over 10 years
    ((input$x6*0.01) * 23040 * -CRP_val() * 10)       # user defined % CRP * total acres in a township * cost/acre * 10 years
  })
  
  tree_change <- reactive({                      # change in % of trees
      county_tree() - (input$x4*0.01)                 # original % trees - user defined % 
  })
  
  tree_cost <- reactive({                        # cost of removing trees
    if(tree_change() > 0){                       # if tree_change is positive, then management is required to remove that % of trees
      (tree_change() * 23040 * -tree_val())      # % change * total acres in a township * cost of removal/acre
    } else {
       0                                         # if tree_change is 0 or negative, no management is required and cost = 0
    }
  })
  
  wet_change <- reactive({                       # change in % wetlands
      (input$x5*0.01) - county_wet()                  # user defined % wetlands - origian % wetlands 
  })
  
  wet_cost <- reactive({                         # cost of restoring wetlands
    if(wet_change() > 0){                        # if wet_change is positive, then management is required to restore wetlands
      (wet_change() * 23040 * -wet_val())        # % change * total acres in a township * cost of restoration/acre
    } else {
      0                                          # if wet_change is 0 or negative, no management is required and cost = 0
    }
  })  
  
  total_mngmt_cost <- reactive({       # total cost of management - cost of tree removal and wetland restoration 
    as.integer(tree_cost() + wet_cost())
  })
  
  
#overall cost 
  
  total_ag_cost <- reactive({                                  # total ag cost over 10 years 
    if((ag_worth_orig() - ag_worth_new()) > 0){                # if change in ag worth is negative  
      as.integer(((ag_worth_orig() - ag_worth_new())*10) + crp_cost())   # total cost is the difference between original and user defined multiplied by 10 with the cost of crp
    } else {
      as.integer(crp_cost())                                               # if total ag worth doesn't change or increases, then the only cost is is the cost of CRP
    }
  })
  
  total_10yr_cost <- reactive({                  # total cost over 10 years = total ag cost + management cost
    (as.integer(total_ag_cost() + total_mngmt_cost()))
  })
  


# Economic Module summary statistics and their captions - VS 12/08/2016
  
  output$text3 <- renderText({        # inserting total ag cost over 10 years as text
   vals$ag_cost <- paste("Total agricultural cost over 10 years: $", prettyNum((round(total_ag_cost())), scientific = F, big.mark = ","))
   vals$ag_cost
   
   })
  
  
  output$text5 <- renderText({        # insterting total management cost over 10 years as text 
  vals$management_cost <- paste("Total management cost over 10 years: $", prettyNum(total_mngmt_cost(), scientific = F, big.mark = ","))
  vals$management_cost  

  })
  
  
  output$text7 <- renderText({        # inserting total cost over 10 years as text
  vals$total_cost <- paste("Total cost over 10 years: $", prettyNum(total_10yr_cost(), scientific = F, big.mark = ",", format = "d"))
  vals$total_cost

    })
  


# Creating a reactively updating bar plot
# Creating vectors based upon user defined county and land use changes


Land_Cover_data <- reactive({                                   # vector with original and new % grass
  Grass_LC <- c("Pasture", county_grass()*100, (input$x1))          # vector with original and new % grass
  Crop_LC <- c("Row crop", county_crop()*100, (input$x2))           # vector with original and new % crop
  Grain_LC <- c("Small grains", county_grain()*100, (input$x3))     # vector with original and new % grain
  Trees_LC <- c("Trees", county_tree()*100, (input$x4))             # vector with original and new % trees
  Wet_LC <-c("Wetlands", county_wet()*100, (input$x5))              # vector with original and new % wetlands
  CRP_LC <- c("CRP", county_CRP()*100, (input$x6))                  # vector with original and new % CRP

  
  Land_Cover_raw <- rbind(Grass_LC, Crop_LC, Grain_LC, Trees_LC, Wet_LC, CRP_LC)    # Putting all the land cover vectors together
  
})


# Creating the bar graph

output$plot3 <- renderPlot({
    Land_Cover_raw <- Land_Cover_data                           # bringing in the land cover data 
    Land_Cover <- Land_Cover_raw()
    colnames(Land_Cover) <- c("Land_Type", "Original", "New")   # adding column names
    Land_Cover <- data.frame(Land_Cover)                        # designating the land cover data as a data frame 
    Land_Cover <- Land_Cover %>%                                # converting data frame from wide to long
      gather(time, percent, -Land_Type) 
      
   Land_Cover$time <- factor(Land_Cover$time, levels=c("Original", "New"))   # specifying time a factor
   Land_Cover$percent <- as.numeric(as.character(Land_Cover$percent))      # specifying percent as a numeric variable
    
# print econ plot   
   vals$econ_plot <- ggplot(Land_Cover)+
      geom_bar(aes(x=Land_Type, y=percent, fill=time), color="black", stat = "identity", position = position_dodge(width=0.9)) +    # seting land cover type as x and y as the proportion on the landscape, with original and new inputs for each land cover type side by side
      scale_fill_manual(values= c("Original"="slategray1", "New"="navy"), guide = guide_legend(title = NULL))+    # specifying bar graph fill colors 
      ggtitle(paste("Land Cover in ", current_county(), "County"))+
      labs(x = "Land cover type", y = "Percent of township area") +                     # creating labels on the x and y axes 
      coord_cartesian(ylim = c(0,105)) +                                                # specifying the y axis scale 
      scale_y_continuous(breaks = seq(0,100,by = 10), expand=c(0,0)) +                  # specifying the breaks on the y axis scale  
      theme_bw()+
      theme(panel.grid=element_blank(),                                                 # Specifying the axis text size and position
            axis.title.y = element_text(size = 10, margin = margin(0, 20, 0,0)),        # Margins go: top, right, bottom, left 
            axis.title.x = element_text(size = 10, margin = margin(20, 0, 20,0)),
            axis.text = element_text(size = 10, color = "black"),
            legend.text = element_text(size = 12),
            plot.title=element_text(size=12, hjust = 0.5))

   vals$econ_plot +
     theme(panel.grid=element_blank(),                                                 # Specifying the axis text size and position
           axis.title.y = element_text(size = 24, margin = margin(0, 20, 0,0)),        # Margins go: top, right, bottom, left 
           axis.title.x = element_text(size = 24, margin = margin(20, 0, 20,0)),
           axis.text = element_text(size = 20, color = "black"),
           legend.text = element_text(size = 20),
           plot.title=element_text(size=26, hjust = 0.5))



# save econ plot with smaller text
     })

#output$text11 = renderText({paste("Counter =", counter$i)})

output$export <- downloadHandler(
  filename = function(){paste("pheasant_graphs_", strftime(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"), ".pdf", sep = "")},
  content = function(file){
  pdf(file, onefile = T, width = 8.5, height = 11)
  grid.arrange(grobs = list(vals$land, vals$phes,
                            vals$econ_plot,
                            grid.text(vals$ag_cost), grid.text(vals$management_cost), grid.text(vals$total_cost)),
                            layout_matrix = rbind   (c(NA,1,1,1,1,1,1,2,2,2,2,2,2,NA),
                                                     c(NA,1,1,1,1,1,1,2,2,2,2,2,2,NA),
                                                     c(NA,1,1,1,1,1,1,2,2,2,2,2,2,NA),
                                                     c(NA,1,1,1,1,1,1,2,2,2,2,2,2,NA),
                                                     c(NA,1,1,1,1,1,1,2,2,2,2,2,2,NA),
                                                     c(NA,1,1,1,1,1,1,2,2,2,2,2,2,NA),
                                                     c(NA,1,1,1,1,1,1,2,2,2,2,2,2,NA),
                                                     c(NA,3,3,3,3,3,3,3,3,3,3,3,3,NA),
                                                     c(NA,3,3,3,3,3,3,3,3,3,3,3,3,NA),
                                                     c(NA,3,3,3,3,3,3,3,3,3,3,3,3,NA),
                                                     c(NA,3,3,3,3,3,3,3,3,3,3,3,3,NA),
                                                     c(NA,3,3,3,3,3,3,3,3,3,3,3,3,NA),
                                                     c(NA,3,3,3,3,3,3,3,3,3,3,3,3,NA),
                                                     c(NA,4,4,4,4,4,4,4,4,4,4,4,4,NA),
                                                     c(NA,5,5,5,5,5,5,5,5,5,5,5,5,NA),
                                                     c(NA,6,6,6,6,6,6,6,6,6,6,6,6,NA),
                                                     c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA))
               )
  dev.off()
  }
)

session$onSessionEnded(function() {
  stopApp()
})

})

# call saved plot object and edit theme in grob list


