###following code is used to create the two dynamic raster figures on the 'Custom Management' page of the app
###we create a low resolution figure (Average Landcover for a Township in xx County) and a high resolution
###figure (Predicted Pheasant Habitat Suitability) using the same procedures but altering the number of pixels
###per plot. Figures either depict default landcover values and associated habitat suitability (if a county is
###selected from the drop down menu and no changes are made to landcover) which correspond to the average landcover
###of a township in the selected county, or user-manipulated proportions of landcover matching values chosen 
###by moving the slider bars.


options(stringsAsFactors = F)


#.libPaths("DataLayers/R.packages") .libPaths should default to r-portable library 
##load supplied data file containing mean, minimum, and maximum proportions of each landcover type (crop, CRP, grass, grain, tree, wetland) per township in each of 94 counties in Nebraska 
load("County_Avg_Twnshp_Defaults2.dat")
library(dplyr)
library(raster) 

# create an empty 'theoretical township' grid for display 
theoretical_grid <- expand.grid(1:18, 1:18) #18 by 18 pixel raster
n <- nrow(theoretical_grid)
x1<-c(1,2,3,4,5,6,7) ##1-6 represent predictors, 7 represents category "other" landcover type not a predictor
r<-rasterFromXYZ(cbind(theoretical_grid[, 1:2], NA)) ##convert to raster for plotting
##random sample of landcover classification by amount of cover in the average twnship

# create an empty 'high resolution' theoretical grid on which to make pheasant abundance predictors
theoretical_grid_N <- expand.grid(1:152, 1:152) #152 by 152 pixel raster
nN <- nrow(theoretical_grid_N)
x1N<-c(1,2,3,4,5,6,7) 
rN<-rasterFromXYZ(cbind(theoretical_grid_N[, 1:2], NA))

# generate pixel values to fill empty theoretical township rasters at default slider bar values for each county in NE
# landcover types will be assigned to pixels based on their average proportions within each county
county_default_raster_values<-as.data.frame(matrix(NA,n, 94))
for (i in 1:94){

  print(i) 
  prob_cover<-c(round(County_defaults_Grass$meanGrass[i],digits=2),round(County_defaults_Crop$meanCrop[i],digits=2),round(County_defaults_Grain$meanGrain[i],digits=2),
                round(County_defaults_Tree$meanTree[i],digits=2),round(County_defaults_Wet$meanWet[i],digits=2),round(County_defaults_CRP$meanCRP[i],digits=2)) ## define probability of a landcover type being assigned to a pixel based on the average proportion expected for a selected county
  prob_other<- 1 - sum(prob_cover) ##probability that a pixel is assigned to a landcover type that is not a predictor (e.g. 'other')
  prob_weight<-c(prob_cover,prob_other) ##pixel assignment probability for each of 6 predictors, plus "other"
  
  county_default_raster_values[,i]<-sample(x1, replace = TRUE, prob = prob_weight,size=n) #assign pixel values via random sampling based on defined sampling probabilities
  
}

##same procedure, but assign pixels in 'high resolution' raster
county_default_raster_values_N<-as.data.frame(matrix(NA,nN, 94))
for (i in 1:94){

  print(i)
  prob_cover<-c(round(County_defaults_Grass$meanGrass[i],digits=2),round(County_defaults_Crop$meanCrop[i],digits=2),round(County_defaults_Grain$meanGrain[i],digits=2),
                round(County_defaults_Tree$meanTree[i],digits=2),round(County_defaults_Wet$meanWet[i],digits=2),round(County_defaults_CRP$meanCRP[i],digits=2))
  prob_other<- 1 - sum(prob_cover) 
  prob_weight<-c(prob_cover,prob_other)
  
  county_default_raster_values_N[,i]<-sample(x1N, replace = TRUE, prob = prob_weight,size=nN) #raster value
  
}



##create low and high resolution grids that can be changed according to reactive slider bar values
county_grid<-cbind(theoretical_grid[, 1:2], county_default_raster_values[,1]) 
county_grid_N<-cbind(theoretical_grid_N[, 1:2], county_default_raster_values_N[,1])

##function below creates the low resolution grid:
sample_grid<-function(x, predictor, percent, predictor_name, next_prediction){ #requires: x= the county grid, predictor= the predictor (a number) which is being adjusted, percent= the percent cells that need to be changed (as designated from the slider bar), predictor_name= the name of the predictor (i.e., the landcover), next_prediction= the name to give grid of the remaining unchanged cells 
  require(dplyr)
  sample_size<-percent*324 #multiply the percent by the grid size to get the number of rows that need to be changed
  
  x$cell_id<-paste("id_", 1:nrow(x), sep="") #create a cell_id column
  
  newdata<-x[sample(nrow(x), size=sample_size, replace = F),] #randomly sample rows from x that need to be changed and create a new data frame
 
   if (sample_size > 0) { #if the number of cells (for this landcover) is greater than 0, change all rows in the new data frame so that the value in column 3 (the predictor) is the value for the predictor you are assigning
    newdata[,3]<-predictor
   }
 
  olddata<-anti_join(x, newdata, by='cell_id') #use anti_join to create an 'olddata' data frame that consists of all the rows in x that are not included in your newdata
  
  assign(next_prediction, olddata, envir = .GlobalEnv) #rename and save the olddata data frame as the name you designated as 'next_prediction'
  assign(predictor_name, newdata ,envir = .GlobalEnv) #rename and save the newdata data frame as the name of the landcover
  
}

##function below works exactly the same way except the grid size is different (for higher resolution plot)

sample_grid_N<-function(x, predictor, percent, predictor_name, next_prediction){
  require(dplyr)
  sample_size<-percent*23104
  
  
  x$cell_id<-paste("id_", 1:nrow(x), sep="") 
  
  newdata<-x[sample(nrow(x), size=sample_size, replace = F),]
  
  if (sample_size > 0) {
    newdata[,3]<-predictor
  }
  
  olddata<-anti_join(x, newdata, by='cell_id')
  
  assign(next_prediction, olddata, envir = .GlobalEnv)
  assign(predictor_name, newdata ,envir = .GlobalEnv)
  
}
