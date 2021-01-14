# Background:

[The Pheasant Habitat Simulator](https://pheasant.shinyapps.io/pheasanthabitatsimulator/) was built in response to a request from the [Nebraska Game and Parks Commission](http://outdoornebraska.gov/) to make the results of a [ring-necked pheasant distribution model](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0099339) more accessible to conservation decisoin makers. The Pheasant Habitat Simulator was built for the specific application of a ring-necked pheasant distribution model in Nebraska, USA, but may readily be customized to other species and systems using any species-habitat regression equation. To learn more about how and why we built the Pheasant Habitat Simulator, check out our paper in PLOS One: (will be linked upon final publication). If you feel like improving this code, or want to share your own Species Habitat Simulator, please do!

# What is contained in this repository:

The files in this reprository comprise a [shiny app](https://shiny.rstudio.com/tutorial/), an interactive R-based application. Detailed information about the how the code functions may be found in comments in the code itself. The function and contents of the files in the repository are as follows:

### Executable Files
+ **ui** defines the user interface. It controls the organization of the application and arranges the widgets users manipulate to interact with the data.  
+ **server** contains the analytical procedure. It runs the model based on the user's inputs and renders dynamically updating plots.  
+ **shiny_functions_09192016.R** includes several functions (built by [Erica Stuber](https://scholar.google.com/citations?user=GhdnpVQAAAAJ&hl=en&oi=sra)) built to turn the user's inputs into queries from the included datasets. 

### Data files
+ **Value_per_Acre** contains county-specific economic valuations for evaluating the economic tradeoffs of conservation actions in agricultural landscapes.
+ **County_Avg_Twnshp_Defaults2.dat** contains the starting landcover values for each county.
+ **WWW** contains all the images included in the app by the ui.  

# Disclaimers and other data sources

If you would like a desktop-deployable version of the application to run offline (currently for windows 7-10 only)
 or transfer to other users, click [here](https://osf.io/yk83m/). 

Although these data have been processed successfully, no warranty expressed or implied is made regarding the display or utility of the data on any other system or for general or scientific purposes, nor shall the act of distribution constitute any such warranty. The USGS or the U.S. Government shall not be held liable for improper or incorrect use of the data described and (or) contained herein.
