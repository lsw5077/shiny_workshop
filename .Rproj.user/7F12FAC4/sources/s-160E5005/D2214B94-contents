library(raster)
library(rgdal)
library(sp)

# read in the raster

NDVI <- raster("C:/Users/Lyndsie/Documents/GitHub/shiny_workshop/exampleApps/leafletActivity/NDVI.tif")

# Delete the ocean

NDVI[NDVI == 255] <- NA

NDVI_merc <- projectRaster(NDVI, crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")

# write out the raster

writeRaster(NDVI_merc, file = "C:/Users/Lyndsie/Documents/GitHub/shiny_workshop/exampleApps/leafletActivity/NDVI_lite.tiff")
