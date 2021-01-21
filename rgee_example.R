# rgee Package demo script
# Author: Ka Hei
# 29.12.2020

#import library
library(easypackages)
libraries("mapedit","raster","scales",
          "cptcity","tmap","rgee","raster",
          "stars","sf","dplyr")

#Install, initialize and open Google Earth Engine (GEE)
#ee_install()
ee_Initialize()
ee_check() 
ee_clean_credentials() 
ee_clean_pyenv() 

ee_search_dataset() %>% #search for data
  ee_search_title("landsat 8") %>% #look for Landsat 8
  ee_search_title("Tier 1 TOA") %>% #the level of data
  ee_search_display()   #display search results

#Example 1: Visualize NDVI
l8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA") #selecting data product

image <- l8$ #filter Landsat 8 data
  filter(ee$Filter$date('2020-01-01', '2020-12-31'))$ #2020 data only
  filter(ee$Filter$calendarRange(1, field = "month"))$ #only January
  mean() #take the average

#Filter high cloud cover
image = image$filterMetadata('CLOUDY_PIXEL_PERCENTAGE','less_than',20); #cloud <20%

ndvi <- image$ #calculate NDVI
  normalizedDifference(c('B5', 'B4'))$ #use the built-in normalized function
  rename('NDVI')

addNDVI <- function(image) { #write a function to add NDVI layer
  ndvi = image$normalizedDifference(c('B5', 'B4'))$rename('NDVI')
  return (image$addBands(ndvi))
}

ndvi <- addNDVI(image)$select('NDVI') #select the band to display on map

#Set up map position and zoom
Map$setCenter(9.94, 49.79, 10) #Wuerzburg, Germany

#Visualization parameters
ndviParams <- {list(min= 0, max= 0.3, 
                    palette= c('red', 'lawngreen', 'darkgreen'), 
                    opacity = 0.7)}

#Display map (add the layer)
Map$addLayer(ndvi, ndviParams, 'NDVI_Wuerzburg')

#Example 2: Night Time Light Image
#Adds a band containing image date as years since 2010
createTimeBand <-function(img) { #Get data and filter time
  year <- ee$Date(img$get('system:time_start'))$
    get('year')$subtract(2010L)
  ee$Image(year)$byte()$addBands(img)
}

#Select data and band
collection <- ee$
  ImageCollection('NOAA/DMSP-OLS/NIGHTTIME_LIGHTS')$ #night time light data
  select('stable_lights')$ #select class
  map(createTimeBand) #map it to the time frame function

#Interpolation
col_reduce <- collection$reduce(ee$Reducer$linearFit()) #computes the slope for regression
col_reduce <- col_reduce$addBands(
  col_reduce$select('scale'))
ee_print(col_reduce)

Map$setCenter(9.94, 49.79, 4) #Wuerzburg, Germany
Map$addLayer( #add map
  eeObject = col_reduce,
  visParams = list(
    bands = c("scale", "offset", "scale"),
    min = 0,
    max = c(0.18, 20, -0.18),
    opacity = 0.6
  ),
  name = "stable lights trend"
)


