# rtsVis demo script
# Author: Ka Hei, https://github.com/JohMast
# 17.01.2021

#import library
library(rtsVis) #To visualize large raster time series (stack)
library(raster)
library(sf)
library(moveVis) #visualize movement/ gif
library(magrittr) 
library(ggplot2)
library(RStoolbox)
library(mapedit)
library(RStoolbox)
library(rasterVis)
library(ggplot2)
library(rgdal)
library(dplyr)

#using the downloaded Sentinel 2 data from Lijiang, Yuannan in China:
#doing classification, using resampling of training data point to 
#perform sensitivity analysis of different class using random forest algorithms

#set and check directory for the files
setwd("C:/Users/Admin/Desktop/IntroQGIS/Yuannan")
dir(getwd())

#import multiband tif file and training points
s2 <- brick("S2lijiang.tif")
train <- readOGR(".","train_point_100")
#sampling different training sample from 100% to 10% of the original size
train_1 <- train[(as.numeric(train$index) %% 10) != 0,]
train_2 <- train_1[(as.numeric(train_1$index) %% 10) != 1,]
train_3 <- train_2[(as.numeric(train_2$index) %% 10) != 2,]
train_4 <- train_3[(as.numeric(train_3$index) %% 10) != 3,]
train_5 <- train_4[(as.numeric(train_4$index) %% 10) != 4,]
train_6 <- train_5[(as.numeric(train_5$index) %% 10) != 5,]
train_7 <- train_6[(as.numeric(train_6$index) %% 10) != 6,]
train_8 <- train_7[(as.numeric(train_7$index) %% 10) != 7,]
train_9 <- train_8[(as.numeric(train_8$index) %% 10) != 8,]

#combine all training samples to a list
train_set.list <- list(train, train_1, train_2, train_3, train_4,
                       train_5, train_6, train_7, train_8, train_9)

#change directory
setwd("C:/Users/Admin/Desktop/Programming/demo/")
dir(getwd())

#rename training list to polygons
polygons <- train_set.list

#write a function to perform classification 
#and writes accuracy as additional layers
pred_with_acc <- function(x){ 
  y <- superClass(s2,trainData = x,responseCol = "id", trainPartition = 0.03) #classification
  z <- stack(y$map)  #extract the predicted map as stack to z
  
  F1scores <- y$validation[[1]]$byClass %>% as.data.frame()  #get accuracy by class to df
  #write the accuracy as layers
  for(i in 1:nrow(F1scores)){
    z[[1+i]] <- F1scores$F1[[i]]
  }
  names(z) <- c("PredClass",y$classMapping$class) #rename the layers to their respective class (the first layer is the predicted class)
  return(z) #return the stack of maps and F1 scores
}

#apply the function to the raster stack
predicted_maps <- lapply(train_set.list,pred_with_acc) 

#as the raster stacks are from the same tif file having same time stamp 
#we need to create fake time stamps for using the library
fake_dates <- Sys.Date() %>% as.POSIXct() +1:length(predicted_maps) #generate time stamps
fake_timeseries <- ts_raster(predicted_maps,fake_dates,fake_dates) #combine time stamps with raster stack

#rename the class to something meaningful
class_name <- c("PredClass","Field","Urban","Forest","Water")
for (n in 1:10){
  names(fake_timeseries[[n]]) <- class_name  
}

#pipe the map frames
fake_timeseries_frames <- ts_makeframes(x_list = fake_timeseries)%>%
  moveVis::add_labels(x = "Longitude", y = "Latitude")%>% #adjust visualization parameters: labels
  moveVis::add_northarrow(colour = "white", position = "bottomright") %>% #north arrow
  moveVis::add_progress() %>% #add progress bar to frames
  lapply(function(x) {
    x+
      scale_fill_manual(values = c("yellow","red","green","blue"),guide=F)
  })

#make the line frames (line chart of F1 score of rf classification)
fake_lineframes <- fake_timeseries %>%   
  rtsVis:::.ts_subset_ts_util(l_indices = -1) %>%   # exclude the map from the first layer
  ts_flow_frames(  
    plot_function = "line",band_names = names(.[[1]]),
    band_legend_title = "Class",val_min = 0,val_max = 1, # set y limit
    legend_position = "left",
    band_legend=T,
    aes_by_pos = T) #visualization parameters

# join the map and the linecharts for the gif generation
point_joined<- moveVis::join_frames(frames_lists = list(fake_timeseries_frames,fake_lineframes))

# animate the joined frames and save it to the disk
setwd("C:/Users/Admin/Desktop/Programming/demo/") #saving path
moveVis::animate_frames(point_joined, fps = 1.2,
                        out_file = "Lijiang_senAnalysis_color.gif",
                        end_pause = T,width=1600)

