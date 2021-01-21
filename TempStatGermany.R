#Visualizing Weather parameters in Germany

http <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/air_temperature_mean/08_Aug/"

library(RCurl)
result <- getURL(http, verbose=TRUE, ftp.use.epsv=TRUE, dirlistonly = TRUE)
library(tidyverse)
library(magrittr)
library(dplyr)

result_tidy <- str_split(result, "\n|\r\n")  
result_tidy <- result_tidy[[1]]
result_tidy <- sort(result_tidy, decreasing = FALSE)
result_tidy <- result_tidy[-1]

#1) Instead of manually remove excess row numbers
#2019 and 20 is grep and removed

###result_tidy <- result_tidy[c(seq(1,138, by=1))]
excess_yr <- grep("2019|2020", result_tidy)
result_tidy <- result_tidy[-excess_yr]

setwd("C:/Users/Admin/Desktop/MB12/DE_Temp")

#2) Autoimatically decides create folder or not

###dir.create("DWDdata/")
ifelse(!dir.exists(file.path("DWDdata/")), 
       dir.create(file.path("DWDdata/")), FALSE)
out_dir <- "DWDdata/"

#3) Add the step of checking data for all years to spot missing data just in case
data_check <- grepl("[1881-2018]", result_tidy)
absent_yr <- 1880 + which(data_check == FALSE)
absent_yr
###

for (i in 1:length(result_tidy)) {
  if(file.exists(paste0(out_dir, result_tidy[i]))){
    print(paste0(result_tidy[i], sep=" ", "file already exists"))
  }
  else
  {
    download.file(paste0(http, result_tidy[i]), paste0(out_dir, result_tidy[i]))
  }
}

mypath <- "DWDdata/"
temp <- grep("*temp*", list.files(path = mypath, pattern="*.gz$"), value=T)

filenames <- paste0(mypath, temp)

library(sp)
library(raster)

for (i in 1:length(filenames)){
  if (i == 1){
    
    current_ascii <- read.asciigrid(filenames[i])
    
    rm(my_raster) 
    my_raster <- raster(current_ascii)
  } else {
    
    current_ascii <- read.asciigrid(filenames[i])
    current_raster <- raster(current_ascii)
    my_raster <- stack(my_raster, current_raster)
    
    rm(i, current_ascii, current_raster)
  }
}

#4) Not assigning sequence of years, but use the year already in the file name
#Construct clean layer names

###layer_names <- c(paste0("Year_", seq(1881, 2018, by=1))) 
layer_names <- result_tidy %>%
  sort(decreasing = F) %>%
  `gsub`("^.*?mean_","Year_",.) %>%
`substring`(., 1, nchar(.) - 9)

names(my_raster) <- layer_names

rasterHist <- my_raster[[grep("1961", layer_names):grep("2017", layer_names)]]

rasterComp <- my_raster$Year_2018

my_crs <- "+init=epsg:31467"

rasterHist@crs <- sp::CRS(my_crs)
rasterComp@crs <- sp::CRS(my_crs)

#5) Automatically look for temp data (not precipitation) to perform manipulation

###rasterHist <- rasterHist/10
###rasterComp <- rasterComp/10
if (grepl("air_temp",result_tidy[1]) == TRUE) {
  rasterHist <- rasterHist/10
  rasterComp <- rasterComp/10
}

rasterHist_mean <- mean(rasterHist)

library(RStoolbox)
library(gridExtra)

maxVal <- max(c(unique(values(rasterComp)),unique(values(rasterHist_mean))),na.rm=T)
minVal <- min(c(unique(values(rasterComp)),unique(values(rasterHist_mean))),na.rm=T)

p1 <- ggR(rasterHist_mean, geom_raster = T)+
  scale_fill_gradient2(low="blue", mid='yellow', high="red", name ="temperature", na.value = NA, limits=c(minVal,maxVal))+
  labs(x="",y="")+
  ggtitle("Mean Temperatures August 1881-2017")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15))+
  theme(legend.title = element_text(size = 12, face = "bold"))+
  theme(legend.text = element_text(size = 10))+
  theme(axis.text.y = element_text(angle=90))+
  scale_y_continuous(breaks = seq(5400000,6000000,200000))+
  xlab("")+
  ylab("")

p2 <- ggR(rasterComp, geom_raster = T)+
  scale_fill_gradient2(low="blue", mid='yellow', high="red", name ="temperature", na.value = NA, limits=c(minVal,maxVal))+
  labs(x="",y="")+
  ggtitle("Temperature August 2018")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15))+
  theme(legend.title = element_text(size = 12, face = "bold"))+
  theme(legend.text = element_text(size = 10))+
  theme(axis.text.y = element_text(angle=90))+
  scale_y_continuous(breaks = seq(5400000,6000000,200000))+
  xlab("")+
  ylab("")

grid.arrange(p1, p2, ncol=2)

library(RStoolbox)

rasterComp_transform <- rasterComp
rasterHist_mean_transform <- rasterHist_mean

#6) Color scale manipulation for better visual effect
#stretching the difference
rasterComp_transform[rasterComp_transform > 19.0] <- 19.0
rasterHist_mean_transform[rasterHist_mean_transform < 16] <- 16
rasterHist_mean_transform[rasterHist_mean_transform > 19] <- 18.9

df <- ggR(rasterHist_mean_transform, ggObj = FALSE)
df2 <- ggR(rasterComp_transform, ggObj = FALSE)
colnames(df)[3] <- colnames(df2)[3] <- "values"
dfab <- rbind(data.frame(df,band="1961-2017 (mean)"), data.frame(df2,band="2018"))

#manually define colors
ggplot(dfab, aes(x,y,fill=values))+geom_raster()+facet_grid(.~band)+
  scale_fill_gradient2(low="#FFFFFF", mid="#EC0001", high="#720000", midpoint=17.1, 
                       name ="August \nDegree Celsius",
                       na.value = NA, limits=c(16,19.1),guide="legend")+
  labs(x="",y="")+
  ggtitle("Average Temperature")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=11))+
  theme(legend.title = element_text(size = 8))+
  theme(legend.text = element_text(size = 7))+
  theme(axis.text.y = element_text(angle=90))+
  scale_y_continuous(breaks = seq(5400000,6000000,200000))+
  xlab("")+
  ylab("")+
  coord_equal()
dev.off()

library(RColorBrewer)

color_regime <- brewer.pal(5,"Reds")
dev.off()

raster_diff <- rasterComp - rasterHist_mean

p3 <- ggR(raster_diff, geom_raster = T)+
  scale_fill_gradient2(low="blue", mid='yellow', high="red", name ="temp. diff.", na.value = NA)+
  labs(x="",y="")+
  ggtitle("Temperature Differences")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15))+
  theme(legend.title = element_text(size = 12, face = "bold"))+
  theme(legend.text = element_text(size = 10))+
  theme(axis.text.y = element_text(angle=90))+
  scale_y_continuous(breaks = seq(5400000,6000000,200000))+
  xlab("")+
  ylab("")

grid.arrange(p1, p2, p3, ncol=3)

#################################
### Create a time Series plot ###
#################################

my_raster@crs <- sp::CRS(my_crs)

my_years <- c(seq(1881, 2018, by=1)) 
my_mat <- matrix(data = NA, nrow = length(my_years), ncol = 2)
my_mat[,1] <- my_years
my_df <- data.frame(my_mat)
names(my_df) <- c("Year", "Mean_Temp")

for (i in 1:length(my_years)){
  current_layer <- my_raster[[i]] 
  current_mean <- mean(current_layer@data@values, na.rm=T)
  my_df[i,2] <- current_mean/10 
  rm(current_layer, current_mean, i) 
}

ggplot(my_df, aes(x=Year, y=Mean_Temp))+ 
  geom_point(size=2)+
  geom_line()+
  geom_smooth(method="loess", se=TRUE, formula= y ~ x)+ 
  labs(title="Time Series of Mean Temperature Across Germany in August", 
       x="Year", y="Mean Temperature in ?C") +
  theme(plot.title = element_text(hjust = 0.5))

# #########
# split by region and see what the differences are
# #########

bnd <- raster::getData("GADM", country='DEU', level=1) 
bnd.utm <- spTransform(bnd, CRS(proj4string(my_raster)))
bnd.utm.by <- bnd.utm[bnd.utm$NAME_1=="Bayern",] 

my_raster.by <- crop(my_raster, bnd.utm.by)
my_raster.by <- mask(my_raster.by, bnd.utm.by)

plot(my_raster.by,1) 

for (i in 1:length(my_years)){
  current_layer <- my_raster.by[[i]]
  current_mean <- mean(getValues(current_layer), na.rm=T)
  my_df[i,2] <- current_mean/10 
  rm(current_layer, current_mean, i) 
}

my_df
ggplot(my_df, aes(x=Year, y=Mean_Temp))+ 
  geom_point(size=2)+
  geom_line()+
  geom_smooth(method="loess", se=TRUE, formula= y ~ x)+
  labs(title="Time Series of Mean Temperature Across Bavaria in August", 
       x="Year", y="Mean Temperature in ?C") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

